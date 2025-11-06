# ============================================================
# Shiny App: Buscador de Artículos Científicos
# Fuentes: PubMed + Google Scholar
# Versión final (corrige error bind_rows Fecha_busqueda)
# ============================================================

library(shiny)
library(rentrez)
library(rvest)
library(xml2)
library(openxlsx)
library(dplyr)
library(stringr)
library(DT)

# -------- CONFIGURACIÓN -------- #
Sys.setenv(ENTREZ_EMAIL = "tu_correo@ejemplo.com")
# Sys.setenv(ENTREZ_KEY = "TU_API_KEY")  # opcional

# -------- FUNCIONES -------- #

# --- PubMed (con extracción XML) ---
buscar_pubmed <- function(palabras_clave, max_resultados = 100) {
  if (nchar(palabras_clave) == 0) return(data.frame())
  
  search <- tryCatch({
    entrez_search(db = "pubmed", term = palabras_clave, retmax = max_resultados)
  }, error = function(e) NULL)
  
  if (is.null(search) || length(search$ids) == 0) return(data.frame())
  
  ids <- search$ids
  resultados <- data.frame()
  batch_size <- 50
  
  for (i in seq(1, length(ids), by = batch_size)) {
    batch <- ids[i:min(i + batch_size - 1, length(ids))]
    
    xml_data <- tryCatch({
      entrez_fetch(db = "pubmed", id = batch, rettype = "xml", retmode = "xml")
    }, error = function(e) NULL)
    
    if (is.null(xml_data)) next
    parsed <- tryCatch(read_xml(xml_data), error = function(e) NULL)
    if (is.null(parsed)) next
    
    articles <- xml_find_all(parsed, ".//PubmedArticle")
    
    df <- data.frame(
      Fuente = "PubMed",
      Titulo = xml_text(xml_find_all(articles, ".//ArticleTitle")),
      Año = xml_text(xml_find_all(articles, ".//PubDate/Year")),
      Abstract = sapply(articles, function(a) {
        abs <- xml_find_all(a, ".//Abstract/AbstractText")
        if (length(abs) == 0) return(NA)
        paste(xml_text(abs), collapse = " ")
      }),
      Palabras_clave = palabras_clave,
      Fecha_busqueda = as.character(Sys.time()), # <- forzamos texto
      stringsAsFactors = FALSE
    )
    
    resultados <- rbind(resultados, df)
    Sys.sleep(1)
  }
  
  resultados$Año[resultados$Año == ""] <- NA
  resultados
}

# --- Google Scholar ---
buscar_google_scholar <- function(palabras_clave, max_paginas = 5) {
  if (nchar(palabras_clave) == 0) return(data.frame())
  
  base_url <- "https://scholar.google.com/scholar?hl=en&q="
  query <- URLencode(palabras_clave)
  resultados <- data.frame()
  
  for (i in 0:(max_paginas - 1)) {
    start <- i * 10
    url <- paste0(base_url, query, "&start=", start)
    
    page <- tryCatch(read_html(url), error = function(e) NULL)
    if (is.null(page)) next
    
    titles <- page %>% html_elements(".gs_rt") %>% html_text(trim = TRUE)
    snippets <- page %>% html_elements(".gs_rs") %>% html_text(trim = TRUE)
    years <- page %>%
      html_elements(".gs_a") %>%
      html_text(trim = TRUE) %>%
      str_extract("[0-9]{4}")
    
    df <- data.frame(
      Fuente = "Google Scholar",
      Titulo = titles,
      Año = years,
      Abstract = snippets,
      Palabras_clave = palabras_clave,
      Fecha_busqueda = as.character(Sys.time()), # <- forzamos texto
      stringsAsFactors = FALSE
    )
    
    resultados <- rbind(resultados, df)
    Sys.sleep(1.5)
  }
  
  return(resultados)
}

# -------- HISTORIAL -------- #

guardar_en_historial <- function(nuevos_resultados, archivo = "historial_busquedas.csv") {
  nuevos_resultados$Fecha_busqueda <- as.character(nuevos_resultados$Fecha_busqueda)
  
  if (!file.exists(archivo)) {
    write.csv(nuevos_resultados, archivo, row.names = FALSE)
  } else {
    historial <- read.csv(archivo, stringsAsFactors = FALSE)
    historial$Fecha_busqueda <- as.character(historial$Fecha_busqueda)
    combinado <- bind_rows(historial, nuevos_resultados)
    write.csv(combinado, archivo, row.names = FALSE)
  }
}

cargar_historial <- function(archivo = "historial_busquedas.csv") {
  if (file.exists(archivo)) {
    read.csv(archivo, stringsAsFactors = FALSE)
  } else {
    data.frame()
  }
}

# -------- INTERFAZ SHINY -------- #

ui <- fluidPage(
  titlePanel("Buscador de Artículos Científicos (PubMed + Google Scholar)"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("keywords_scholar", "Palabras clave (Google Scholar):", 
                    placeholder = "Ejemplo: orthodontic aligners accuracy\northodontic tooth movement prediction"),
      numericInput("pages", "Número de páginas (Google Scholar):", value = 5, min = 1, max = 50),
      br(),
      textAreaInput("keywords_pubmed", "Palabras clave booleanas (PubMed):", 
                    placeholder = "(orthodontic aligners) AND (accuracy OR predict*)"),
      numericInput("max_results", "Máximo de resultados (PubMed):", value = 100, min = 10, max = 1000),
      br(),
      actionButton("buscar", "Ejecutar búsqueda", class = "btn-primary"),
      br(), br(),
      downloadButton("descargar", "⬇️ Descargar resultados actuales (Excel)"),
      br(), br(),
      h5("📁 Historial guardado en: historial_busquedas.csv"),
      actionButton("ver_historial", "Ver historial completo")
    ),
    
    mainPanel(
      h4("Resultados combinados:"),
      DTOutput("tabla_interactiva")
    )
  )
)

# -------- SERVIDOR SHINY -------- #

server <- function(input, output, session) {
  resultados_globales <- reactiveVal(data.frame())
  
  observeEvent(input$buscar, {
    palabras_google <- unlist(strsplit(input$keywords_scholar, "\n"))
    palabras_pubmed <- unlist(strsplit(input$keywords_pubmed, "\n"))
    
    todos_resultados <- data.frame()
    
    withProgress(message = "Buscando artículos...", value = 0, {
      
      # Google Scholar
      if (length(palabras_google) > 0) {
        for (kw in palabras_google) {
          incProgress(0.4 / length(palabras_google), detail = paste("Google Scholar:", kw))
          res <- buscar_google_scholar(kw, max_paginas = input$pages)
          todos_resultados <- rbind(todos_resultados, res)
        }
      }
      
      # PubMed
      if (length(palabras_pubmed) > 0) {
        for (kw in palabras_pubmed) {
          incProgress(0.6 / length(palabras_pubmed), detail = paste("PubMed:", kw))
          res <- buscar_pubmed(kw, max_resultados = input$max_results)
          todos_resultados <- rbind(todos_resultados, res)
        }
      }
    })
    
    if (nrow(todos_resultados) > 0) {
      guardar_en_historial(todos_resultados)
    }
    
    resultados_globales(todos_resultados)
  })
  
  observeEvent(input$ver_historial, {
    historial <- cargar_historial()
    resultados_globales(historial)
  })
  
  output$tabla_interactiva <- renderDT({
    df <- resultados_globales()
    if (nrow(df) == 0) return(NULL)
    
    datatable(df, 
              filter = "top", 
              options = list(pageLength = 10, scrollX = TRUE), 
              rownames = FALSE)
  })
  
  output$descargar <- downloadHandler(
    filename = function() {
      paste0("resultados_articulos_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write.xlsx(resultados_globales(), file, overwrite = TRUE)
    }
  )
}

# -------- EJECUTAR APP -------- #
shinyApp(ui = ui, server = server)
