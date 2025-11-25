# ============================================================
# app.R - Pipeline estable
# Google Scholar (SerpAPI) -> (EuropePMC / CrossRef / PubMed by title)
# -> PubMed overall (añade registros no encontrados en GS)
# Output CSV: title, year, doi, url, abstract, journal, source
# Keys: serp_key.txt, pubmed_key.txt, pubmed_email.txt (o variables de entorno)
# ============================================================
library(shiny)
library(httr)
library(jsonlite)
library(xml2)
library(rentrez)
library(dplyr)
library(stringr)
library(DT)
library(readr)
# -------------------- 0. Leer API keys (archivo o env) --------------------
read_key_safe <- function(path) {
  if (nzchar(Sys.getenv(path))) return(Sys.getenv(path))
  if (file.exists(path)) {
    txt <- trimws(readLines(path, warn = FALSE))
    if (length(txt) >= 1 && nzchar(txt[1])) return(txt[1])
  }
  return("")
}
# prefer environment variables names, else files
SERPAPI_KEY  <- ifelse(nzchar(Sys.getenv("SERPAPI_KEY")), Sys.getenv("SERPAPI_KEY"), read_key_safe("serp_key.txt"))
ENTREZ_KEY   <- ifelse(nzchar(Sys.getenv("ENTREZ_KEY")), Sys.getenv("ENTREZ_KEY"), read_key_safe("pubmed_key.txt"))
ENTREZ_EMAIL <- ifelse(nzchar(Sys.getenv("ENTREZ_EMAIL")), Sys.getenv("ENTREZ_EMAIL"), read_key_safe("pubmed_email.txt"))
if (nzchar(ENTREZ_KEY)) try(rentrez::set_entrez_key(ENTREZ_KEY), silent = TRUE)
if (nzchar(ENTREZ_EMAIL)) Sys.setenv(ENTREZ_EMAIL = ENTREZ_EMAIL)
# -------------------- 1. Helpers --------------------
# safe iconv + UTF-8 with proper encoding detection
# Función fix_encoding mejorada para reemplazar en app.R
# Copiar desde la línea 33 hasta la línea 80 aproximadamente

# safe iconv + UTF-8 with Mojibake repair

# VERSIÓN ULTRA-SIMPLE de fix_encoding
# Esta versión reemplaza caracteres problemáticos con equivalentes ASCII simples
# Reemplazar la función fix_encoding completa en app.R (líneas ~42-115)

fix_encoding <- function(x) {
  if (is.null(x)) return(NA_character_)
  
  out <- tryCatch({
    y <- as.character(x)
    if (is.na(y) || !nzchar(y)) return(NA_character_)
    
    # Remove non-breaking spaces
    y <- gsub("\u00A0", " ", y, fixed = TRUE)
    
    # Estrategia simple: usar iconv con transliteración
    # Esto convierte caracteres especiales a sus equivalentes ASCII
    y_trans <- iconv(y, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "?")
    
    # Si la transliteración funcionó, usarla
    if (!is.na(y_trans) && nzchar(y_trans)) {
      y <- y_trans
    }
    
    # Limpiar caracteres de reemplazo
    y <- gsub("?", "", y, fixed = TRUE)
    
    # Normalizar espacios múltiples
    y <- gsub("\\s+", " ", y)
    
    Encoding(y) <- "UTF-8"
    trimws(y)
  }, error = function(e) {
    y <- as.character(x)
    Encoding(y) <- "UTF-8"
    y
  })
  
  out
}

# normalizar títulos para comparar (lowercase, quitar puntuación, tags, corchetes)
normalize_title <- function(x) {
  if (is.null(x) || !nzchar(as.character(x))) return(NA_character_)
  y <- as.character(x)
  y <- gsub("<[^>]+>", " ", y)
  y <- gsub("\\[.*?\\]", " ", y)
  y <- gsub("['`´]", "'", y)
  y <- iconv(y, to = "ASCII//TRANSLIT", sub = " ")
  y <- tolower(y)
  y <- gsub("[^a-z0-9 ]+", " ", y)
  y <- gsub("\\s+", " ", y)
  trimws(y)
}
# safe HTTP GET with user agent and timeout
safe_GET <- function(url, query = list(), timeout_sec = 20) {
  tryCatch({
    resp <- httr::GET(url, query = query, httr::user_agent("R (Shiny) - RevAutoSearch"), httr::timeout(timeout_sec))
    if (httr::status_code(resp) != 200) return(NULL)
    resp
  }, error = function(e) NULL)
}
# Función safe_json mejorada
# Reemplazar líneas 138-143 en app.R

safe_json <- function(resp) {
  if (is.null(resp)) return(NULL)
  # Leer como raw bytes y decodificar manualmente
  txt <- tryCatch({
    raw_content <- httr::content(resp, "raw")
    rawToChar(raw_content)
  }, error = function(e) NULL)
  if (is.null(txt) || !nzchar(txt)) return(NULL)
  tryCatch(jsonlite::fromJSON(txt, flatten = TRUE), error = function(e) NULL)
}
# extrae campo de item SerpAPI sin provocar $ on atomic
safe_item_field <- function(item, field) {
  if (is.null(item)) return(NA_character_)
  # if item is atomic string (rare), return NA
  if (!is.list(item)) return(NA_character_)
  val <- item[[field]]
  if (is.null(val)) return(NA_character_)
  # Ensure scalar return
  v <- as.character(val)
  if (length(v) > 1) v <- v[1]
  v
}
# -------------------- 2. Parse SerpAPI results robustamente --------------------
parse_serpapi_results <- function(json_raw, log_fun = NULL) {
  if (is.null(json_raw)) return(tibble::tibble())
  
  items <- NULL
  if (!is.null(json_raw$organic_results)) items <- json_raw$organic_results
  if (is.null(items) && !is.null(json_raw$scholar_results)) items <- json_raw$scholar_results
  
  if (is.null(items) || length(items) == 0) return(tibble::tibble())
  
  # CRITICAL FIX: jsonlite::fromJSON con flatten=TRUE devuelve un data.frame si la estructura es regular.
  # Si items es un data.frame, iterar seq_along(items) itera sobre COLUMNAS, no filas.
  # Debemos asegurar que iteramos sobre ITEMS (filas).
  
  list_items <- list()
  if (is.data.frame(items)) {
    # Convertir filas de data.frame a lista de listas para procesar uniformemente
    # split(items, seq(nrow(items))) es una forma, pero lenta.
    # Mejor iterar por índice de fila.
    for (i in seq_len(nrow(items))) {
      list_items[[i]] <- as.list(items[i, , drop = FALSE])
      # Nota: al convertir a lista una fila de DF, los elementos que eran listas (como resources) se mantienen.
      # Pero jsonlite a veces simplifica vectores.
    }
  } else {
    list_items <- items
  }
  
  if (!is.null(log_fun)) {
    log_fun(paste("DEBUG: items es", class(items)[1], "con", ifelse(is.data.frame(items), nrow(items), length(items)), "elementos"))
    log_fun(paste("DEBUG: list_items tiene", length(list_items), "elementos"))
  }
  # Helper para detectar títulos sospechosos (dominios, PDF, etc.)
  is_suspicious <- function(t) {
    if (is.na(t)) return(TRUE)
    t_low <- tolower(t)
    if (nchar(t) < 5) return(TRUE)
    if (grepl("\\.(com|org|net|edu|gov|io|pdf|html)$", t_low)) return(TRUE)
    if (t_low %in% c("pdf", "html", "full view", "full text", "download")) return(TRUE)
    FALSE
  }
  # Helper para extraer info de un item individual (sea lista o dataframe row)
  extract_info <- function(it) {
    if (!is.list(it)) return(NULL)
    
    # Intentar extraer título
    title_raw <- NA_character_
    
    # Prioridad 1: result_title (suele ser el título real en SerpAPI scholar)
    if (!is.null(it$title)) title_raw <- safe_item_field(it, "title")
    
    # Si el título extraído es sospechoso, intentar buscar en 'result_title' o hermanos
    if (is_suspicious(title_raw)) {
       if (!is.null(it$result_title)) {
         title_raw <- safe_item_field(it, "result_title")
       } else {
         # Si este item no tiene título bueno, quizás es un link de acceso (e.g. [PDF] thelancet.com)
         # En la estructura anidada, a veces el título real está en un objeto hermano en la misma lista
         # Pero aquí estamos procesando un item individual. Si es basura, lo descartamos.
         # Sin embargo, a veces SerpAPI pone el título en 'title' pero es el nombre del archivo.
         # Vamos a ser estrictos: si sigue siendo sospechoso, lo marcamos como NA para que el loop superior busque en sub-items si es lista
         if (is_suspicious(title_raw)) title_raw <- NA_character_
       }
    }
    
    # Si no hay título, no es un resultado válido
    if (is.na(title_raw) || !nzchar(title_raw)) return(NULL)
    snippet <- NA_character_
    if (!is.null(it$snippet)) snippet <- safe_item_field(it, "snippet")
    if ((is.na(snippet) || !nzchar(snippet)) && !is.null(it$description)) snippet <- safe_item_field(it, "description")
    
    link <- NA_character_
    if (!is.null(it$link)) link <- safe_item_field(it, "link")
    
    year_raw <- NA_character_
    if (!is.null(it$publication_info) && is.list(it$publication_info)) {
      pubinfo <- it$publication_info
      if (!is.null(pubinfo$year)) year_raw <- as.character(pubinfo$year)
      else if (!is.null(pubinfo$summary)) {
        yr <- str_extract(as.character(pubinfo$summary), "\\b(19|20)\\d{2}\\b")
        if (!is.na(yr)) year_raw <- yr
      }
    }
    
    list(
      title_raw = fix_encoding(title_raw),
      snippet = fix_encoding(snippet),
      link = fix_encoding(link),
      year_raw = as.character(year_raw)
    )
  }
  rows <- list()
  
  # SerpAPI a veces devuelve una lista de listas, donde cada elemento principal puede contener sub-listas
  # Iteramos y aplanamos si es necesario
  for (i in seq_along(list_items)) {
    it <- list_items[[i]]
    
    if (!is.null(log_fun) && i <= 3) {  # Solo log primeros 3 items para no saturar
      log_fun(paste("DEBUG: Item", i, "- tipo:", class(it)[1], "- campos:", paste(names(it), collapse=", ")))
    }
    
    # Estrategia: Si 'it' es una lista sin nombres (un grupo de cosas), iterar sobre sus elementos.
    # Si 'it' es un objeto con nombres (un item directo), procesarlo.
    
    candidates <- list()
    if (is.list(it) && is.null(names(it))) {
      candidates <- it
    } else {
      candidates <- list(it)
    }
    
    # Buscar el mejor candidato dentro de este grupo
    best_res <- NULL
    
    for (cand in candidates) {
      res <- extract_info(cand)
      if (!is.null(res)) {
        # Si encontramos un resultado válido, verificamos si es mejor que lo que tenemos
        # Un resultado es "mejor" si su título NO es sospechoso.
        # Si extract_info devuelve algo, ya pasó el filtro básico de NA, pero extract_info
        # devuelve el título tal cual. Verifiquemos si es un dominio.
        
        # Re-verificar sospecha sobre el título extraído
        t_check <- res$title_raw
        if (!is_suspicious(t_check)) {
          # Es un buen título, nos quedamos con este y paramos de buscar en este grupo
          best_res <- res
          break 
        } else {
          # Es un título sospechoso (dominio), lo guardamos por si no encontramos nada mejor
          if (is.null(best_res)) best_res <- res
        }
      }
    }
    
    if (!is.null(best_res)) {
      rows[[length(rows) + 1]] <- best_res
    }
  }
  if (length(rows) == 0) return(tibble::tibble())
  
  df <- dplyr::bind_rows(rows)
  df <- df %>% mutate(title_clean = sapply(title_raw, function(t) ifelse(is.na(t), NA_character_, normalize_title(t))))
  df
}
# -------------------- 3. Search Google Scholar via SerpAPI --------------------
gs_search_serpapi <- function(query, max_pages = 3, sleep = 1.0, log_fun = NULL) {
  if (!nzchar(SERPAPI_KEY)) {
    if (!is.null(log_fun)) log_fun("ERROR: SERPAPI_KEY no configurada.")
    return(dplyr::tibble())
  }
  all_pages <- list()
  for (p in seq_len(max_pages)) {
    start <- (p - 1) * 10
    url <- "https://serpapi.com/search.json"
    q <- list(engine = "google_scholar", q = query, start = start, api_key = SERPAPI_KEY, hl = "en")
    resp <- safe_GET(url, query = q)
    if (is.null(resp)) {
      if (!is.null(log_fun)) log_fun(paste("SerpAPI: página", p, "no respondió"))
      next
    }
    json_raw <- safe_json(resp)
    if (!is.null(log_fun)) log_fun(paste("DEBUG: SerpAPI keys root:", paste(names(json_raw), collapse=", ")))
    page_df <- parse_serpapi_results(json_raw, log_fun)
    if (nrow(page_df) > 0) all_pages[[length(all_pages) + 1]] <- page_df
    Sys.sleep(sleep)
  }
  if (length(all_pages) == 0) return(dplyr::tibble())
  out <- dplyr::bind_rows(all_pages)
  out <- out %>% distinct(title_clean, .keep_all = TRUE)
  out
}
# -------------------- 4. Resolve title -> abstract & metadata (EuropePMC -> CrossRef -> PubMed) --------------------
query_europepmc_by_title <- function(title) {
  if (is.na(title) || title == "") return(NULL)
  url <- "https://www.ebi.ac.uk/europepmc/webservices/rest/search"
  q <- paste0('TITLE:"', title, '"')
  resp <- safe_GET(url, query = list(query = q, format = "json", pageSize = 1))
  j <- safe_json(resp)
  if (is.null(j) || !is.list(j) || is.null(j$resultList) || !is.list(j$resultList) || is.null(j$resultList$result) || length(j$resultList$result) == 0) return(NULL)
  it <- j$resultList$result[[1]]
  if (!is.list(it)) return(NULL)
  list(
    abstract = if (!is.null(it$abstractText) && nzchar(it$abstractText)) fix_encoding(it$abstractText) else NA_character_,
    doi = if (!is.null(it$doi)) as.character(it$doi) else NA_character_,
    journal = if (!is.null(it$journalTitle)) as.character(it$journalTitle) else NA_character_,
    year = if (!is.null(it$firstPublicationDate)) substr(it$firstPublicationDate, 1, 4) else NA_character_,
    url = if (!is.null(it$fullTextUrlList$fullTextUrl[[1]]$url)) as.character(it$fullTextUrlList$fullTextUrl[[1]]$url) else NA_character_,
    source = "EuropePMC"
  )
}
query_crossref_by_title <- function(title) {
  if (is.na(title) || title == "") return(NULL)
  url <- "https://api.crossref.org/works"
  resp <- safe_GET(url, query = list("query.title" = title, rows = 1))
  j <- safe_json(resp)
  if (is.null(j) || !is.list(j) || is.null(j$message) || !is.list(j$message) || is.null(j$message$items) || length(j$message$items) == 0) return(NULL)
  it <- j$message$items[[1]]
  if (!is.list(it)) return(NULL)
  abs <- NA_character_
  if (!is.null(it$abstract)) abs <- clean_crossref <- gsub("<[^>]+>", " ", it$abstract)
  doi <- if (!is.null(it$DOI)) as.character(it$DOI) else NA_character_
  journal <- if (!is.null(it$`container-title`) && length(it$`container-title`) > 0) as.character(it$`container-title`[[1]]) else NA_character_
  url_res <- if (!is.null(it$URL)) as.character(it$URL) else NA_character_
  year <- NA_character_
  if (!is.null(it$`published-print`) && !is.null(it$`published-print`$`date-parts`[[1]])) {
    year <- as.character(it$`published-print`$`date-parts`[[1]][1])
  } else if (!is.null(it$`published-online`) && !is.null(it$`published-online`$`date-parts`[[1]])) {
    year <- as.character(it$`published-online`$`date-parts`[[1]][1])
  }
  list(abstract = ifelse(!is.na(abs), fix_encoding(abs), NA_character_), doi = doi, journal = journal, year = year, url = url_res, source = "CrossRef")
}
query_pubmed_by_title <- function(title) {
  if (is.na(title) || title == "") return(NULL)
  term <- paste0('"', title, '"[Title]')
  res <- tryCatch(rentrez::entrez_search(db = "pubmed", term = term, retmax = 5), error = function(e) NULL)
  if (is.null(res) || length(res$ids) == 0) return(NULL)
  id <- res$ids[1]
  xml_data <- tryCatch(rentrez::entrez_fetch(db = "pubmed", id = id, rettype = "xml", retmode = "xml"), error = function(e) NULL)
  if (is.null(xml_data)) return(NULL)
  parsed <- tryCatch(read_xml(xml_data), error = function(e) NULL)
  if (is.null(parsed)) return(NULL)
  art <- xml_find_first(parsed, ".//PubmedArticle")
  if (is.null(art)) return(NULL)
  abs_nodes <- xml_find_all(art, ".//Abstract/AbstractText")
  abstract <- if (length(abs_nodes) == 0) NA_character_ else paste(xml_text(abs_nodes), collapse = " ")
  if (!is.null(log_fun)) log_fun(paste("  -> CrossRef:", title_raw))
  cr <- tryCatch(query_crossref_by_title(title_raw), error = function(e) { if (!is.null(log_fun)) log_fun(paste("CrossRef err:", e$message)); NULL })
  if (!is.null(cr) && !is.na(cr$abstract) && nzchar(cr$abstract)) return(cr)
  Sys.sleep(0.4)
  if (!is.null(log_fun)) log_fun(paste("  -> PubMed (title):", title_raw))
  pm <- tryCatch(query_pubmed_by_title(title_raw), error = function(e) { if (!is.null(log_fun)) log_fun(paste("PubMed title err:", e$message)); NULL })
  if (!is.null(pm) && !is.na(pm$abstract) && nzchar(pm$abstract)) return(pm)
  NULL
}

query_semanticscholar_by_title <- function(title) {
  if (is.na(title) || title == "") return(NULL)
  
  # Semantic Scholar search endpoint
  url <- "https://api.semanticscholar.org/graph/v1/paper/search"
  
  # Buscar por título
  resp <- safe_GET(url, query = list(
    query = title,
    limit = 1,
    fields = "title,abstract,year,authors,venue,externalIds,url"
  ))
  
  j <- safe_json(resp)
  if (is.null(j) || !is.list(j) || is.null(j$data) || length(j$data) == 0) return(NULL)
  
  paper <- j$data[[1]]
  if (!is.list(paper)) return(NULL)
  
  # Extraer abstract
  abstract <- if (!is.null(paper$abstract) && nzchar(paper$abstract)) {
    fix_encoding(paper$abstract)
  } else {
    NA_character_
  }
  
  # Extraer DOI si está disponible
  doi <- NA_character_
  if (!is.null(paper$externalIds) && !is.null(paper$externalIds$DOI)) {
    doi <- as.character(paper$externalIds$DOI)
  }
  
  # Extraer journal/venue
  journal <- if (!is.null(paper$venue) && nzchar(paper$venue)) {
    as.character(paper$venue)
  } else {
    NA_character_
  }
  
  # Extraer año
  year <- if (!is.null(paper$year)) as.character(paper$year) else NA_character_
  
  # URL
  url_paper <- if (!is.null(paper$url) && nzchar(paper$url)) {
    as.character(paper$url)
  } else if (!is.na(doi) && nzchar(doi)) {
    paste0("https://doi.org/", doi)
  } else {
    NA_character_
  }
  
  list(
    abstract = abstract,
    doi = doi,
    journal = journal,
    year = year,
    url = url_paper,
    source = "SemanticScholar"
  )
}

resolve_title_to_metadata <- function(title_raw, title_norm, log_fun = NULL) {
  # 1. EuropePMC
  if (!is.null(log_fun)) log_fun(paste("  -> EuropePMC:", title_raw))
  epmc <- tryCatch(query_europepmc_by_title(title_raw), error = function(e) { 
    if (!is.null(log_fun)) log_fun(paste("EPMC err:", e$message)); NULL 
  })
  if (!is.null(epmc) && !is.na(epmc$abstract) && nzchar(epmc$abstract)) return(epmc)
  Sys.sleep(0.4)
  
  # 2. Semantic Scholar (NUEVO)
  if (!is.null(log_fun)) log_fun(paste("  -> Semantic Scholar:", title_raw))
  ss <- tryCatch(query_semanticscholar_by_title(title_raw), error = function(e) { 
    if (!is.null(log_fun)) log_fun(paste("SemanticScholar err:", e$message)); NULL 
  })
  if (!is.null(ss) && !is.na(ss$abstract) && nzchar(ss$abstract)) return(ss)
  Sys.sleep(0.4)
  
  # 3. CrossRef
  if (!is.null(log_fun)) log_fun(paste("  -> CrossRef:", title_raw))
  cr <- tryCatch(query_crossref_by_title(title_raw), error = function(e) { 
    if (!is.null(log_fun)) log_fun(paste("CrossRef err:", e$message)); NULL 
  })
  if (!is.null(cr) && !is.na(cr$abstract) && nzchar(cr$abstract)) return(cr)
  Sys.sleep(0.4)
  
  # 4. PubMed
  if (!is.null(log_fun)) log_fun(paste("  -> PubMed (title):", title_raw))
  pm <- tryCatch(query_pubmed_by_title(title_raw), error = function(e) { 
    if (!is.null(log_fun)) log_fun(paste("PubMed title err:", e$message)); NULL 
  })
  if (!is.null(pm) && !is.na(pm$abstract) && nzchar(pm$abstract)) return(pm)
  
  NULL
}

# -------------------- 5. PubMed direct search (by keywords) --------------------
pubmed_search_direct <- function(query, retmax = 200, log_fun = NULL) {
  empty_res <- tibble::tibble(
    title = character(), year = character(), doi = character(), url = character(),
    abstract = character(), journal = character(), source = character(), title_norm = character()
  )
  res <- tryCatch(rentrez::entrez_search(db = "pubmed", term = query, retmax = retmax), error = function(e) {
    if (!is.null(log_fun)) log_fun(paste("Error en entrez_search:", e$message))
    NULL
  })
  if (is.null(res)) return(empty_res)
  if (!is.null(log_fun)) log_fun(paste("PubMed direct: encontrados", length(res$ids), "IDs para la consulta."))
  if (length(res$ids) == 0) return(empty_res)
  ids <- res$ids
  batch_size <- 100
  records <- list()
  for (i in seq(1, length(ids), by = batch_size)) {
    batch <- ids[i:min(i+batch_size-1, length(ids))]
    xml_data <- tryCatch(rentrez::entrez_fetch(db = "pubmed", id = batch, rettype = "xml", retmode = "xml"), error = function(e) NULL)
    if (is.null(xml_data)) next
    parsed <- tryCatch(read_xml(xml_data), error = function(e) NULL)
    if (is.null(parsed)) next
    arts <- xml_find_all(parsed, ".//PubmedArticle")
    for (a in arts) {
      t <- fix_encoding(xml_text(xml_find_first(a, ".//ArticleTitle")))
      abs_nodes <- xml_find_all(a, ".//Abstract/AbstractText")
      abstract_pm <- if (length(abs_nodes) == 0) NA_character_ else fix_encoding(paste(xml_text(abs_nodes), collapse = " "))
      doi_node <- xml_find_first(a, ".//ArticleIdList/ArticleId[@IdType='doi']")
      doi_pm <- if (is.na(doi_node) || is.null(doi_node)) NA_character_ else xml_text(doi_node)
      journal_pm <- fix_encoding(xml_text(xml_find_first(a, ".//Journal/Title")))
      year_pm <- xml_text(xml_find_first(a, ".//PubDate/Year"))
      url_pm <- if (!is.na(doi_pm) && nzchar(doi_pm)) paste0("https://doi.org/", doi_pm) else NA_character_
      records[[length(records)+1]] <- tibble::tibble(
        title = t,
        year = as.character(year_pm),
        doi = as.character(doi_pm),
        url = as.character(url_pm),
        abstract = as.character(abstract_pm),
        journal = as.character(journal_pm),
        source = "PubMed",
        title_norm = normalize_title(t)
      )
    }
    Sys.sleep(0.8)
  }
  if (length(records) == 0) return(dplyr::tibble())
  dplyr::bind_rows(records)
}
# -------------------- 6. Shiny UI --------------------
ui <- fluidPage(
  titlePanel("RevAutoSearch - Scholar -> EuropePMC/CrossRef/PubMed -> PubMed faltantes"),
  sidebarLayout(
    sidebarPanel(
      textInput("query", "Palabras clave (una sola consulta):", value = ""),
      numericInput("gs_pages", "Páginas SerpAPI (10 resultados/pág):", value = 3, min = 1, max = 20),
      numericInput("pubmed_max", "Máx. resultados PubMed (retmax):", value = 200, min = 10, max = 2000),
      sliderInput("year_min", "Año mínimo:", min = 1900, max = as.integer(format(Sys.Date(), "%Y")), value = 2000),
      actionButton("run", "Ejecutar pipeline"),
      br(), br(),
      downloadButton("download_csv", "Descargar CSV (abstracts completos)"),
      br(), br(),
      verbatimTextOutput("log")
    ),
    mainPanel(
      DTOutput("results_table"),
      br(),
      uiOutput("progress_ui")
    )
  )
)
# -------------------- 7. Shiny server --------------------
server <- function(input, output, session) {
  logs <- reactiveVal(character(0))
  append_log <- function(txt) {
    logs(c(logs(), paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", txt)))
    # keep last 800 lines
    if (length(logs()) > 800) logs(tail(logs(), 800))
  }
  output$log <- renderText({ paste(logs(), collapse = "\n") })
  results_store <- reactiveVal(dplyr::tibble())
  output$progress_ui <- renderUI(NULL)
  observeEvent(input$run, {
    query <- trimws(input$query)
    if (!nzchar(query)) {
      showNotification("Escribe palabras clave antes de ejecutar", type = "warning")
      append_log("Consulta vacía.")
      return()
    }
    # reset
    logs(character(0))
    results_store(dplyr::tibble())
    append_log(paste("Iniciando pipeline para:", query))
    withProgress(message = "Pipeline en ejecución...", value = 0, {
      # 1) Google Scholar via SerpAPI
      incProgress(0.05, detail = "Buscando en Scholar (SerpAPI)...")
      gs <- tryCatch(gs_search_serpapi(query, max_pages = input$gs_pages, sleep = 1.0, log_fun = append_log), error = function(e) { append_log(paste("Error GS:", e$message)); NULL })
      if (is.null(gs) || nrow(gs) == 0) {
        append_log("No se obtuvieron resultados de Google Scholar via SerpAPI.")
        gs <- dplyr::tibble()
      } else {
        append_log(paste0("Google Scholar (SerpAPI) devolvió ", nrow(gs), " items (crudos)."))
      }
      # 2) resolve title by title -> EuropePMC / CrossRef / PubMed
      records <- list()
      ngs <- max(0, nrow(gs))
      if (ngs > 0) {
        for (i in seq_len(ngs)) {
          title_raw <- gs$title_raw[i]
          title_norm <- gs$title_clean[i]
          snippet <- gs$snippet[i]
          link <- gs$link[i]
          append_log(paste0("Resolviendo (", i, "/", ngs, "): ", substr(title_raw, 1, 200)))
          md <- tryCatch(resolve_title_to_metadata(title_raw, title_norm, log_fun = append_log), error = function(e) { append_log(paste("Error resolve:", e$message)); NULL })
          if (!is.null(md)) {
            abstract_full <- ifelse(is.null(md$abstract) || !nzchar(md$abstract), snippet, md$abstract)
            doi <- ifelse(is.null(md$doi) || md$doi == "", NA_character_, md$doi)
            journal <- ifelse(is.null(md$journal) || md$journal == "", NA_character_, md$journal)
            year <- ifelse(is.null(md$year) || md$year == "", NA_character_, md$year)
            url <- ifelse(is.null(md$url) || md$url == "", link, md$url)
            source <- md$source
          } else {
            abstract_full <- snippet
            doi <- NA_character_
            journal <- NA_character_
            year <- NA_character_
            url <- link
            source <- "Scholar-snippet"
          }
          records[[length(records)+1]] <- tibble::tibble(
            title = fix_encoding(title_raw),
            year = as.character(year),
            doi = as.character(doi),
            url = as.character(url),
            abstract = fix_encoding(abstract_full),
            journal = as.character(journal),
            source = as.character(source),
            title_norm = as.character(title_norm)
          )
          incProgress(0.6 / ngs, detail = paste0("Resolviendo GS (", i, "/", ngs, ")"))
          Sys.sleep(0.2)
        }
      }
      df_gs <- if (length(records) == 0) dplyr::tibble() else dplyr::bind_rows(records)
      append_log(paste("Registros resueltos desde GS:", nrow(df_gs)))
      # 3) PubMed overall search for query (add missing)
      incProgress(0.05, detail = "Buscando PubMed (consulta global)...")
      pm_all <- tryCatch(pubmed_search_direct(query, retmax = input$pubmed_max, log_fun = append_log), error = function(e) { 
        append_log(paste("Error PubMed direct:", e$message))
        tibble::tibble(title = character(), year = character(), doi = character(), url = character(), abstract = character(), journal = character(), source = character(), title_norm = character())
      })
      append_log(paste0("PubMed (global) devolvió registros procesados: ", nrow(pm_all)))
      # 4) find PubMed records not in GS (by normalized title)
      gs_norms <- if (nrow(df_gs) > 0) unique(df_gs$title_norm) else character(0)
      pm_missing <- pm_all %>% filter(!(title_norm %in% gs_norms))
      append_log(paste("Registros PubMed no presentes en GS (se añadirán):", nrow(pm_missing)))
      # 5) For missing pm, attempt to resolve abstract via CrossRef/EPMC if NA
      if (nrow(pm_missing) > 0) {
        for (i in seq_len(nrow(pm_missing))) {
          if (is.na(pm_missing$abstract[i]) || !nzchar(pm_missing$abstract[i])) {
            t_i <- pm_missing$title[i]
            append_log(paste("Resolviendo abstract faltante PubMed para:", substr(t_i, 1, 150)))
            md_res <- tryCatch(resolve_title_to_metadata(t_i, normalize_title(t_i), log_fun = append_log), error = function(e) { append_log(paste("Error resolve missing:", e$message)); NULL })
            if (!is.null(md_res) && !is.na(md_res$abstract) && nzchar(md_res$abstract)) {
              pm_missing$abstract[i] <- fix_encoding(md_res$abstract)
              if (is.na(pm_missing$doi[i]) || !nzchar(pm_missing$doi[i])) pm_missing$doi[i] <- md_res$doi
              if ((is.na(pm_missing$journal[i]) || !nzchar(pm_missing$journal[i])) && !is.null(md_res$journal)) pm_missing$journal[i] <- md_res$journal
              if ((is.na(pm_missing$year[i]) || !nzchar(pm_missing$year[i])) && !is.null(md_res$year)) pm_missing$year[i] <- md_res$year
              if (is.na(pm_missing$url[i]) || !nzchar(pm_missing$url[i])) pm_missing$url[i] <- md_res$url
            }
            Sys.sleep(0.3)
          }
        }
      }
      # 6) assemble final dataset
      # Definir estructura vacía base para asegurar columnas
      empty_final <- tibble::tibble(
        title = character(), year = character(), doi = character(), url = character(),
        abstract = character(), journal = character(), source = character()
      )
      if (nrow(df_gs) == 0 && nrow(pm_missing) == 0) {
        final_df <- empty_final
      } else if (nrow(df_gs) == 0) {
        final_df <- pm_missing %>% select(any_of(names(empty_final)))
      } else if (nrow(pm_missing) == 0) {
        final_df <- df_gs %>% select(any_of(names(empty_final)))
      } else {
        final_df <- dplyr::bind_rows(
          df_gs %>% select(any_of(names(empty_final))),
          pm_missing %>% select(any_of(names(empty_final)))
        )
      }
      
      # Asegurar que todas las columnas existan (rellenar con NA si falta alguna)
      for (col in names(empty_final)) {
        if (!col %in% names(final_df)) final_df[[col]] <- NA_character_
      }
      # year filter
      final_df <- final_df %>% mutate(year_num = suppressWarnings(as.numeric(year))) %>%
        filter(is.na(year_num) | year_num >= input$year_min) %>%
        select(-year_num)
      # ensure UTF-8 and remove empty strings -> NA
      # Use vectorized logic properly. is.na(x) is vectorized.
      final_df <- final_df %>% mutate(across(everything(), ~ {
        x <- .x
        # Convert to character if not already
        if (!is.character(x)) x <- as.character(x)
        # Fix encoding and trim
        x <- fix_encoding(x)
        # Empty strings to NA
        ifelse(!nzchar(x) | is.na(x), NA_character_, x)
      }))
      results_store(final_df)
      append_log(paste("Pipeline completado. Registros finales:", nrow(final_df)))
      incProgress(1, detail = "Completado")
    }) # withProgress end
  }, ignoreNULL = TRUE)
  output$results_table <- renderDT({
    df <- results_store()
    if (is.null(df) || nrow(df) == 0) {
      datatable(data.frame(msg = "No hay resultados"), options = list(dom = 't'))
    } else {
      datatable(df, filter = "top", options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    }
  })
  output$download_csv <- downloadHandler(
    filename = function() paste0("results_full_abstracts_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- results_store()
      if (is.null(df) || nrow(df) == 0) {
        readr::write_csv(dplyr::tibble(), file)
      } else {
        # write UTF-8 CSV
        readr::write_csv(df, file)
      }
    }
  )
  output$log <- renderText({ paste(logs(), collapse = "\n") })
}
# -------------------- 8. Run app --------------------
shinyApp(ui, server)