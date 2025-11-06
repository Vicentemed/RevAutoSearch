# 🔍 Buscador de Artículos Científicos (PubMed + Google Scholar)

Aplicación **Shiny en R** para buscar artículos científicos automáticamente en **PubMed** y **Google Scholar**, extrayendo:

- 🧾 Título  
- 📄 Resumen (Abstract)  
- 📅 Año de publicación  
- 🔍 Palabras clave utilizadas  
- 🕒 Fecha y hora de la búsqueda  

Incluye opciones para **guardar el historial** de búsquedas y **descargar los resultados** en formato **Excel (.xlsx)**.

---

## 🧰 Funcionalidades principales

| Función | Descripción |
|----------|-------------|
| **Búsqueda en PubMed** | Usa `rentrez` y operadores booleanos (AND, OR, NOT). Extrae título, año y abstract desde XML. |
| **Búsqueda en Google Scholar** | Scraping hasta 50 páginas de resultados por conjunto de palabras clave. |
| **Descarga a Excel** | Exporta los resultados actuales en formato `.xlsx`. |
| **Historial automático** | Guarda todas las búsquedas en `historial_busquedas.csv`. |
| **Interfaz interactiva** | Filtros, ordenamiento y búsqueda directa en la tabla (usando `DT`). |

---

## ⚙️ Requisitos

### 🔸 Software necesario

- **R (≥ 4.2.0)**
- **RStudio (recomendado)**
- Conexión a internet para acceder a PubMed y Google Scholar.

### 🔸 Paquetes de R

Instálalos (solo una vez):

```r
packages <- c("shiny", "rentrez", "rvest", "xml2", "openxlsx", "dplyr", "stringr", "DT")
install.packages(setdiff(packages, rownames(installed.packages())))
