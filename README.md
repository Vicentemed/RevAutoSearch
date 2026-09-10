# RevAutoSearch - Automated Review & Abstract Gatherer

[![R](https://img.shields.io/badge/R-4.0+-blue.svg)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-1.7+-green.svg)](https://shiny.rstudio.com/)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

**RevAutoSearch** is a powerful Shiny application for automated academic literature search and abstract retrieval. It combines multiple academic databases (Google Scholar, EuropePMC, Semantic Scholar, CrossRef, and PubMed) to provide comprehensive metadata and complete abstracts for biomedical and scientific research.

## 🎯 Features

- **Multi-Source Search**: Integrates 5 major academic databases
  - Google Scholar (via SerpAPI)
  - EuropePMC
  - Semantic Scholar
  - CrossRef
  - PubMed/NCBI Entrez

- **Complete Abstract Retrieval**: Cascading API calls to maximize abstract completeness
- **Smart Encoding Handling**: Automatic UTF-8 encoding correction for special characters
- **Deduplication**: Intelligent title normalization to avoid duplicates
- **Year Filtering**: Filter results by publication year
- **CSV Export**: Download complete results with all metadata
- **Real-time Logs**: Monitor search progress and API calls

## 📋 Prerequisites

- R (>= 4.0)
- RStudio (recommended)
- API Keys:
  - **SerpAPI** (for Google Scholar) - [Get key](https://serpapi.com/)
  - **NCBI Entrez** (optional, for PubMed) - [Get key](https://www.ncbi.nlm.nih.gov/account/)

## 🚀 Installation

### 1. Clone the Repository

```bash
git clone https://github.com/yourusername/RevAutoSearch.git
cd RevAutoSearch
```

### 2. Install Required R Packages

```r
install.packages(c(
  "shiny",
  "httr",
  "jsonlite",
  "xml2",
  "rentrez",
  "dplyr",
  "stringr",
  "DT",
  "readr"
))
```

### 3. Configure API Keys

Create the following text files in the project directory:

**serp_key.txt**
```
your_serpapi_key_here
```

**pubmed_key.txt** (optional)
```
your_ncbi_entrez_key_here
```

**pubmed_email.txt** (optional)
```
your_email@example.com
```

Alternatively, set environment variables:
```r
Sys.setenv(SERPAPI_KEY = "your_key")
Sys.setenv(ENTREZ_KEY = "your_key")
Sys.setenv(ENTREZ_EMAIL = "your_email")
```

## 💻 Usage

### Running the Application

```r
library(shiny)
runApp("path/to/RevAutoSearch")
```

Or from RStudio:
1. Open `app.R`
2. Click "Run App"

### Search Workflow

1. **Enter Keywords**: Type your search query (e.g., "biomarkers alzheimer disease")
2. **Configure Parameters**:
   - SerpAPI pages (10 results per page)
   - Max PubMed results
   - Minimum publication year
3. **Execute Pipeline**: Click "Ejecutar pipeline"
4. **Review Results**: Browse the interactive table
5. **Export Data**: Download CSV with complete abstracts

## 🔄 Search Pipeline

The application follows a sophisticated multi-stage pipeline:

```
┌─────────────────────┐
│  Google Scholar     │
│  (via SerpAPI)      │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  For each result:   │
│  1. EuropePMC       │
│  2. Semantic Scholar│ ← NEW!
│  3. CrossRef        │
│  4. PubMed          │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  PubMed Direct      │
│  (add missing)      │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  Deduplicate &      │
│  Export Results     │
└─────────────────────┘
```

## 📊 Output Format

The application generates a CSV file with the following columns:

| Column    | Description                          |
|-----------|--------------------------------------|
| title     | Article title                        |
| year      | Publication year                     |
| doi       | Digital Object Identifier            |
| url       | Article URL                          |
| abstract  | Complete abstract text               |
| journal   | Journal/venue name                   |
| source    | Data source (EuropePMC, SemanticScholar, CrossRef, PubMed, Scholar-snippet) |

## 🛠️ Technical Details

### Encoding Handling

The application includes robust UTF-8 encoding correction to handle:
- Mojibake patterns (e.g., `â€¦` → `…`)
- Special characters (Greek letters, mathematical symbols)
- Non-breaking spaces and other whitespace issues

### API Rate Limits

- **SerpAPI**: 100 searches/month (free tier)
- **Semantic Scholar**: 100 requests/5 minutes (no key required)
- **EuropePMC**: No rate limit (no key required)
- **CrossRef**: 50 requests/second (no key required)
- **PubMed**: 3 requests/second without key, 10/second with key

The application includes automatic delays (`Sys.sleep`) to respect rate limits.

## 🔧 Configuration

### Customizing Search Parameters

Edit `app.R` to modify:

```r
# Default values in UI
numericInput("gs_pages", ..., value = 3)      # Google Scholar pages
numericInput("pubmed_max", ..., value = 200)  # PubMed max results
sliderInput("year_min", ..., value = 2000)    # Minimum year
```

### Adjusting API Timeouts

```r
safe_GET <- function(url, query = list(), timeout_sec = 20) {
  # Increase timeout_sec for slower connections
}
```

## 📝 Example Use Cases

### Biomarker Research
```
Query: "biomarkers early detection alzheimer"
Results: ~50-100 papers with complete abstracts
```

### Systematic Reviews
```
Query: "machine learning cancer diagnosis"
Results: Comprehensive metadata for meta-analysis
```

### Literature Mapping
```
Query: "CRISPR gene editing applications"
Results: Recent publications with DOIs and URLs
```

## 🐛 Troubleshooting

### No Results from Google Scholar
- Check SerpAPI key validity
- Verify API quota hasn't been exceeded
- Try a more specific query

### Incomplete Abstracts
- Normal for some papers (publisher restrictions)
- Check the `source` column - "Scholar-snippet" indicates no full abstract available
- Semantic Scholar often has more complete abstracts than other sources

### Encoding Issues
- The application automatically fixes most UTF-8 encoding problems
- If issues persist, check that your R session is using UTF-8 locale

### API Errors
- Check internet connection
- Verify API keys are correctly configured
- Review logs for specific error messages

## 🤝 Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- **SerpAPI** for Google Scholar access
- **EuropePMC** for biomedical literature
- **Semantic Scholar** for comprehensive academic coverage
- **CrossRef** for DOI resolution
- **NCBI** for PubMed/Entrez services

## 📧 Contact

For questions or support, please open an issue on GitHub.
Dr. Vicente Esparza Villalpando

**Made with ❤️ for researchers, by researchers**

## 📧 Citation

Esparza-Villalpando V. RevAutoSearch: https://github.com/Vicentemed/RevAutoSearch
DOI: 10.5281/zenodo.22695284 


