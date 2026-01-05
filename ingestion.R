# R/ingestion.R
# Data ingestion functions for QuantKiosk Insider Intent data

library(here)
library(dplyr)
library(lubridate)

to_snake_case <- function(df) {
  names(df) <- gsub("([A-Z])", "_\\1", names(df))
  names(df) <- gsub("^_", "", names(df))
  names(df) <- tolower(names(df))
  names(df) <- gsub("__+", "_", names(df))
  names(df) <- gsub("\\.", "_", names(df))
  return(df)
}

normalize_intent_data <- function(data) {
  data <- to_snake_case(data)
  
  date_cols <- grep("date|filed|transaction", names(data), 
                    value = TRUE, ignore.case = TRUE)
  
  for (col in date_cols) {
    if (col %in% names(data)) {
      data[[col]] <- tryCatch({
        as.Date(data[[col]])
      }, error = function(e) {
        as.Date(ymd_hms(data[[col]], quiet = TRUE))
      })
    }
  }
  
  if ("date_filed" %in% names(data)) {
    data$date_filed <- as.Date(data$date_filed)
    data <- data %>% arrange(desc(date_filed))
  }
  
  return(data)
}

fetch_insider_intent <- function(ticker, use_sample = FALSE) {
  if (missing(ticker) || is.null(ticker) || ticker == "") {
    stop("Ticker symbol is required")
  }
  
  ticker <- toupper(ticker)
  cat("Fetching insider intent data for:", ticker, "\n")
  
  if (!requireNamespace("qkiosk", quietly = TRUE)) {
    stop("qkiosk package not installed. Run: remotes::install_github('quantkiosk/qkiosk-r')")
  }
  
  library(qkiosk)
  
  if (use_sample) {
    cat("Using sample data...\n")
    data(nvda_intent, package = "qkiosk", envir = environment())
    if (!exists("nvda_intent")) {
      stop("Sample data 'nvda_intent' not found in qkiosk package")
    }
    intent_data <- nvda_intent
  } else {
    api_key <- Sys.getenv("QK_API_KEY")
    if (api_key == "" || api_key == "your_key_here") {
      stop("QK_API_KEY not set. Please configure .Renviron with your API key and restart R.")
    }
    
    cat("Calling QuantKiosk API...\n")
    intent_data <- tryCatch({
      qkiosk::get_insider_intent(ticker)
    }, error = function(e) {
      stop(paste("API call failed:", e$message))
    })
  }
  
  cat("Normalizing data...\n")
  intent_data <- normalize_intent_data(intent_data)
  
  output_path <- here::here("data", "raw", paste0(tolower(ticker), "_intent.csv"))
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  
  cat("Saving to:", output_path, "\n")
  write.csv(intent_data, output_path, row.names = FALSE)
  
  cat("Successfully fetched and saved", nrow(intent_data), "records\n")
  
  return(intent_data)
}

load_intent_data <- function(ticker) {
  ticker <- tolower(ticker)
  file_path <- here::here("data", "raw", paste0(ticker, "_intent.csv"))
  
  if (!file.exists(file_path)) {
    stop(paste("Data file not found:", file_path))
  }
  
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  data <- normalize_intent_data(data)
  
  cat("Loaded", nrow(data), "records for", toupper(ticker), "\n")
  return(data)
}
