# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .R
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.13.3
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

# +
# install.packages("rvest")
# install.packages("RSelenium")
# -

# ##### Link Libraries

library(rvest)
library(RSelenium)
library(tidyverse, warn.conflicts = FALSE)

setwd("C:/Niloy/Code/R/R_Codebase/MoH_Data_Extraction")

# ##### Define Global Variables

url <- 'https://www.mohfw.gov.in'

current_dt <- Sys.Date()

state_data_headers <- 
  c("Name of State / UT", "Active Cases - Total", "Active Cases - Change Since Yesterday", 
    "Cured/Discharged/Migrated - Cumulative", "Cured/Discharged/Migrated - Change Since Yesterday", 
    "Deaths - Cumulative", "Deaths - Change Since Yesterday", "Deaths - Reconciled",
    "Deaths - Total")

# ##### Open pseudo-browser session

# rd <- rsDriver(port = 4573L, browser = "chrome", chromever = "96.0.4664.45", verbose = FALSE)
rd <- rsDriver(port = 4573L, browser = "chrome", chromever = "97.0.4692.36", verbose = FALSE)
remDr <- rd[["client"]]
remDr$navigate(url)
# remDr$click("#state-data")
# remDr$getPageSource()[[1]]
all_data <- read_html(remDr$getPageSource()[[1]])
remDr$close
rd$server$stop

# ##### Close browser session in the back-end

system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
pingr::ping_port("localhost", 4573)

# ##### View extracted data

# all_data

# ##### Get Header Data

header_data <- all_data %>%
  html_elements("#state-data > div > div > div > div > table > thead") %>%
  html_table() %>%
  .[[1]]

header_data_col <- ncol(header_data)

print(paste("# of columns in the State Data:", header_data_col))

# ##### Get report Date

report_date <- all_data %>% 
  html_elements("#state-data > div > div > div > div > h5 > span") %>%
  html_text2()
# report_date
report_date_cleaned <- str_extract(report_date, "(as on : .*,)")
report_date_cleaned <- sub(",", "", strsplit(report_date_cleaned, ": ")[[1]][2])
report_date_cleaned <- as.Date(report_date_cleaned, format = "%d %B %Y")
print(paste("Report Date:", report_date_cleaned))

# ##### Get States Data

data_table <- 
  all_data %>% 
  html_elements("#state-data > div > div > div > div > table > tbody") %>% 
  html_table() %>% .[[1]]
# all_data %>% html_elements("#state-data > div > div > div > div > table > thead > tr.row1 > th") %>% html_text2()
# all_data %>% html_elements("#state-data > div > div > div > div > table > thead > tr.row2 > th") %>% html_text2()

# ##### Data Cleansing and manipulation

# Get individual column data

get_col_data <- function(col_no) {
  col_data <- 
    tryCatch(
      {
        all_data %>% 
          html_elements(paste0("#state-data > div > div > div > div > table > tbody > tr > td:nth-child(",col_no,")")) %>%
          html_text2()
      },
      error = function(cond){
        message("Error: Unable to get Data for column")
        return(NA)
      },
      finally = {
        message("Processed data for column")
      }
    )
  return(col_data)
}

x <- lapply(1:header_data_col, get_col_data)

no_of_states <- length(na.omit(as.numeric(x[[1]])))

data_table <- c(1:no_of_states)

for (each_col in 1:header_data_col) {
  data_table <- cbind(data_table, head(x[[each_col]], no_of_states))
}


# ele <- all_data %>% 
#     html_elements("#state-data > div > div > div > div > table > tbody > tr > td:nth-child(10)") %>%
#     html_text2()
# 
# print(ele)

data_table_df <- data.frame(data_table)

data_table_df["data_table"] <- NULL
data_table_df["V2"] <- NULL

str(data_table_df)

names(data_table_df) <- state_data_headers

data_table_df["Report Date"] <- report_date_cleaned
data_table_df["Load Date"] <- current_dt
# data_table_df <- head(data_table_df, -6)

# ##### Write Data to File

if (file.exists("All_Data.csv")) {
  old_data <- read.csv("All_Data.csv", check.names = FALSE)
  max_load_date <- max(old_data$"Load Date", na.rm = TRUE)
  max_report_date <- as.Date(max(old_data$"Report Date", na.rm = TRUE), format = "%Y-%m-%d")
  print(paste("Last Report Date:", max_report_date))
  if (report_date_cleaned != max_report_date) {
    write.table(data_table_df, "All_Data.csv", row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
  } else {
    old_data <- old_data %>% filter(report_date_cleaned != max_report_date)
    write.table(old_data, "All_Data.csv", row.names = FALSE, col.names = TRUE, append = FALSE, sep = ",")
    write.table(data_table_df, "All_Data.csv", row.names = FALSE, col.names = FALSE, append = TRUE, sep = ",")
  }
} else {
  write.table(data_table_df, "All_Data.csv", row.names = FALSE, col.names = TRUE, append = FALSE, sep = ",")
}

# all_data <- all_data %>%
# rvest::html_elements(css = '.table-striped') %>%
# rvest::html_table()

# +
# all_data %>% html_elements(css = "tbody > tr")
