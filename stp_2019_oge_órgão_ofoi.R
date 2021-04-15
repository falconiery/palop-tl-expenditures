#######################################################
# This code parses the 
#     SAO TOME E PRINCIPE OGE organic report 
# based on 2019.
#
# The code is working for the 2019 report, but it can 
# also work for other years if the report structure
# doesn't change.
#######################################################

#install.packages("tidyverse")
#install.packages("stringr")
#install.packages("pdftools")

library("stringr")
library("pdftools")
library("tidyverse")
library("readr")


# Settings
input_file = "stp_2016_oge_órgão_ofoi.pdf"
output_file = "stp_2016_oge_órgão_ofoi.csv"

# Code

filename = input_file
fullpath <- file.path(getwd(), filename)

parsed_pdf <- pdftools::pdf_text(pdf = fullpath) %>%
  str_split("\n")

# Organizing data 

# Create an empty list
datalist = list()

# Gather all pages into one dataframe

for (i in 1: pdf_info(fullpath)$pages){
  datalist[[i]]  <- parsed_pdf[[i]] %>%
    str_trim() %>% 
    data.frame(stringsAsFactors = FALSE) %>% 
    filter(grepl("^[0-9]{2}\\.", .))}

output_dataset <-  do.call(rbind, datalist) %>%  
  na.omit() %>% as.data.frame()

# spare names and values into different columns 
output_dataset1 <- str_split(output_dataset$., "\\s{10,}", simplify = TRUE)
colnames(output_dataset1) <- c("subcategory_code", "budget_aggregated", "zero1", "zero2")
head(output_dataset1)
output_dataset2 <- as.data.frame(output_dataset1) %>% 
  select(subcategory_code, budget_aggregated)

# Create an object  with the adm units codes (the fisrt 2 digits of the first column)
category_code <- as.data.frame(substr(output_dataset2$subcategory_code, 1, 2))
colnames(category_code) <- "category_code"

# Import the file "stp_classificador_orgão.csv" to associate codes with names of the adm unit 
filename <- "stp_classificador_orgão.csv"
fullpath <- file.path(getwd(), filename)
stp_classificador_orgao <- read_csv2(fullpath)

head(stp_classificador_orgao)
# New column cointaing only the 2 first digits that indicate the adm units 
stp_classificador_orgao$category_code <- substr(stp_classificador_orgao$category_code, 1,2)

# Associate codes with names of the adm unit 
categories <- left_join(category_code, stp_classificador_orgao, by = "category_code") %>% 
  select(category_code, category)
nrow(categories)

# vector containing the sub adm unit codes
subcategory_code <- substr(output_dataset2$subcategory_code, 1, 7)

head(output_dataset2)
# vector contain the sub adm unit name 
subcategory <- str_trim(substr(output_dataset2$subcategory_code, 9, 10000))

output_dataset2$budget_aggregated

# vector containing budget values
budget_aggregated <- output_dataset2$budget_aggregated %>% 
  str_replace_all("\\.", "") %>%
  str_replace_all(",", ".") %>%
  as.numeric() %>%
  str_replace_na("0") %>%
  as.numeric()


# check if the sum of authorized values is equal to the total authorized budget officially reported  

sum(output_dataset2$budget_aggregated)

# the totals arent the same because there are some duplicities. 
# every document`s new page repeats the adm unit that was being detailed at the previous page
# to solve that consider only the unique rows, remove the duplicated rows
# Bind vectors and get unique rows


output_dataset3 <- cbind(categories, subcategory_code, subcategory, budget_aggregated, stringsAsFactors = FALSE) %>% 
  as.data.frame() %>%
  unique()

# check totals again 
# at 2019 report one duplicated row wasnt removed because it has a different spacement
# in this case we will remove this specific row
# do not run this part for future reports 
head(output_dataset4)
output_dataset4 <- output_dataset3 %>% add_column(report_type = "organic", .before = 1)
sum(output_dataset4$budget_aggregated)
# Creating output file
write_excel_csv2(output_dataset4, file.path(getwd(), output_file))
