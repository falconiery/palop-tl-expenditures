#######################################################
# This code parses the 
#     CV 2018 OGE function report  
#
# The code is working for the 2018 report, but it can 
# also work for other years if the report structure
# doesn't change.
#######################################################


#install.packages("tidyverse")
#install.packages("stringr")
#install.packages("pdftools")
#install.packages("DataCombine")
#install.packages("zoo")

library("stringr")
library("pdftools")
library("tidyverse")
library("DataCombine")
library("zoo")

# Settings
input_file = "cv_2018_oge_funcao_of_oi.pdf"
output_file = "cv_2018_oge_funcao_of_oi.csv"

# Code
filename = input_file
fullpath <- file.path(getwd(), filename)

parsed_pdf <- pdftools::pdf_text(pdf = fullpath) %>%
  str_split("\n")

parsed_pdf[[1]] [9:37]

parsed_pdf[[2]]
parsed_pdf[[3]]

parsed_pdf[[1]][5:37] %>%
    str_trim() %>% 
    str_squish() %>%
    str_split("\\s+(?=\\d)", simplify = TRUE) %>%
    data.frame(stringsAsFactors = FALSE)

# Create an empty list
datalist = list()

# Gather all pages into one dataframe

for (i in 1: pdf_info(fullpath)$pages){
  datalist[[i]]  <- parsed_pdf[[i]][5:37] %>%
    str_trim() %>% 
    str_split_fixed("\\s+(?=\\d)", n=4) %>%
    data.frame(stringsAsFactors = FALSE)
}

  output_dataset <-  do.call(rbind, datalist) %>%  
  na.omit() %>% as.data.frame()

# Remove rows out of the df
output_dataset1 <- output_dataset[5:72, 1:4] 
output_dataset1 <- output_dataset1[!output_dataset1$X4 == "",]

# Add a column for category 

# Create a df for category names
category <- c("Funcionais", "Serviços Públicos Gerais", "Assuntos económicos", "Defesa", "Educação", "Habitação e desenvolvimento urbanístico", 
        "Protecção ambiental", "Saúde", "Segurança e ordem pública", "Serviços culturais  recreativos e religiosos",
        "Serviços Públicos Gerais")
# Fill new column with category names 
output_dataset2 <- output_dataset1 %>% add_column(X0 = ifelse(output_dataset1[,1] %in% category, output_dataset1[,1], NA), .before = 1)
output_dataset2$X0 <- na.locf(output_dataset2$X0)

# Delete category subtotals rows 
output_dataset3 <- output_dataset2[!output_dataset2$X0 == output_dataset2$X1,]
# Fix these specifics out of pattern row 
output_dataset4 <- add_row(output_dataset3, X0 = "Serviços Públicos Gerais", X1 = "Serviços Públicos Gerais", X2 = "0", X3= "45.000.000", X4="45.000.000", .before = 1)
output_dataset4 <- output_dataset4[-50,]

# Rename
colnames(output_dataset4) <- c("category", "subcategory", "budget_operation", "budget_investment", "budget_agreggated")

# add a column for report type
output_dataset5 <- add_column(output_dataset4, report_type = "functional", .before = 1)

# Format as numeric and absolute numbers 
output_dataset5$budget_operation <- output_dataset5$budget_operation %>% str_replace_all("\\.", "") %>% as.numeric()
output_dataset5$budget_investment<- output_dataset5$budget_investment %>% str_replace_all("\\.", "") %>% as.numeric()
output_dataset5$budget_agreggated<- output_dataset5$budget_agreggated %>% str_replace_all("\\.", "") %>% as.numeric()

# save 
write_excel_csv2(output_dataset5, file.path(getwd(), output_file))
