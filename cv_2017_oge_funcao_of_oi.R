#######################################################
# This code parses the 
#     CV 2017 OGE function report  
#
# The code is working for the 2017 report, but it can 
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
input_file = "cv_2017_oge_funcao_of_oi.pdf"
output_file = "cv_2017_oge_funcao_of_oi.csv"

# Code
filename = input_file
fullpath <- file.path(getwd(), filename)

parsed_pdf <- pdftools::pdf_text(pdf = fullpath) %>%
  str_split("\n")

# Create an empty list
datalist = list()

# Gather all pages into one dataframe

datalist[[1]] <- parsed_pdf[[1]][10:39] %>% 
  str_trim() %>% 
  str_split_fixed("\\s+(?=\\d)", n=4) %>%
  data.frame(stringsAsFactors = FALSE)

datalist[[2]] <- parsed_pdf[[2]][9:48] %>% 
  str_trim() %>% 
  str_split_fixed("\\s+(?=\\d)", n=4) %>%
  data.frame(stringsAsFactors = FALSE)

  output_dataset1 <-  do.call(rbind, datalist) %>%  
  na.omit() %>% as.data.frame()

# Create a df for category names
category <- c("Funcionais", "Serviços Públicos Gerais", "Assuntos económicos", "Defesa", "Educação", "Habitação e desenvolvimento urbanístico", 
        "Protecção ambiental", "Saúde", "Segurança e ordem pública", "Serviços culturais        recreativos e religiosos",
        "Serviços Públicos Gerais")

# Fill new column with category names 
output_dataset2 <- output_dataset1 %>% add_column(X0 = ifelse(output_dataset1[,1] %in% category, output_dataset1[,1], NA), .before = 1)
output_dataset2$X0 <- na.locf(output_dataset2$X0)

# Delete category subtotals rows 
output_dataset3 <- output_dataset2[!output_dataset2$X0 == output_dataset2$X1,]


# Fix specific problems
output_dataset3 <- output_dataset3[1:51,]
output_dataset3 <- output_dataset3[-c(26, 29, 46),]
output_dataset3[43,]$X1 <- "Serviços Públicos Gerais Órgãos Executivos E Legislativos Administração Financeira E Fiscal Negócios Estrangeiros"

output_dataset4 <- add_row(output_dataset3, X0 = c("Serviços Públicos Gerais", "Serviços Públicos Gerais", "Serviços Públicos Gerais", "Serviços Públicos Gerais", "Serviços Públicos Gerais"), 
                           X1 = c("Família e crianças", "Habitação", "ID Protecção Social", "Outros não especificados", "Sobrevivência"), 
                           X2 = c("76.818.306", "0","0", "4.721.139.246", "1.190.471.520") , 
                           X3 = c("66.455.188", "15.501.509", "223.762.574", "454.809.188", "371.178.974") , 
                           X4 = c("143.273.494", "15.501.509", "223.762.574", "5.175.948.434", "1.561.650.494"))


# Rename
colnames(output_dataset4) <- c("category", "subcategory", "budget_operation", "budget_investment", "budget_agreggated")

# add a column for report type
output_dataset5 <- add_column(output_dataset4, report_type = "functional", .before = 1)

# Format as numeric and absolute numbers 
output_dataset5$budget_operation <- output_dataset5$budget_operation %>% str_replace_all("\\.", "") %>% as.numeric() %>% na.omit
output_dataset5$budget_investment <- output_dataset5$budget_investment %>% str_replace_all("\\.", "") %>% as.numeric() %>% na.omit

# Delete last column unformatted
output_dataset5$budget_agreggated <- NULL

sum(output_dataset5$budget_investment)

# Remove extra spaces
output_dataset5$subcategory <- gsub("\\s+", " ", str_trim(output_dataset5$subcategory))
output_dataset5$category <- gsub("\\s+", " ", str_trim(output_dataset5$ccategory))

# save 
write_excel_csv2(output_dataset5, file.path(getwd(), output_file))
