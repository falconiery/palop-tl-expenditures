#######################################################
# This code parses the 
#     CV 2020 OGE function report  
#
# The code is working for the 2020 report, but it can 
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
input_file = "cv_2019_oge_funcao_of_oi.pdf"
output_file = "cv_2019_oge_funcao_of_oi.csv"

# Code
filename = input_file
fullpath <- file.path(getwd(), filename)

parsed_pdf <- pdftools::pdf_text(pdf = fullpath) %>%
  str_split("\n")

parsed_pdf[[1]][11:101]
parsed_pdf[[2]][9:69]

# Create an empty list
datalist = list()

# Gather all pages into one dataframe

datalist[[1]] <- parsed_pdf[[1]][11:101] %>% 
  str_trim() %>% 
  str_split_fixed("\\s+(?=\\d)", n=4) %>%
  data.frame(stringsAsFactors = FALSE) %>% 
  select(X1, X2, X3)


datalist[[2]] <- parsed_pdf[[2]][9:113] %>% 
  str_trim() %>% 
  str_split_fixed("\\s+(?=\\d)", n=4) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  select(X1, X2, X3)


output_dataset1 <-  do.call(rbind, datalist) %>%  
  na.omit() %>% as.data.frame()

output_dataset2 <- output_dataset1[!(output_dataset1$X2  == ""),]
output_dataset2 <- output_dataset2[!(output_dataset2$X3  == ""),]

# Remove extra spaces
output_dataset1$X1 <- gsub("\\s+", " ", str_trim(output_dataset1$X1))

# Create a df for category names
category <- c("Serviços Públicos Gerais", "Assuntos económicos", "Defesa", "Educação", "Habitação e desenvolvimento urbanístico", 
        "Protecção ambiental", "Saúde", "Segurança e ordem pública", "Serviços culturais recreativos e religiosos",
        "Serviços Públicos Gerais")

# Fill new column with category names 
output_dataset2 <- output_dataset1 %>% add_column(X0 = ifelse(output_dataset1[,1] %in% category, output_dataset1[,1], NA), .before = 1)
output_dataset2$X0 <- na.locf(output_dataset2$X0)

# Delete category subtotals rows 
output_dataset3 <- output_dataset2[!output_dataset2$X0 == output_dataset2$X1,]

# Reset rows index 
row.names(output_dataset3) <- NULL

### recolocar a linha de prisõesok, transação da dívida públicaOK, outos nao especificados em recreaçãoOK, exclusão social, familia e crianças, habitação, outros nao especificados em defesa 
# Fix specific problems
output_dataset4 <- output_dataset3[-c(1,4,12,15:17,26,36, 43:45, 50, 53, 55:58, 60, 62:69, 72:80, 82, 85),]
row.names(output_dataset4) <- NULL
output_dataset4[42,]$X1 <- "Serviços Públicos Gerais Órgãos Executivos E Legislativos Administração Financeira E Fiscal Negócios Estrangeiros"

output_dataset4 <- add_row(output_dataset4, 
                           X0 = c("Segurança e ordem pública", "Serviços Públicos Gerais", "Serviços culturais recreativos e religiosos", 
                                  "Serviços Públicos Gerais", "Serviços Públicos Gerais", "Defesa", "Protecção ambiental"),
                           X1 = c("Prisões","Transacções da dívida pública", "Outros não especificados", "Exclusão social", "Família e crianças", 
                                  "Outros não especificados", "Gestão de resíduos e substâncias perigosas"), 
                           X2 = c("325,342,814", "5,662,000,000", "40,855,759", "0", "149,274,411", "1,615,010", "0"), 
                           X3 = c("134,799,658", "0", "84,913,947", "97,020,000", "71,118,435", "54,271,600", "21,784,556")) 

# Rename
colnames(output_dataset4) <- c("category", "subcategory", "budget_operation", "budget_investment")

# add a column for report type
output_dataset5 <- add_column(output_dataset4, report_type = "functional", .before = 1)

# Format as numeric and absolute numbers 
output_dataset5$budget_operation <- output_dataset5$budget_operation %>% str_replace_all(",", "") %>% as.numeric() %>% na.omit
output_dataset5$budget_investment <- output_dataset5$budget_investment %>% str_replace_all(",", "") %>% as.numeric() %>% na.omit

sum(output_dataset5$budget_investment)

# save 
write_excel_csv2(output_dataset5, file.path(getwd(), output_file))
