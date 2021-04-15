#######################################################
# This code parses the 
#     GB 2020 OGE organic report  
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
input_file = "gb_2020_oge_orgao_ofoi.pdf"
output_file = "gb_2020_oge_orgao_ofoi.csv"

# Code
filename = input_file

# Set your working diretory
# setwd("")
setwd("/Users/andressa/Documents/PROPALOP/GB")

fullpath <- file.path(getwd(), filename)

parsed_pdf <- pdftools::pdf_text(pdf = fullpath) %>%
  str_split("\n")

# Investigate de range of rows that holds the data you need
## view some pages of the document to be sure you are choosing all rows with the data you need

parsed_pdf [[150]]
parsed_pdf [[77]]
parsed[[50]]

# Create an empty list
datalist = list()

# Gather all pages into one dataframe

for (i in 1:(pdf_info(fullpath)$pages)){
  datalist[[i]]  <- parsed_pdf[[i]] [5:60] %>% 
    str_split_fixed("\\s{6,}", n = 7) %>%
    data.frame(stringsAsFactors = FALSE)
}


output_dataset <-  do.call(rbind, datalist) %>%  
  na.omit()

# Subset first and last columns
output_dataset2 <- output_dataset[,c("X1","X7")]

# Format last column as numbers
output_dataset2$X7 <- output_dataset2$X7 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all(",", "")  %>%
  as.numeric()

# Filter rows with relevant content and trim
output_dataset3 <- output_dataset2 %>% filter(X1 != "" | X7 != "NA")
output_dataset3$X1 <- output_dataset3$X1 %>% str_trim() 


# filter1 for filtering the index of the group that sum totals
filter1 <- as.numeric(which(grepl("^Total da Secçao", output_dataset3$X1)))

# filter2 for filtering the index of the first row of the group "Total da Secção"
filter2 <- filter1 - 1

# filter3 for filtering the index of the last row of the group "Total da Secção"
filter3 <- filter1 + 1

# filter4 for filtering the next agency name, we know this happen 2 times after the filte1 rows
filter4 <- filter1 + 2

# order filter1, 2, 3, 4 filter3  
all_filters <- sort(c(filter1, filter2, filter3, filter4)) 

# apply the filter
output_dataset4 <- output_dataset3[all_filters,]

# Fill the first element with the name of the first agency of the report 
output_dataset4[1,1] <- "1-Assembleia Nacional Popular"

# Identify the groups of information with the name of the agency
output_dataset4$X1[output_dataset4$X1==""] <- NA
output_dataset4$X1[output_dataset4$X1=="Total da Secçao :"] <- NA
output_dataset4$X1 <- na.locf(output_dataset4$X1)

# Omit NA rows
output_dataset5 <- output_dataset4 %>%
  na.omit()

# Aggregate by agency 
output_dataset6 <- aggregate(output_dataset5$X7, by = list(output_dataset5$X1), FUN = sum)

# Fix broken names as they are at the report 
# Do not run this code 
output_dataset6$Group.1[3] <- "11-Ministério da Administração territorial e Poder local"
output_dataset6$Group.1[5] <- "13-Ministério da Administração Pública, Trabalho, Emprego e Segurança Social"
output_dataset6$Group.1[7] <- "15-MINISTERIO DA EDUCAÇÃO E ENSINO SUPERIOR"
output_dataset6$Group.1[8] <- "17-MINISTÉRIO DA PRESIDÊNCIA DE CONSELHO DE MINISTROS E ASSUNTOS PARLAMENTARES"
output_dataset6$Group.1[11] <- "21-Ministério da Obras Publicas, Habitação e Urbanismo"
output_dataset6$Group.1[18] <- "35-SECRETARIA DE ESTADO DA JUVENTUDE E DESPORTO"
output_dataset6$Group.1[19] <- "36-SECRETARIA DE ESTADO DA GESTÃO HOSPITALAR"
output_dataset6$Group.1[21] <- "39-Secretaria de Estado dos Combatentes da Liberdade da Pátria"
output_dataset6$Group.1[25] <- "46-SECRETARIA DE ESTADO DO PLANO E INTEGRAÇÃO REGIONAL"
output_dataset6$Group.1[32] <- "9-MINISTÉRIO DA DEFESA NACIONAL E COMBATENTES DA LIBERDADE DA PATRIA"
output_dataset6$Group.1[33] <- "8-Ministério dos Negócios Estrangeiros, Cooperação Internacional e das Comunidades"
output_dataset6$Group.1[34] <-  "31-Secretaria de Estado da Cooperação Internacional"

# Delete rows that does not show total of an agency 
output_dataset6 <- output_dataset6[-c(35, 36), ]

# Separate codes and names of agencies in 2 columns 
output_dataset7<- output_dataset6 %>% 
  separate(Group.1, c("category_code", "category"), sep ="-")

# Rename last column 
colnames(output_dataset7)[3] <- "budget_aggregated"


# Check totals and complete missing rows 
# Do not run this code 
category_code <- c(19, 23, 30)
category <- c("Ministério da Mulher, Família e Solidariedade Social", "Ministério da Agricultura e Desenvolvimento Rural", "Secretaria de Estado das Comunidades")
budget_aggregated <- c(4805069, 8125189, 60540)

# Add report_type column 
output_dataset10 <- add_column(output_dataset9, report_type = "organic", .before = 1)

# Final 
write_excel_csv2(output_dataset10, file.path(getwd(), output_file))

