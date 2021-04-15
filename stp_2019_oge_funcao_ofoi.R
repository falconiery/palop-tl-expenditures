#######################################################
# This code parses the 
#     SAO TOME E PRINCIPE OGE function report 
# based on 2019.
#
# The code is working for the 2019 report, but it can 
# also work for other years if the report structure
# doesn't change.
#######################################################

#install.packages("tidyverse")
#install.packages("stringr")
#install.packages("pdftools")
#install.packages("zoo")

library("stringr")
library("pdftools")
library("tidyverse")
library("zoo")


# Settings
input_file = "stp_2016_oge_funcao_ofoi.pdf"
output_file = "stp_2016_oge_funcao_ofoi_df.csv"
output_file2 = "stp_classificador_funcao.csv"


# Code
filename = input_file
fullpath <- file.path(getwd(), filename)

parsed_pdf <- pdftools::pdf_text(pdf = fullpath) %>%
  str_split("\n")

# Create an empty list 
datalist = list()  

# Gathering data from all pages and turning it into a dataframe

for (i in 1:(pdf_info(fullpath)$pages)){
  datalist[[i]]  <- parsed_pdf[[i]][7:70] %>% 
    str_trim() %>% 
    str_split("\\s{2,}", simplify = TRUE) %>% 
    data.frame(stringsAsFactors = FALSE)
}

output_dataset <-  do.call(rbind, datalist) %>%  
  na.omit()

# Filter rows that present values for function and subfunction
output_dataset1 <- output_dataset %>% filter(grepl("^[A-Z]", X3))

# Create and save a data frame with only function codes and names 
stp_classificador_função <- output_dataset1 %>% 
  filter(grepl("^[0-9]{2}$", X1)) %>% 
  select(X1, X4) %>% 
  unique()

write_excel_csv2(stp_classificador_função, file.path(getwd(), output_file2))


list.files()
# Create new column for function names
output_dataset2 <- left_join(output_dataset1, stp_classificador_função, by = "X1")
output_dataset2 <- output_dataset2[-1,]

output_dataset2$X4.y <- na.locf(output_dataset2$X4.y)
output_dataset2 <- output_dataset2[output_dataset2$X1 == "SUB. FU", ]
output_dataset2$X1 <- substr(output_dataset2$X2, 1, 2)

head(output_dataset2)
# Naming columns
output_dataset3 <- output_dataset2 %>% select(X1, X4.y, X2, X3, X4.x)
colnames(output_dataset3) <- c("category_code", "category", "subcategory_code", "subcategory", "budget_agreggated")
head(output_dataset3)
# Turning values into numbers 
output_dataset3$budget_agreggated <- str_remove(output_dataset3$budget_agreggated, "\\s.+,.+%$") %>%  
  str_replace_all(" ", "") %>% 
  str_replace_all("\\.", "") %>%
  str_replace_all(",", ".") %>%
  as.numeric()


output_dataset3 <- output_dataset3 %>% add_column(report_type = "functional", .before = 1)


# save csv file
write_excel_csv2(output_dataset3, file.path(getwd(), output_file))
