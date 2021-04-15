#######################################################
# This code parses the 
#     MZ 2019 CGE organic report  
#
# The code is working for the 2019 report, but it can 
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
input_file = "mz_2019_cge_orgao_of.pdf"
output_file = "mz_2019_cge_orgao_of_fase2.csv"

setwd("/Users/andressa/Documents/PROPALOP/MZ")
# Code
filename = input_file
fullpath <- file.path(getwd(), filename)

setwd("/Users/andressa/Documents/PROPALOP/MZ")
parsed_pdf <- pdftools::pdf_text(pdf = fullpath) %>%
  str_split("\n")

# Create an empty list
datalist = list()

parsed_pdf[[62]]
# Gather all pages into one dataframe

for (i in 1:(pdf_info(fullpath)$pages)){
  datalist[[i]]  <- parsed_pdf[[i]] [11:52] %>% 
    str_trim() %>%
    str_replace_all("\\d{1,3},\\d{1,2}\\s+%", "") %>% 
    str_split_fixed("\\s{1,}(?<=\\d) | \\s{2,} | (?<=\\d)\\s+(?=\\d) | '--'", n=20) %>% 
    data.frame(stringsAsFactors = FALSE)
}

output_dataset <-  do.call(rbind, datalist) %>%  
  na.omit()


#### Setting data as numbers #############################

output_dataset$X20 <- output_dataset$X20 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric()

output_dataset$X19 <- output_dataset$X19 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric()

output_dataset$X18 <- output_dataset$X18 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric()

output_dataset$X17 <- output_dataset$X17 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric()


output_dataset$X16 <- output_dataset$X16 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>% 
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric()


output_dataset$X15 <- output_dataset$X15 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>% 
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric()

output_dataset$X14 <- output_dataset$X14 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric()

output_dataset$X13 <- output_dataset$X13 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric()

output_dataset$X12 <- output_dataset$X12 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric()

output_dataset$X11 <- output_dataset$X11 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric()

output_dataset$X10 <- output_dataset$X10 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric()

output_dataset$X9 <- output_dataset$X9 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric()

output_dataset$X8 <- output_dataset$X8 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric()

output_dataset$X7 <- output_dataset$X7 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric()

output_dataset$X6 <- output_dataset$X6 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric()

output_dataset$X5 <- output_dataset$X5 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric()

##### Getting the last two values ##############

category <- output_dataset["X1"]

lastValue <- function(x)   tail(x[!is.na(x)], 2)
f <- apply(output_dataset, 1, lastValue)
transposed_f <- as.data.frame(t(f))
output_dataset2 <- cbind(category, transposed_f)

output_dataset2$V1 <- output_dataset2$V1 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", ".") %>%
  as.numeric(as.character()) %>%
  str_replace_na("0")

output_dataset2$V1 <- as.numeric(output_dataset2$V1)/100

output_dataset2$V2 <- output_dataset2$V2 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", ".") %>%
  as.numeric(as.character()) %>%
  str_replace_na("0")

output_dataset2$V2 <- as.numeric(output_dataset2$V2)/100

output_dataset3 <- add_column(output_dataset2, subcategoty_code = substr(output_dataset$X1, 1, 7), .before = 1)

output_dataset3 <- output_dataset2 %>% 
  filter(str_detect(output_dataset2$subcategoty_code, "^\\d")) %>%
  select(subcategoty_code, V1, V2)

colnames(output_dataset3)[2] <- "budget_operational"
colnames(output_dataset3)[3] <- "execution_operational"


####### salvei o doc. aguardando retorno do arsênio. 
write_excel_csv2(output_dataset3, file.path(getwd(), output_file))
  



