#######################################################
# This code parses the 
#     MZ 2019 REO organic report  
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
getwd()
setwd("/Users/andressa/Documents/PROPALOP/MZ")
input_file = "mz_2016_reo4_orgao_oi_int.pdf"
output_file = "mz_2016_reo4_orgao_oi_int2.csv"

# Code
filename = input_file
fullpath <- file.path(getwd(), filename)

parsed_pdf <- pdftools::pdf_text(pdf = fullpath) %>%
  str_split("\n")

parsed_pdf[[32]]
# Create an empty list
datalist = list()

# Gather all pages into one dataframe

for (i in 1:(pdf_info(fullpath)$pages)){
  datalist[[i]]  <- parsed_pdf[[i]] [10:38] %>% 
    str_trim() %>%
    str_replace_all("\\d{1,3},\\d{1,2}\\s+%", "") %>% 
    str_split_fixed("\\s{1,}(?<=\\d) | \\s{2,} | (?<=\\d)\\s+(?=\\d)", n=18) %>% 
    data.frame(stringsAsFactors = FALSE)
}

output_dataset <-  do.call(rbind, datalist) %>%  
  na.omit()

#### Setting data as numbers #############################

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

##### Getting the last two values ##############

category <- output_dataset["X1"]
lastValue <- function(x)   tail(x[!is.na(x)], 2)
i_int <- apply(output_dataset, 1, lastValue)
transposed_i_int <- as.data.frame(t(i_int))
output_dataset2 <- cbind(category, transposed_i_int)
as.data.frame(output_dataset2)

output_dataset2$V1 <- output_dataset2$V1 %>%
  str_trim() %>% 
  as.numeric() %>% 
  str_replace_na("0") %>%
  as.numeric()

output_dataset2$V2 <- output_dataset2$V2 %>%
  str_trim() %>% 
  as.numeric() %>% 
  str_replace_na("0") %>%
  as.numeric()

sum(output_dataset2$V1)
sum(output_dataset2$V2)


####### salvei o doc. aguardando retorno do arsênio. 
write_excel_csv2(output_dataset2, file.path(getwd(), output_file))




