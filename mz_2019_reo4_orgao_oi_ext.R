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
input_file = "mz_2019_reo4_orgao_oi_ext.pdf"
output_file = "mz_2019_reo4_orgao_oi_ext.csv"

# Code
filename = input_file
fullpath <- file.path(getwd(), filename)

setwd("/Users/andressa/Documents/PROPALOP/MZ")
parsed_pdf <- pdftools::pdf_text(pdf = fullpath) %>%
  str_split("\n")

# Create an empty list
datalist = list()

# Gather all pages into one dataframe

for (i in 1:(pdf_info(fullpath)$pages)){
  datalist[[i]]  <- parsed_pdf[[i]] [9:40] %>% 
    str_trim() %>%
    str_replace_all("\\d{1,3},\\d{1,2}\\s+%", "") %>% 
    str_split_fixed("\\s{1,}(?<=\\d) | \\s{2,} | (?<=\\d)\\s+(?=\\d)", n=11) %>% 
    data.frame(stringsAsFactors = FALSE)
}
  
############

    parsed_pdf[[3]] [9:40] %>% 
    str_trim() %>%
    str_replace_all("\\d{1,3},\\d{1,2}\\s+%", "") %>% 
    str_split_fixed("\\s{1,}(?<=\\d) | \\s{2,} | (?<=\\d)\\s+(?=\\d)", n=11) %>% 
    data.frame(stringsAsFactors = FALSE)

################
output_dataset <-  do.call(rbind, datalist) %>%  
  na.omit()

#### Setting data as numbers

output_dataset$X6 <- output_dataset$X6 %>%
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


output_dataset$X8 <- output_dataset$X8 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric()

output_dataset$X9 <- output_dataset$X9%>%
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


output_dataset$X11 <- output_dataset$X11 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric()

##### Getting the last two values 

category <- output_dataset["X1"]
lastValue <- function(x)   tail(x[!is.na(x)], 2)
i_ext <- apply(output_dataset, 1, lastValue)
class(i_ext)
transposed_i_ext <- t(i_ext)

output_dataset2 <- cbind(category, transposed_i_ext)
colnames(output_dataset2) <- c("cat", "bi_e", "ei_e")

output_dataset2$bi_e <- output_dataset2$bi_e %>%
  str_trim() %>% 
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric() %>% 
  str_replace_na("0") %>%
  as.numeric()

output_dataset2$ei_e <- output_dataset2$ei_e %>%
  str_trim() %>% 
  str_replace_all("\\.", "") %>%
  str_replace_all(",", "\\.") %>% 
  as.numeric() %>% 
  str_replace_na("0") %>%
  as.numeric()


sum(output_dataset2$bi_e)

####### salvei o doc. aguardando retorno do arsênio. 
write_excel_csv2(output_dataset2, file.path(getwd(), output_file))




