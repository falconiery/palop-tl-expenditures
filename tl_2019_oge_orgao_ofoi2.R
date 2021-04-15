#######################################################
# This code parses the 
#     TL 2019 OGE organic report  
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
input_file = "tl_2019_oge_orgao_ofoi.pdf"
output_file = "tl_2019_oge_orgao_ofoi.csv"

# Code
filename = input_file
setwd("/Users/andressa/Documents/PROPALOP/TL")
fullpath <- file.path(getwd(), filename)

parsed_pdf <- pdftools::pdf_text(pdf = fullpath) %>%
  str_split("\n")

parsed_pdf [[20]]
# Create an empty list
datalist = list()

# Gather all pages into one dataframe

for (i in 1:(pdf_info(fullpath)$pages)){
  datalist[[i]]  <- parsed_pdf[[i]] [3:38] %>% 
    str_split_fixed("\\s{10,}", n = 7) %>%
    data.frame(stringsAsFactors = FALSE)
}

output_dataset <-  do.call(rbind, datalist) %>%  
  na.omit()

# Fill new column with category names 

output_dataset2 <- output_dataset %>%
  add_column(X0 = ifelse(str_detect(output_dataset[,1], "^\\s"), NA, 
                         output_dataset[,1]), .before = 1)

output_dataset2 <- output_dataset2[23:972,]
output_dataset2$X0 <- na.locf(output_dataset2$X0)
output_dataset3 <- output_dataset2[!apply(output_dataset2 == "", 1, all),]

# Format numbers 

output_dataset3$X7 <- output_dataset3$X7 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all(",", "") %>%
  as.numeric()

output_dataset3$X6 <- output_dataset3$X6 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all(",", "") %>%
  as.numeric()

output_dataset3$X5 <- output_dataset3$X5 %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all(",", "") %>%
  as.numeric()

##### Getting the last value from dataframe  ##############

category <- output_dataset3[,c("X0","X1")]
lastValue <- function(x)   tail(x[!is.na(x)], 1)
f <- apply(output_dataset3, 1, lastValue)
output_dataset4 <- cbind(category, f)

## deleting empty rows again 
output_dataset5 <- output_dataset4[!apply(output_dataset4 == "", 1, all),]
output_dataset5$X0 <-  str_trim(output_dataset5$X0)
output_dataset5$X1 <-  str_trim(output_dataset5$X1)

## repeat values above
output_dataset5$X0 <-  str_trim(output_dataset5$X0)
output_dataset5$X1 <-  str_trim(output_dataset5$X1)
output_dataset5$X0 <- na.locf(output_dataset5$X0)

output_dataset6 <- output_dataset5 %>% 
  filter(output_dataset5$X0 != output_dataset5$X1)
  
class(output_dataset6$f) 

output_dataset6$f <- output_dataset6$f %>%
  str_trim() %>% 
  str_remove_all(".+\\s+") %>%
  str_replace_all("\\.", "") %>%
  str_replace_all(",", ".") %>%
  as.numeric(as.character()) %>%
  str_replace_na("0") %>%
  as.numeric()

sum(output_dataset6$f)

####### salvei o doc. aguardando retorno do arsênio. 
write_excel_csv2(output_dataset6, file.path(getwd(), output_file))




