#######################################################
# This code parses the 
#     MOZAMBIQUE OGE function report 
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
input_file = "mz_2019_oge_funcao_ofoi.pdf"
output_file = "mz_2019_oge_funcao_of_oi_df.csv"


# Code
here = dirname(sys.frame(1)$ofile)
filename = input_file
fullpath <- file.path(getwd(), filename)

parsed_pdf <- pdftools::pdf_text(pdf = fullpath) %>%
  str_split("\n")

# Get table info
output_dataset <- as.list(parsed_pdf)[[1]][11:20]

# Structure table info as dataframe
# obs. choose to exclude finantial operations
output_dataset1 <- output_dataset %>%
  str_trim() %>% 
  str_split("\\s{2,}", simplify = TRUE) %>% 
  data.frame(stringsAsFactors = FALSE) %>% 
  select(X1, X2, X3, X4)


# Name columns
colnames(output_dataset1) <- c("category_code", "category", "budget_operation", "budget_investment")

# Turning values into numbers 
# For "budget_operation" values
output_dataset1$budget_operation <- output_dataset1$budget_operation %>%  
  str_replace_all("\\.", "") %>% 
  str_replace_all(",", ".") %>%
  as.numeric()

# For "oi_autorizado" values
output_dataset1$budget_investment <- output_dataset1$budget_investment %>%  
  str_replace_all("\\.", "") %>% 
  str_replace_all(",", ".") %>%
  as.numeric()

sum(output_dataset1$budget_investment)

# save csv file
write.csv(output_dataset1, file = file.path(getwd(), output_file))
list.files()
