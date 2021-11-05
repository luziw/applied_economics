

# how to read in and clean the data for macro problem set 3 ---------------


#' first, delete the last two rows containing the following in every excel sheet 
#' (this confuses R and I haven't found another way to deal with that)
  #' Special value	
  #'  :	not available

#' INFO ABOUT THE DATA
#' e, u and i the colnames correspond to employment, unemployment, and inactivity
#' 
#' Examples:
#'  e_e tells you how many people transitioned from employed status (in the previous period) to employed status in the current period
#'  e_u tells you how many people transitioned from employed status (in the previous period) to unemployed status in the current period
#'  
#'  _f and _m correspond to male and female 
#'  data without _f or _m means both genders (data for the whole country, not separated by gender)
#'  
#'  aut_ and gr_ correspond to austria and greece 


#' SET YOUR WORKING DIRECTORY
#' set your working directory to wherever you files are:
#' 
#' use the files manager to the right, navigate to the right folder on your computer, 
#' click on the gear symbol and select "set as working directory"
#' 
#' or write you path like this and run it in the console
#' setwd("~/Documents/Applied Economics/Applied Macroeconomics/Uebung/ProblemSet3")

# LIBRARIES
library(tidyverse) # for cleaning and everything fun with R
library(readxl)    # best package for reading in excel sheets 

#' we differentiate between female, male and total datasets for now and join them later 
#' (this makes it easier to work with the messy colnames)

# read in all excel sheets at once ---------------------------------------------
# load all female datasets
temp_f <- list.files(pattern="*_f.xlsx")
excel_files_female <- lapply(temp_f, read_xlsx)

# load all male datasets
temp_m <- list.files(pattern="*_m.xlsx")
excel_files_male <- lapply(temp_m, read_xlsx)

# load all datasets (we remove female and male a few lines below - see line 67)
temp_t <- list.files(pattern="*.xlsx")
excel_files_total <- lapply(temp_t, read_xlsx)

# convert the list to a data frame and add a gender column
data_female <- excel_files_female %>%
  reduce(left_join, by = "Date") %>% 
  # create gender column
  mutate(gender = "female")

data_male <- excel_files_male %>%
  reduce(left_join, by = "Date") %>%
  # create gender column
  mutate(gender = "male")

data_total <- excel_files_total %>%
  reduce(left_join, by = "Date") %>% 
  # select every column except those that are female or male
  select(everything(), -ends_with("_f"), -ends_with("_m")) %>% 
  # create gender column 
  mutate(gender = "all")

#' ADD COUNTRY COLUMN ----------------------------------------------------------
#' to add a country column, we separate the datasets into austrian and greek data
#' there are more elegant ways to do this 
#' but the code below works and is relatively easy to grasp

#' female greece data  
#' this creates a new dataframe called data_female_gre
#' first, we select date, gender and all columns that start with "gr_"
#' then, we create a country column, which contains "GRE" for every observation 
data_female_gre <- data_female %>% 
  select(Date, gender, starts_with("gr_")) %>% 
  mutate(country = "GRE")

# male greece data  
data_male_gre <- data_male %>% 
  select(Date, gender, starts_with("gr_")) %>% 
  mutate(country = "GRE")

# total greece data
data_total_gre <- data_total %>% 
  select(Date, gender, starts_with("gr_")) %>% 
  mutate(country = "GRE")

# female austria data
data_female_aut <- data_female %>% 
  select(Date, gender, starts_with("aut_")) %>% 
  mutate(country = "AUT")

# male austria data
data_male_aut <- data_male %>% 
  select(Date, gender, starts_with("aut_")) %>% 
  mutate(country = "AUT")

# total austria data
data_total_aut <- data_total %>% 
  select(Date, gender, starts_with("aut_")) %>% 
  mutate(country = "AUT")

# clean up colnames
clean_colnames <- c("Date", "gender", "e_e", "e_i", "e_u", "i_e", 
                    "i_i", "i_u", "u_e","u_i", "u_u", "country") 

colnames(data_female_aut) <- clean_colnames
colnames(data_male_aut)   <- clean_colnames
colnames(data_female_gre) <- clean_colnames
colnames(data_male_gre)   <- clean_colnames
colnames(data_total_aut)  <- clean_colnames
colnames(data_total_gre)  <- clean_colnames


# joining the data back together
data_cleaned <- data_female_aut %>% 
  full_join(data_male_aut) %>% 
  full_join(data_female_gre) %>% 
  full_join(data_male_gre) %>% 
  full_join(data_total_aut) %>% 
  full_join(data_total_gre)


# write csv for use in problem set 3
data_cleaned %>% write_csv("LM_transitions_data.csv")
