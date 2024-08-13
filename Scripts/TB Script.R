#Script for TB LAM and Anti-TB
##The two datasets are worked on separately and merged/joined- full join

#Load Libraries

needed_packages <- c("tidyverse", "tidyr", "readr", "scales",
                     "ggplot2", "dplyr", "ggthemes", "egg", 
                     "glamr", "glitr", "extrafont", "readxl",
                     "remotes", "stringr", "ggrepel", 
                     "janitor", "patchwork", "RColorBrewer", 
                     "gophr", "magrittr", "gt", "ggpubr", "gtExtras",
                     "gagglr", "rlang", "ggrepel", "ggpol", "data.table",
                     "gtsummary", "webshot2", "openxlsx", "lubridate", "gt",
                     "generics", "googledrive", "googlesheets4", "httpuv")

lapply(needed_packages, library, character.only=TRUE)

#Set up folders
#folder_setup()



#To use this package to pull directly from google drive- install googledrive and googlesheets4 packages
#Also convert the data (.xlsx) stored in the drive to google sheets, otherwise it will not read

#data_lm <- read_sheet("https://docs.google.com/spreadsheets/d/1WIVCCQfm7XjbN0EnxXJLipk5WIALunmxhnYUkHBlNXo/edit?usp=drive_link",
                      #sheet = "8. TB-LAM")


# 1: TB LAM ---------------------------------------------------------------
#Create workbook that will host the data

df_tb_lam <- read_excel("Data/Crypto and TB.xlsx", 
                     sheet = "8. TB-LAM", 
                     col_names = FALSE)
# Use the second row as the column names
df_tb_lam <- df_tb_lam[-1,]
colnames(df_tb_lam) <- df_tb_lam[1, ]
df_tb_lam <- df_tb_lam[-1,]

#Check if the IDs in the data are unique
df_tb_lam <- df_tb_lam %>% 
  group_by(`Client Number (De-identified)`) %>% 
  mutate(Flag = ifelse(n() > 1,
                       "duplicate",
                       "unique")) %>% 
  ungroup() %>% 
  distinct(`Client Number (De-identified)`, .keep_all=TRUE)

#Rename columns to standard names
df_tb_lam <- df_tb_lam %>% 
  rename("ip" = `Implementing Partner`,
         "facility" = `Facility Name`,
         "mfl_code" = `Facility MFL Code`,
         "county" = `County`,
         "client_id" = `Client Number (De-identified)`,
         "dob" = `Date of Birth`,
         "sex" = `Sex`,
         "tb_lam_test_date" = `TB-LAM testing date`,
         "tb_lam_results" = `TB-LAM Results`) %>% 
  select(-Flag)

#Convert dob and tb_lam_test_date to numeric and then to date format- dates from excel file converted to date values during data call
df_tb_lam$dob <- convertToDateTime(as.numeric(df_tb_lam$dob), 
                                   origin = "1900-01-01")
df_tb_lam$tb_lam_test_date <- convertToDateTime(as.numeric(df_tb_lam$tb_lam_test_date), 
                                                origin = "1900-01-01")

df_tb_lam$sex <- as.factor(df_tb_lam$sex)
df_tb_lam$tb_lam_results <- as.factor(df_tb_lam$tb_lam_results)

#Convert the all capitalized county column elements to string titled format
df_tb_lam$county <- str_to_title(df_tb_lam$county, 
                                 locale = "en")
#Tables to check whether there is any missing data
table(df_tb_lam$county, 
      useNA = "ifany")
table(df_tb_lam$sex, 
      useNA = "ifany")
table(df_tb_lam$tb_lam_results, 
      useNA = "ifany")

#Create workbook and write the data there
tb_lam_data <- createWorkbook()
addWorksheet(tb_lam_data, 
             sheetName = "TB LAM")
writeData(tb_lam_data, sheet = "TB LAM", 
          x = df_tb_lam)
saveWorkbook(tb_lam_data, 
             paste0("Dataout/TB_LAM Data", ".xlsx"), 
             overwrite = T)

# 2: Anti-TB -----------------------------------------------------------------
#Create workbook that will host the data

df_anti_tb <- read_excel("Data/Crypto and TB.xlsx", 
                        sheet = "9. Anti-TB drugs", 
                        col_names = FALSE)
# Use the second row as the column names
df_anti_tb <- df_anti_tb[-1,]
colnames(df_anti_tb) <- df_anti_tb[1, ]
df_anti_tb <- df_anti_tb[-1,]

#Check if the IDs in the data are unique
df_anti_tb <- df_anti_tb %>% 
  group_by(`Client Number (De-identified)`) %>% 
  mutate(Flag = ifelse(n() > 1,
                       "duplicate",
                       "unique")) %>% 
  ungroup() %>% 
  distinct(`Client Number (De-identified)`, .keep_all=TRUE)

#Rename columns to standard names
df_anti_tb <- df_anti_tb %>% 
  rename("ip" = `Implementing Partner`,
         "facility" = `Facility Name`,
         "mfl_code" = `Facility MFL Code`,
         "county" = `County`,
         "client_id" = `Client Number (De-identified)`,
         "dob" = `Date of Birth`,
         "sex" = `Sex`,
         "tb_tx_date" = `Date initiating Anti-TB drugs`) %>% 
  select(-Flag)

#Convert dob and tb_tx_date to numeric and then to date format- dates from excel file converted to date values during data call
df_anti_tb$dob <- convertToDateTime(as.numeric(df_anti_tb$dob), 
                                   origin = "1900-01-01")
df_anti_tb$tb_tx_date <- convertToDateTime(as.numeric(df_anti_tb$tb_tx_date), 
                                                origin = "1900-01-01")
df_anti_tb$sex <- as.factor(df_anti_tb$sex)
#Convert the all capitalized county column elements to string titled format
df_anti_tb$county <- str_to_title(df_anti_tb$county, 
                                 locale = "en")

#Tables to check whether there is any missing data
table(df_anti_tb$county, 
      useNA = "ifany")
table(df_anti_tb$sex, 
      useNA = "ifany")

#Create workbook and write the data there
anti_tb_data <- createWorkbook()
addWorksheet(anti_tb_data, 
             sheetName = "Anti TB")
writeData(anti_tb_data, sheet = "Anti TB", 
          x = df_anti_tb)
saveWorkbook(anti_tb_data, 
             paste0("Dataout/Anti_TB Data", ".xlsx"), 
             overwrite = T)



