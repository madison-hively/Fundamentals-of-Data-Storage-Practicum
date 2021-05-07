getwd()
setwd("/Users/madisonhively/Documents/Data_Analytics_Program")

#install packages
install.packages("readxl")
install.packages("janitor")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")

#load readxl and janitor packages 
library(readxl)
library(janitor)

#import each sheet of excel spreadsheet to environment window 
#this method we can control the column types and make sure they are accurate 
mental_health_1 <- read_excel("Mental Health in Tech Dataset.xlsx", 
                                sheet = "Sheet1",
                              col_types = c("date", 
                                            "numeric", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text")) 
mental_health_2 <- read_excel("Mental Health in Tech Dataset.xlsx", 
                               sheet = "Sheet2",
                              col_types = c("date", 
                                            "numeric", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text"))

#add code to clean column names for each dataset
#this will allow for columns to be in the same format and this is a faster method than renaming each column name 
mental_health_1 <- read_excel("Mental Health in Tech Dataset.xlsx", 
                              sheet = "Sheet1",
                              col_types = c("date", 
                                            "numeric", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text"),
                              .name_repair = make_clean_names)
mental_health_2 <- read_excel("Mental Health in Tech Dataset.xlsx", 
                              sheet = "Sheet2",
                              col_types = c("date", 
                                            "numeric", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "text", "text"), 
                              .name_repair = make_clean_names)

#load packages 
library(tidyverse)
library(lubridate)
library(tidyr)
library(dplyr)

#combine mental_health_1 and mental_health_2 into one dataframe 
#rename the datafram to mental_health_full
mental_health_full <- rbind(mental_health_1, mental_health_2)

#there are a total of 27 columns and 1259 observations in this dataframe 

#create table with explanations about what each variable in the dataset measures
#reference when creating visualizations in data cleaning and preparation stages
table <- data.frame(
  Column_Name = c("timestamp", "age", "gender", "country", "state", "self_employed",
                  "family_history", "treatment", "work_interfere", "no_employees", "remote_work",
                  "tech_company", "benefits", "care_options", "wellness_program",
                  "seek_help", "anonymity", "leave", "mental_health_consequence",
                  "phys_health_consequence", "coworkers", "supervisor", 
                  "mental_health_interview", "phys_health_interview", "mental_vs_physical",
                  "obs_consequence", "comments"),
  Description = c("Timestamp", "Age of participant", "Gender of participant", "Which country do you live in?",
                  "If you live in the United States, which state or territory do you live in?", 
                  "Are you self-employed?", "Do you have a family history of mental illness?",
                  "Have you sought treatment for a mental health condition?",
                  "If you have a mental health condition, do you feel that it interferes with your work?",
                  "How many employees does your company or organization have?",
                  "Do you work remotely (outside of an office) at least 50% of the time?",
                  "Is your employer primarily a tech company/organization?",
                  "Does your employer provide mental health benefits?",
                  "Do you know the options for mental health care your employer provides?",
                  "Has your employer ever discussed mental health as part of an employee wellness program?",
                  "Does your employer provide resources to learn more about mental health issues and how to seek help?",
                  "Is your anonymity protected if you choose to take advantage of mental health or substance abuse treatment resources?",
                  "How easy is it for you to take medical leave for a mental health condition?",
                  "Do you think that discussing a mental health issue with your employer would have negative consequences?",
                  "Do you think that discussing a physical health issue with your employer would have negative consequences?",
                  "Would you be willing to discuss a mental health issue with your coworkers?",
                  "Would you be willing to discuss a mental health issue with your direct supervisor(s)?",
                  "Would you bring up a mental health issue with a potential employer in an interview?",
                  "Would you bring up a physical health issue with a potential employer in an interview?",
                  "Do you feel that your employer takes mental health as seriously as physical health?",
                  "Have you heard of or observed negative consequences for coworkers with mental health conditions in your workplace?",
                  "Any additional notes or comments"))

#all column names are currently lowercase 
#change column names to titlecase 
mental_health_full <- mental_health_full %>%
  rename_with(str_to_title)

#there are many missing values and incorrect timestamps for the timestamp variable  
#do not want to remove entire rows that only have a missing value for this timestamp variable
#rename those values to NA
mental_health_full$Timestamp <- as.character(mental_health_full$Timestamp)
mental_health_full$Timestamp[is.na(mental_health_full$Timestamp)] <- " "
mental_health_full$Timestamp <- recode(mental_health_full$Timestamp, " " = "NA", 
                                       "1905-07-06 00:00:00" = "NA", 
                                       "1905-07-07 00:00:00" = "NA")

#drop rows with missing values 
mental_health_full <- mental_health_full %>% drop_na()

#remove negative rows and incorrectly formatted rows for age variable
summary(mental_health_full)
mental_health_full = mental_health_full[mental_health_full$Age >= 0,]
mental_health_full = mental_health_full[mental_health_full$Age <= 100,]

#create bar graph of ages to identify any outliers 
ggplot(mental_health_full) +
  geom_bar(aes(x = Age, y = frequency(Age)), stat = "identity") +
  ggtitle("Ages of Participants")
  
#outliers appear below 18 and around 65 and 72 
#remove values below 18, are likely incorrect but there is no way of knowing the true values 
#keep 65 and 72 values as they are not much different than bulk of ages 
mental_health_full = mental_health_full[mental_health_full$Age >= 18,]

#load tools package
#many of the values for the gender variable are lowercase
#change all values for gender variable to title case 
library(tools)
mental_health_full$Gender <- toTitleCase(mental_health_full$Gender)

#rename values for gender variable to appropriate values
mental_health_full$Gender <- recode(mental_health_full$Gender, "2" = "Unsure", 
              "1" = "Unsure", "Agender" = "Non-Binary", "All" = "Unsure", 
              "Androgyne" = "Androgynous", "Cis Female" = "Female", "Cis Male" = "Male",
              "Cis-Female/Femme" = "Female", "Enby" = "Non-Binary", "f" = "Female",
              "F" = "Female", "Femail" = "Female", "Femake" = "Female", 
              "Female (Cis)" = "Female", "Female (Trans)" = "Trans-Female", 
              "Fluid" = "Non-Binary", "Genderqueer" = "Non-Binary", 
              "Guy (-Ish) ^_^" = "Male", "m" = "Male", "M" = "Male", "Mail" = "Male",
              "Maile" = "Male", "Make" = "Male", "Mal" = "Male", "Male (CIS)" = "Male", 
              "Male Leaning Androgynous" = "Androgynous", 
              "Malr" = "Male", "Msle" = "Male", "Man" = "Male", "Nah" = "Unsure", 
              "Neuter" = "Non-Binary", "Ostensibly Male, Unsure What that Really Means" = "Male-ish",
              "p" = "Unsure", "Queer/She/They" = "Queer", "Something Kinda Male?" = "Male-ish",
              "Trans Woman" = "Trans-Female", "Woman" = "Female", "Cis Man" = "Male")

#rename values of US to United States to match current format in column for country variable
mental_health_full$Country <- recode(mental_health_full$Country, "US" = "United States")

#a few countries outside of the US have values in the state varaible as US instead of NA
#change values to NA for state variable 
mental_health_full[262, 5] = "NA"
mental_health_full[298, 5] = "NA"
mental_health_full[483, 5] = "NA"

#a few values for the state variable are spelled out fully instead of being abbreviations
#rename those values to state abbreviations
mental_health_full$State <- recode(mental_health_full$State, "California" = "CA", 
                             "New York" = "NY", "Texas" = "TX")

#the remaining columns have a variety of different values, many of which have obscure values that do not match 
#rename those obscure values to appropriate values for each variable 

#rename -, N, and Y values for treatment variable
mental_health_full$Treatment <- recode(mental_health_full$Treatment, "-" = "NA", 
                                       "N" = "No", "Y" = "Yes")

#rename 0 values as NA values for work interfere variable 
mental_health_full$Work_interfere <- recode(mental_health_full$Work_interfere, "0" = "NA")

#create bar graph for work interfere variable 
ggplot(mental_health_full) +
  geom_bar(aes(x = Work_interfere, y = frequency(Work_interfere)), stat = "identity") +
  ggtitle("If participant has a mental health condition, does it interfere with work?")

#work_interfere variable is difficult to interpret
#Does this mean that any answer above never indicates that the participant has a mental health condition? 
#There are many participants that feel as though their mental health condition impacts work

#create bar graph for number of employees variable 
ggplot(mental_health_full) +
  geom_bar(aes(x = No_employees, y = frequency(No_employees)), stat = "identity") +
  ggtitle("How many employees does your company or organization have?")

#no of employees variable contains values of 44201 and 44372
#change 44201 and 44372 values to correspond to 1-25 employees 
mental_health_full$No_employees <- recode(mental_health_full$No_employees, "44201" = "1-25",
                                          "44372" = "1-25")

#rename - values as NA values for tech company variable 
mental_health_full$Tech_company <- recode(mental_health_full$Tech_company, "-" = "NA")

#tech_company variable
ggplot(mental_health_full) +
  geom_bar(aes(x = Tech_company, y = frequency(Tech_company)), stat = "identity") +
  ggtitle("Is your employer primarily a tech company/organization?")

#not all participants work for tech company
#recommendation would be to filter dataset to only analyze data for participants who work for tech companies since 
#this survey measures attitudes toward mental health in tech workplace 

#rename not sure values to Don't know for benefits variable 
mental_health_full$Benefits <- recode(mental_health_full$Benefits, "not sure" = "Don't know",
                                      "Not sure" = "Don't know")

#rename not sure values to Don't know for care options variable 
mental_health_full$Care_options <- recode(mental_health_full$Care_options, "Not sure" = "Don't know")


#rename not sure values to Don't know for seek help variable 
mental_health_full$Seek_help <- recode(mental_health_full$Seek_help, "Not sure" = "Don't know",
                                       "not sure" = "Don't know")

#rename not sure values to Don't know for anonymity variable 
mental_health_full$Anonymity <- recode(mental_health_full$Anonymity , "not sure" = "Don't know")

#rename N and 2 values to No and Don't know for physical health consequence variable 
mental_health_full$Phys_health_consequence <- recode(mental_health_full$Phys_health_consequence, 
                                      "N" = "No", "2" = "Don't know")

#rename 3 and 2 values to Don't know for coworkers variable 
mental_health_full$Coworkers <- recode(mental_health_full$Coworkers, 
                                      "3" = "Don't know", "2" = "Don't know")

#export cleaned dataset as a new excel file  
library(rio)
export(mental_health_full, "Mental Health in Tech Cleaned Data.xlsx")
