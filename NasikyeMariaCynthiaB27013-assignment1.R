#Installing packages
install.packages("readxl")
install.packages("readr")
install.packages("DT")
install.packages("ggplot2")
install.packages("TSstudio")

#Calling upon the libraries
library(readxl)
library(tidyverse)
library(dplyr)
library(readr)
library(DT)
library(ggplot2)
library(TSstudio)

#Importing the dataset
data <- read_csv("C:/Users/ADMIN/Desktop/Big data analytics/NASIKYEMARIACYNTHIAB27013-ASSIGNMENT1/ida_credits_to_uganda_09-21-2025.csv")

#Viewing the dataset
View(data)
#There are 30 variables and 191 observations 

#Viewing first 10 rows of dataset
head(data, n=10)

#To view the columns and rows in the dataset
str(data)

#To view the data types of each variable
sapply(data,class)

####QN.1 Run a time series analysis that explains the trend of World bank disbursement to Uganda(hint: use the variable "Disbursed Amount(US$)")
data$`Board Approval Date` <- as.Date(data$`Board Approval Date`, format = "%d/%m/%Y")

disbursement_trend <- data %>%
  mutate(year = year(`Board Approval Date`)) %>%  
  group_by(year) %>%                             
  summarize(total_disbursed = sum(`Disbursed Amount (US$)`, na.rm = TRUE))  

disbursement_trend


##Visualising the trend
# Convert to millions
data$disbursed_millions <- data$`Disbursed Amount (US$)` / 1000000

ggplot(data, aes(x = format(`Board Approval Date`, "%Y"), y = disbursed_millions)) +
  geom_col(fill = "steelblue") +
  labs(title = "World Bank Funds to Uganda",
       x = "Year", 
       y = "Millions of US Dollars") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Most of the disbursed amounts are very little


####QN.2 Describe the overall "Credit Status" of Uganda based on the provided dataset
#Describing the structure of credit status
str(data$`Credit Status`)
##Credit status is a categorical variable

#Checking the levels of
levels(data$`Credit Status`)
#There are no unique categories under credit status

#check for missing information
is.na(data$`Credit Status`)
missing <- sum(is.na(data$`Credit Status`))
#There are no missing values

#Generating the statistics of credit status
summary(data$`Credit Status`)
cstatus_stats <- table(data$`Credit Status`)
cstatus_stats
##There are 3 approved credit statuses, 10 disbursing, 8 disbursing and repaying, 1 effective, fully cancelled, 2 fully disbursed, 96 fully paid, 65 repaying and 2 terminated.

#Generating mode for credit status
#Generate a function for mode
getmode = function(v) {
  uniqv = unique(v) 
  uniqv[which.max(tabulate(match(v, uniqv)))]}

v = data$`Credit Status`
mode_cstatus = getmode(v)
mode_cstatus
#The most frequent credit status is fully repaid

#Visualise the distribution of credit status using a bar plot
cstatus_stats <- table(data$`Credit Status`)
cstatus_stats

# Visualise the distribution of credit status using a bar plot
cstatus_barplot <- barplot(cstatus_stats, border = F,
                           names.arg = names(cstatus_stats), 
                           las = 2,
                           col = rainbow(n = length(cstatus_stats)), 
                           ylim = c(0, max(cstatus_stats) * 1.1),
                           main = "Distribution of Credit Status",
                           xlab = "Credit Status",  
                           ylab = "Count of Loans") 

abline(v=c(4.9 , 9.7) , col="grey")

##From the bar plot, fully paid credit is the highest credit status of Uganda and effective is the lowest credit status. Terminated and fully disbursed have the same distribution and are the second lowest credit sstatuses. Repaying is the 2nd highest followed by disbursing then disbursing and repaying, fully cancelled, and then approved as the 3rd lowest.



####QN.3 Explain the "Original principal amount" Uganda borrowed from the world bank, what patterns can you deduce from it?
##Checking for the type of variable
str(data$`Original Principal Amount (US$)`)
#Original principal amount is a continuous variable

##Generating statistics of original principal amount
summary(data$`Original Principal Amount (US$)`)

#The average original principal amount is US$ 62198592
#The lowest original principal amount is US$ 0
#The highest original principal amount is US$ 518000000
#The median original principal amount is US$ 29000000

##The median is larger than the maen showing that the distribution is left-skewed.

#Finding the mode original principal amount
#Generate a function for mode
getmode = function(v) {
  uniqv = unique(v) 
  uniqv[which.max(tabulate(match(v, uniqv)))]}

v = data$`Original Principal Amount (US$)`
mode_opa = getmode(v)
mode_opa
#The most frequent original principal amount is 1e+08

#Check for missing values
is.na(data$`Original Principal Amount (US$)`)
sum_opa <- sum(is.na(data$`Original Principal Amount (US$)`))
#There are no missing values

##Visualise the original principal amount using a histogram
ggplot(data, aes(x = `Original Principal Amount (US$)`)) +
  geom_histogram(binwidth = 5000000,
                 fill = "maroon",
                 color = "black",
                 alpha = 0.8) +
  labs(title = "Distribution of Original Principal Amount",
       x = "Original Principal Amount (US$)",
       y = "Count")

#Finding the relationship between original principal amount and credit status
ggplot(data, aes(x = `Credit Status`, y = `Original Principal Amount (US$)`)) +
  geom_boxplot() +
  labs(title = "Relationship between credit status and original principal amount",
       y = "Original Principal Amount (US$)",
       x = "Credit Status")
  