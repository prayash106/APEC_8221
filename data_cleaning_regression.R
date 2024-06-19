#_______________________________________________________________
#  Script Information----APEC 8221: Programming for Econometrics
#_______________________________________________________________
##
## Script Title: Assignment 1
##
## Author: Prayash Pathak Chalise
##
## Date Last Modified: 2023-09-14
##
## Date Created: 2023-09-10
##
## Email: patha106@umn.edu
##
## ___________________________

#____________________________
#  Loading required libraries
#____________________________
#loading required libraries
library(rio)
library("data.table")

#____________________________
#  Question 1:
#The data are in a .csv format. Read the data into R.
#____________________________

#importing the given data set as data table with name WDI
WDI <- import(file="../data/WDI_Transport.csv", setclass = "data.table") 

#____________________________
#  Question 2.1:
#Using a naming convention adjust the column names so they are short,informative and consistent.
#____________________________

#using colnames to view the given names and change them to underscore separated convention
colnames(WDI)
colnames(WDI) <- c("year", "region", "inc_grp", "coun_code", "country", "at_freight", 
                  "at_pass_carried", "at_carrier_dep", "CPT", "LSCI", "pp_diesel",
                  "pp_gasoline", "rail_lines", "rail_goods_trans", "rail_pass_carried")

#____________________________
#  Question 2.2:
#Review all categorical variables and change any blank values to NA. 
#____________________________

#using table function to count empty values in income group
table(WDI$inc_grp, exclude = NULL)

#changing blank values to NA and checking again with table function
WDI$inc_grp[WDI$inc_grp == ""] <- NA
table(WDI$inc_grp, exclude = NULL)

#____________________________
#  Question 2.3(a). 
#Make a dummy variable for countries with zero air transport in a year. 
#____________________________

#using ifelse to create dummies if all three air transport variables equal zero
WDI <- WDI[, dummy := ifelse(
  is.na(WDI$at_freight) & 
  is.na(WDI$at_pass_carried) &
  is.na (WDI$at_carrier_dep), 0, 1)]

#____________________________
#  Question 2.3(a). 
#How many countries have zero air transport per year?
#____________________________

#here, air_trans is a temporary data frame so as to count the dummies by year and countries
air_trans <- WDI[, .(sum = sum(dummy, na.rm = TRUE)), by = c("country", "year")]
air_trans <- air_trans[, .(sum2 = sum(sum, na.rm = TRUE)), by = c("country")]
count <- sum(air_trans$sum == 0)
print(count)
# 32 countries have zero air transport per year.

#____________________________
#  Question 3. 
#Tabulate how many countries have missing values for each of the numeric variables
#by year in the WDI Transport dataset.
#____________________________

#defining the numeric columns 
numeric_columns <- c("at_freight", "at_pass_carried", "at_carrier_dep", "CPT", 
                     "LSCI", "pp_diesel", "pp_gasoline", "rail_lines",
                     "rail_goods_trans", "rail_pass_carried")

#tabulating the required missing values by year in missing data frame. 
missing <- WDI[, lapply(.SD, function(x) .(sum(is.na(x)))),
            by = year, .SDcols = numeric_columns]

#____________________________
#  Question 4.1.  
#Choose a range of years that you feel contains the richest set of data for your variables of interest.
#____________________________

# I choose all the even years starting from 1998 to 2016. 

#____________________________
#  Question 4.2.  
#Why did you choose this particular range?
#____________________________

#The restriction was made based on the missing data frame created above. 
#Looking at the missing data frame, the fuel price variables that are to be used
#in the final regression equation have more richness in this range. 

#____________________________
#  Question 4.3.  
#Make a data frame that represents your choice in time frame and selected variables of interest. 
#____________________________

start_year <- 1998
end_year <- 2016

#The subset created within this year range in named as WDI2 with variables of interest.
WDI2 <- subset(WDI, subset = year %in% seq(start_year, end_year, by = 2),
               select= c("year", "at_freight", "at_pass_carried", "at_carrier_dep",
                         "CPT", "LSCI", "pp_diesel", "pp_gasoline", "rail_lines",
                         "rail_goods_trans", "rail_pass_carried"))

#____________________________
#  Question 5. 
#Descriptive Statistics w/ Loop
#____________________________

#creating an empty data frame named stats
stats <- data.frame(NULL)

#looping over variables in WDI2
for (col_name in names(WDI2)) {
  #calculating mean and sd rounded to 2 and missing values
  mean <- round(mean(WDI2[[col_name]], na.rm = TRUE), 2)
  sd   <- round(sd(WDI2[[col_name]], na.rm = TRUE), 2)
  mvalue <- sum(is.na(WDI2[[col_name]]))
  #creating temp_frame as one-row data frame
  temp_frame <- data.frame(mean = c(mean), sd = c(sd), mvalue = c(mvalue))
  stats <- rbind(stats, temp_frame)
}

#adding names to the temp_frame
temp_frame <- data.frame(variable = names(WDI2))
stats <- cbind(temp_frame, stats)

#____________________________
#  Question 6. 
#Regression and Methods
#____________________________

# linear regression with passenger air transport variables as dependent variable and
# fuel prices as independent variables done in WDI2 data frame created in 4.3.
reg1 <- lm(WDI2$at_pass_carried ~ WDI2$pp_diesel + WDI2$pp_gasoline)
#the regression is stored in reg1.

#____________________________
#  Question 6.1. 
#Check the class of reg1 with the class function.
#____________________________

class(reg1)
#The class of reg1 is lm.

#____________________________
#  Question 6.2. 
#Try summary(reg1) and summary.lm(reg1).
#____________________________

summary(reg1)
summary.lm(reg1)
#output of both is same.

#____________________________
#  Question 6.3. 
#To reinforce the message, try summary.glm(reg1)
#____________________________

summary.glm(reg1)
 
 
 
 

