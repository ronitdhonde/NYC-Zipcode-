#-----------Airbnb_Zillow_Challenge-------#
#-------Ronit Dhonde---------#
library(tidyverse) 
library(knitr) 
library(ggplot2)
library(kableExtra) 
library(dplyr)
library(rlang)
library(readxl)
library(stringr)
library(rvest)
library(devtools)


#-----Importing Datasets-----#
Revenue_data <- read.csv("C:/Users/ronit/OneDrive/Desktop/data challenge/listings.csv")
View(Revenue_data)
Cost_data<-read.csv("C:/Users/ronit/OneDrive/Desktop/data challenge/Zip_Zhvi_2bedroom (1).csv")
View(Cost_data)

str(Revenue_data) # To understand the classes of variables  
str(Cost_data)

#------Processing Airbnb Dataset-------#
sum(duplicated(Revenue_data))  # Checking duplicated values
sum(is.na(Revenue_data$zipcode)) # Chiecking for NA's in zipcode
unique(Revenue_data$zipcode)  # Indicates there some blank values in the zipcode column
sum(Revenue_data$zipcode == "")  
missing_value_zipcode_percent<-(517/48895)*100 # Indicates around 1% of the values in Zip code are blanks

#-----Distribution of blank zip codes across neighbourhoods--------#
missing_zipcode_list = as.data.frame(table(Revenue_data[Revenue_data$zipcode == "", "neighbourhood"]))
missing_zipcode_list <- missing_zipcode_list[missing_zipcode_list$Freq != 0, ]
names(missing_zipcode_list) <- c("neighbourhood name","Frequency")
missing_zipcode_list <- missing_zipcode_list[order(-missing_zipcode_list$"Frequency"), ]
missing_zipcode_list

#-----Checking distribution of blank zip codes in top 5 neighbourhoods of NY-----#
df_Brooklyn <- missing_zipcode_list[missing_zipcode_list$neighbourhood == "Brooklyn" , ]$Frequency 
Brooklyn_percent = df_Brooklyn/nrow(Revenue_data[Revenue_data$neighbourhood == "Brooklyn" , ])*100 

df_Manhattan <- missing_zipcode_list[missing_zipcode_list$neighbourhood == "Manhattan" , ]$Frequency 
Manhattan_percent = df_Manhattan/nrow(Revenue_data[Revenue_data$neighbourhood == "Manhattan" , ])*100

df_Queens <- missing_zipcode_list[missing_zipcode_list$neighbourhood == "Queens" , ]$Frequency 
Queens_percent = df_Queens/nrow(Revenue_data[Revenue_data$neighbourhood == "Queens" , ])*100  

df_Bedford <- missing_zipcode_list[missing_zipcode_list$neighbourhood == "Bedford-Stuyvesant" , ]$Frequency 
Bedford_percent = df_Bedford/nrow(Revenue_data[Revenue_data$neighbourhood == "Bedford-Stuyvesant" , ])*100  

df_Harlem <- missing_zipcode_list[missing_zipcode_list$neighbourhood == "Harlem" , ]$Frequency 
Harlem_percent = df_Harlem/nrow(Revenue_data[Revenue_data$neighbourhood == "Harlem" , ])*100  

Brooklyn_percent
Manhattan_percent
Queens_percent
Bedford_percent
Harlem_percent

# These blank zip code values can be ignored since they are negligible and evenly distributed

#-----Processing Zillow datasets----#
sum(duplicated(Cost_data))
sum(is.na(Cost_data$RegionName))
sum(is.na(Cost_data$X2017.06))
unique(Cost_data$X2017.06)
unique(Cost_data$RegionName)

# No blank values or NA's are present in the important variables of Zillow dataset

Revenue_data$zipcode <- as.character(Revenue_data$zipcode) # Converting zip codes of Airbnb dataset to character 

sum(is.na(Revenue_data$price))
sum(Revenue_data$price == "")

sum(is.na(Revenue_data$weekly_price))
sum(Revenue_data$weekly_price == "")

sum(is.na(Revenue_data$monthly_price))
sum(Revenue_data$monthly_price == "")

# Moving forward with the Price varible only, since weekly_price and monthly_price have lot of blank values

sum(is.na(Revenue_data$bedrooms))

Revenue_data <- Revenue_data[complete.cases(Revenue_data$bedrooms) ] # Selecting records with info on number of bedrooms
Revenue_data <-  Revenue_data[Revenue_data$zipcode != "", ] # Eliminating entries with missing zip codes

unique(Revenue_data$bedrooms)

Revenue_data <- filter(Revenue_data, bedrooms > 0) # Eliminating records with zero number of bedrooms
Revenue_data$price <- as.numeric(Revenue_data$price)
class(Revenue_data$price)

unique(Revenue_data$zipcode)
sum(Revenue_data$zipcode == "")

important_revenue <- Revenue_data[ , c("zipcode","price","bedrooms")] # Important variables from the Airbnb dataset

class(Cost_data$RegionName)

x <- filter(Cost_data, RegionName>10000 & RegionName<12000) # Filtering zip codes within the NYC region
view(x)

important_cost <- x[ , c("RegionName","X2017.06")] # Important variables from the Zillow Dataset
names(important_cost) <- c("zipcode" , "Recent_Median_Price")

important_cost <-  important_cost %>% mutate(zipcode  = as.character(zipcode))
important_revenue <-  important_revenue %>% mutate(zipcode  = as.character(zipcode))
# Converting both the zip code varibles to character class in order to merge the datasets

combo <- important_revenue %>% left_join(important_cost, by = c("zipcode" = "zipcode"))
view(combo) # Data merged

combo <- combo[is.na(combo$Recent_Median_Price) == FALSE, ] # Ensuring no NA's are present
view(combo)

zipcode_count = as.data.frame(table(combo$zipcode))
names(zipcode_count) = c("zipcode" , "count")  # Created new table to understand the number of records per zip

combo$revenue = 0.75 * 365*combo$price*(2/combo$bedrooms) # Calculating revenue per property
view(combo)

tes <- combo
tes <- tes %>% group_by(zipcode) %>% transmute(zip_rev = mean(revenue)) # Calculating mean revenue for each zip code area
tes <- as.data.frame(tes)
tes = unique(tes)
view(tes)

combo1 <- combo %>% left_join(tes , by = "zipcode")
combo1 <- combo1 %>% left_join(zipcode_count , by = "zipcode") # Merging the zipcode_count with combo to match the zip code and corresponding mean revenue as zip_revenue 
view(combo1)
combo1 <- combo1[combo1$count > 50,] # Eliminating records with less than 50 samples

#-----Calculating Break even point and Profitability Index-------#
revenue_acc_zipcode <-  combo1[ , c("zipcode","Recent_Median_Price","zip_rev")]
revenue_acc_zipcode <- revenue_acc_zipcode %>% as.data.frame() %>%  group_by(zipcode) %>% summarise_at(c("Recent_Median_Price" , "zip_rev"), mean, na.rm = TRUE)
revenue_acc_zipcode <- revenue_acc_zipcode %>%   mutate(break_even = Recent_Median_Price/zip_rev) %>% mutate(ten_year_PI = (10*zip_rev + Recent_Median_Price)/Recent_Median_Price)
view(revenue_acc_zipcode)
knitr::kable(revenue_acc_zipcode) %>% kable_styling()

#-----Calculating the revenue generated in particular zip code properties over a period of time (10 yrs, 15 yrs, 20 yrs, 25 yrs and 30 yrs)----#
revenue_acc_zipcode1 <-  combo1[ , c("zipcode","Recent_Median_Price","zip_rev")]
revenue_acc_zipcode1 <- revenue_acc_zipcode1 %>% as.data.frame() %>%  group_by(zipcode) %>% summarise_at(c("Recent_Median_Price" , "zip_rev"), mean, na.rm = TRUE)
revenue_acc_zipcode1 <- revenue_acc_zipcode1 %>%   mutate(break_even = Recent_Median_Price/zip_rev) %>% mutate(ten_year_PI = (10*zip_rev + Recent_Median_Price)/Recent_Median_Price) %>% mutate(ten_yrs = 10*zip_rev - Recent_Median_Price) %>% mutate(fifteen_yrs = 15*zip_rev - Recent_Median_Price) %>% mutate(twenty_yrs = 20*zip_rev - Recent_Median_Price) %>%  mutate(twentyfive_yrs = 25*zip_rev - Recent_Median_Price) %>% mutate(thirty_yrs = 30*zip_rev - Recent_Median_Price)
knitr::kable(revenue_acc_zipcode1) %>% kable_styling()


write.csv(revenue_acc_zipcode1,"C:/Users/ronit/OneDrive/Desktop/data challenge\\revenue_acc_zipcode1.csv", row.names = F) # Exporting the result dataset

#------Visualizing output data---------#
p<-ggplot(data=revenue_acc_zipcode1, aes(x=zipcode, y=break_even)) +
  geom_bar(stat="identity", width = 0.4, fill="steelblue")
p 
# Zip codes with shortest break even points - 10304, 10305, 11434, 11234
# Zip codes with longest break even points - 10013, 10014

PI<-ggplot(data=revenue_acc_zipcode1, aes(x=zipcode, y=ten_year_PI)) +
  geom_bar(stat="identity", width = 0.4, fill="Red")
PI
PI + coord_flip()
# Higher value of Profitability Index indicates good investment
# Zip codes that are really good investments - 11434, 11234, 10305, 10304

highest_zip_rev <- filter(revenue_acc_zipcode1, zip_rev > 150000)
highest_zip_rev  # Filtering zip codes with high revenues

h<-ggplot(data=highest_zip_rev, aes(x=zipcode, y=zip_rev)) +
  geom_bar(stat="identity", width = 0.5, fill="seagreen4")
h 
# Zip codes that are bound to generate high revenues ($150000+) - 10025, 10304, 10305, 11234, 11434


low_investment<-ggplot(data=revenue_acc_zipcode1, aes(x=zipcode, y=Recent_Median_Price)) +
  geom_bar(stat="identity", width = 0.5, fill="lightsalmon1")
low_investment
low_investment + coord_flip()
# Zip codes with least investment (<$500000) - 11434, 11234, 10305, 10304

fifteen<-ggplot(data=revenue_acc_zipcode1, aes(x=zipcode, y=fifteen_yrs)) +
  geom_bar(stat="identity", width = 0.5, fill="plum3")
fifteen # Zip codes that haven't provided ROI after 15 yrs - 10003, 10011, 10013, 10014, 10021, 10022, 10023, 10028 

twenty<-ggplot(data=revenue_acc_zipcode1, aes(x=zipcode, y=twenty_yrs)) +
  geom_bar(stat="identity", width = 0.5, fill="springgreen2")
twenty # Zip codes that haven't provided ROI after 20 yrs - 10013, 10014 

twenty_five<-ggplot(data=revenue_acc_zipcode1, aes(x=zipcode, y=twentyfive_yrs)) +
  geom_bar(stat="identity", width = 0.5, fill="lightcoral")
twenty_five # Zip codes that haven't provided ROI after 25 yrs - 10013

thirty<-ggplot(data=revenue_acc_zipcode1, aes(x=zipcode, y=thirty_yrs)) +
  geom_bar(stat="identity", width = 0.5, fill="slateblue")
thirty

ggarrange(fifteen, twenty, twenty_five, thirty + rremove("x.text"), 
          labels = c("Revenue in 15", "Revenue in 20", "Revenue in 25", "Revenue in 30"),
          ncol = 2, nrow = 2)
# Combining all the 4 plots in one frame

# From the analysis, we can conclude that properties in the following zip codes will generate the most and quickest profits for our client
# 10304 (Staten Island)
# 10305 (Staten Island)
# 11234 (Brooklyn)
# 11434 (Queens)


#---------End of Analysis--------#