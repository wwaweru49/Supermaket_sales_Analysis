#1.ASK
#1.0 Business task
#.The average sales amount and the number of sales at each branch 
#.The average unit price for each product line
#.Do branches make more sale on a specific day of the week
#.distribution of average sales amount by gender
#.distribution of average sales amount by gender per branch
#which is the most popular product line by quantity sold
#which is the most profitable product line
#1.1 Stakeholders
#stakeholders include the following
#.Marketing & sales director
#.Analytics team
#.Sales team
#2.PREPARE
#.The data is located on Kaggle & contains sales data of different branches over a 3 month period.
#.Data is organized in csv files
#.Credibility of data not in question
#.This data has been stripped of all identifying information ensuring its privacy
#3.PROCESS
#.For this project I choose RStudio Desktop in order to prepare, process, clean, analyze and create the visualizations.
#Data review involved the following:
#.Checking column names across all the 12 original files.
#.Checking for missing values.
#.Checking of white spaces.
#.Checking of duplicate records.
#3.1 COLLECT & DATA WRANGLING
#load readr for reading rectangular data
#load dplyr for data wrangling
#load ggplot2 for data visualization
library(dplyr)
library(readr)
library(ggplot2)
#read data set using the read_csv function
supermarket_sales<-read_csv("C:/Users/user/Desktop/RSTUDIO/supermarket_sales.csv")
#3.2 Data Validation
head(supermarket_sales,5)
tail(supermarket_sales)
str(supermarket_sales)
glimpse(supermarket_sales)
#4Data Cleaning
#Check for missing values
colSums(is.na(supermarket_sales))
#check for duplicates
distinct(supermarket_sales)
#5.Analysis
#.The average sales amount per branch
avg_sales_amnt<-supermarket_sales%>%
  group_by(Branch)%>%
  summarise(avg_sales=mean(Total))
avg_sales_amnt
#.The average unit price of products in each product line
avg_unit_price<-supermarket_sales%>%
  group_by(Product_line)%>%
  summarise(avg_unit_price=mean(Unit_price))%>%
  arrange(desc(avg_unit_price))
avg_unit_price
#.distribution of average sales amount by gender
avg_sales_by_gender<-supermarket_sales%>%
  group_by(Gender)%>%
  summarise(avg_sales=mean(Total))
avg_sales_by_gender
#.distribution of average sales amount by gender per branch
avg_sales_by_gender_branch<-supermarket_sales%>%
  group_by(Gender,Branch)%>%
  summarise(avg_sales=mean(Total))
avg_sales_by_gender_branch
#which is the most popular product line by quantity sold
popular_product<-supermarket_sales%>%
  group_by(Product_line)%>%
  summarise(avg_qnty=mean(Quantity))%>%
  arrange(desc(avg_qnty))
popular_product
#which is the most profitable product line
profitable_product<-supermarket_sales%>%
  group_by(Product_line)%>%
  summarise(avg_gross_income=mean(gross_income))%>%
  arrange(desc(avg_gross_income))
profitable_product
#6.VISUALIZATON
#.The average sales amount per branch
ggplot(avg_sales_amnt, aes(x=Branch,y=avg_sales))+
         geom_col()
#.The average unit price of products in each product line
ggplot(avg_unit_price,aes(x=Product_line,y=avg_unit_price))+
  geom_col()
#.distribution of average sales amount by gender
ggplot(avg_sales_by_gender,aes(x=Gender,y=avg_sales,color=Gender))+
  geom_col()
#.distribution of average sales amount by gender per branch
ggplot(avg_sales_by_gender_branch,aes(x=Gender,y=avg_sales))+
  geom_col()+
  facet_wrap(~Branch)
#which is the most popular product line by quantity sold
ggplot(popular_product,aes(x=Product_line,y=avg_qnty))+
  geom_col()
#which is the most profitable product line
ggplot(profitable_product,aes(x=Product_line,y=avg_gross_income,color=Product_line))+
  geom_col()

