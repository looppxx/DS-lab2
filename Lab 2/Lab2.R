install.packages("tidyverse")
install.packages("dplyr")
install.packages("dlookr")
install.packages("ISLR")
install.packages("EDA")

library(tidyverse)
library(dplyr)
library(dlookr)
library(ISLR)
library(EDA)

# Activity 1: Data Exploration
str(Carseats)
?Carseats
Carseats

#Calculate descriptive statistics using describe()
describe(Carseats)      #select columns by name
describe(Carseats,Sales,CompPrice,Income) #selects all columns between sales and incomes (include)
describe(Carseats,Sales:Income) #select all column except sales until income
describe(Carseats,-(Sales:Income))

#Test of normality on numeric variables using normality()
normality(Carseats)
normality(Carseats,Sales,CompPrice,Income)
normality(Carseats,Sales:Income)
normality(Carseats,-(Sales:Income))

#plot normality
plot_normality(Carseats,Sales,CompPrice,Income)

#calculate correlate
correlate(Carseats)
correlate(Carseats,Sales,CompPrice,Income)
correlate(Carseats,Sales:Income)
correlate(Carseats,-(Sales:Income))

#plot correlate method 1
plot_correlate(Carseats) #without title

#plot correlate method 2
Carseats %>%            #with title
  correlate() %>%
  plot()

correlate(Carseats,Sales,Price) %>%
  plot()

#EDA based on target variable
categ<-target_by(Carseats,US)

#EDA when target variable is categorical, predictor is numerical
cat_num<-relate(categ,Sales)
cat_num
summary(cat_num)
plot(cat_num)

#EDA when target variable is categorical, predictor is numerical
cat_cat<-relate(categ,ShelveLoc)
cat_cat
summary(cat_cat)
plot(cat_cat)

#EDA when target variable is numerical, predictor is numerical
num<-target_by(Carseats,Sales)
num_num<-relate(num,Price)
num_num
summary(num_num)
plot(num_num)

#EDA when target variable is numerical, predictor is categorical
num_cat<-relate(num,ShelveLoc)
num_cat
summary(num_cat)
plot(num_cat)

Churn_Train = read.csv("C:/Users/DELL/OneDrive - Universiti Teknologi PETRONAS/2nd 2nd/Data Science/Lab/Lab 2/Churn_Train.csv", header=TRUE) 
view(Churn_Train)

#check for missing values for monthly charges
Churn_Train$Monthly.Charges
is.na(Churn_Train)
sum(is.na(Churn_Train$Monthly.Charges)) #sum of missing value
colSums(is.na(Churn_Train))             #sum of column of missing value

#replace with missing value
Churn_Train <- Churn_Train %>%
  mutate(Total.Charges=replace(Total.Charges, is.na(Total.Charges),median(Total.Charges, na.rm=T)))
is.na(Churn_Train$Total.Charges)
summary(is.na(Churn_Train))

#Calculate descriptive statistics using describe()
describe(Churn_Train)
describe(Churn_Train,Tenure,Monthly.Charges,Total.Charges) #select columns by name
describe(Churn_Train,Tenure:Monthly.Charges) 
describe(Churn_Train,-(Tenure:Monthly.Charges)) 

#test manually
normality(Churn_Train)
normality(Churn_Train,Tenure,Monthly.Charges,Total.Charges) #select columns by name
normality(Churn_Train,Tenure:Monthly.Charges) 
normality(Churn_Train,-(Tenure:Monthly.Charges))

#plot normality
plot_normality(Churn_Train,Monthly.Charges,Total.Charges)

#calculate correlate
correlate(Churn_Train)
correlate(Churn_Train,Tenure,Monthly.Charges,Total.Charges) #select columns by name
correlate(Churn_Train,Tenure:Monthly.Charges) 
correlate(Churn_Train,-(Tenure:Monthly.Charges))

#plot correlate
Churn_Train %>%
  correlate() %>%
  plot()

correlate(Churn_Train,Monthly.Charges,Total.Charges) %>%
  plot()

#EDA based on target variable
Churn_Train$Payment.Method <- as.factor(Churn_Train$Payment.Method)
categ <- target_by(Churn_Train,Payment.Method)

#EDA when target variable is categorical, predictor is numerical
cat_num<-relate(categ,Monthly.Charges)
cat_num
summary(cat_num)
plot(cat_num)

#EDA when target variable is categorical, predictor is categorical
Churn_Train$Paperless.Billing <- as.factor(Churn_Train$Paperless.Billing)
cat_cat<-relate(categ,Paperless.Billing)
cat_cat
summary(cat_cat)
plot(cat_cat)

#EDA when target variable is numerical, predictor is numerical
num<-target_by(Churn_Train,Monthly.Charges)
num_num<-relate(num,Total.Charges)
num_num
summary(num_num)
plot(num_num)

#EDA when target variable is numerical, predictor is categorical
num_cat<-relate(num,Payment.Method)
num_cat
summary(num_cat)
plot(num_cat)

Churn_Train %>%
  eda_web_report(target="Total.Charges",
                 subtitle="Churn_Train",
                 output_dir="C:/Users/DELL/OneDrive - Universiti Teknologi PETRONAS/2nd 2nd/Data Science/Lab/Lab 2",
                 output_file="EDA.pdf",
                 theme="blue",
                 output_format = "PDF" )

