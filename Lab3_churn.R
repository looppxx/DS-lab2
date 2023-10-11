Churn_Train = read.csv("C:/Users/DELL/OneDrive - Universiti Teknologi PETRONAS/2nd 2nd/Data Science/Lab/Lab 2/Churn_Train.csv", header=TRUE) 
Churn_Train

#1. replace missing values
# Replace missing values with 0
Churn_Train$Monthly.Charges[is.na(Churn_Train$Monthly.Charges)] <- 0
Churn_Train$Monthly.Charges

#rewrite data
Churn_Train = read.csv("C:/Users/DELL/OneDrive - Universiti Teknologi PETRONAS/2nd 2nd/Data Science/Lab/Lab 2/Churn_Train.csv", header=TRUE) 
# Replace missing values with the mean
Churn_Train$Monthly.Charges[is.na(Churn_Train$Monthly.Charges)] <- mean(Churn_Train$Monthly.Charges, na.rm = TRUE)
Churn_Train$Monthly.Charges

#rewrite data
Churn_Train = read.csv("C:/Users/DELL/OneDrive - Universiti Teknologi PETRONAS/2nd 2nd/Data Science/Lab/Lab 2/Churn_Train.csv", header=TRUE) 
# Replace missing values with the median
Churn_Train$Monthly.Charges[is.na(Churn_Train$Monthly.Charges)] <- median(Churn_Train$Monthly.Charges, na.rm = TRUE)
Churn_Train$Monthly.Charges

#2. 
summary(Churn_Train)
str(Churn_Train)
colSums(is.na(Churn_Train))
Churn_Train$Monthly.Charges

#3. histogram
library(ggplot2)
library(dplyr)
library(cowplot)
ggplot(Churn_Train, aes(Monthly.Charges)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  ggtitle("Variable distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

#4. Perform simple value imputation and view the data
#impute with value
install.packages("modeest") 
library(modeest)

#rewrite data
churn = read.csv("C:/Users/DELL/OneDrive - Universiti Teknologi PETRONAS/2nd 2nd/Data Science/Lab/Lab 2/Churn_Train.csv", header=TRUE) 

value_imputed <- data.frame(
  original = Churn_Train$Monthly.Charges,
  imputed_zero = replace(Churn_Train$Monthly.Charges, 
                         is.na(Churn_Train$Monthly.Charges), 0),
  imputed_mean = replace(Churn_Train$Monthly.Charges, 
                         is.na(Churn_Train$Monthly.Charges), mean(Churn_Train$Monthly.Charges, na.rm = TRUE)),
  imputed_median = replace(Churn_Train$Monthly.Charges, 
                           is.na(Churn_Train$Monthly.Charges), median(Churn_Train$Monthly.Charges, na.rm = TRUE)),
  imputed_mode = replace(Churn_Train$Monthly.Charges, 
                         is.na(Churn_Train$Monthly.Charges), mfv(Churn_Train$Monthly.Charges, na.rm = TRUE))
)
value_imputed

h1<-ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = 
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(value_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = 
                   "identity") +
  ggtitle("Zero-imputed distribution") +
  theme_classic()
h3 <- ggplot(value_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = 
                   "identity") +
  ggtitle("Mean-imputed distribution") +
  theme_classic()
h4 <- ggplot(value_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = 
                   "identity") +
  ggtitle("Median-imputed distribution") +
  theme_classic()
plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

library(mice)
# Select columns, note the backticks around variable names with spaces
churn_numeric <- churn %>%
  select(`Monthly.Charges`, `Total.Charges`)

# Check missing data pattern
md.pattern(churn_numeric)

# Impute missing values
mice_imputed <- data.frame(
  original = churn$`Total.Charges`,
  imputed_pmm = complete(mice(churn_numeric, method = "pmm"))$`Total.Charges`,
  imputed_cart = complete(mice(churn_numeric, method = "cart"))$`Total.Charges`,
  imputed_lasso = complete(mice(churn_numeric, method = "lasso.norm"))$`Total.Charges`
)
mice_imputed

h1 <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = 
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = 
                   "identity") +
  ggtitle("Pmm-imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = 
                   "identity") +
  ggtitle("Cart-imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = 
                   "identity") +
  ggtitle("Lasso-imputed distribution") +
  theme_classic()
plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

#Activity 3 – Imputation with R missForest Package
install.packages("missForest")
library(missForest)
missForest_imputed<-data.frame(
  original=churn_numeric$`Total.Charges`,
  imputed_missForest=missForest(churn_numeric)$ximp$`Total.Charges`
)
missForest_imputed

h1 <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = 
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(missForest_imputed, aes(x = imputed_missForest)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = 
                   "identity") +
  ggtitle("missForest-imputed distribution") +
  theme_classic()
plot_grid(h1, h2, nrow = 1, ncol = 2)

#Activity 4: Normalize data with scaling methods
log_scale=log(as.data.frame(churn$`Total.Charges`))

library(caret)
# Apply Min-Max Scaling
process<-preProcess(as.data.frame(churn$`Total.Charges`),method=c("range"))
norm_scale<-predict(process,as.data.frame(churn$`Total.Charges`))

scale_data<-as.data.frame(scale(churn$`Total.Charges`))

#Activity 5: Feature Encoding
#label encoding
#process of replacing the different levels of a categorical variable with dummy numbers
gender_encode<-ifelse(churn$Gender=="Male",1,0)
table(gender_encode)

churn_encode<-ifelse(churn$`Multiple Lines`=="Yes",1,
                     ifelse(churn$`Multiple Lines`=="No",2,
                            ifelse(churn$`Multiple Lines`=="No phone service",3,0)))
table(churn_encode)

#One hot encoding
new_dat=data.frame(churn$`Total Charges`,churn$Gender,churn$`Multiple Lines`)
summary(new_dat)

library(caret)
dmy<-dummyVars("~.",data=new_dat,fullRank = T)
dat_transformed<-data.frame(predict(dmy,newdata=new_dat))

glimpse(dat_transformed)

#Encoding Continuous(or Numeric) variables
summary(new_dat$churn..Total.Charges.)

# Ensure that the data is numeric
new_dat$churn..Total.Charges. <- as.numeric(new_dat$churn..Total.Charges.)

#create a vector of cut-off points based on 1st Quarter value and 3rd Quarter values
bins<-c(-Inf,7.91,31.00,Inf)
#gives the respective names to these cut-off points
bin_names<-c("Low","Mid50","High")
# cut() function to break the vector using the cut-off points
new_dat$new_Fare<-cut(new_dat$churn..Total.Charges.,breaks=bins,labels=bin_names)

summary(new_dat$churn..Total.Charges.)
summary(new_dat$new_Fare)

library(tidyverse)
library(dplyr)
library(dlookr)
library(ISLR)
library(EDA)
churn %>%
  eda_web_report(target="Total.Charges",subtitle="Churn_Train",
                 output_dir="C:/Users/DELL/OneDrive - Universiti Teknologi PETRONAS/2nd 2nd/Data Science/Lab/Lab 3",
                 output_file="EDA.html",theme="blue")


