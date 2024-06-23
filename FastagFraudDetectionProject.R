############################### R Project  #################################

######################### Fastag Fraud Detection ###########################

## 1) Descriptive analysis
## 2) Data Preprocessing
## 3) Data Visualization
## 4) Model Development
## 5) Create Model Pipeline
## 6) Model Deployment


## 1) Descriptive analysis

# Let's load dataset  
df <- read.csv('FastagFraudDetection.csv')

# Let's look at first 5 row
head(df)

# Let's look at dataframe with view 
View(df)

# Let's look at dataframe info
str(df)

# Let's look at summary of dataframe
View(summary(df))

# Dimensions (rows and columns)
dim(df) 
nrow(df)
ncol(df)

# Let's look at column names
names(df) 
colnames(df)

# Let's look at target column
tail(df[,'Fraud_indicator'])
df[['Fraud_indicator']]
df$Fraud_indicator

# Let's look at vehicle columns
df[, c("Vehicle_Dimensions", "Vehicle_Speed",'Vehicle_Plate_Number','Vehicle_Type')]

# For select a first row
df[1, ]  

# For select first three rows
df[1:3,]

# Let's filter rows where vehicle type is bus
head(df[df$Vehicle_Type == "Bus ", ],n = 2)

# Let's use subset function
nrow(subset(df, Transaction_Amount > 300))

# Let's look at count of null  values over each columns
null_counts <- colSums(is.na(df))
null_counts

# Okey, missing data is not in dataframe :)


## 2) Data Preprocessing

# Let's divide timestamp column to time and date 
df[['Date','Time']] <- df$Timestamp.split()

# Load necessary libraries
#install.packages("tidyverse")
#library(tidyverse)

# Let's check for rows with infinite values
rows_with_inf <- apply(df, 1, function(row) any(is.infinite(row)))
View(df[rows_with_inf, ])

# Let's split Timestamp into Date and Time
df <- df %>%
  separate(Timestamp, into = c("Date", "Time"), sep = " ") #-- Okey, Great 

# Let's do great only first letter of words of vehicle type
df$Vehicle_Type <- str_to_title(df$Vehicle_Type)

# Let's remove fastag id 
df <- df[, !colnames(df) %in% c('FastagID')]

# Let's group transaction amount and amount paid column 
group <- function(x) {
  ifelse(x < 100, '<100',
         ifelse(x < 200, '100-200',
                ifelse(x < 300, '200-300', '300+')))
}

# Create a new column with transaction amount groups
df$Transaction_Amount_Group <- group(df$Transaction_Amount)


# SOLUTION 2: 

# Let's define the amount paid groups
cut_points <- c(0, 100, 200, 300, Inf)
group_names <- c('<100', '100-200', '200-300', '300+')

# Let's create a new column with amount paid groups
df$Amount_Paid_Group <- cut(df$Amount_paid, breaks = cut_points, labels = group_names, include.lowest = TRUE)

# Let's define the vehicle speed groups
cut_points <- c(0, 30, 60, 80 , 100 , Inf)
group_names <- c('<30', '30-60', '60-80', '80-100','100+')

# Let's create a new column with transaction amount groups
df$Vehicle_Speed_Group <- cut(df$Vehicle_Speed, breaks = cut_points, labels = group_names, include.lowest = TRUE)

# Let's find ratio transaction amount and amount paid
divide <- function(x) {
  ifelse(is.na(x) | x == 0, 
         0, 
         round(df$Amount_paid / x, digits = 3)
  )
}

df$Transaction_Amount_Ratio <- divide(df$Transaction_Amount)

# Let's seperate Geographical_Location column to long and lat 
df <- df %>%
  separate(Geographical_Location, into = c("Longitude", "Latitude"), sep = " ") 

# Let's define fraud column as numeric for calculate process
df$Fraud_Number <- ifelse(df$Fraud_indicator == 'Fraud', 1, 0)

# Let's look at columns
colnames(df)

# Let's add new columns whether weekend or weekday
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
df$weekend <- ifelse(weekdays(df$Date) %in% c("Saturday", "Sunday"), 1, 0)

# Let's create column and add day of week
df$weekdays <- weekdays(df$Date)

# Let's create pm and am clock
df$Time1 <- as.POSIXct(df$Time, format = "%H:%M")
df$clock <- ifelse(hour(df$Time) >= 12 & hour(df$Time) < 24, 1, 0)
df <- df[, !colnames(df) %in% c("Time1")]

## 3) Data Visualization

# • Scatter Plot
plot(df$Transaction_Amount, df$Vehicle_Speed, main = "Transaction Amount & Vehicle Speed", xlab = "Amount", ylab = "Speed", col = "darkgreen", pch = 1, cex = 1.2, font.main = 9)

# • Histogram
hist(df$Amount_paid, main = "Amount Paid Group", xlab = "Groups", ylab = "Frequency", col = "green", border = "blue", font.main = 3)

# • Bar Plot
barplot(table(df$Vehicle_Type), main = "Frequency of Vehicle Types", xlab = "Vehicle Type", ylab = "Frequency", col = "blue")
text(x = 1:length(counts), y = counts + 11, labels = counts, pos = 3, cex = 0.9, col = "black", xpd = TRUE)

# • Bar Plot
lane_type_avg <- aggregate(Fraud_Number ~ Lane_Type, data = df, mean)
barplot(lane_type_avg$Fraud_Number, main = "Frequency of Fraud by Lane Type", xlab = "Lane Type",ylab = "Frequency", col = "blue", names.arg = lane_type_avg$Lane_Type)

# • Box Plot 
boxplot(df$Transaction_Amount, df$Amount_paid, names = c("Transaction Amount", "Amount Paid"), main = "Transaction Amount & Amount Paid", col = c("blue", "green"), border = "black", font.main = 3)
boxplot(df$Vehicle_Speed, names = c("Speed"), main = "Vehicle Speed", col = c("green"), border = "black", font.main = 3)
boxplot(df$Transaction_Amount_Ratio, names = c("Ratio"), main = "Transaction Amount Ratio", col = c("red"), border = "blue", font.main = 3)

# • Pie Chart
pie(table(df$Transaction_Amount_Group), labels = levels(factor(df$Transaction_Amount_Group)) , main = "Transaction Amount Group", col = rainbow(length(levels(factor(df$Transaction_Amount_Group)))), border = "darkred", font.main = 4)

# • Pie Chart
#install.packages("plotrix")
#library("plotrix")
pie3D(table(df$Transaction_Amount_Group), labels = levels(factor(df$Transaction_Amount_Group)) , main = "Transaction Amount Group", col = rainbow(length(levels(factor(df$Transaction_Amount_Group)))), border = "darkred", font.main = 4)

# • Line Plot
library("tidyverse")
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
daily_sum <- aggregate(Transaction_Amount ~ Date, data = df, sum)
plot(daily_sum$Date, daily_sum$Transaction_Amount, type = "l", 
     main = "Daily Sum of Transaction Amount", 
     xlab = "Date", ylab = "Sum of Transaction Amount", 
     col = "red", lwd = 2, font.main = 2)

# Let's extract analyze by month
df$MonthName <- format(df$Date, "%B")
monthly_avg <- aggregate(Transaction_Amount ~ MonthName, data = df, mean)

monthly_avg$MonthName <- factor(monthly_avg$MonthName, 
                                levels = c("Yanvar", "Fevral", "Mart", "Aprel", "May", "İyun", 
                                           "İyul", "Avqust", "Sentyabr", "Oktyabr", "Noyabr", "Dekabr"),
                                ordered = TRUE)

plot(monthly_avg$MonthName, monthly_avg$Transaction_Amount, type = "l", 
     main = "Daily Sum of Transaction Amount", 
     xlab = "Date", ylab = "Sum of Transaction Amount", 
     col = "red", lwd = 2, font.main = 2)

# • Scatter with ggplot2 Plot
library(ggplot2)
ggplot(df, aes(x = df$Transaction_ID, y = df$Transaction_Amount)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Transaction Id & Amount", x = "Transaction Id", y = "Amount") +
  theme_bw()

# • Bar plot with ggplot2
vehicle_type_avg <- aggregate(Amount_paid ~ Vehicle_Type, data = df, mean)

ggplot(vehicle_type_avg, aes(x = Vehicle_Type, y = Amount_paid)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  labs(title = "Average Amount Paid by Vehicle Type", x = "Vehicle Type", y = "Average Amount Paid") +
  theme_bw()


# Let's find transaction counts by longitude and latitude
transaction_counts <- aggregate(Transaction_ID ~ Longitude + Latitude, data = df, FUN = length)

# Plot the map
install.packages("maps")
library(maps)

world <- map_data("world")
transaction_counts$Longitude <- as.numeric(as.character(transaction_counts$Longitude))
transaction_counts$Latitude <- as.numeric(as.character(transaction_counts$Latitude))

# Plot the map
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = transaction_counts, aes(x = Longitude, y = Latitude, size = Transaction_ID), color = "red") +
  labs(title = "Transaction Count by Location", x = "Longitude", y = "Latitude", size = "Transaction Count") +
  theme_minimal()


## 4) Model Development

# Let's analyze needs : Analyzing Fastag fraud involves identifying 
# patterns and trends in fraudulent transactions to enhance system 
# security and user trust. By leveraging data analytics and machine 
# learning, businesses can predict and prevent future fraudulent activities.
# This proactive approach helps mitigate financial losses and ensures 
# the integrity of the Fastag system. Ultimately, maintaining a secure 
# and reliable Fastag system promotes user satisfaction and supports 
# efficient toll collection.

# Let's remove unnecessary columns from dataframe

df <- df[, !colnames(df) %in% c("Time","Fraud_indicator","Date","Longitude","Latitude","Vehicle_Plate_Number","Transaction_ID")]
# df <- df[,!colnames(df) %in% c('weekend',"weekdayscümə axşamı",'weekdaysçərşənbə axşamı','weekdaysşənbə')]

# Let's dummy some columns
library(dplyr)
cols_to_dummy <- c("Vehicle_Type", "Lane_Type", "TollBoothID", "Vehicle_Dimensions", 
                   "Transaction_Amount_Group", "Amount_Paid_Group", "Vehicle_Speed_Group", "weekdays")

# Creating dummy variables
df_dummies <- df %>%
  select(all_of(cols_to_dummy)) %>%
  model.matrix(~ . - 1, data = .) %>%
  as.data.frame()

# Combining the dummies with the original dataframe excluding the original columns
df <- bind_cols(df %>% select(-all_of(cols_to_dummy)), df_dummies)

# install.packages("randomForest")
library(randomForest)
library(caret)  

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(df$Fraud_Number, p = 0.8, list = FALSE)
trainData <- df[trainIndex,]
testData <- df[-trainIndex,]

# Separate inputs and target for training
trainInput <- trainData[, !colnames(trainData) %in% c('Fraud_Number')]
trainTarget <- trainData$Fraud_Number

# Train the random forest model
rf_model <- randomForest(trainInput, trainTarget, ntree = 100, mtry = 3, importance = TRUE)

# Predict on the test set
testInput <- testData[, !colnames(testData) %in% c('Fraud_Number')]
testTarget <- testData$Fraud_Number
predictions <- predict(rf_model, testInput)
binary_predictions <- ifelse(predictions >= 0.5, 1, 0)

# Evaluate model performance
binary_predictions <- factor(binary_predictions, levels = c(0, 1))
testTarget <- factor(testTarget, levels = c(0, 1))
# Create the confusion matrix
conf_matrix <- confusionMatrix(binary_predictions, testTarget)

# Variable importance
importance(rf_model)
varImpPlot(rf_model)

# Let's remove non important columns
df <- df[,!colnames(df) %in% c('weekend',"weekdayscümə axşamı",'weekdaysçərşənbə axşamı','weekdaysşənbə')]
# --Let's again create model


# Let's predict new_value
new_data <- testInput[1,]
View(new_data)
predictions <- predict(rf_model, new_data)
binary_predictions <- ifelse(predictions >= 0.5, 1, 0) # -------------------
predictions

# install.packages("pROC")
# install.packages("ggplot2")
# library(pROC)
# library(ggplot2)

# Let's calculate the ROC curve and AUC
roc_obj <- roc(testData$Fraud_Number, predictions)
auc_value <- auc(roc_obj)

roc_df <- data.frame(
  tpr = roc_obj$sensitivities, 
  fpr = 1 - roc_obj$specificities, 
  thresholds = roc_obj$thresholds
)

ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed", color = "red") +
  labs(title = paste("ROC Curve (AUC =", round(auc_value, 3), ")"),
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal() # Great ✅


## 5) Create Model Pipeline

# Function 1:
# Let's create model deployment function
data_preprocessing_function <- function(df){
  df <- separate(df, Timestamp, into = c("Date", "Time"), sep = " ")
  df$Vehicle_Type <- str_to_title(df$Vehicle_Type)
  df <- df[, !colnames(df) %in% c('FastagID')]
  group <- function(x) {
    ifelse(x < 100, '<100',
           ifelse(x < 200, '100-200',
                  ifelse(x < 300, '200-300', '300+')))
  }
  df$Transaction_Amount_Group <- group(df$Transaction_Amount)
  
  cut_points <- c(0, 100, 200, 300, Inf)
  group_names <- c('<100', '100-200', '200-300', '300+')
  # Let's create a new column with amount paid groups
  df$Amount_Paid_Group <- cut(df$Amount_paid, breaks = cut_points, labels = group_names, include.lowest = TRUE)
  
  cut_points <- c(0, 100, 200, 300, Inf)
  group_names <- c('<100', '100-200', '200-300', '300+')
  df$Vehicle_Speed_Group <- cut(df$Vehicle_Speed, breaks = cut_points, labels = group_names, include.lowest = TRUE)
  divide <- function(x) {
    ifelse(is.na(x) | x == 0, 
           0, 
           round(df$Amount_paid / x, digits = 3)
    )
  }
  df$Transaction_Amount_Ratio <- divide(df$Transaction_Amount)
  df <- separate(df, Geographical_Location, into = c("Longitude", "Latitude"), sep = " ") 
  df$Fraud_Number <- ifelse(df$Fraud_indicator == 'Fraud', 1, 0)
  df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
  df$weekend <- ifelse(weekdays(df$Date) %in% c("Saturday", "Sunday"), 1, 0)
  df$weekdays <- weekdays(df$Date)
  df$Time <- as.POSIXct(df$Time, format = "%H:%M")
  df$clock <- ifelse(hour(df$Time) >= 12 & hour(df$Time) < 24, 1, 0)
  df <- df[, !colnames(df) %in% c("Time1")]
  
  return(df)
}

# Function 2:
# Let's create model deployment function
model_deployment_function <- function(df, model_file){
  df <- df[, !colnames(df) %in% c("Time","Fraud_indicator","Date","Longitude","Latitude","Vehicle_Plate_Number","Transaction_ID",'weekend',"weekdayscümə axşamı",'weekdaysçərşənbə axşamı','weekdaysşənbə')]
  
  cols_to_dummy <- c("Vehicle_Type", "Lane_Type", "TollBoothID", "Vehicle_Dimensions", 
                     "Transaction_Amount_Group", "Amount_Paid_Group", "Vehicle_Speed_Group", "weekdays")
  
  df_dummies <- df %>%
    select(all_of(cols_to_dummy)) %>%
    model.matrix(~ . - 1, data = .) %>%
    as.data.frame()
  
  df <- bind_cols(df %>% select(-all_of(cols_to_dummy)), df_dummies)
  df_main <<- df
  set.seed(123)
  trainIndex <- createDataPartition(df$Fraud_Number, p = 0.8, list = FALSE)
  trainData <- df[trainIndex,]
  testData <- df[-trainIndex,]
  
  trainInput <- trainData[, !colnames(trainData) %in% c('Fraud_Number')]
  trainTarget <- trainData$Fraud_Number
  
  rf_model <- randomForest(trainInput, trainTarget, ntree = 100, mtry = 3, importance = TRUE)
  
  testInput <- testData[, !colnames(testData) %in% c('Fraud_Number')]
  testTarget <- testData$Fraud_Number
  
  predictions <- predict(rf_model, testInput)
  binary_predictions <- ifelse(predictions >= 0.5, 1, 0)
  
  # Let's evaluate model
  binary_predictions <- factor(binary_predictions, levels = c(0, 1))
  testTarget <- factor(testTarget, levels = c(0, 1))
  conf_matrix <- confusionMatrix(binary_predictions, testTarget)
  print(conf_matrix)

  # Save the model
  saveRDS(rf_model, model_file)
  
  return(df)
}



# Let's applying the model_function to the dataframe
df_processed <- data_preprocessing_function(df)

# Let's create model and save model file as rds
model_file <- "C:/Users/HP/OneDrive/İş masası/R Programming/rf_model.rds"
processed_df <- model_deployment_function(df_processed, model_file)



## 6) Model Deployment 

# Load the saved random forest model
load_model_function <- function(model_file) {
  rf_model <- readRDS(model_file)
  return(rf_model)
}

# Function to make predictions using the loaded model
predict_with_model <- function(rf_model, new_data) {
  # Preprocess new data
  new_data <- separate(new_data, Timestamp, into = c("Date", "Time"), sep = " ")
  new_data$Vehicle_Type <- str_to_title(new_data$Vehicle_Type)
  new_data <- new_data[, !colnames(new_data) %in% c('FastagID')]
  
  group <- function(x) {
    ifelse(x < 100, '<100',
           ifelse(x < 200, '100-200',
                  ifelse(x < 300, '200-300', '300+')))
  }
  new_data$Transaction_Amount_Group <- group(new_data$Transaction_Amount)
  
  cut_points <- c(0, 100, 200, 300, Inf)
  group_names <- c('<100', '100-200', '200-300', '300+')
  
  new_data$Amount_Paid_Group <- cut(new_data$Amount_paid, breaks = cut_points, labels = group_names, include.lowest = TRUE)
  new_data$Vehicle_Speed_Group <- cut(new_data$Vehicle_Speed, breaks = cut_points, labels = group_names, include.lowest = TRUE)
  
  divide <- function(x) {
    ifelse(is.na(x) | x == 0, 0, round(new_data$Amount_paid / x, digits = 3))
  }
  new_data$Transaction_Amount_Ratio <- divide(new_data$Transaction_Amount)
  
  new_data <- separate(new_data, Geographical_Location, into = c("Longitude", "Latitude"), sep = " ")
  new_data$Date <- as.Date(new_data$Date, format = "%m/%d/%Y")
  new_data$weekend <- ifelse(weekdays(new_data$Date) %in% c("Saturday", "Sunday"), 1, 0)
  new_data$weekdays <- weekdays(new_data$Date)
  new_data$Time <- as.POSIXct(new_data$Time, format = "%H:%M")
  new_data$clock <- ifelse(hour(new_data$Time) >= 12 & hour(new_data$Time) < 24, 1, 0)
  
  new_data <- new_data[, !colnames(new_data) %in% c("Time1", "Time", "Fraud_indicator", "Date", "Longitude", "Latitude", "Vehicle_Plate_Number", "Transaction_ID", "weekend", "weekdayscümə axşamı", "weekdaysçərşənbə axşamı", "weekdaysşənbə")]
  
  cols_to_dummy <- c("Vehicle_Type", "Lane_Type", "TollBoothID", "Vehicle_Dimensions", "Transaction_Amount_Group", "Amount_Paid_Group", "Vehicle_Speed_Group", "weekdays")
  
  # Ensure each categorical variable has at least two levels
  for (col in cols_to_dummy) {
    if (length(unique(new_data[[col]])) < 2) {
      new_data[[col]] <- factor(new_data[[col]], levels = c(unique(new_data[[col]]), "dummy_level"))
    }
  }
  
  new_data_dummies <- new_data %>%
    select(all_of(cols_to_dummy)) %>%
    model.matrix(~ . - 1, data = .) %>%
    as.data.frame()
  
  new_data <- bind_cols(new_data %>% select(-all_of(cols_to_dummy)), new_data_dummies)
  
  # List of columns to be checked and added if not present
  cols_to_add <- c(
    "Transaction_Amount", "Amount_paid", "Vehicle_Speed", "Transaction_Amount_Ratio", 
    "clock", "Vehicle_TypeBus ", "Vehicle_TypeCar", "Vehicle_TypeMotorcycle", 
    "Vehicle_TypeSedan", "Vehicle_TypeSuv", "Vehicle_TypeTruck", "Vehicle_TypeVan", 
    "Lane_TypeRegular", "TollBoothIDB-102", "TollBoothIDC-103", "TollBoothIDD-104", 
    "TollBoothIDD-105", "TollBoothIDD-106", "Vehicle_DimensionsMedium", 
    "Vehicle_DimensionsSmall", "Transaction_Amount_Group100-200", 
    "Transaction_Amount_Group200-300", "Transaction_Amount_Group300+", 
    "Amount_Paid_Group100-200", "Amount_Paid_Group200-300", "Amount_Paid_Group300+", 
    "Vehicle_Speed_Group100-200", "Vehicle_Speed_Group200-300", "Vehicle_Speed_Group300+", 
    "weekdaysbazar ertəsi", "weekdayscümə", "weekdaysçərşənbə"
  )
  
  # Add missing columns with value 0
  missing_cols <- setdiff(cols_to_add, colnames(new_data))
  if (length(missing_cols) > 0) {
    new_data[, missing_cols] <- 0
  }
  
  # Select input features
  new_input <- new_data[, cols_to_add]
  
  # Make predictions
  predictions <- predict(rf_model, new_input)
  binary_predictions <- ifelse(predictions >= 0.5, 1, 0)
  
  return(binary_predictions)
}



# Example usage:
model_file <- "C:/Users/HP/OneDrive/İş masası/R Programming/rf_model.rds"
rf_model <- load_model_function(model_file)

# Example new data
new_data <- data.frame(
  Transaction_ID = 1,
  Timestamp = c("1/6/2023 11:20"),
  Vehicle_Type = c("Car"),
  FastagID = c("12345"),
  TollBoothID = c("A-101"),
  Lane_Type = c("Express"),
  Vehicle_Dimensions = c("Medium"),
  Transaction_Amount = c(150),
  Amount_paid = c(110),
  Geographical_Location = c("34.0522118, 40.7128"),
  Vehicle_Speed = c(60),
  Vehicle_Plate_Number = c("ABC123")
)

# Make predictions
predictions <- predict_with_model(rf_model, new_data)
print(predictions)

View(predictions)

colnames(df_main)
