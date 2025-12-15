library(tidyverse)

df <- read.csv("D:\\Jahanvi\\COEP\\SEM2\\R\\Assignment\\Clean Dmart sales data.csv")
head(df)

# Now I have to perform classification for Order Status 

str(df)

 # Here Total order value is "char", so we convert it into "num"
df$Total.Order.Value <- as.numeric(df$Total.Order.Value)  # still it have NA value so we remove it
df$Total.Order.Value[is.na(df$Total.Order.Value)] <- median(df$Total.Order.Value, na.rm = TRUE)
str(df$Total.Order.Value)  # Now "Total Order Value" is now num type

table(df$Payment.Status)    # Cancelled(1784), Paid(17460), Pending(4399), Returned(1349)



# Remove unnecessary Columns that can not help for prediction
df_clean <- df %>%
  select(-c(Customer.ID, Product.ID, Order.ID, State, City, Year, Month, Order.Date, Delivery.Date, Cancellation.Date, Pin.Code))

head(df_clean)


# Devide clean data into train and test

install.packages("caret")    # caret = Classification And REgression Training
library(caret)               # Use "caret" for ML task

set.seed(123)
train_index <- createDataPartition(df_clean$Payment.Status, p=0.8, list = FALSE)     # p = 0.8 it means it devide data into 80% and 20%
train <- df_clean[train_index, ]
test <- df_clean[-train_index, ]

head(train)
head(test)


# Buid the model

library(rpart) # recursive partitioning
library(rpart.plot)

model <- rpart(
  Payment.Status ~.,
  data = train,
  method = "class"
)

print(model)
rpart.plot(model)



# Plot Pie Chart for Train data only 

status_count <- table(train$Payment.Status)
status_count

# Create percentage
status_percent <- round(status_count / sum(status_count) * 100, 1)
labels <- paste0(names(status_count),": ",status_percent, "%")

pie(
  status_count,
  labels = labels,
  main = "Payment status distribution for training data",
  col = rainbow(length(status_count))
)         
# Cancelled:7.1%, Returned:5.4%, Pending:17.6%, Paid:69.9%

# Test data
library(dplyr)

predictions <- predict(model, test, type = "class")
predictions

# Plot pie chart for test data only
status_count1 <- table(predictions)
status_count1

# Create percentage
status_percent1 <- round(status_count1 / sum(status_count1) * 100, 1)
labels1 <- paste0(names(status_percent1), ": ",status_percent1,"%")

pie(
  status_count1,
  labels = labels1,
  main = "Payment status distribution for test data",
  col = rainbow(length(status_count))
) 
# Cancelled: 7.1%, Returned: 5.4%, Pending: 0%, Paid: 87.5%










