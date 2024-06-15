library( tidyverse );
library( caret );
library( e1071 );
library( randomForest );
library( nnet );

w_data <- read.csv( '/media/General/Projects/WP/DataSets/csv/weatherHistory.csv');
w_data = subset(w_data, select = -c(date, Daily));

head( w_data );

colSums( is.na(w_data) );

w_data <- na.omit( w_data );

# set.seed(123) 
train_index <- createDataPartition(w_data$Summary, p = 0.8, list = FALSE)
train_data <- w_data[train_index, ]
test_data <- w_data[-train_index, ]

head( train_data );
head( test_data );

# Ensure that 'weather' is a factor in both train and test data
train_data$Summary <- as.factor(train_data$Summary)
test_data$Summary <- as.factor(test_data$Summary)

# Check the levels to make sure they are the same
levels(train_data$Summary)
levels(test_data$Summary)

# Fit the logistic regression model
logistic_model <- randomForest(Summary ~ ., data = train_data, ntree=100)

# Make predictions on the test set
logistic_predictions <- predict(logistic_model, test_data)

# Convert predictions to factors with the same levels as the actual values
logistic_predictions <- factor(logistic_predictions, levels = levels(test_data$Summary))

# Evaluate the model using confusionMatrix
conf_matrix <- confusionMatrix(logistic_predictions, test_data$Summary)
print(conf_matrix)