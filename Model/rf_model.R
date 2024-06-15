library( tidyverse );
library( caret );
library( e1071 );
library( randomForest );
# library( nnet );

w_data <- read.csv( '/media/General/Projects/WP/DataSets/csv/seattle-weather.csv');
w_data = subset(w_data, select = -c(date));

head( w_data );

colSums( is.na(w_data) );

w_data <- na.omit( w_data );

# set.seed(123) 
train_index <- createDataPartition(w_data$weather, p = 0.8, list = FALSE)
train_data <- w_data[train_index, ]
test_data <- w_data[-train_index, ]

head( train_data );
head( test_data );

# Ensure that 'weather' is a factor in both train and test data
train_data$weather <- as.factor(train_data$weather)
test_data$weather <- as.factor(test_data$weather)

# Check the levels to make sure they are the same
levels(train_data$weather)
levels(test_data$weather)

# Fit the logistic regression model
logistic_model <- randomForest(weather ~ ., data = train_data, ntree=100)

# Make predictions on the test set
logistic_predictions <- stats::predict(logistic_model, test_data);

# Convert predictions to factors with the same levels as the actual values
logistic_predictions <- factor(logistic_predictions, levels = levels(test_data$weather))

# Evaluate the model using confusionMatrix
conf_matrix <- confusionMatrix(logistic_predictions, test_data$weather)
print(conf_matrix)

saveRDS( logistic_model, file = "/media/General/Projects/WP/Web/server/Model.rds");