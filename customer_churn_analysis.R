# Make churn a factor
churn$churn = as.factor(churn$churn)

# Binning tenure
churn$tenure_binned = cut(churn$tenure, 
                          breaks = c(0, 12, 24, 36, 48, 60, 72), 
                          labels = c("0-12", "12-24", "24-36", "36-48", "48-60", "60-72"), 
                          include.lowest = TRUE)

# Binning monthly charges
churn$monthly_charges_binned = cut(churn$monthly_charges, 
                                   breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200), 
                                   labels = c("0-25", "25-50", "50-75", "75-100", "100-125", "125-150", "150-175", "175-200"), 
                                   include.lowest = TRUE)

# 1. Overall Churn percentage
table_churn = table(churn$churn)
prop.table(table_churn) * 100

# 2. Cross tables (Done manually one by one instead of using a loop)
table(churn$churn, churn$senior_citizen)
table(churn$churn, churn$contract)
table(churn$churn, churn$internet_service)
table(churn$churn, churn$phone_service)
table(churn$churn, churn$home_security)
table(churn$churn, churn$program_recording)
table(churn$churn, churn$tv_service)
table(churn$churn, churn$payment_method)
table(churn$churn, churn$tech_support)
table(churn$churn, churn$tenure_binned)
table(churn$churn, churn$monthly_charges_binned)

# Split the data into 80% train and 20% validation
set.seed(42)
train_size = floor(0.80 * nrow(churn))
train_rows = sample(1:nrow(churn), train_size)

train_data = churn[train_rows, ]
validation_data = churn[-train_rows, ]

# Make the tree model
library(rpart)
library(rpart.plot)

tree_model = rpart(churn ~ . - tenure - monthly_charges, data = train_data, method = "class")
rpart.plot(tree_model, extra = 104, fallen.leaves = TRUE)

# --- EVALUATE TRAINING DATA ---
# (Calculating the math manually using matrix positions instead of a custom function)
train_pred = predict(tree_model, train_data, type = "class")
train_matrix = table(Predicted = train_pred, Actual = train_data$churn)
train_matrix

# Train Math
train_accuracy = (train_matrix[1,1] + train_matrix[2,2]) / sum(train_matrix)
train_accuracy

train_misclass = 1 - train_accuracy
train_misclass

train_sensitivity = train_matrix[2,2] / (train_matrix[2,2] + train_matrix[1,2])
train_sensitivity

train_specificity = train_matrix[1,1] / (train_matrix[1,1] + train_matrix[2,1])
train_specificity


# --- EVALUATE VALIDATION DATA ---
val_pred = predict(tree_model, validation_data, type = "class")
val_matrix = table(Predicted = val_pred, Actual = validation_data$churn)
val_matrix

# Validation Math
val_accuracy = (val_matrix[1,1] + val_matrix[2,2]) / sum(val_matrix)
val_accuracy

val_misclass = 1 - val_accuracy
val_misclass

val_sensitivity = val_matrix[2,2] / (val_matrix[2,2] + val_matrix[1,2])
val_sensitivity

val_specificity = val_matrix[1,1] / (val_matrix[1,1] + val_matrix[2,1])
val_specificity