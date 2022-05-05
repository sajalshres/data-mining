# Load library
library(e1071)
library(dplyr)

# Useful functions

get_normalized_data <- function(data) {
  norm_value <- (data - min(data)) / (max(data) - min(data))
  return(norm_value)
}

get_results <- function(c) {
  TP <- c[1, 1]
  FP <- c[1, 2]
  FN <- c[2, 1]
  TN <- c[2, 2]
  
  acc <- (TP + TN) / (TP + FP + FN + TN) * 100
  pre <- (TP) / (TP + FP) * 100
  rec <- (TP) / (TP + FN) * 100
  
  print(paste("Accuracy:", acc, "Precision:",  pre, "Recall:",  rec))
  
  return(c(acc, pre, rec))
}


################################ CLEAN AND MERGE DATA SOURCE #######################################

load_and_merge_data <- function(file_name_a, file_name_b) {
  # load data
  a_df <- read.table(file_name_a, header = TRUE)
  b_df <- read.table(file_name_b, header = TRUE)
  
  # merge
  df <- merge(a_df, b_df, by = "id", all = TRUE)
  
  # Sort the data by id
  df <- df[order(df$id), ]
  
  # move atRisk column to end
  df <-  df %>% relocate(atRisk, .after = diarrhea)
  
  return(df)
}

train_df <- load_and_merge_data(file_name_a = "data8_A_train.txt", file_name_b = "data8_B_train.txt")
test_df <- load_and_merge_data(file_name_a = "data8_A_test.txt", file_name_b = "data8_B_test.txt")

cat("Dataset Summary:\n")
print(summary(train_df))

# handle missing data
handle_missing_data <- function(df) {
  missing_df = df[!complete.cases(df), ]
  print("Below dataset are missing")
  print(missing_df)
  
  print("Cleaning missing data with more than 2 empty columns")
  ind <- rowSums(is.na(df)) > 2
  missing_df <- df[ind,]
  
  print("Following rows has more thant 2 missing columns")
  print(missing_df)
  
  # remove
  df <- df[!ind,]
  
  print("Remaining missing data: ")
  print(df[!complete.cases(df),])
  
  print("Estimating missing data for remaining:")
  # estimate values for temp, bpSys, vo2 and throat using mean
  for (i in 2:7) {
    df[, i][is.na(df[, i])] <- mean(df[, i], na.rm = TRUE)
  }
  
  print("checking for missing data:")
  print(df[!complete.cases(df),])
  
  return(df)
}

print("Handling missing data:")
print(paste("Number of rows:",nrow(train_df)))

train_df <- handle_missing_data(train_df)

print(paste("Number of rows after cleaning:",nrow(train_df)))


handle_noise <- function(df) {
  # assign NA to values less than 0
  df[df < 0] = NA
  
  # Find noise
  noise_df = df[!complete.cases(df), ]
  print(paste("Rows with noise:", nrow(noise_df)))
  
  # only temp, bpSys, vo2 has noise in 3 rows
  # estimate values for temp, bpSys, vo2 using mean
  for (i in 2:4) {
    df[, i][is.na(df[, i])] <- mean(df[, i], na.rm = TRUE)
  }
  
  return(df)
}

print("Handling noise data:")
train_df <- handle_noise(train_df)

# remove id as it is not required during training
train_df <- subset(train_df, select=-id)

# calculate normalized data
print("Normalized data")
normalized_df <- as.data.frame(lapply(train_df[1:6], get_normalized_data))

################################### DECISION TREE (rpart) ##########################################
library(rpart)
library(rpart.plot)

build_decision_tree <- function(df, test_df) {
  set.seed(123)
  # combine normalized data and factor of
  dt_df <- cbind(df[1:4], as.data.frame(lapply(df[5:11], as.factor)))
  
  # build model
  dt_mod  <- rpart(atRisk ~ ., dt_df, method = "class")
  
  print(rpart.plot(dt_mod))
  print(dt_mod)
  
  # perform predictions
  dt_test_df <- cbind(test_df[1:5], as.data.frame(lapply(test_df[6:12], as.factor)))
  dt_pred = predict(dt_mod, dt_test_df, type = "class")
  # create confusion matrix
  dt_c_matrix <- table(dt_pred, dt_test_df[, 12])
  results <- get_results(dt_c_matrix)
  
  return(dt_pred)
}

print("DECISION TREE (rpart)  **********************")

dt_pred <- build_decision_tree(train_df, test_df)

####################################### NAIVE BAYES ################################################

build_naive_bayes <- function(df, test_df) {
  naive_bayes_df <- cbind(df[1:4], as.data.frame(lapply(df[5:11], as.factor)))
  naive_bayes_test_df = cbind(test_df[1:5], as.data.frame(lapply(test_df[6:12], as.factor)))
  
  # build model
  naive_bayes_model = naiveBayes(atRisk ~ ., naive_bayes_df)
  print(naive_bayes_model)
  
  # Predictions for naive bayes
  naive_bayes_pred = predict(naive_bayes_model, naive_bayes_test_df)
  
  naive_bayes_c_matrix <- table(naive_bayes_pred, naive_bayes_test_df[, 12])
  names(dimnames(naive_bayes_c_matrix)) <- c("predicted", "actual")
  get_results(naive_bayes_c_matrix)
  
  return(naive_bayes_pred)
  
}

print("NAIVE BAYES  **********************")
naive_bayes_pred <- build_naive_bayes(train_df, test_df)

########################################## KNN #####################################################
set.seed(123)
library(class)

build_knn <- function(df, test_df, class) {
  knn_df = data.frame(df)
  knn_test_df <- as.data.frame(lapply(test_df[2:7], get_normalized_data))

  # run knn
  knn_pred <- knn(knn_df, knn_test_df, cl = class, k = 78, prob=TRUE)
  
  
  # create confusion matrix
  knn_c_matrix <- table(knn_pred, test_df$atRisk)
  
  # print the results
  knn_results <- get_results(knn_c_matrix)
  
  return(knn_pred)
}

print("KNN  **********************")
knn_pred <- build_knn(normalized_df, test_df, train_df$atRisk)



########################################## SVM #####################################################
set.seed(123)

build_svm <- function(df, test_df, at_risk) {
  svm_df <- data.frame(df)
  svm_df$atRisk <- as.factor(at_risk)
  svm_model <- svm(atRisk~., data = svm_df, kernel = "linear", cost = 10, scale = FALSE)
  
  print(svm_model)
  
  svm_test_df <- as.data.frame(lapply(test_df[2:7], get_normalized_data))
  svm_test_df$atRisk = test_df$atRisk
  svm_pred <- predict(svm_model, svm_test_df, type="vector")
  
  svm_c_matrix <- table(svm_pred, svm_test_df[,7])
  
  svm_results <- get_results(svm_c_matrix)
  
  return(svm_pred)
}


print("SVM  **********************")
svm_pred <- build_svm(normalized_df, test_df, train_df$atRisk)

########################################## ANN #####################################################
library(neuralnet)
set.seed(123)

build_ann <- function(df, test_df, at_risk) {
  ann_df <- data.frame(df)
  ann_df$atRisk <- at_risk
  nn_model <- neuralnet(atRisk~bpSys+vo2+throat+headA+bodyA+temp, data = ann_df, hidden = 5)
  plot(nn_model)
  
  # build predictions
  ann_test_df <- as.data.frame(lapply(test_df[2:7], get_normalized_data))
  ann_test_df$atRisk = (test_df$atRisk)
  
  ann_pred <- neuralnet::compute(nn_model, ann_test_df)
  
  ind = ann_pred$net.result < 0.5
  ann_pred$net.result[ind] <- 0
  ann_pred$net.result[!ind] <- 1
  
  ann_c_matrix <- table(as.factor(c(ann_pred$net.result)), as.factor(ann_test_df$atRisk))
  
  ann_results <- get_results(ann_c_matrix)
  
  return(as.factor(c(ann_pred$net.result)))
}

print("ANN  **********************")
ann_pred <- build_ann(normalized_df, test_df, train_df$atRisk)



# Write predictions to test file
final_preds <-
  data.frame(
    atRisk = test_df$atRisk,
    decision_tree = dt_pred,
    naive_bayes = naive_bayes_pred,
    knn = knn_pred,
    svm = svm_pred,
    ann = ann_pred
  )

write.table(final_preds, file = "final_preds.txt", sep = "\t", row.names = FALSE, quote = FALSE)