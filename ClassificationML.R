data<-read.csv("breastCancerTrain.csv",sep = "\t")
library(tidyverse)
library(GGally)

library(DataExplorer)
create_report(data)

sum(!is.na(data$Bratio))# Data quality issue #1 Bratio column is empty.
 


library(skimr)
skim(data)

#replace -19

data[5,4]<-19.98



start_col <- 3
end_col <- ncol(data) - 2
column_indices <- start_col:end_col

# Convert the selected columns to numeric
data[, column_indices] <- lapply(data[, column_indices], as.numeric)
knn_dat<-data
data%>%ggpairs()
  
library(tidymodels)
#For the knn model

#1. Impute missing values by median

median_impute <- function(data) {
  for (col in names(data)) {
    if (anyNA(data[[col]])) {
      median_val <- median(data[[col]], na.rm = TRUE)
      data[[col]][is.na(data[[col]])] <- median_val
    }
  }
  return(data)
}

knn_dat<-median_impute(knn_dat)
knn_df<-knn_dat%>%select(-X,-Bratio,-id,-gender)




#1.Model recipe,
library(tidy.outliers)

can_rec<-recipe(diagnosis~.,data = knn_df)%>%
        step_string2factor(diagnosis)%>%
        step_YeoJohnson(all_predictors())%>%
        step_scale(all_predictors())%>%
        step_normalize(all_predictors())%>%
        prep()


 


#Model spec
can_mod<-nearest_neighbor(neighbors = tune(),dist_power =2 )%>%set_engine("kknn")%>%set_mode("classification")



#fit model
can_fit<-can_mod%>%fit(diagnosis~., data=juice(can_rec))





set.seed(1237)
can_vfold<-vfold_cv(juice(can_rec),v=5,strata = diagnosis)

#
can_wkf<-workflow()%>%add_recipe(can_rec)%>%add_model(can_mod) #knn


#
set.seed(1234)
can_grid<-tibble(neighbors=seq(from=1,to=60,by=3)) 

set.seed(123456)
can_results<-can_wkf%>%tune_grid(resamples = can_vfold,grid = can_grid)%>%collect_metrics()
can_results

#plot
ggplot(can_results, aes(x = neighbors, y = mean, color = .metric)) +geom_point()

#metrics
set.seed(12)
ui<-can_results%>%filter(.metric=="accuracy")
best_accuracy_row<-ui%>%filter(mean==max(mean))
best_accuracy_row


udm<-can_results%>%filter(.metric=="roc_auc")
udm%>%filter(mean==max(mean))


#testing
can_test<-read.csv("breastCancerTest.csv",sep = "\t")
#Preprocessing
can_test[, column_indices] <- lapply(can_test[, column_indices], as.numeric)
#impute
can_test<-median_impute(can_test)
can_test<-can_test%>%select(-X,-Bratio,-id)
can_test$diagnosis<-as.factor(can_test$diagnosis)
can_test<-can_test%>%select(-gender)
optimal_can_mod<-nearest_neighbor(neighbors = 25,dist_power = 2)%>%
  set_engine("kknn")%>%
  set_mode("classification")

test_rec <- prep(can_rec, data = can_test)
test_preprocessed <- bake(test_rec, new_data = can_test)


optimal_can_fit<-optimal_can_mod%>%fit(diagnosis~.,data = juice(can_rec))
test_pred<-predict(optimal_can_fit,new_data = test_preprocessed )%>%bind_cols(can_test)


knn_accuracy<-accuracy(test_pred,truth = diagnosis,estimate = .pred_class)

test_pred_roc<-predict(optimal_can_fit,type = "prob" ,new_data = test_preprocessed)%>%bind_cols(can_test)
pred_prob <- test_pred_roc$.pred_M
true_labels <- ifelse(test_pred_roc$diagnosis == "M", 1, 0)
roc(true_labels, pred_prob)


#F1, RECALL,PRECISION

predicted_labels <- ifelse(pred_prob >= 0.5, 1, 0)



TP <- sum(predicted_labels == 1 & true_labels == 1)
FP <- sum(predicted_labels == 1 & true_labels == 0)
FN <- sum(predicted_labels == 0 & true_labels == 1)

# Calculate precision, recall, and F1 score
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * (precision * recall) / (precision + recall)

precision
recall
f1









#DECISION TREE model
tree_data<-data%>%select(-id,-X,-Bratio,-gender)
tree_rec<-recipe(diagnosis~.,data = tree_data)%>% 
          step_string2factor(diagnosis)%>%prep()




tree_model <- decision_tree(tree_depth = tune()) %>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_workflow <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(tree_model)

tree_grid <- expand.grid(
  tree_depth = seq(1, 20)
)

metrics <- metric_set(accuracy, roc_auc)

set.seed(3)
tree_vfold<-vfold_cv(juice(tree_rec),v=5,strata = diagnosis)

tree_tune <- tree_workflow %>%
  tune_grid(
    resamples = tree_vfold,
    grid = tree_grid, 
    metrics = metrics, 
    control = control_grid(save_pred = TRUE) 
  ) %>%
  collect_metrics()

tree_validation_accuracy<-tree_tune%>%filter(.metric=="accuracy")%>%filter(mean==max(mean))
tree_validation_roc<-tree_tune%>%filter(.metric=="roc_auc")%>%filter(mean==max(mean))
tree_validation_f1<-tree_tune%>%filter(.metric=="f_meas")%>%filter(mean==max(mean))


#Evaluating on test set
optimal_tree_mod<-decision_tree(tree_depth = 4)%>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_test_rec <- prep(tree_rec, data = can_test)
tree_test_preprocessed <- bake(tree_test_rec, new_data = can_test)



optimal_tree_fit<-optimal_tree_mod%>%fit(diagnosis~.,data = juice(tree_rec))
tree_pred<-predict(optimal_tree_fit,new_data = tree_test_preprocessed )%>%bind_cols(can_test)

tree_accuracy<-accuracy(tree_pred,truth = diagnosis,estimate = .pred_class) 


tree_test_pred_prob<-predict(optimal_tree_fit,type = "prob" ,new_data = tree_test_preprocessed)%>%bind_cols(can_test)
tree_pred_prob <- tree_test_pred_prob$.pred_M
tree_true_labels <- ifelse(tree_test_pred_prob$diagnosis == "M", 1, 0)
roc(tree_true_labels, tree_pred_prob)

tree_predicted_labels <- ifelse(tree_pred_prob >= 0.5, 1, 0)
tree_TP <- sum(tree_predicted_labels == 1 & tree_true_labels == 1)
tree_FP <- sum(tree_predicted_labels == 1 & tree_true_labels == 0)
tree_FN <- sum(tree_predicted_labels == 0 & tree_true_labels == 1)

# Calculate precision, recall, and F1 score
tree_precision <- tree_TP / (tree_TP + tree_FP)
tree_recall <- tree_TP / (tree_TP + tree_FN)
tree_f1 <- 2 * (tree_precision * tree_recall) / (tree_precision + tree_recall)

tree_precision
tree_recall
tree_f1


#Support Vector machine
installed.packages("kernlab")
svm_data<-knn_dat%>%select(-id,-X,-Bratio,-gender)#Remove unwanted columns
svm_data$diagnosis<-as.factor(svm_data$diagnosis)
svm_data<-median_impute(svm_data)

svm_rec<-recipe(diagnosis~.,data = svm_data)%>%
  step_scale(all_predictors())%>%
  step_normalize(all_predictors())%>%
  step_YeoJohnson(all_predictors())%>%  
  prep()

svm_model<-svm_rbf(cost = tune())%>%
           set_mode("classification")%>%
           set_engine("kernlab")

svm_wkf<-workflow()%>%add_recipe(svm_rec)%>%add_model(svm_model)

set.seed(2)
svm_vfold<-vfold_cv(juice(svm_rec),v=5,strata = diagnosis)

svm_tune<-tune_grid(
  svm_wkf,
  resamples = svm_vfold,
  grid = expand.grid(cost = c(1, 10, 100)),
                     metrics = metric_set(accuracy,roc_auc), 
                     control = control_grid(save_pred = TRUE))%>%collect_metrics()

#Evaluating on test set
optimal_svm_mod<-svm_rbf(cost = 1)%>%
  set_mode("classification")%>%
  set_engine("kernlab")

svm_test_rec <- prep(svm_rec, data = can_test)
svm_test_preprocessed <- bake(svm_test_rec, new_data = can_test)

optimal_svm_fit<-optimal_svm_mod%>%fit(diagnosis~.,data = juice(svm_rec))
svm_pred<-predict(optimal_svm_fit,new_data = svm_test_preprocessed )%>%bind_cols(can_test)

svm_accuracy<-accuracy(svm_pred,truth = diagnosis,estimate = .pred_class)

svm_test_pred_prob<-predict(optimal_svm_fit,type = "prob" ,new_data = svm_test_preprocessed)%>%bind_cols(can_test)
svm_pred_prob <- svm_test_pred_prob$.pred_M
svm_true_labels <- ifelse(svm_test_pred_prob$diagnosis == "M", 1, 0)
roc(svm_true_labels,svm_pred_prob)


svm_predicted_labels <- ifelse(svm_pred_prob >= 0.5, 1, 0)
svm_TP <- sum(svm_predicted_labels == 1 & svm_true_labels == 1)
svm_FP <- sum(svm_predicted_labels == 1 & svm_true_labels == 0)
svm_FN <- sum(svm_predicted_labels == 0 & svm_true_labels == 1)

# Calculate precision, recall, and F1 score
svm_precision <- svm_TP / (svm_TP + svm_FP)
svm_recall <- svm_TP / (svm_TP + svm_FN)
svm_f1 <- 2 * (svm_precision * svm_recall) / (svm_precision + svm_recall)

svm_precision
svm_recall
svm_f1
