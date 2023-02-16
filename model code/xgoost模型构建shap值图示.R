###xgboost模型构建

library(xgboost)
library(tidyverse)
library(skimr)
library(DataExplorer)
library(caret)
library(pROC)
library(e1071)
library(ROSE)
setwd("D:/R work")

cdata <- read.csv("sepsis2_28d_NOlog.csv") 
##Mortality_at_28d不能转换为因子变量，且为数字1，0，其它分类变量可改变为因子变量。
for (i in names(cdata)[c(34:39)]) {
  cdata[,i] <- as.factor(cdata[,i])
}
set.seed(42)
#过和欠类采样法都有自身的缺陷，欠采样会损失信息，过采样容易导致过拟合，
#因而ROSE包也提供了ROSE()函数来合**工数据，它能提供关于原始数据的更好估计
cdata.rose <- ROSE(Mortality_at_28d ~ ., data = cdata, seed = 42)$data
table(cdata.rose$Mortality_at_28d)

trains <- createDataPartition(
  y = cdata.rose$Mortality_at_28d,
  p = 0.85,
  list = F,
  times = 1
)
#数据分拆
trains1 <- sample(trains, nrow(cdata.rose)*0.7)
data_train <- cdata.rose[trains1, ]
data_test <- cdata.rose[-trains1, ]
##数据标准化处理
dvfunc <- dummyVars(~., data = data_train[, 2:39], fullRank = T)
data_trainx <- predict(dvfunc, newdata = data_train[, 2:39])
data_trainy <- data_train$Mortality_at_28d
data_testx <- predict(dvfunc, newdata = data_test[, 2:39])
data_testy <- data_test$Mortality_at_28d
dtrain <- xgb.DMatrix(data = data_trainx, label = data_trainy)
dtest <- xgb.DMatrix(data = data_testx, label = data_testy)
watchlist <- list(train = dtrain, test = dtest)

###1参数调优
set.seed(42)
param <- list(objective = "binary:logistic",
              max_depth = sample(1:13, 1),
              eta = runif(1, .01, .1),
              gamma = runif(1, .01, .1), 
              subsample = runif(1, .6, 1),
              colsample_bytree = runif(1, .5, 1), 
              min_child_weight = sample(1:40, 1),
              max_delta_step = sample(1:10, 1),
              colsample_bylevel=runif(1, .5, 1),
              lambda=runif(1,0,2),
              alpha=runif(1,0,2)
)
sepsis_xgb<- xgb.train(  
  maximize=FALSE, verbose=1, 
  data = dtrain,
  params= param, 
  nrounds = 1000,
  eval_metric = "auc"
)

##XGBoost模型构建
#3直接参数方法
set.seed(42)
sepsis_xgb<- xgb.train(
  data = dtrain,
  eta = 0.05,
  gamma = 0.05,
  max_depth = 4,
  subsample = 0.7,
  colsample_bytree = 0.8,
  objective = "binary:logistic",
  booster="gbtree",
  nrounds = 1000,
  watchlist = watchlist,
  verbose = 1, 
  min_child_weight=5,
  scale_pos_weight=3,
  eval_metric = "auc",
  print_every_n = 100,
  early_stopping_rounds = 50 
  )

sepsis_xgb
 
#重要特征
importance_matrix <- xgb.importance(model = sepsis_xgb)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    measure = "Cover")

#训练集合预测结果
trainpred <- predict(sepsis_xgb,
                     newdata = dtrain)
#测试集预测结果
testpred <- predict(sepsis_xgb,
                    newdata = dtest)

#画ROC曲线
set.seed(42)
train_ROC <- roc(
  data_trainy ~ trainpred,
  plot = TRUE,print.thres=TRUE,
  print.auc = TRUE,
  col = "red",
  lwd = 2,
  legacy.axes = TRUE,
  main = "ROC Curves of Xgboost Model"
)

test_ROC <- roc(
  data_testy ~ testpred,
  plot = TRUE,print.thres=TRUE,
  print.auc = TRUE,
  col = "blue",
  lwd = 2,
  print.auc.y = 0.4,
  legacy.axes = TRUE,
  add = TRUE
)
legend(
  "bottomright",
  legend = c("train_ROC", "test_ROC"),
  col = c("red", "blue"),
  lwd = 2
)
#混淆矩阵
pre_xgb = round(predict(sepsis_xgb,newdata = dtest))
table(data_testy,pre_xgb,dnn=c("true","pre"))



##图SHAP图
set.seed(42)
library("SHAPforxgboost")
shap_values <- shap.values(xgb_model = sepsis_xgb, X_train = data_trainx)
shap_values$mean_shap_score
shap_long <- shap.prep(xgb_model = sepsis_xgb, X_train = data_trainx)
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = data_trainx)
shap.plot.summary(shap_long)
shap.plot.summary(shap_long, x_bound  = 2, dilute = 10)
shap.plot.summary.wrap1(sepsis_xgb, X = data_trainx)
shap.plot.summary.wrap2(shap_values$shap_score, data_trainx)
#变量趋势图
set.seed(42)
shap.plot.dependence(data_long = shap_long, x= "dayint",
                     y = "Column_WV", color_feature = "Column_WV")
shap.plot.dependence(data_long = shap_long, x = "dayint")
shap.plot.dependence(data_long = shap_long, x= "dayint",
                     color_feature = "Column_WV")
fig_list = lapply(names(shap_values$mean_shap_score)[2:39], shap.plot.dependence,
                  data_long = shap_long, dilute = 5)
gridExtra::grid.arrange(grobs = fig_list, ncol = 2)


##最新方法
set.seed(42)
library(shapviz)
dtrain <- xgboost::xgb.DMatrix(data.matrix(data_trainx), label = data_train[, 1])
#fit <- xgboost::xgb.train(data = dtrain, nrounds = 50)
x <- shapviz(sepsis_xgb, X_pred = dtrain, X = data_trainx)
sv_force(x)
sv_force(x, row_id = 266, max_display = 7, size = 10, fill_colors = 6:7)



sv_waterfall(x)
sv_waterfall(x, row_id = 2066, max_display = 11, size = 9, fill_colors = 6:7)


sv_importance(x)
sv_importance(x, kind = "beeswarm", show_numbers = TRUE)
sv_importance(x, kind = "no")


