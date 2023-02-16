###应用creditmodel自动构建LR、GBM、RF、XGB四个模型

library(creditmodel)
library(randomForest)
library(gbm)
library(xgboost)
setwd("D:/R work")
cdata <- read.csv("sepsis_28d.csv") 
for (i in names(cdata)[c(32:37)]) {
  cdata[,i] <- as.factor(cdata[,i])
}


library(ROSE)
cdata.rose <- ROSE(Mortality_at_28d ~ ., data = cdata, seed = 46)$data

lending_model=training_model(
       dat=cdata.rose,
       model_name="Prediction Model of 28-Day Mortality",
       target="Mortality_at_28d",
       occur_time=NULL,
       obs_id=NULL,
       x_list=NULL,
       ex_cols=NULL,
       dat_test=NULL,
       pos_flag=NULL,
       prop=0.7,
       preproc=TRUE,
       low_var=0.99,missing_rate=0.98,merge_cat=30,
       outlier_proc=TRUE,
       missing_proc='median',
       miss_values=c("Missing",-1),
       one_hot=FALSE,
       trans_log=FALSE,
       feature_filter=list(filter=c("IV","COR","PSI","XGB"),
                          cv_folds=10,iv_cp=0.02,psi_cp=0.2,
                          cor_cp=0.95,xgb_cp=0,hopper=FALSE),
                          algorithm = list("LR", "XGB", "GBM", "RF"), 
                          breaks_list=NULL,
                         LR.params=lr_params(
                               iter=5,
                               method='random_search',
                               tree_control=list(p=c(0.02,0.03,0.05),
                                                 cp=0.00000001,
                                                 xval=3:5,
                                                 maxdepth=c(8,10,15)),
                               bins_control=list(bins_num=10,
                                                 bins_pct=c(0.02,0.03,0.05),
                                                 b_chi=c(0.01,0.02,0.03),
                                                 b_odds=c(0.05,0.1,0.15,0.2),
                                                 b_psi=c(0.02,0.06),
                                                 b_or=c(.05,0.1,0.15,0.2),
                                                 mono=c(0.1,0.2,0.4,0.5),
                                                 odds_psi=c(0.1,0.15,0.2),
                                                 kc=1),
                               f_eval='ks',
                               lasso=TRUE,best_lambda="lambda.ks",
                               step_wise=FALSE,score_card=TRUE,
                               sp_values=NULL,forced_in=NULL,
                               obsweight=c(1,1),
                               thresholds=list(cor_p=0.8,iv_i=0.02,
                                               psi_i=0.1,cos_i=0.5)),
                        XGB.params=xgb_params(
                               iter=10,
                               method='random_search',
                               params=list(max_depth=c(3:6),cv.folds = 10,
                                           eta=c(0.01,0.05,0.1,0.2),
                                           gamma=c(0.01,0.05,0.1),
                                           min_child_weight=c(1,5,10,20,30,40,50),
                                           subsample=c(0.8,0.7,0.6,0.5),
                                           colsample_bytree=c(0.8,0.7,0.6,0.5),
                                           scale_pos_weight=c(1,2,3)),
                               f_eval='auc'),
                       GBM_params= gbm_params(
                               iter = 10,
                               method = 'random_search',
                               params = list(n.trees = c(10,100,200,500,1000), 
                                             interaction.depth = c(1:6), 
                                             shrinkage = c(0.001,0.01,0.05,0.1), 
                                             bag.fraction = 0.5, 
                                             train.fraction = 0.7, 
                                             n.minobsinnode = 30, 
                                             cv.folds = 10),
                               f_eval = 'auc'),
                      RF_params=rf_params(
                               iter = 10,
                               method = 'random_search',
                               params = list(ntree = c(50,100,200,500,1000),cv.folds = 10, 
                                             nodesize = c(5,10,15,20,30), 
                                             samp_rate =c(0.1,0.3, 0.5, 0.7,0.9),
                                             tune_rf = TRUE),
                               f_eval = 'auc'),
      parallel=FALSE,
      cores_num=4,
      save_pmml=FALSE,
      plot_show=TRUE,
      vars_plot=TRUE,
      model_path=tempdir(),
      seed=46)

