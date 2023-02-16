##限制性立方样条
library(rms) #RCS
library(survminer)#曲线
library(ggplot2)#画图
library(ggsci)#调色板
library(scales)
library(ggrcs)
library(survival)
library(Rcpp)
library(ROSE)
library(caret)
library(tidyr)
library(corrplot)
setwd("D:/R work")

cdata <- read.csv("sepsis2_28d_endcox.csv") 

cdata$Mortality_at_28d <- ifelse(cdata$Mortality_at_28d=='Mortality',1,1)
cdata$cerebrovascular_disease <-factor(cdata$cerebrovascular_disease,levels=c(0,1), labels=c("No","Yes"))
#cdata$dementia <-factor(cdata$dementia,levels=c(0,1), labels=c("No","Yes"))
cdata$mild_liver_disease <-factor(cdata$mild_liver_disease,levels=c(0,1), labels=c("No","Yes"))
#cdata$severe_liver_disease <-factor(cdata$severe_liver_disease,levels=c(0,1), labels=c("No","Yes"))
cdata$metastatic_solid_tumor <-factor(cdata$metastatic_solid_tumor,levels=c(0,1), labels=c("No","Yes"))
#设定数据环境
aa<-cdata
dt<-aa
dd<-datadist(dt) 
options(datadist='dd')
S <- Surv(dt$dead_time,dt$Mortality_at_28d==1)

fit <- coxph(Surv(dead_time, Mortality_at_28d) ~ Age + X1.sqrt_BMI + Heart_rate
             + SBP+DBP+MBP+X1.Respiration_rate+Temperature+Spo2+log_ROX_HR+X1.sqrt_Lactate 
             +X1.sqrt_Pao2 + X1.Paco2 + log_Pao2Fio2_ratio + X1.sqrt_Anion_gap 
             +sqrt_Bicarbonate+log_WBC+Hemoglobin+Chloride+
               Sodium+X1.sqrt_BUN+X1.sqrt_Creatinine+Albumin
             +log_urine_output+cerebrovascular_disease+mild_liver_disease+
               metastatic_solid_tumor,
             data = cdata)
##构建模型log.crrt_adimmit_time.+age+LODS+sapsii+respiratory_rate+SPO2+Ph
##+PaO2.FiO2+X1.sqrt.ROX.HR.+neutrophils+RDW+aniongap+chloride+potassium+log.bilirubin.+urineoutput
fit <- cph(S ~ rcs(Age,3)  +Age+cerebrovascular_disease+mild_liver_disease+
             metastatic_solid_tumor, x=TRUE, y=TRUE,data=aa)
cox.zph(fit,"rank")             # PH 检验
ggcoxzph(cox.zph(fit, "rank"))   #可视化等比例假定
anova(fit)                       #非线性检验
##注意：这里的R命令是“cph”，而不是常见的生存分析中用到的“coxph"
##Tips：若因变量为二分类变量，改用lrm函数拟合模型：fit<- lrm(y ~  rcs(x1),data=data)；
##若因变量为连续变量，改用ols函数拟合模型：fit<-ols(y~ rcs(x1)

Pre0 <-rms::Predict(fit,Age,fun=exp,type="predictions",
                    ref.zero=TRUE,conf.int = 0.95,digits=2)
##其中fun是转化函数
ggplot(Pre0)
View(Pre0)

ggrcs(data=dt,fit=fit,x="Age",histcol="blue",
      histbinwidth=1,ribcol="green",ribalpha=0.5,xlab ="Age",
      ylab="X1.Paco2",title ="X1.Paco2",liftname="Probability Density
",lift=F,P.Nonlinear=T)




Pre1 <- rms::Predict(fit, age, Gender=c('Male','Female'),fun=exp,type="predictions",ref.zero=TRUE,conf.int = 0.95,digits=2)
par(mfrow=c(1,2))
ggplot(Pre1)
View(Pre1)