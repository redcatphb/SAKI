###多重插补：是从一个包含缺失值的数据集中生成一组完整的数据，如此多次，
###从而产生缺失值的一组随机样本，R中的mice()函数可以用来进行多重插补。

###需要安装VIM包和mice包
#install.packages("corrplot")
#install.packages(c("VIM","mice"))
#（1）加载数据，检查数据
#mice包中的md.pattern（）函数可以生成一个以矩阵或数据框形式展示缺失值模式的表格

library(mice)
library(VIM)
setwd("D:/R work")
sepsis<-read.csv("sepsis_cb.csv") 
head(sepsis)
md.pattern(sepsis)
md.pattern(sepsis,rotate.names=TRUE) #检查数据缺失模式（missing data pattern）
##(2)可视化分析，识别缺失值
##VIM包中提供大量能可视化数据集中缺失值模式的
##函数：aggr（）、matrixplot（）、scattMiss（）
aggr(sepsis,prop=FALSE,numbers=TRUE,names=TRUE)#用比例代替了计数


matrixplot(sepsis) #函数可生成展示每个实例数据的图形

#函数可生成一幅散点图，在图形边界展示两个变量的缺失值信息。
marginplot(sepsis[c("weight","Height")],pch=c(20), col=c("darkgray","red","blue")) 

##基于mice包的分析通常符合以下分析过程：c("pmm","logreg","polyreg","polr")
imp<-mice(sepsis,m=5,defaultMethod = "pmm", seed=222)
fluxplot(sepsis)##观察缺失值相对量
plot(imp)##观察插补收敛情况
##stripplot(imp,pch=8,cex=2)##分类散点图,样本大多，显示困难不能运行
bwplot(imp)##箱线图显示
densityplot(imp,pch=8,cex=1)
##插补后单变量分析 
ttest<-with(imp,t.test(Respiration_rate~ Mortality_at_28d))
##所有插补拟合
fit<-with(imp,glm(Mortality_at_28d~ weight+Height+Heart_rate_max+SBP_min+DBP_min+MBP_min
                 +Respiration_rate_max+Temperature_max+Spo2_min+Lactate_max+Ph_min
                 +Pao2_min+Paco2_max+Pao2Fio2_ratio_min+Base_excess_min+Anion_gap_max+
                   Bicarbonate_max+WBC_max+Hemoglobin_min+Platelets_min+Basophils_max
                 +Eosinophils_max+Lymphocytes_min+Monocytes_max+Neutrophils_max
                 +Calcium_min+Chloride_min+Sodium_min+Potassium_max+BUN_max+Creatinine_max+
                   Glucose_min+Albumin_min+urine_output))


fit_vit<-with(imp,glm(Mortality_at_28d~ Weight+Height+Heart_rate+SBP+DBP+MBP
                 +Respiration_rate+Temperature+Spo2+Lactate+Ph+Pao2+Paco2+Pao2Fio2_ratio
                 +Base_excess+Anion_gap+Bicarbonate+WBC+Hemoglobin+Platelets+Basophils
                 +Eosinophils+Lymphocytes+Monocytes+Neutrophils+Calcium+Chloride+Sodium+Potassium
                 +BUN+Creatinine+Glucose+Albumin+urine_output))

fit_chemist<-with(imp,glm(Mortality_at_28d~ Calcium+Chloride+Sodium+Potassium
                          +BUN+Creatinine+Glucose+Albumin+urine_output))

fit_blood<-with(imp,glm(Mortality_at_28d~ WBC+Hemoglobin+Platelets+Basophils
                        +Eosinophils+Lymphocytes+Monocytes+Neutrophils))

fit_bg<-with(imp,glm(Mortality_at_28d~ Lactate+Ph+Pao2+Paco2+Pao2Fio2_ratio
                     +Base_excess+Anion_gap+Bicarbonate))

pooled<-pool(fit_vit)
m<-summary(pooled)
imp$imp$height #了解某个变量插补情况
library(broom)
write.csv(m,file="psm.csv")##输出smmary表格

data5<-complete(imp,action=5) #输出插补数据库，action=1，选择1至5，5个不同插补数据集(差不多)
write.csv(data5,file="D:/R work/sepsiscb5.csv",quote=F,row.names = F)
