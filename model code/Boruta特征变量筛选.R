###Boruta特征变量筛选###需要插补后才能筛选变量
#install.packages("Boruta")
library(Boruta)
setwd("D:/R work")
cdata<-read.csv("sepsis28d.csv") 
##因子转换
##3 定性变量转化成因子

for (i in names(cdata)[c(1,41:61)]) {
  cdata[,i] <- as.factor(cdata[,i])
}

cdata$Mortality_at_90d <-factor(cdata$Mortality_at_28d,levels=c(0,1),                    
                                labels=c("survival","mortality"))

set.seed(2022)
cdata<-na.omit(cdata)
# Perform Boruta search
boruta_output <- Boruta(Mortality_at_28d ~ ., data=cdata, doTrace=2,ntree= 200)

#输出结果
names(boruta_output)

##获取重要特征，可行性较强的其它特征
# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  

##查看下变量重要性鉴定结果（实际上面的输出中也已经有体现了）

##TentativeRoughFix参数控制可行性强的其它特征
# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

#获得最终变量重度评分
# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort
##可视化特征重要性
# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  


