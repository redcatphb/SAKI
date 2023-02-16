###数据不平衡的处理
install.packages("ROSE")

library(ROSE)
# Import dataset
setwd("D:/R work")
cdata <- read.csv("sepsis2_28d_log.csv") 

##如果变量是文字，侧应用下面转换为因子变量
for (i in names(cdata)[c(1,34:39)]) {
  cdata[,i] <- as.factor(cdata[,i])
}
set.seed(100)

#过和欠类采样法都有自身的缺陷，欠采样会损失信息，过采样容易导致过拟合，
#因而ROSE包也提供了ROSE()函数来合**工数据，它能提供关于原始数据的更好估计
cdata.rose <- ROSE(Mortality_at_28d ~ ., data = cdata, seed = 100)$data
table(cdata.rose$Mortality_at_28d)

#这个包为我们提供了一些基于holdout和bagging的模型评估方法，
#这有助于我们判断预测结果是否有太大的方差

ROSE.holdout <- ROSE.eval(Mortality_at_28d ~ ., data = cdata, learner = rpart, 
                          method.assess = "holdout", 
                          extr.pred = function(obj)obj[,2], seed = 100)



ROSE.holdout











