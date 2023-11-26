#-----summary code of boston -----#

# package
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)
library(randomForestExplainer)
library(foreign)#平均分
par(family= "HiraKakuProN-W3")
#library(fastAdaboost)
#library(gbm)
#library(MASS)
#library(pROC)


# データの前処理

## d0: original data
d0 = read.csv('/Users/suxuanrong/Desktop/关于横市啊/学生活動/club活动/ボストン住宅価格データ.csv')
summary(d0)

## d01: princeの四分位数より分類したデータ(prince -> cp)
d01 = d0
d01['cp'] = d01[14]
d01<- mutate(d01, cp = if_else(d01$cp < 17.02, 1, if_else(d01$cp < 21.20, 2, if_else(d01$cp < 25, 3, 4))))
d01 = d01[,-14]

## d02: num(cp) -> factor(cp)
d02 = d01
d02$cp = as.factor(d02$cp)

## d03: standardization of indus, nox, rm, ptratio, lstat
d03 = d02 %>% mutate(indus_st = scale(indus), 
                     nox_st = scale(nox),
                     rm_st = scale(rm),
                     ptratio_st = scale(ptratio),
                     lstat_st = scale(lstat))
head(d03)
d03 = d03[, -c(3,5,6,11,13)]
d03[,10:14] = as.numeric(unlist(d03[,10:14]))
cp = d03[,'cp']
d03 = d03[,-9]
d03 = cbind(d03, cp)

## d04: log(c(crim, age, dis, b))
d04 = d03 %>% mutate(crim_log = log(crim), 
                     age_log = log(age),
                     dis_log = log(dis),
                     b_log = log(b))
d04 = d04[, -c(1,4,5,8)]
d04 = d04[,-10]
d04 = cbind(d04, cp)

## d05: standardize all
d05 = d04
d05[, 1:13] = scale(d05[, 1:13])

## d06: 標準化＆四分割log(crim, zn+1, dis, rad, tax, lstat), as.factor(chas), scale(nox, rm, ptratio), 1/log(age, b)
d06 = d01
d06 = d06 %>% mutate(crim = log(crim), 
                     zn = log(zn+1),
                     dis = log(dis),
                     rad = log(rad),
                     tax = log(tax),
                     lstat = log(lstat),
                     age = 1/log(age),
                     b = 1/log(b))
d06[,'chas'] = as.factor(d06[,'chas'])
d06[,'cp'] = as.factor(d06[,'cp'])
d06[,-c(4, 14)] = scale(d06[, -c(4, 14)])

## d07:  log(crim), scale(zn), as.factor(chas), scale(nox, rm, rad, tax, ptratio), 1/log(age, b), log(dis, lstat), 
d07 = d01
d07 = d07 %>% mutate(crim = log(crim), 
                     dis = log(dis),
                     lstat = log(lstat),
                     age = 1/log(age),
                     b = 1/log(b))
d07[,'chas'] = as.factor(d07[,'chas'])
d07[,'cp'] = as.factor(d07[,'cp'])
d07[,-c(4, 14)] = scale(d07[, -c(4, 14)])

## d08:  log(crim, zn+1, dis, rad, tax, lstat), scale(nox, rm, ptratio), 1/log(age, b)
d08 = d01
d08 = d08 %>% mutate(crim = log(crim), 
                     zn = log(zn+1),
                     dis = log(dis),
                     rad = log(rad),
                     tax = log(tax),
                     lstat = log(lstat),
                     age = 1/log(age),
                     b = 1/log(b))
d08[,'cp'] = as.factor(d08[,'cp'])
d08[,-c(4, 14)] = scale(d08[, -c(4, 14)])

## d09 標準化＆三分割
d09 = d0
cp = cut(d0$medv, breaks = 3, labels = c(1, 2, 3))
d09 = cbind(d06, cp)
d09 = d09[,-14]

## d10 そのまま＆三分割
d10 = d0
d10 = cbind(dd10, cp)
d10 = d10[,-14]

## d11 そのまま＆四分割
d11 = d0
d11['cp'] = d11[14]
d11<- mutate(d11, cp = if_else(d11$cp < 17.02, 1, if_else(d11$cp < 21.20, 2, if_else(d11$cp < 25, 3, 4))))
d11 = d11[,-14]
d11$cp = as.factor(d11$cp)

# 可視化

## d01
layout(matrix(c( 1,2,3,4,5,6,7,8,9,10,11,12,13,0,0,0),4,4,  byrow = TRUE))
for (i in 1:13) {
  boxplot(d01[,i], main = colnames(d01)[i])
}
layout(matrix(c( 1,2,3,4,5,6,7,8,9,10,11,12,13,0,0,0),4,4,  byrow = TRUE))
for (i in 1:13) {
  hist(d01[,i], main = colnames(d01)[i], col = 'lightgoldenrod1', xlab = ' ')
}
## d03
layout(matrix(c( 1,2,3,4,5,6,7,8,9,10,11,12,13,0,0,0),4,4,  byrow = TRUE))
for (i in 1:13) {
  hist(d03[,i], main = colnames(d03)[i])
}
## d04
layout(matrix(c( 1,2,3,4,5,6,7,8,9,10,11,12,13,0,0,0),4,4,  byrow = TRUE))
for (i in 1:13) {
  hist(d04[,i], main = colnames(d04)[i])
}
## d05
layout(matrix(c( 1,2,3,4,5,6,7,8,9,10,11,12,13,0,0,0),4,4,  byrow = TRUE))
for (i in 1:13) {
  hist(d05[,i], main = colnames(d05)[i])
}
##d06
layout(matrix(c( 1,2,3,4,5,6,7,8,9,10,11,12,13,0,0,0),4,4,  byrow = TRUE))
for (i in 1:13) {
  hist(d08[,i], main = colnames(d08)[i], col = 'lightgoldenrod1', xlab = ' ')
}

## d01 y
boxplot(d01$cp, d0$medv)
table(d01$cp)
boxplot(d0$medv~ d01$cp, main = '価格の分布', xlab = '分類', ylab = '価格', col = 'lightgoldenrod1')
summary(d0$medv)

## d09 y
boxplot(d0$medv ~ d09$cp, main = '価格の分布', xlab = '分類', ylab = '価格', col = 'lightgoldenrod1')
summary(d0$medv, d09$cp)
table(d09$cp)

hist(d01$zn, main = '広い家の割合', xlab = ' ', col = 'lightgoldenrod1')
hist(d01$age, main = '家の古さ', xlab = ' ', col = 'lightgoldenrod1')
hist(d01$rm, main = '平均部屋数', xlab = ' ', col = 'lightgoldenrod1')

hist(d01$lstat, main = '低取得者の割合', xlab = ' ', col = 'lightgoldenrod1')


# split train data & test data

## caret -> createDataPartition()
#trainlist = createDataPartition(d05$cp, p = 0.75, list = F)

## d01
even.n = 4*(1:126)
train.data = d01[-even.n,]
test.data = d01[even.n, ]
table(train.data$cp)
table(test.data$cp)

## d02
train.data1 = d02[-even.n,]
test.data1 = d02[even.n, ]
table(train.data1$cp)
table(test.data1$cp)

## d04
train.data2 = d04[-even.n,]
test.data2 = d04[even.n, ]
table(train.data2$cp)
table(test.data2$cp)

## d05
train.data3 = d05[-even.n,]
test.data3 = d05[even.n, ]
table(train.data3$cp)
table(test.data3$cp)

## d06
train.data4 = d06[-even.n,]
test.data4 = d06[even.n, ]

## d07
train.data5 = d07[-even.n,]
test.data5 = d07[even.n, ]

## d08
train.data6 = d08[-even.n,]
test.data6 = d08[even.n, ]

## d09 
train.data7 = d09[-even.n,]
test.data7 = d09[even.n, ]

## d11
train.data9 = d11[-even.n,]
test.data9 = d11[even.n, ]


# 试错其一：mass -> LDA线性判别

z = lda(cp ~., data = train.data)
z
table(train.data[,14], predict(z)$class)#0.75
y = predict(z, test.data)
table(test.data[,14], y$class)#0.67

## train.data step
t1 = lm(cp ~ ., data = train.data)
summary(t1)
t1_s = step(t1)
terms = t1_s$terms
z1 = lda(terms, data = train.data)
z1
table(train.data[,14], predict(z1)$class)#0.72
y1 = predict(z1, test.data)
table(test.data[,14], y1$class)#0.69

## train.data vif
vif(t1)
train1 = train.data[,-10]
t2 = lm(cp ~ ., data = train1)
t2
pairs(train1[,1:13])
vif(t2)
terms2 = t2$terms
z2 = lda(terms2, data = train.data)
table(train.data[,14], predict(z2)$class)#0.69
y2 = predict(z2, test.data)
table(test.data[,14], y1$class)#0.69
### vif更加重视train和test的精度靠近，而step则更注重train的准确率。根据需要自选🉑️


# Decision Tree

## rpart
### sample rpart(cp isn't factor)
model = rpart(train.data$cp ~ ., data =train.data)
model
summary(model)

### control maxdepth(cp isn't factor)
model1 = rpart(train.data$cp ~ ., data =train.data, control = rpart.control(maxdepth = 3))
model1

### factor(cp)
model2 = rpart(train.data1$cp ~ ., data =train.data1)
model2
model3 = rpart(train.data1$cp ~ ., data =train.data1, control = rpart.control(maxdepth = 3))
model3
pred = predict(model3, test.data1, type = 'class')
pred
sum(diag(table(pred, test.data3$cp)))/sum(table(pred, test.data3$cp))#精度 = 0.6666667

### control 折交叉验证 & minsplit & cp(complexity pamemeter)
 ## try https://danzhuibing.github.io/r_decision_tree.html
ct <- rpart.control(xval=10, minsplit=20, cp=0.01)  
cfit <- rpart(cp ~ .,
              data=train.data3,
              method="class",control=ct,
              parms=list(split="gini")
)
print(cfit)
rpart.plot(cfit, main="Raw Decision Tree")
printcp(cfit)
cfit2 <- prune(cfit, cp=0.02)
printcp(cfit2)
rpart.plot(cfit2, main="Pruned Decision Tree")
sum(diag(table(predict1, test.data3$cp)))/sum(table(predict1, test.data3$cp))#精度 =  0.6587302

## rpart.plot
### 不能用(cause cp isn't factor)
rpart.plot(model, extra = 1, type = 2)
rpart.plot(model1, extra = 1, type = 2)
### OK(factor(cp))
rpart.plot(model2, extra = 1, type = 2)
rpart.plot(model3, extra = 1, type = 2)


# randomForest
## d02基础上的随机森林
model4 = randomForest(train.data1$cp ~ ., data = train.data1)
model4
importance(model4)
varImpPlot(model4)
pred1 = predict(model4, test.data1)
table(pred1)
table(pred1, test.data1$cp)
sum(diag(table(pred1, test.data3$cp)))/sum(table(pred1, test.data3$cp))#精度 = 0.7539683

## d04基础上的随机森林
model5 = randomForest(train.data2$cp ~ ., data = train.data2)
model5
importance(model5)
varImpPlot(model5)
pred2 = predict(model5, test.data2)
table(pred2)
table(pred2, test.data2$cp)
sum(diag(table(pred2, test.data3$cp)))/sum(table(pred2, test.data3$cp))#精度 = 0.7619048

## d05基础上的随机森林
model6 = randomForest(train.data3$cp ~ ., data = train.data3)
model6
importance(model6)
varImpPlot(model6)
pred3 = predict(model6, test.data3)
table(pred3)
table(pred3, test.data3$cp)
plot(model6)
sum(diag(table(pred3, test.data3$cp)))/sum(table(pred3, test.data3$cp))#精度 = 0.7777778
### prune
print(model6$cptable)
r_prune = prune(tree = model6, cp = 0.2)
pred4 = predict(r_prune, test.data3, type = 'class')
sum(diag(table(pred4, test.data3$cp)))/sum(table(pred4, test.data3$cp))#精度 = 0.5793651😤
#rf.cf = caret::confusionMatrix(pred3, test.data3$cp)

## d06
model8 = randomForest(cp ~ ., data = train.data4)
model8
importance(model8)
varImpPlot(model8)
pred4 = predict(model8, test.data4)
sum(diag(table(pred4, test.data4$cp)))/sum(table(pred4, test.data4$cp))#精度 = 0.7857143
#有factor就不能有cptable了么？那得怎么剪枝啊？emmmmm
#RF的图！真的！好！丑！！！哼！

## d06 -> rpart  ； cp(complexity parameter)
model9 = rpart(cp ~ ., data =train.data4)
print(model9)
rpart.plot(model9, extra = 2, type = 2, main = 'Raw Decision Tree Of 4 Class')
pred5 = predict(model9, test.data4, type = 'class')
pred17 = predict(model9, train.data4, type = 'class')
sum(diag(table(pred17, train.data4$cp)))/sum(table(pred17, train.data4$cp))#精度 = 0.8052632

sum(diag(table(pred5, test.data4$cp)))/sum(table(pred5, test.data4$cp))#精度 = 0.7857143
printcp(model9)
r_prune1 = prune(tree = model9, cp = 0.04)
pred18 = predict(r_prune1, train.data4, type = 'class')
sum(diag(table(pred18, train.data4$cp)))/sum(table(pred18, train.data4$cp))#精度 = 0.7857143

pred7 = predict(r_prune1, test.data4, type = 'class')
sum(diag(table(pred7, test.data4$cp)))/sum(table(pred7, test.data4$cp))#精度 = 0.6666667😤
rpart.plot(r_prune1, extra = 2, type = 2, main="Pruned Decision Tree Of 4 Class")
r_prune2 = prune(tree = model9, cp = 0.02)
pred8 = predict(r_prune2, test.data4, type = 'class')
sum(diag(table(pred8, test.data4$cp)))/sum(table(pred8, test.data4$cp))#精度 = 0.6587302😤



## d07 RF
model10 = randomForest(cp ~ ., data = train.data5, method = 'class')
model10
importance(model10)
varImpPlot(model10)
pred5 = predict(model10, test.data5)
sum(diag(table(pred5, test.data5$cp)))/sum(table(pred5, test.data5$cp))#精度 = 0.7857143
#---所以，看起来还是d06比较好用---#

## d08  
model11 = randomForest(cp ~ ., data = train.data6, method = 'class')
model11
importance(model11)
varImpPlot(model11)
pred6 = predict(model11, test.data6)
sum(diag(table(pred6, test.data6$cp)))/sum(table(pred6, test.data6$cp))#精度 = 0.7698413,0.7936508
print(model11)

## d09 -> RF
model12 = randomForest(cp ~ ., data = train.data7)
model12
importance(model12)
varImpPlot(model12)
pred9 = predict(model12, test.data7)
sum(diag(table(pred9, test.data7$cp)))/sum(table(pred9, test.data7$cp))#精度 = 0.8174603

## d09 -> rpart
model13 = rpart(cp ~ ., data =train.data7)
pred20 = predict(model13, train.data7, type = 'class')
sum(diag(table(pred20, train.data7$cp)))/sum(table(pred20, train.data7$cp))#精度 = 0.8631579

pred10 = predict(model13, test.data7, type = 'class')
sum(diag(table(pred10, test.data7$cp)))/sum(table(pred10, test.data7$cp))#精度 = 0.7777778
rpart.plot(model13, extra = 2, type = 2, main = 'Raw Decision Tree Of 3 Class')

printcp(model13)
r_prune3 = prune(tree = model9, cp = 0.026) #s精度が低い原因はmodel9のほうがfactorが４つです。
r_prune5 = prune(tree = model13, cp = 0.026)
pred21 = predict(r_prune5, train.data7, type = 'class')
sum(diag(table(pred21, train.data7$cp)))/sum(table(pred21, train.data7$cp))#精度 = 0.85

pred11 = predict(r_prune3, test.data7, type = 'class')
pred14 = predict(r_prune5, test.data7, type = 'class')
sum(diag(table(pred11, test.data7$cp)))/sum(table(pred11, test.data7$cp))#精度 = 0.3253968😤
sum(diag(table(pred14, test.data7$cp)))/sum(table(pred14, test.data7$cp))#精度 = 0.7936508
rpart.plot(r_prune5, extra = 2, type = 2, main="Pruned Decision Tree Of 3 Class")



## d09 RF set.seed(100)
n = length(names(train.data7))
set.seed(100)
for(i in 1:(n-1)){
  mtry_fit = randomForest(cp ~ ., data = train.data7, mtry = i)
  error = mean(mtry_fit$err.rate)
  print(error)
}
sample_d.tune<-tuneRF(train.data7[,-14],train.data7[,14],doBest=T)#https://tjo.hatenablog.com/entry/2013/09/02/190449
#对应误差值最小为6
set.seed(100)
ntree_fit = randomForest(cp ~ ., data = train.data7, mtry = 6, ntree = 500)
plot(ntree_fit, main = ' ')
set.seed(100)
model14 = randomForest(cp ~ ., data = train.data7, mtry = 6, ntree = 400)
model14
importance(model14)
set.seed(100)
varImpPlot(model14)
pred12 = predict(model14, test.data7, type = 'class')
pred22 = predict(model14, train.data7, type = 'class')
sum(diag(table(pred22, train.data7$cp)))/sum(table(pred22, train.data7$cp))#精度 = 1

table(pred12, test.data7$cp)
sum(diag(table(pred12, test.data7$cp)))/sum(table(pred12, test.data7$cp))#精度 = 0.8174603

set.seed(100)
imp = varImpPlot(model14)
imp = as.data.frame(imp)
imp$varnames = rownames(imp)
rownames(imp) = NULL
#imp$var_categ = rep(1:13, 1)
ggplot(imp, aes(x = reorder(varnames, MeanDecreaseGini), weight = MeanDecreaseGini, fill = varnames))+
  geom_bar() 
#imp = imp[order(imp$MeanDecreaseGini, decreasing = T),]
#rownames(imp) = 1:nrow(imp)
imp = imp[,1:2]
barplot(imp$MeanDecreaseGini~reorder(imp$varnames, imp$MeanDecreaseGini),
        xlab = '特徴', ylab = 'mean decrease gini', col = 'lightgoldenrod1',
        main = '3分割の特徴の重要度')
varImpPlot(model14)


  
## d10的基础上RF
model15 = randomForest(cp ~ ., data = train.data8)

## d10的基础上rpart
set.seed(1)
model16 = rpart(cp ~ ., data = train.data8)
pred12 = predict(model16, test.data8, type = 'class')
sum(diag(table(pred12, test.data8$cp)))/sum(table(pred12, test.data8$cp))#精度 = 0.7777778
print(model16$cptable)
r_prune4 = prune(tree = model16, cp = 0.026)
pred13 = predict(r_prune4, test.data8, type = 'class')
sum(diag(table(pred13, test.data8$cp)))/sum(table(pred13, test.data8$cp))#精度 = 0.7936508?!!!


rpart.plot(model16, extra = 2, type = 0)# extra = 9 or 2 type = 0 or 2

## d11的基础上rpart
set.seed(2)
model17 = rpart(cp ~ ., data = train.data9)
rpart.plot(model17, extra = 2, type = 2, main = 'Raw Decision Tree Of 4 Class')
pred19 = predict(model17, train.data9, type = 'class')
sum(diag(table(pred19, train.data9$cp)))/sum(table(pred19, train.data9$cp))#精度 = 0.8052632

pred15 = predict(model17, test.data9, type = 'class')
sum(diag(table(pred15, test.data9$cp)))/sum(table(pred15, test.data9$cp))#精度 = 0.6666667
print(model17$cptable)
printcp(model17)#自分の精度0.74211
r_prune6 = prune(tree = model17, cp = 0.022)
pred16 = predict(r_prune6, test.data9, type = 'class')
sum(diag(table(pred16, test.data9$cp)))/sum(table(pred16, test.data9$cp))#精度 = 0.6825397


model9 = rpart(cp ~ ., data =train.data4)
print(model9)
rpart.plot(model9, extra = 2, type = 2, main = 'Raw Decision Tree Of 4 Class')
pred5 = predict(model9, test.data4, type = 'class')
pred17 = predict(model9, train.data4, type = 'class')
sum(diag(table(pred17, train.data4$cp)))/sum(table(pred17, train.data4$cp))#精度 = 0.7857143

sum(diag(table(pred5, test.data4$cp)))/sum(table(pred5, test.data4$cp))#精度 = 0.7857143
printcp(model9)
r_prune1 = prune(tree = model9, cp = 0.04)
pred18 = predict(r_prune1, train.data4, type = 'class')
sum(diag(table(pred18, train.data4$cp)))/sum(table(pred18, train.data4$cp))#精度 = 0.7857143

pred7 = predict(r_prune1, test.data4, type = 'class')
sum(diag(table(pred7, test.data4$cp)))/sum(table(pred7, test.data4$cp))#精度 = 0.6666667😤
rpart.plot(r_prune1, extra = 2, type = 2, main="Pruned Decision Tree Of 4 Class")
r_prune2 = prune(tree = model9, cp = 0.02)
pred8 = predict(r_prune2, test.data4, type = 'class')
sum(diag(table(pred8, test.data4$cp)))/sum(table(pred8, test.data4$cp))#精度 = 0.6587302😤

## d06 data4 RF
sample_d.tune1<-tuneRF(train.data4[,-14],train.data4[,14],doBest=T)#https://tjo.hatenablog.com/entry/2013/09/02/190449
n1 = length(names(train.data4))
set.seed(101)
for(i in 1:(n-1)){
  mtry_fit1 = randomForest(cp ~ ., data = train.data4, mtry = i)
  error = mean(mtry_fit1$err.rate)
  print(error)
}
#对应误差值最小为2
set.seed(101)
ntree_fit1 = randomForest(cp ~ ., data = train.data4, mtry = 2, ntree = 200)#200,600,800
plot(ntree_fit1, main = ' ')
set.seed(101)
model18 = randomForest(cp ~ ., data = train.data4, mtry = 2, ntree = 200)
importance(model18)
set.seed(101)
varImpPlot(model18)
pred23 = predict(model18, test.data4, type = 'class')
pred24 = predict(model18, train.data4, type = 'class')
sum(diag(table(pred23, test.data4$cp)))/sum(table(pred23, test.data4$cp))#精度 = 0.7698413(800) 0.7698413(200) 0.7460317(600)
sum(diag(table(pred24, train.data4$cp)))/sum(table(pred24, train.data4$cp))#精度 = 0.9921053 0.9947368 0.9921053

imp1 = varImpPlot(model18)
imp1 = as.data.frame(imp1)
imp1$varnames = rownames(imp1)
rownames(imp1) = NULL
barplot(imp1$MeanDecreaseGini~reorder(imp1$varnames, imp1$MeanDecreaseGini),
        xlab = '特徴', ylab = 'mean decrease gini', col = 'lightgoldenrod1',
        main = '4分割の特徴の重要度',
        y)

barplot(imp1$MeanDecreaseGini ~ reorder(imp1$varnames, imp1$MeanDecreaseGini),
        xlab = '特徴', ylab = 'mean decrease gini', col = 'lightgoldenrod1',
        main = '4分割の特徴の重要度',
        ylim = c(0, 70))


# gbm
model7 = gbm(cp~.,data=train.data3, shrinkage=0.01,
             distribution='gaussian',cv.folds=5,
             n.trees=30,verbose=T)
model7
### error

# fastAdaboost
### 没搞懂

# randomForestExplainer
set.seed(1234)
forest1 = randomForest(cp ~ ., data = train.data3)
forest1
min_depth_frame1 =  min_depth_distribution(forest1)
head(min_depth_frame1)

## Distribution of minimal depth
plot_min_depth_distribution(min_depth_frame1)
plot_min_depth_distribution(forest1) # gives the same result as below but takes longer
plot_min_depth_distribution(min_depth_frame1, mean_sample = "relevant_trees", k = 4)
importance_frame1 = measure_importance(forest1)
importance_frame1

## Multi-way importance plot
plot_multi_way_importance(importance_frame1, size_measure = "no_of_nodes")
plot_multi_way_importance(importance_frame1, x_measure = "gini_decrease", 
                          y_measure = "no_of_trees", size_measure = "p_value", 
                          no_of_labels = 5)

## Compare measures using ggpairs
plot_importance_ggpairs(importance_frame1)
plot_importance_rankings(importance_frame1)

# questions
## 1. effect of factor 
p = rpart(cp ~., data = train.data7)
p1 = predict(p, test.data7, type = 'class')
table(p1, test.data7$cp)
sum(diag(table(p1, test.data7$cp)))/sum(table(p1, test.data7$cp)) # = 0.7777778

p2 = rpart(cp ~ ., data = train.data8)
p3 = predict(p2, test.data8, type = 'class')
sum(diag(table(p3, test.data8$cp)))/sum(table(p3, test.data8$cp)) # = 0.7777778
## conclusion: no change
## 1. 
q = d11
q$chas = as.factor(q$chas)
q1 = q[-even.n,]
q2 = q[even.n,]
q3 = rpart(cp ~ ., data = q1)
printcp(q3)
q4 = predict(q3, q2, type = 'class')
sum(diag(table(q4,q2$cp)))/sum(table(q4, q2$cp))#0.667
## no change







