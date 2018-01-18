setwd("C:/Study/Competitions/TI")
rm(list=ls(all=TRUE))

library(data.table)

## Reading all the surveys in a R
ws<- read.csv("WorkSatisfaction.csv")
ts<- read.csv("Team Satisfaction 1.csv")
IS<- read.csv("IS.csv")
CF<- read.csv("CF.csv")

##########################################
# Work Satisfaction
##########################################

colnames(ws)[2]<-"s1_w1"
colnames(ws)[3]<-"s1_w2"
colnames(ws)[4]<-"s1_w3"
colnames(ws)[5]<-"s1_w4"
colnames(ws)[6]<-"s1_w5"
colnames(ws)[7]<-"s1_w6"
colnames(ws)[8]<-"s2_w1"
colnames(ws)[9]<-"s2_w2"
colnames(ws)[10]<-"s2_w3"

colnames(ws)[11]<-"s3_w1"
colnames(ws)[12]<-"s3_w2"
colnames(ws)[13]<-"s3_w3"
colnames(ws)[14]<-"s3_w4"

ws1<-ws[complete.cases(ws),]

# Principal component analysis
pca1 <- princomp(ws1[,-1], scores=TRUE, cor=TRUE)
summary(pca1)

# Loadings of principal components
loadings(pca1)
#pca1$loadings

# Scree plot of eigenvalues
plot(pca1)
screeplot(pca1, type="line", main="Scree Plot")

# Biplot of score variables
biplot(pca1)

pca <- prcomp(ws1[,-1])
screeplot(pca,type="lines") # looks like there are 5 principal components

pca$rotation[,1:5]*100 # because we have 5 prin. comp.s 

# get the principal components
factors <- pca$x[,1:5]
head(factors)
summary(factors)
cov(factors)

# algebra to standardize the principal components
# install.packages("expm")

library(expm)
factors <- factors %*% solve(sqrtm(crossprod(factors))) * sqrt(nrow(factors)) 
crossprod(factors)/nrow(factors)
cov(factors)

##########################################
# Team Satisfaction
##########################################

colnames(ts)[2]<-"s1_t1"
colnames(ts)[3]<-"s1_t2"
colnames(ts)[4]<-"s1_t3"

colnames(ts)[5]<-"s2_t1"
colnames(ts)[6]<-"s2_t2"

colnames(ts)[7]<-"s3_t1"
colnames(ts)[8]<-"s3_t2"
colnames(ts)[9]<-"s3_t3"

ts1<-ts[complete.cases(ts),]

# Principal component analysis
pca1 <- princomp(ts1[,-1], scores=TRUE, cor=TRUE)
summary(pca1)

# Loadings of principal components
loadings(pca1)
#pca1$loadings

# Scree plot of eigenvalues
plot(pca1)
screeplot(pca1, type="line", main="Scree Plot")

# Biplot of score variables
biplot(pca1)

pca <- prcomp(ts1[,-1])
screeplot(pca,type="lines") # looks like there are 5 principal components

pca$rotation[,1:5]*100 # because we have 5 prin. comp.s 

# get the principal components
factors <- pca$x[,1:5]
head(factors)
summary(factors)
cov(factors)

# algebra to standardize the principal components
# install.packages("expm")

library(expm)
factors <- factors %*% solve(sqrtm(crossprod(factors))) * sqrt(nrow(factors)) 
crossprod(factors)/nrow(factors)
cov(factors)

###################################################
# IS
###################################################
colnames(IS)[2]<-"s1_I1"
colnames(IS)[3]<-"s1_I2"
colnames(IS)[4]<-"s1_I3"

colnames(IS)[5]<-"s2_I1"
colnames(IS)[6]<-"s2_I2"
colnames(IS)[7]<-"s2_I3"

colnames(IS)[8]<-"s3_I1"
colnames(IS)[9]<-"s3_I2"
colnames(IS)[10]<-"s3_I3"

IS1<-IS[complete.cases(IS),]

# Principal component analysis
pca1 <- princomp(IS1[,-1], scores=TRUE, cor=TRUE)
summary(pca1)

# Loadings of principal components
loadings(pca1)
#pca1$loadings

# Scree plot of eigenvalues
plot(pca1)
screeplot(pca1, type="line", main="Scree Plot")

# Biplot of score variables
biplot(pca1)

pca <- prcomp(IS1[,-1])
screeplot(pca,type="lines") # looks like there are 5 principal components

pca$rotation[,1:5]*100 # because we have 5 prin. comp.s 

# get the principal components
factors <- pca$x[,1:5]
head(factors)
summary(factors)
cov(factors)

# algebra to standardize the principal components
# install.packages("expm")

library(expm)
factors <- factors %*% solve(sqrtm(crossprod(factors))) * sqrt(nrow(factors)) 
crossprod(factors)/nrow(factors)
cov(factors)


###################################################
# CF
###################################################
colnames(CF)[2]<-"s1_c1"
colnames(CF)[3]<-"s1_c2"
colnames(CF)[4]<-"s1_c3"
colnames(CF)[5]<-"s1_c4"
colnames(CF)[6]<-"s1_c5"
colnames(CF)[7]<-"s1_c6"
colnames(CF)[8]<-"s1_c7"
colnames(CF)[9]<-"s1_c8"
colnames(CF)[10]<-"s1_c9"

colnames(CF)[11]<-"s2_c1"
colnames(CF)[12]<-"s2_c2"
colnames(CF)[13]<-"s2_c3"
colnames(CF)[14]<-"s2_c4"
colnames(CF)[15]<-"s2_c5"

colnames(CF)[16]<-"s3_c1"
colnames(CF)[17]<-"s3_c2"
colnames(CF)[18]<-"s3_c3"
colnames(CF)[19]<-"s3_c4"
colnames(CF)[20]<-"s3_c5"
colnames(CF)[21]<-"s3_c6"
colnames(CF)[22]<-"s3_c7"
colnames(CF)[23]<-"s3_c8"
colnames(CF)[24]<-"s3_c9"

CF1<-CF[complete.cases(CF),]

# Principal component analysis
pca1 <- princomp(CF1[,-1], scores=TRUE, cor=TRUE)
summary(pca1)

# Loadings of principal components
loadings(pca1)
#pca1$loadings

# Scree plot of eigenvalues
plot(pca1)
screeplot(pca1, type="line", main="Scree Plot")

# Biplot of score variables
biplot(pca1)

pca <- prcomp(CF1[,-1])
screeplot(pca,type="lines") # looks like there are 5 principal components

pca$rotation[,1:5]*100 # because we have 5 prin. comp.s 

# get the principal components
factors <- pca$x[,1:5]
head(factors)
summary(factors)
cov(factors)


# algebra to standardize the principal components
# install.packages("expm")

library(expm)
factors <- factors %*% solve(sqrtm(crossprod(factors))) * sqrt(nrow(factors)) 
crossprod(factors)/nrow(factors)
cov(factors)

##################################
# Model Selection
##################################
colnames(ws1)[1]<-"Emp_id"
colnames(IS1)[1]<-"Emp_id"
colnames(ts1)[1]<-"Emp_id"
colnames(CF1)[1]<-"Emp_id"
d1<-merge(ws1,IS1,by="Emp_id")
d1<-merge(d1,ts1,by="Emp_id")
d1<-merge(d1,CF1,by="Emp_id")

# write.csv(d1,"D:/UTD/Competitions/TI/Slides/merged.csv")
attach(d1)
imp_var<-cbind(Emp_id,s1_w5,s1_w6,s2_w1,s2_w2,s3_w1,s3_w4,s1_I1,s1_I2,s2_I1,s3_I1,s3_I2,s1_t1,s1_t2,s2_t2,s3_t2,s3_t3,s1_c4,s1_c5,s2_c2,s2_c4,s3_c5,s3_c6)
write.csv(imp_var,"D:/UTD/Competitions/TI/Slides/imp_var1.csv")


data1<-read.csv("D:/UTD/Competitions/TI/Slides/Final Data.csv")
data1
str(data1)
# Removing Data of Interns when It is not Interested
data1<-data1[which(data1$Resolve..Accept..Decline..Undecided.!="Global - TI Not Interested"),]

str(data1)
names(data1)
attach(data1)

library(caret)


d1<-data1[,c(1,34:45,48,49,53:55,57:59)]
d1<-d1[complete.cases(d1),]

colnames(d1)[19]<-"Resolve"
colnames(d1)

colnames(d1)[21]<-"Location"

inTrain = createDataPartition(d1$Resolve, p = 0.80, list = FALSE)
dfTrain=d1[inTrain,]
dfTest=d1[-inTrain,]
d1$Location<-as.numeric(d1$Location)

names(d1)
attach(d1)

model<-glm(d1$Resolve~., family=binomial(link="logit"),data=d1)
summary(model)
model <- glm(Resolve~TE12+WS13+WS23+I(Wage*WS13)+I(Wage*WS13)+I(Wage*TE12)+I(Location*WS13)+I(Location*WS23)+IS13+I(Location*IS13)+CF12+Location+Gender+I(CF12*Gender)+Wage,family=binomial(link="logit"),data=d1)
model <- glm(Resolve~WS23+I(Wage*TE12)+IS13+I(Location*Gender)+I(Wage*TE12)+CF12,family=binomial(link="logit"),data=d1)
coeftest(model,vcov. = vcovHC)

model <- glm(Resolve~WS23+I(Wage*TE12)+IS13+I(Location*Gender)+I(Wage*TE12)+CF12,family=binomial(link="logit"),data=d1)
coeftest(model,vcov. = vcovHC)

inTrain = createDataPartition(d1$Resolve, p = 0.80, list = FALSE)
dfTrain=d1[inTrain,]
dfTest=d1[-inTrain,]
d1$Location.1<-as.numeric(d1$Location.1)

attach(dfTrain)
# Prediction Model On Training Data
model <- glm( as.factor(Resolve) ~ WS23 + I(Wage*TE12) + IS13 + I(Location*Gender)+ I(Wage*TE12) + CF12,family=,data=dfTrain)

# White Correction Test to Remove Heteroskedasticity 
coeftest(model,vcov. = vcovHC)

# Output :
# z test of coefficients:
#   
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)           0.406653   0.335002  1.2139   0.2248
# WS23                  0.091744   0.114266  0.8029   0.4220
# I(Wage * TE12)        0.042967   0.039355  1.0918   0.2749
# IS13                  0.255582   0.177565  1.4394   0.1500
# I(Location * Gender) -0.019626   0.013400 -1.4646   0.1430
# CF12                  0.132612   0.099558  1.3320   0.1829

library(ROCR)
# Prediction on Testing Data
fitted.results <- predict(model,dfTest,type='response')
pr <- prediction(fitted.results, dfTest$Resolve)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]

# Accuracy In Percentage
auc * 100
# 69.44444
