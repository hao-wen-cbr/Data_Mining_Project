ass2_data <- read.csv("~/Desktop/Notes/COMP/COMP8410/Assignment 2-20190905/ass2_data.csv")
STEM_subjects <- read.csv("~/Desktop/Notes/COMP/COMP8410/Assignment 2-20190905/STEM_subjects.csv", header=FALSE)
stem=STEM_subjects[,]
dim(ass2_data) #5641,220
main_data=ass2_data[,c(2,5:19)]


#subject
data_1=data_subject[8,]
data_1[data_1==NA]
data_1[is.na(data_1)==FALSE]
data_1[ , colSums(is.na(data_1)) == 0]
col_name=colnames(data_1[ , colSums(is.na(data_1)) == 0])
intersect(col_name,stem)

data_2=data_subject[2,]
data_2[data_2==NA]
data_2[is.na(data_2)==FALSE]
data_2[ , colSums(is.na(data_2)) == 0]
col_name2=colnames(data_2[ , colSums(is.na(data_2)) == 0])
intersect(col_name2,stem)

# eda
#missing value
colSums(is.na(main_data))

table(main_data$College.attended)
table(main_data$Gender)
table(main_data$Final.year)

sum(is.na(main_data$Numeracy.Naplan))
sum(is.na(main_data$Final.average.score))
#444 missing value means 444 accredited students
missmap(main_data)
par(mai=c(5,5,5,5))
library(naniar)
vis_miss(main_data)
library(Amelia)
missmap(main_data)

#explore y1 and y2
par(mfrow=c(1,3))
hist(main_data$Average.GPA,main ="Histogram of Average GPA")
hist(main_data$Final.average.score,main ="Histogram of Final.average.score")
plot(y_gpa,y_score,main="Scatter plot of two response variables",xlab="Average.GPA",ylab="Final.average.score")
#title("Scatter plot of Final.average.score and Average.GPA")
cor.test(main_data$Average.GPA,main_data$Final.average.score)

#explore x
par(mfrow=c(4,4))
barplot(table(main_data[,1]),main=colnames(main_data)[1])
#hist(main_data[,2],main=colnames(main_data)[2])
#barplot(table(main_data[,3]),main=colnames(main_data)[3])
for (i in seq(2,16)){
 hist(main_data[,i],main=colnames(main_data)[i],xlab=" ")
}
#pair plot
#par(mfrow=c(3,5),mar=c(2.5,2.5,1,1))
par(mfrow=c(3,5))
plot(main_data[,1],main_data$Final.average.score,xlab=colnames(main_data)[1],ylab="Final.average.score")
plot(main_data[,2],main_data$Final.average.score,xlab=colnames(main_data)[2],ylab="Final.average.score")

#plot(main_data[,3][main_data[,3]>0],main_data$Final.average.score[main_data[,3]>0],xlab=colnames(main_data)[3],ylab="Final.average.score")

for (i in seq(3,7)){
  plot(main_data[,i][main_data[,i]>0],main_data$Final.average.score[main_data[,i]>0],xlab=colnames(main_data)[i],ylab="Final.average.score")
}
for (i in seq(8,14)){
  plot(main_data[,i],main_data$Final.average.score,xlab=colnames(main_data)[i],ylab="Final.average.score")
}

#plot(y_gpa,y_score,xlab="Average.GPA",ylab="Final.average.score")
#text(0.5,0.5,"First title",cex=2,font=2)
par(mfrow=c(1,1))


#prepare data
#ignore college and year, and gender x
data2=subset(main_data,(main_data$Gender=='M')|(main_data$Gender=='F'))
data2$Numeracy.Naplan[data2$Numeracy.Naplan==0]=NA
data2$Reading.Naplan[data2$Reading.Naplan==0]=NA
data2$Writing.Naplan[data2$Writing.Naplan==0]=NA
data2$Spelling.Naplan[data2$Spelling.Naplan==0]=NA
data2$Grammar.Naplan[data2$Grammar.Naplan==0]=NA
table(data2$Gender)
#main_data=data[,c(2,5:18)]
colnames(data2)
#subject_data=data[,20:220]

y_score=data2$Final.average.score
y_gpa=data2$Average.GPA

#accredit student 313*12
gpa_data<-subset(data2, (is.na(data2$Final.average.score))&(!is.na(data2$Average.GPA)))
main_gpa_data=gpa_data[,c(1:7,10,11,12,14,16)]
dim(main_gpa_data)
#tertiary student 5180*15
score_data<-subset(data2, !is.na(data2$Final.average.score))
main_score_data=score_data[,c(1:16)]
dim(main_score_data)
#explore
#score
par(mfrow=c(4,4))
barplot(table(main_score_data[,1])[c(2,3)],main=colnames(main_score_data)[1])
#hist(main_data[,2],main=colnames(main_data)[2])
#barplot(table(main_data[,3]),main=colnames(main_data)[3])
for (i in seq(2,16)){
  hist(main_score_data[,i],main=colnames(main_score_data)[i],xlab=" ")
}
#gpa
par(mfrow=c(3,4))
barplot(table(main_gpa_data[,1])[c(2,3)],main=colnames(main_gpa_data)[1])
#hist(main_data[,2],main=colnames(main_data)[2])
#barplot(table(main_data[,3]),main=colnames(main_data)[3])
for (i in seq(2,12)){
  hist(main_gpa_data[,i],main=colnames(main_gpa_data)[i],xlab=" ")
}
par(mfrow=c(1,1))


#naplan and non-naplan
score_naplan_data=subset(score_data, (!is.na(score_data$Numeracy.Naplan))&(!is.na(score_data$Reading.Naplan))&(!is.na(score_data$Writing.Naplan))&(!is.na(score_data$Spelling.Naplan))&(!is.na(score_data$Grammar.Naplan)))
score_non_naplan_data=subset(score_data, (is.na(score_data$Numeracy.Naplan))&(is.na(score_data$Reading.Naplan))&(is.na(score_data$Writing.Naplan))&(is.na(score_data$Spelling.Naplan))&(is.na(score_data$Grammar.Naplan)))[,-seq(3,7)]
gpa_naplan_data=subset(gpa_data, (!is.na(gpa_data$Numeracy.Naplan))&(!is.na(gpa_data$Reading.Naplan))&(!is.na(gpa_data$Writing.Naplan))&(!is.na(gpa_data$Spelling.Naplan))&(!is.na(gpa_data$Grammar.Naplan)))[,-c(8,9,13,15)]
gpa_non_naplan_data=subset(gpa_data, (is.na(gpa_data$Numeracy.Naplan))&(is.na(gpa_data$Reading.Naplan))&(is.na(gpa_data$Writing.Naplan))&(is.na(gpa_data$Spelling.Naplan))&(is.na(gpa_data$Grammar.Naplan)))[,-c(seq(3,7),8,9,13,15)]

write.csv(score_data, file = "main_score_data.csv")
write.csv(gpa_data, file = "main_gpa_data.csv")
#impute data
c(nrow(score_naplan_data),nrow(score_non_naplan_data),nrow(gpa_naplan_data),nrow(gpa_non_naplan_data))
summary(score_naplan_data)
colSums(is.na(score_naplan_data))
colSums(is.na(gpa_naplan_data))
colSums(is.na(score_non_naplan_data))
colSums(is.na(gpa_non_naplan_data))

par(mfrow=c(4,4))
barplot(table(score_naplan_data[,1]),main=colnames(score_naplan_data)[1])
#hist(main_data[,2],main=colnames(main_data)[2])
#barplot(table(main_data[,3]),main=colnames(main_data)[3])
for (i in seq(2,16)){
  hist(score_naplan_data[,i],main=colnames(score_naplan_data)[i],xlab=" ")
}
summary(score_naplan_data)

#impute
colSums(is.na(score_naplan_data))
for(i in c(2,8,9,10,11,13,14)){
  score_naplan_data[is.na(score_naplan_data[,i]), i] <- mean(score_naplan_data[,i], na.rm = TRUE)
}
colSums(is.na(score_naplan_data))

par(mfrow=c(3,4))
barplot(table(score_non_naplan_data[,1]),main=colnames(score_non_naplan_data)[1])
#hist(main_data[,2],main=colnames(main_data)[2])
#barplot(table(main_data[,3]),main=colnames(main_data)[3])
for (i in seq(2,11)){
  hist(score_non_naplan_data[,i],main=colnames(score_non_naplan_data)[i],xlab=" ")
}
summary(score_non_naplan_data)
#impute
colSums(is.na(score_non_naplan_data))
score_non_naplan_data[is.na(score_non_naplan_data[,7]), 7] <- median(score_non_naplan_data[,7], na.rm = TRUE)
for(i in c(2,3,4,5,6,8,9)){
  score_non_naplan_data[is.na(score_non_naplan_data[,i]), i] <- mean(score_non_naplan_data[,i], na.rm = TRUE)
}
colSums(is.na(score_non_naplan_data))

par(mfrow=c(3,4))
barplot(table(gpa_naplan_data[,1]),main=colnames(gpa_naplan_data)[1])
#hist(main_data[,2],main=colnames(main_data)[2])
#barplot(table(main_data[,3]),main=colnames(main_data)[3])
for (i in seq(2,12)){
  hist(gpa_naplan_data[,i],main=colnames(gpa_naplan_data)[i],xlab=" ")
}
summary(gpa_naplan_data)
colSums(is.na(gpa_naplan_data))
for(i in c(8,9,11)){
  gpa_naplan_data[is.na(gpa_naplan_data[,i]), i] <- mean(gpa_naplan_data[,i], na.rm = TRUE)
}
colSums(is.na(gpa_naplan_data))

par(mfrow=c(2,4))
barplot(table(gpa_non_naplan_data[,1]),main=colnames(gpa_non_naplan_data)[1])
#hist(main_data[,2],main=colnames(main_data)[2])
#barplot(table(main_data[,3]),main=colnames(main_data)[3])
for (i in seq(2,7)){
  hist(gpa_non_naplan_data[,i],main=colnames(gpa_non_naplan_data)[i],xlab=" ")
}
summary(gpa_non_naplan_data)
#impute
colSums(is.na(gpa_non_naplan_data))
for(i in c(3,4,6)){
  gpa_non_naplan_data[is.na(gpa_non_naplan_data[,i]), i] <- mean(gpa_non_naplan_data[,i], na.rm = TRUE)
}
colSums(is.na(gpa_non_naplan_data))

colSums(is.na(score_naplan_data))
colSums(is.na(gpa_naplan_data))
colSums(is.na(score_non_naplan_data))
colSums(is.na(gpa_non_naplan_data))

#stem score
stem_01_score_naplan=sapply(c(score_naplan_data$Final.average.STEM.score>=mean(score_naplan_data$Final.average.STEM.score)),as.numeric)
stem_01_score_non_naplan=sapply(c(score_non_naplan_data$Final.average.STEM.score>=mean(score_non_naplan_data$Final.average.STEM.score)),as.numeric)
#stem gpa
stem_01_gpa_naplan=sapply(c(gpa_naplan_data$Final.average.STEM.GPA>=mean(gpa_naplan_data$Final.average.STEM.GPA)),as.numeric)
stem_01_gpa_non_naplan=sapply(c(gpa_non_naplan_data$Final.average.STEM.GPA>=mean(gpa_non_naplan_data$Final.average.STEM.GPA)),as.numeric)

under_performance_score_naplan=quantile(score_naplan_data$Final.average.score,0.25)
under_performance_score_non_naplan=quantile(score_non_naplan_data$Final.average.score,0.25)
under_performance_gpa_naplan=quantile(gpa_naplan_data$Average.GPA,0.25)
under_performance_gpa_non_naplan=quantile(gpa_non_naplan_data$Average.GPA,0.25)

under_01_score_naplan=sapply(c(score_naplan_data$Final.average.score<=under_performance_score_naplan),as.numeric)
under_01_score_non_naplan=sapply(c(score_non_naplan_data$Final.average.score<=under_performance_score_non_naplan),as.numeric)
under_01_gpa_naplan=sapply(c(gpa_naplan_data$Average.GPA<=under_performance_gpa_naplan),as.numeric)
under_01_gpa_non_naplan=sapply(c(gpa_non_naplan_data$Average.GPA<=under_performance_gpa_non_naplan),as.numeric)

write.csv(cbind(score_naplan_data,stem_01_score_naplan,under_01_score_naplan), file = "score_naplan_data.csv")
write.csv(cbind(gpa_naplan_data,stem_01_gpa_naplan,under_01_gpa_naplan), file = "gpa_naplan_data.csv")
write.csv(cbind(score_non_naplan_data,stem_01_score_non_naplan,under_01_score_non_naplan), file = "score_non_naplan_data.csv")
write.csv(cbind(gpa_non_naplan_data,stem_01_gpa_non_naplan,under_01_gpa_non_naplan), file = "gpa_non_naplan_data.csv")

score_naplan_data=cbind(score_naplan_data,stem_01_score_naplan,under_01_score_naplan)
gpa_naplan_data=cbind(gpa_naplan_data,stem_01_gpa_naplan,under_01_gpa_naplan)
score_non_naplan_data=cbind(score_non_naplan_data,stem_01_score_non_naplan,under_01_score_non_naplan)
gpa_non_naplan_data=cbind(gpa_non_naplan_data,stem_01_gpa_non_naplan,under_01_gpa_non_naplan)

par(mfrow=c(2,2))
hist(score_naplan_data$under_01_score_naplan)
hist(score_non_naplan_data$under_01_score_non_naplan)
hist(gpa_naplan_data$under_01_gpa_naplan)
hist(gpa_non_naplan_data$under_01_gpa_non_naplan)

par(mfrow=c(2,2))
hist(score_naplan_data$stem_01_score_naplan)
hist(score_non_naplan_data$stem_01_score_non_naplan)
hist(gpa_naplan_data$stem_01_gpa_naplan)
hist(gpa_non_naplan_data$stem_01_gpa_non_naplan)

score_naplan_data_under_1=subset(score_naplan_data,score_naplan_data$under_01_score_naplan==1)
score_naplan_data_under_0=subset(score_naplan_data,score_naplan_data$under_01_score_naplan==0)
gpa_naplan_data_under_1=subset(gpa_naplan_data,gpa_naplan_data$under_01_gpa_naplan==1)
gpa_naplan_data_under_0=subset(gpa_naplan_data,gpa_naplan_data$under_01_gpa_naplan==0)
par(mfrow=c(2,4))
hist(score_naplan_data_under_1$First.maths.score,main="math score under_1")
hist(score_naplan_data_under_1$First.English.score,main="english score under_1")
hist(score_naplan_data_under_0$First.maths.score,main="math score under_0")
hist(score_naplan_data_under_0$First.English.score,main="english score under_0")
hist(gpa_naplan_data_under_1$First.maths.grade,main="math GPA under_1")
hist(gpa_naplan_data_under_1$First.English.grade,main="english GPA under_1")
hist(gpa_naplan_data_under_0$First.maths.grade,main="math GPA under_0")
hist(gpa_naplan_data_under_0$First.English.grade,main="english GPA under_0")

par(mfrow=c(2,2))
score_naplan_data_stem_1=subset(score_naplan_data,score_naplan_data$stem_01_score_naplan==1)
score_naplan_data_stem_0=subset(score_naplan_data,score_naplan_data$stem_01_score_naplan==0)
gpa_naplan_data_stem_1=subset(gpa_naplan_data,gpa_naplan_data$stem_01_gpa_naplan==1)
gpa_naplan_data_stem_0=subset(gpa_naplan_data,gpa_naplan_data$stem_01_gpa_naplan==0)

hist(score_naplan_data_stem_1$First.maths.score,main="math score stem_1")
hist(score_naplan_data_stem_0$First.maths.score,main="math score stem_0")
hist(gpa_naplan_data_stem_1$First.maths.grade,main="math grade stem_1")
hist(gpa_naplan_data_stem_0$First.maths.grade,main="math grade stem_0")

summary(lm(First.maths.score~.,data=score_naplan_data[,seq(3,11)]))
summary(lm(First.English.score~.,data=score_naplan_data[,seq(3,11)]))


#under performance
#tertiary student model
#score_naplan_data
index_score_naplan <- sample(1:nrow(score_naplan_data),round(0.75*nrow(score_naplan_data)))
#fix this index
train_score_naplan <- score_naplan_data[index_score_naplan,]
test_score_naplan <- score_naplan_data[-index_score_naplan,]

#lm regression
regression_model_score_naplan=lm(train_score_naplan$Final.average.score ~ .,data = train_score_naplan[,1:11])
summary(regression_model_score_naplan)
#model selection
regression_model_score_naplan2=lm(Final.average.score ~ Gender+Numeracy.Naplan+Reading.Naplan+Writing.Naplan+First.maths.score+First.English.score,data = train_score_naplan)
summary(regression_model_score_naplan2)
#prediction and evaluation
lm_pred_score_naplan=predict(regression_model_score_naplan2,test_score_naplan)
MAE_lm_score_naplan <- sum(abs(lm_pred_score_naplan - test_score_naplan$Final.average.score))/nrow(test_score_naplan)
MAE_lm_score_naplan  #4.00
#glm classification
glm_model_score <- glm(train_score$score_01~.,data=train_score[,1:14],family=binomial())
summary(glm_model)

#score_non_naplan_data
index_score_non_naplan_data <- sample(1:nrow(score_non_naplan_data),round(0.75*nrow(score_non_naplan_data)))
train_score_non_naplan_data <- score_non_naplan_data[index_score_non_naplan_data,]
test_score_non_naplan_data <- score_non_naplan_data[-index_score_non_naplan_data,]
#lm regression
regression_model_score_non_naplan_data=lm(train_score_non_naplan_data$Final.average.score ~ .,data = train_score_non_naplan_data[,1:6])
summary(regression_model_score_non_naplan_data)
#model selection
regression_model_score_non_naplan_data2=lm(Final.average.score ~ Gender+First.maths.score+First.English.score+Final.average.STEM.score,data = train_score_non_naplan_data)
summary(regression_model_score_non_naplan_data2)
#prediction and evaluation
lm_pred_score_non_naplan_data=predict(regression_model_score_non_naplan_data2,test_score_non_naplan_data)
MAE_lm_score_non_naplan_data <- sum(abs(lm_pred_score_non_naplan_data - test_score_non_naplan_data$Final.average.score))/nrow(test_score_non_naplan_data)
MAE_lm_score_non_naplan_data #4.2

#gpa_naplan_data
index_gpa_naplan_data <- sample(1:nrow(gpa_naplan_data),round(0.75*nrow(gpa_naplan_data)))
train_gpa_naplan_data <- gpa_naplan_data[index_gpa_naplan_data,]
test_gpa_naplan_data <- gpa_naplan_data[-index_gpa_naplan_data,]
#lm regression
regression_model_gpa_naplan_data=lm(train_gpa_naplan_data$Average.GPA ~ .,data = train_gpa_naplan_data[,1:9])
summary(regression_model_gpa_naplan_data)
#model selection
regression_model_gpa_naplan_data2=lm(Average.GPA ~ First.English.grade+Final.average.STEM.GPA,data = train_gpa_naplan_data)
summary(regression_model_gpa_naplan_data2)
#prediction and evaluation
lm_pred_gpa_naplan_data=predict(regression_model_gpa_naplan_data2,test_gpa_naplan_data)
MAE_lm_gpa_naplan_data <- sum(abs(lm_pred_gpa_naplan_data - test_gpa_naplan_data$Average.GPA))/nrow(test_gpa_naplan_data)
MAE_lm_gpa_naplan_data #0.28

#gpa_non_naplan_data
index_gpa_non_naplan_data <- sample(1:nrow(gpa_non_naplan_data),round(0.75*nrow(gpa_non_naplan_data)))
train_gpa_non_naplan_data <- gpa_non_naplan_data[index_gpa_non_naplan_data,]
test_gpa_non_naplan_data <- gpa_non_naplan_data[-index_gpa_non_naplan_data,]
#lm regression
regression_model_gpa_non_naplan_data=lm(train_gpa_non_naplan_data$Average.GPA ~ .,data = train_gpa_non_naplan_data[,1:4])
summary(regression_model_gpa_non_naplan_data)
#model selection
regression_model_gpa_non_naplan_data2=lm(Average.GPA ~ First.English.grade+Final.average.STEM.GPA,data = train_gpa_non_naplan_data)
summary(regression_model_gpa_non_naplan_data2)
#prediction and evaluation
lm_pred_gpa_non_naplan_data=predict(regression_model_gpa_non_naplan_data2,test_gpa_non_naplan_data)
MAE_lm_gpa_non_naplan_data <- sum(abs(lm_pred_gpa_non_naplan_data - test_gpa_non_naplan_data$Average.GPA))/nrow(test_gpa_non_naplan_data)
MAE_lm_gpa_non_naplan_data #0.3795


#neural network
library(neuralnet)
#score_naplan_data
#normalise
gender_01=sapply(c(score_naplan_data$Gender=='M'),as.numeric)
num_score_naplan_data=cbind(gender_01,score_naplan_data)[,-2]
maxs_score_naplan_data <- apply(num_score_naplan_data, 2, max) 
mins_score_naplan_data <- apply(num_score_naplan_data, 2, min)
scaled_score_naplan_data <- as.data.frame(scale(num_score_naplan_data, center = mins_score_naplan_data, scale = maxs_score_naplan_data - mins_score_naplan_data))

train_nn_score_naplan_data <- scaled_score_naplan_data[index_score_naplan,]
test_nn_score_naplan_data <- scaled_score_naplan_data[-index_score_naplan,]

#first layer have 5 units, second layer has 3 units
#nn_score_naplan_data <- neuralnet(Final.average.score~.,data=train_nn_score_naplan_data[,1:15],hidden=c(5, 3), act.fct = "logistic", linear.output=T)
nn_score_naplan_data <- neuralnet(Final.average.score~gender_01+Numeracy.Naplan+Reading.Naplan+Writing.Naplan+First.maths.score+First.English.score,data=train_nn_score_naplan_data,hidden=c(1,1), act.fct = "logistic", linear.output=T)
#plot(nn_score_naplan_data)
predict_nn_score_naplan_data=compute(nn_score_naplan_data, test_nn_score_naplan_data)
#rescale to original scale
predict_nn_score_naplan_original <- predict_nn_score_naplan_data$net.result*(maxs_score_naplan_data[15]-mins_score_naplan_data[15])+mins_score_naplan_data[15]
test_nn_score_naplan_data_original <- (test_nn_score_naplan_data$Final.average.score)*(maxs_score_naplan_data[15]-mins_score_naplan_data[15])+mins_score_naplan_data[15]
# plot the result
#scaled result
par(mfrow=c(1,2),oma=c(0,0,2,0))
#plot(test_nn_score_naplan_data$Final.average.score,predict_nn_score_naplan_data$net.result,col='red',main='Real vs predicted NN after scale',pch=18,cex=0.7,xlab="ture test value",ylab="nn predicted value")
#original result
plot(test_nn_score_naplan_data_original,predict_nn_score_naplan_original,col='red',main='Neural Network',pch=18,cex=0.7,xlab="ture test value",ylab="nn predicted value")
plot(test_score_naplan$Final.average.score, lm_pred_score_naplan,col='red',main='Linear Model',pch=18,cex=0.7,xlab="ture test value",ylab="linear model predicted value")
title(main="score_naplan_data",outer=TRUE)
#average absolute difference
MAE.nn <- sum(abs(test_nn_score_naplan_data_original - predict_nn_score_naplan_original))/nrow(test_nn_score_naplan_data)
print(MAE.nn)  #3.89

#selected variable
nn_score_naplan_data <- neuralnet(Final.average.score~gender_01+Writing.Naplan+First.maths.score+First.English.score+Final.average.STEM.score,data=train_nn_score_naplan_data,hidden=c(5, 3), act.fct = "logistic", linear.output=T)
predict_nn_score_naplan_data2=compute(nn_score_naplan_data2, test_nn_score_naplan_data)
predict_nn_score_naplan_original2 <- predict_nn_score_naplan_data2$net.result*(max(test_nn_score_naplan_data$Final.average.score)-min(test_nn_score_naplan_data$Final.average.score))+min(test_nn_score_naplan_data$Final.average.score)
test_nn_score_naplan_data_original2 <- (test_nn_score_naplan_data$Final.average.score)*(max(test_nn_score_naplan_data$Final.average.score)-min(test_nn_score_naplan_data$Final.average.score))+min(test_nn_score_naplan_data$Final.average.score)
MAE.nn2 <- sum(abs(test_nn_score_naplan_data_original2 - predict_nn_score_naplan_original2))/nrow(test_nn_score_naplan_data)
print(MAE.nn2) 

#score_non_naplan_data
gender_01_score_non_naplan=sapply(c(score_non_naplan_data$Gender=='M'),as.numeric)
num_score_non_naplan_data=cbind(gender_01_score_non_naplan,score_non_naplan_data)[,-2]
maxs_score_non_naplan_data <- apply(num_score_non_naplan_data, 2, max) 
mins_score_non_naplan_data <- apply(num_score_non_naplan_data, 2, min)
scaled_num_score_non_naplan_data <- as.data.frame(scale(num_score_non_naplan_data, center = mins_score_non_naplan_data, scale = maxs_score_non_naplan_data - mins_score_non_naplan_data))

train_nn_score_non_naplan_data <- scaled_num_score_non_naplan_data[index_score_non_naplan_data,]
test_nn_score_non_naplan_data <- scaled_num_score_non_naplan_data[-index_score_non_naplan_data,]

nn_score_non_naplan_data <- neuralnet(Final.average.score ~ gender_01_score_non_naplan+First.maths.score+First.English.score+Final.average.STEM.score,data=train_nn_score_non_naplan_data,hidden=c(5, 3), act.fct = "logistic", linear.output=T)
predict_nn_score_non_naplan_data=compute(nn_score_non_naplan_data, test_nn_score_non_naplan_data)
predict_nn_score_non_naplan_original <- predict_nn_score_non_naplan_data$net.result*(maxs_score_non_naplan_data[10]-mins_score_non_naplan_data[10])+mins_score_non_naplan_data[10]
test_nn_score_non_naplan_data_original <- (test_nn_score_non_naplan_data$Final.average.score)*(maxs_score_non_naplan_data[10]-mins_score_non_naplan_data[10])+mins_score_non_naplan_data[10]
MAE.nn_score_non_naplan <- sum(abs(test_nn_score_non_naplan_data_original - predict_nn_score_non_naplan_original))/nrow(test_nn_score_non_naplan_data)
print(MAE.nn_score_non_naplan) #4.25
par(mfrow=c(1,2))
plot(test_nn_score_non_naplan_data_original,predict_nn_score_non_naplan_original,col='red',main='Neural Network',pch=18,cex=0.7,xlab="ture test value",ylab="nn predicted value")
plot(test_score_non_naplan_data$Final.average.score, lm_pred_score_non_naplan_data,col='red',main='Linear Model',pch=18,cex=0.7,xlab="ture test value",ylab="linear model predicted value")
title(main="score_non_naplan_data",outer=TRUE)


#gpa_naplan_data
gender_01_gpa_naplan=sapply(c(gpa_naplan_data$Gender=='M'),as.numeric)
num_gpa_naplan_data=cbind(gender_01_gpa_naplan,gpa_naplan_data)[,-2]
maxs_gpa_naplan_data <- apply(num_gpa_naplan_data, 2, max) 
mins_gpa_naplan_data <- apply(num_gpa_naplan_data, 2, min)
scaled_num_gpa_naplan_data <- as.data.frame(scale(num_gpa_naplan_data, center = mins_gpa_naplan_data, scale = maxs_gpa_naplan_data - mins_gpa_naplan_data))

train_nn_gpa_naplan_data <- scaled_num_gpa_naplan_data[index_gpa_naplan_data,]
test_nn_gpa_naplan_data <- scaled_num_gpa_naplan_data[-index_gpa_naplan_data,]

nn_gpa_naplan_data <- neuralnet(Average.GPA ~ First.English.grade+Final.average.STEM.GPA,data=train_nn_gpa_naplan_data,hidden=c(5,3), act.fct = "logistic", linear.output=T)
predict_nn_gpa_naplan_data=compute(nn_gpa_naplan_data, test_nn_gpa_naplan_data)
predict_nn_gpa_naplan_original <- predict_nn_gpa_naplan_data$net.result*(maxs_gpa_naplan_data[12]-mins_gpa_naplan_data[12])+mins_gpa_naplan_data[12]
test_nn_gpa_naplan_data_original <- (test_nn_gpa_naplan_data$Average.GPA)*(maxs_gpa_naplan_data[12]-mins_gpa_naplan_data[12])+mins_gpa_naplan_data[12]
MAE.nn_gpa_naplan <- sum(abs(test_nn_gpa_naplan_data_original - predict_nn_gpa_naplan_original))/nrow(test_nn_gpa_naplan_data)
print(MAE.nn_gpa_naplan) #0.3215
par(mfrow=c(1,2))
plot(test_nn_gpa_naplan_data_original,predict_nn_gpa_naplan_original,col='red',main='Neural Network',pch=18,cex=0.7,xlab="ture test value",ylab="nn predicted value")
plot(test_gpa_naplan_data$Average.GPA, lm_pred_gpa_naplan_data,col='red',main='Linear Model',pch=18,cex=0.7,xlab="ture test value",ylab="linear model predicted value")
title(main="GPA_naplan_data",outer=TRUE)

#gpa_non_naplan_data
gender_01_gpa_non_naplan=sapply(c(gpa_non_naplan_data$Gender=='M'),as.numeric)
num_gpa_non_naplan_data=cbind(gender_01_gpa_non_naplan,gpa_non_naplan_data)[,-2]
maxs_gpa_non_naplan_data <- apply(num_gpa_non_naplan_data, 2, max) 
mins_gpa_non_naplan_data <- apply(num_gpa_non_naplan_data, 2, min)
scaled_num_gpa_non_naplan_data <- as.data.frame(scale(num_gpa_non_naplan_data, center = mins_gpa_non_naplan_data, scale = maxs_gpa_non_naplan_data - mins_gpa_non_naplan_data))

train_nn_gpa_non_naplan_data <- scaled_num_gpa_non_naplan_data[index_gpa_non_naplan_data,]
test_nn_gpa_non_naplan_data <- scaled_num_gpa_non_naplan_data[-index_gpa_non_naplan_data,]

nn_gpa_non_naplan_data <- neuralnet(Average.GPA ~ First.English.grade+Final.average.STEM.GPA,data=train_nn_gpa_non_naplan_data,hidden=c(5, 3), act.fct = "logistic", linear.output=T)
predict_nn_gpa_non_naplan_data=compute(nn_gpa_non_naplan_data, test_nn_gpa_non_naplan_data)
predict_nn_gpa_non_naplan_original <- predict_nn_gpa_non_naplan_data$net.result*(maxs_gpa_non_naplan_data[7]-mins_gpa_non_naplan_data[7])+mins_gpa_non_naplan_data[7]
test_nn_gpa_non_naplan_data_original <- (test_nn_gpa_non_naplan_data$Average.GPA)*(maxs_gpa_non_naplan_data[7]-mins_gpa_non_naplan_data[7])+mins_gpa_non_naplan_data[7]
MAE.nn_gpa_non_naplan <- sum(abs(test_nn_gpa_non_naplan_data_original - predict_nn_gpa_non_naplan_original))/nrow(test_nn_gpa_non_naplan_data)
print(MAE.nn_gpa_non_naplan) #0.3387
plot(test_nn_gpa_naplan_data_original,predict_nn_gpa_naplan_original,col='red',main='Neural Network',pch=18,cex=0.7,xlab="ture test value",ylab="nn predicted value")
plot(test_gpa_naplan_data$Average.GPA, lm_pred_gpa_naplan_data,col='red',main='Linear Model',pch=18,cex=0.7,xlab="ture test value",ylab="linear model predicted value")
title(main="GPA_naplan_data",outer=TRUE)

#dt
library(rpart)
dt_model <- rpart(score_01~.,method="class", data=main_score_data[,1:14])
printcp(dt_model)
plot(dt_model, uniform=TRUE,main="Classification Tree for underperformance")
text(dt_model, use.n=TRUE, all=TRUE, cex=.8)

#result shows three variables has effect: Final.average.STEM.score, First.English.score,STEM.to.total.ratio
underperf_data=subset(main_score_data, main_score_data$Final.average.score<=under_performance)
non_underperf_data=subset(main_score_data, main_score_data$Final.average.score>under_performance)
par(mfrow=c(3,2))
hist(underperf_data$Final.average.STEM.score,main="Final.average.STEM.score",xlab="under-performance")
hist(non_underperf_data$Final.average.STEM.score,main='Final.average.STEM.score',xlab='non-under-performance')
hist(underperf_data$First.English.score,main="First.English.score",xlab="under-performance")
hist(non_underperf_data$First.English.score,main="First.English.score",xlab='non-under-performance')
hist(underperf_data$STEM.to.total.ratio,main="STEM.to.total.ratio",xlab="under-performance")
hist(non_underperf_data$STEM.to.total.ratio,main="STEM.to.total.ratio",xlab='non-under-performance')

mean(na.omit(underperf_data$Final.average.STEM.score))
mean(na.omit(non_underperf_data$Final.average.STEM.score))
median(na.omit(underperf_data$Final.average.STEM.score))
median(na.omit(non_underperf_data$Final.average.STEM.score))

mean(na.omit(underperf_data$First.English.score))
mean(na.omit(non_underperf_data$First.English.score))
median(na.omit(underperf_data$First.English.score))
median(na.omit(non_underperf_data$First.English.score))

mean(na.omit(underperf_data$STEM.to.total.ratio))
mean(na.omit(non_underperf_data$STEM.to.total.ratio))
median(na.omit(underperf_data$STEM.to.total.ratio))
median(na.omit(non_underperf_data$STEM.to.total.ratio))

#accredited student model
#mlm
regression_model2=lm(main_gpa_data$Average.GPA ~ .,data = main_gpa_data)
summary(regression_model2)
#glm

gpa_01=sapply(c(main_gpa_data$Average.GPA<=under_performance_gpa),as.numeric)
glm_model2 <- glm(gpa_01~.,data=main_gpa_data[,1:11],family=binomial())
summary(glm_model2)
#dt
dt_model2 <- rpart(gpa_01~.,method="class", data=main_gpa_data[,1:11])
printcp(dt_model2)
plot(dt_model2, uniform=TRUE,main="Classification Tree for underperformance")
text(dt_model2, use.n=TRUE, all=TRUE, cex=.8)

par(mfrow=c(4,4))
barplot(table(main_gpa_data[,1]),main=colnames(main_gpa_data)[1])
#hist(main_data[,2],main=colnames(main_data)[2])
#barplot(table(main_data[,3]),main=colnames(main_data)[3])
for (i in seq(2,16)){
  hist(main_gpa_data[,i],main=colnames(main_gpa_data)[i],xlab=" ")
}
main_gpa_data$First.maths.score #all na
main_gpa_data$First.English.score #all na
main_gpa_data$First.maths.grade
main_gpa_data$First.English.grade
main_gpa_data$STEM.to.total.ratio
main_gpa_data$Final.average.STEM.score #all na
main_gpa_data$Final.average.STEM.GPA


#explore stem
pairs(main_data[,c(12,13,14,15)])
#cor(main_data$Final.average.STEM.score,main_data$Final.average.score)
pairs(main_data[,c(seq(3,7),13)])
new_Numeracy.Naplan=
main_data$Numeracy.Naplan[main_data$Numeracy.Naplan == 0] <- NA
main_data$Reading.Naplan[main_data$Reading.Naplan == 0] <- NA
main_data$Writing.Naplan[main_data$Writing.Naplan==0]=NA
main_data$Spelling.Naplan[main_data$Spelling.Naplan==0]=NA
main_data$Grammar.Naplan[main_data$Grammar.Naplan==0]=NA




