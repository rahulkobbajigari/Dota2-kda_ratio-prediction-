#clearing the r environment
rm(list = ls(all=TRUE))

#loading the required libraries


library(dplyr)
library(MlBayesOpt)
#devtools::install_github("jakob-r/mlrHyperopt", dependencies = TRUE)
library(mlrHyperopt)


#reading the data into r

train1<-read.csv("train1.csv")

train9<-read.csv("train9.csv")

test9<-read.csv("test9.csv")

hero<-read.csv("hero_data.csv")

test1<-read.csv("test1.csv")
str(train1)
#the data consists in four parts
#the dota data consists 10 games for each player 
#the train1 consists of 10 game of each player kda ratio
#and the train9 consists of 9 games kda_ratio of each player
#and the test9 consists of 9 games kda_ratio of another players that players data is not there in train1
#we should predict the kda_ratio of test9 players 10Th game that data present in test9
#train9.csv and train1.csv contain the user performance for their most frequent 9 heroes and 10th hero respectively. Both train9.csv and train1.csv have below fields.
#test9.csv contain the different set of user (different from training set) performance for their most frequent 9 heroes. test9.csv has similar fields as train9.csv. Now, the aim is to predict the performance (kda_ratio) of the same set of users (test users) with the 10th hero which is test1.csv.

#We also have "hero_data.csv" which contains information about heros.

#calculating the of kda-RATIO WITH respect to the hero_id in train and test
herokda_ratio<-aggregate(train9$kda_ratio,list(train9$hero_id), data=train9, mean)
herokda_ratio1<-aggregate(test9$kda_ratio,list(test9$hero_id), data=test9, mean)
herokda_ratio2<-(herokda_ratio+herokda_ratio1)/2
hero$herokda_ratio<-herokda_ratio2$x


#calculating the mean of kda_ratio with respect to the user id
user<-aggregate(train9$kda_ratio,list(train9$user_id),mean)
names(user)[1]<-paste("user_id")


#joining the mean of kda_ratio with respect to the user_id in train and test 
train9<-train9%>%inner_join(user,c("user_id"))
names(train9)[7]<-paste("user_ratio")


usertest<-aggregate(test9$kda_ratio,list(test9$user_id), data=test9, mean)
names(usertest)[1]<-paste("user_id")

test9<-test9%>%inner_join(usertest,c("user_id"))
names(test9)[7]<-paste("user_ratio")

test1<-test1%>%inner_join(usertest,c("user_id"))
names(test1)[5]<-paste("user_ratio")


train1<-train1%>%inner_join(user,c("user_id"))
names(train1)[7]<-paste("user_ratio")




#from the formula of kda_ratio 
#i used to calulate the deaths of a game by using no of wins and no of games

deaths<-(train9$num_games-train9$num_wins)


#from my  side i thought that wins consists of kills and assists


# in wins  i assumed that half kills and half assists
assists<-(train9$kda_ratio*(deaths))-((train9$num_wins)/3)
assists<-assists/1000

#aggregated the kda_ratio mean withs respect to assists in hero and user
userassists<-aggregate(assists,list(train9$user_id), mean)
names(userassists)[1]<-paste("user_id")


heroassists<-aggregate(assists,list(train9$hero_id), mean)


#joined the assists data with respect to user_id in train9 and train1 test9 and test1
train9<-train9%>%inner_join(userassists,c("user_id"))
names(train9)[8]<-paste("user_assists")



train1<-train1%>%inner_join(userassists,c("user_id"))
names(train1)[8]<-paste("user_assists")




#aggregated the kda_ratio mean withs respect to deaths in hero and user

userdeaths<-aggregate(deaths,list(train9$user_id),mean)
names(userdeaths)[1]<-paste("user_id")



train9<-train9%>%inner_join(userdeaths,c("user_id"))
names(train9)[9]<-paste("user_deaths")



train1<-train1%>%inner_join(userdeaths,c("user_id"))
names(train1)[9]<-paste("user_deaths")



herodeaths<-aggregate(deaths,list(train9$hero_id),mean)





#in wins kills consists of half
userkills<-aggregate((train9$num_wins/2),list(train9$user_id),mean)
names(userkills)[1]<-paste("user_id")

train9<-train9%>%inner_join(userkills,c("user_id"))
names(train9)[10]<-paste("user_kills")



train1<-train1%>%inner_join(userkills,c("user_id"))
names(train1)[10]<-paste("user_kills")



herokills<-aggregate((train9$num_wins/2),list(train9$hero_id),mean)



#from the formula of kda_ratio 
#i used to calulate the deaths of a game by using no of wins and no of games

#from my  side i thought that wins consists of kills and assists


# in wins  i assumed that half kills and half assists

deaths1<-(test9$num_games-test9$num_wins)/2
assists1<-(test9$kda_ratio*(deaths1))-(test9$num_wins)
assists1<-assists1/1000


userassists1<-aggregate(assists1,list(test9$user_id), mean)
names(userassists1)[1]<-paste("user_id")

test9<-test9%>%inner_join(userassists1,c("user_id"))
names(test9)[8]<-paste("user_assists")


test1<-test1%>%inner_join(userassists1,c("user_id"))
names(test1)[6]<-paste("user_assists")



heroassists1<-aggregate(assists1,list(test9$hero_id), mean)




userdeaths1<-aggregate(deaths1,list(test9$user_id),mean)
names(userdeaths1)[1]<-paste("user_id")

test9<-test9%>%inner_join(userdeaths1,c("user_id"))
names(test9)[9]<-paste("user_deaths")


test1<-test1%>%inner_join(userdeaths1,c("user_id"))
names(test1)[7]<-paste("user_deaths")




herodeaths1<-aggregate(deaths1,list(test9$hero_id),mean)


userkills1<-aggregate((test9$num_wins/2),list(test9$user_id),mean)
names(userkills1)[1]<-paste("user_id")

test9<-test9%>%inner_join(userkills1,c("user_id"))
names(test9)[10]<-paste("user_kills")


test1<-test1%>%inner_join(userkills1,c("user_id"))
names(test1)[8]<-paste("user_kills")




herokills1<-aggregate((test9$num_wins/3),list(test9$hero_id),mean)




heroassists2<-(heroassists+heroassists1)/2
hero$hero_assists<-heroassists2$x


herodeaths2<-(herodeaths+herodeaths1)/2
hero$hero_deaths<-herodeaths2$x


herokills2<-(herokills+herokills1)/2
hero$hero_kills<-herokills2$x

#merging the data
#train1 is used as validation
#i merged train9 and test9 as a train9
#test1 is kept as a test 
mergedata<-rbind(train9,test9)
minuserid = min(mergedata$user_id)
maxuserid = max(mergedata$user_id)
# Generating the sequence of dates from start date to end date
seq <- data.frame("user_id"=seq(minuserid,maxuserid))


#merging the data withrespect to user_id
mergeseq <- seq %>% full_join(mergedata,c("user_id"))


#merging the data with respect to hero_id
mergeseq1<-mergeseq%>%inner_join(hero,c("hero_id"))

#summary of a data
summary(mergeseq1)
#structure of a data
str(mergeseq1)

train2<-mergeseq1

validation<-train1%>%inner_join(hero,c("hero_id"))

test2<-test1%>%inner_join(hero,c("hero_id"))





herokda_ratio3<-aggregate(train2$kda_ratio,list(train2$hero_id,train2$user_id), mean)



#in train data role attribute in string type which was compressed in to 1 column with 6 columns 
#then splitted  in to six columns 
library(stringr)
role<-str_split_fixed(as.character(train2$roles), ":",6)
role[role==""]<-"None"
train3<-cbind(train2,role)
names(train3)[42]<-paste("role6")
names(train3)[41]<-paste("role5")
names(train3)[40]<-paste("role4")
names(train3)[39]<-paste("role3")
names(train3)[38]<-paste("role2")
names(train3)[37]<-paste("role1")

train3$roles<-NULL
str(train3)


role<-str_split_fixed(as.character(test2$roles), ":",6)
role[role==""]<-"None"
test3<-cbind(test2,role)
names(test3)[40]<-paste("role6")
names(test3)[39]<-paste("role5")
names(test3)[38]<-paste("role4")
names(test3)[37]<-paste("role3")
names(test3)[36]<-paste("role2")
names(test3)[35]<-paste("role1")

test3$roles<-NULL
str(test3)




role<-str_split_fixed(as.character(validation$roles), ":",6)
role[role==""]<-"None"
validation<-cbind(validation,role)
names(validation)[42]<-paste("role6")
names(validation)[41]<-paste("role5")
names(validation)[40]<-paste("role4")
names(validation)[39]<-paste("role3")
names(validation)[38]<-paste("role2")
names(validation)[37]<-paste("role1")

validation$roles<-NULL
str(validation)





#sapply(train3,levels)



####removing the num_wins column column because it doesnt explain any thing for target variable and it doesnt consists in test1

train3$num_wins<-NULL
validation$num_wins<-NULL




train4<-train3
test4<-test3
validation1<-validation
train4$kda_ratio<-NULL
#validation1$roles<-NULL
validation1$kda_ratio<-NULL

library(dummies)
colnames(train4)

#binding the all data to reduce time for preprocessing
total<-rbind(train4,test4,validation1)
#total[,c(6,7,8,31,32,33)]<-NULL


#the dota heros consists of 9 roles 
#by using ifelse function i used to add 9 attributes withe roles of heroes
total$carry<-ifelse(total$role1=="Carry"|total$role2=="Carry"|total$role3=="Carry"|total$role4=="Carry"|total$role5=="Carry"|total$role6=="carry",1,0)
total$disabler<-ifelse(total$role1=="Disabler"|total$role2=="Disabler"|total$role3=="Disabler"|total$role4=="Disabler"|total$role5=="Disabler"|total$role6=="Disabler",1,0)
total$durable<-ifelse(total$role1=="Durable"|total$role2=="Durable"|total$role3=="Durable"|total$role4=="Durable"|total$role5=="Durable"|total$role6=="Durable",1,0)
total$escape<-ifelse(total$role1=="Escape" |total$role2=="Escape" |total$role3=="Escape" |total$role4=="Escape"|total$role5=="Escape"|total$role6=="Escape",1,0)
total$initiator<-ifelse(total$role1=="Initiator"|total$role2=="Initiator"|total$role3=="Initiator"|total$role4=="Initiator"|total$role5=="Initiator"|total$role6=="Initiator",1,0)
total$nuker<-ifelse(total$role1=="Nuker"|total$role2=="Nuker"|total$role3=="Nuker"|total$role4=="Nuker"|total$role5=="Nuker"|total$role6=="Nuker",1,0)
total$support<-ifelse(total$role1=="Support"|total$role2=="Support"|total$role3=="Support"|total$role4=="Support"|total$role5=="Support"|total$role6=="Support",1,0)
total$jungler<-ifelse(total$role1=="Jungler"|total$role2=="Jungler"|total$role3=="Jungler"|total$role4=="Jungler"|total$role5=="Jungler"|total$role6=="Jungler",1,0)
total$pusher<-ifelse(total$role1=="Pusher"|total$role2=="Pusher"|total$role3=="Pusher"|total$role4=="Pusher"|total$role5=="Pusher"|total$role6=="Pusher",1,0)

#removing the role columns 
total$role1<-NULL
total$role2<-NULL
total$role3<-NULL
total$role4<-NULL
total$role5<-NULL
total$role6<-NULL

#removing the unexplained columns 
total$id<-NULL
total$base_health<-NULL
total$base_mana<-NULL
total$base_mana_regen<-NULL
#train3$hero_id<-NULL
#total$role6<-NULL





cat_var<-select_if(total,is.factor)
num_var<-select_if(total,is.numeric)

max(total$user_id)


#i have tried to do clusters with respect to user and hero
usercluster <- kmeans(num_var,9) 
herocluster<-kmeans(num_var,9)
usercluster$cluster

num_var$usercluster<-usercluster$cluster
num_var$herocluster<-herocluster$cluster
#num_var$usercluster<-as.factor(num_var$usercluster)

total1<-cbind(num_var,cat_var)

#total1$cosine<-cos(total1$user_id*total1$hero_id*2*pi/250)

summary(total1)

train5<-total1[1:26928,]
test5<-total1[26929:27826,]
validation2<-total1[27827:29920,]



train5$kda_ratio<-train3$kda_ratio
validation2$kda_ratio<-validation$kda_ratio

#train5$user_id<-as.factor(train5$user_id)

summary(train5)

#calculating the mean of kda_ratio with respect to the clusters
user_cluster_ratio<-aggregate(train5$kda_ratio,list(train5$usercluster),mean)
names(user_cluster_ratio)[1]<-paste("usercluster")

hero_cluster_ratio<-aggregate(train5$kda_ratio,list(train5$herocluster),mean)
names(hero_cluster_ratio)[1]<-paste("herocluster")


train5<-train5%>%inner_join(user_cluster_ratio,c("usercluster"))
names(train5)[42]<-paste("user_cluster_ratio")



train5<-train5%>%inner_join(hero_cluster_ratio,c("herocluster"))
names(train5)[43]<-paste("hero_cluster_ratio")



test5<-test5%>%inner_join(user_cluster_ratio,c("usercluster"))
names(test5)[41]<-paste("user_cluster_ratio")



test5<-test5%>%inner_join(hero_cluster_ratio,c("herocluster"))
names(test5)[42]<-paste("hero_cluster_ratio")



validation2<-validation2%>%inner_join(user_cluster_ratio,c("usercluster"))
names(validation2)[42]<-paste("user_cluster_ratio")



validation2<-validation2%>%inner_join(hero_cluster_ratio,c("herocluster"))
names(validation2)[43]<-paste("hero_cluster_ratio")





#changing the outlier values with respect to the upperwhisker

mean.base_armor<-mean(train5$base_armor)
upperwhisker<-boxplot.stats(train5$base_armor)$stats[5]
train5$base_armor[train5$base_armor>upperwhisker]<-mean.base_armor



mean.base_armor<-mean(test5$base_armor)
upperwhisker<-boxplot.stats(test5$base_armor)$stats[5]
test5$base_armor[test5$base_armor>upperwhisker]<-mean.base_armor




mean.base_armor<-mean(validation2$base_armor)
upperwhisker<-boxplot.stats(validation2$base_armor)$stats[5]
validation2$base_armor[validation2$base_armor>upperwhisker]<-mean.base_armor



boxplot((train5$base_armor+3),horizontal = T)

#transforming the data
train5$base_armor<-train5$base_armor+3

test5$base_armor<-test5$base_armor+3

validation2$base_armor<-validation2$base_armor+3


#transforming the attack_rate by log
train5$attack_rate<-log(train5$attack_rate)
test5$attack_rate<-log(test5$attack_rate)
validation2$attack_rate<-log(validation2$attack_rate)




boxplot(train5$projectile_speed,horizontal = T)


#mean.projectile_speed<-mean(train5$projectile_speed)
#upperwhisker<-boxplot.stats(train5$projectile_speed)$stats[5]
#lowerwhisker<-boxplot.stats(train5$projectile_speed)$stats[1]
#train5$projectile_speed[train5$projectile_speed>upperwhisker]<-upperwhisker
#train5$projectile_speed[train5$projectile_speed<lowerwhisker]<-lowerwhisker

#target variable is not  normally distributed so sqrt transformation is used
train5$kda_ratio<-sqrt(train5$kda_ratio)


#linear regression model
lm1<-lm(kda_ratio~.,data=train5)
summary(lm1)
library(MASS)
#linear regression with significant variables
lm2<-lm(formula = kda_ratio ~ num_games + user_ratio + user_assists + 
          user_deaths + user_kills + base_health_regen + base_armor + 
          base_attack_max + base_strength + base_intelligence + strength_gain + 
          agility_gain + intelligence_gain + attack_range + projectile_speed + 
          attack_rate + turn_rate + herokda_ratio + hero_assists + 
          hero_deaths + carry + disabler + initiator + nuker + support + 
          jungler + pusher + herocluster + primary_attr + attack_type + 
          hero_cluster_ratio, data = train5)
summary(lm2)
stepAIC(lm1)
#predicting on validation
pred_lm2_validation<-predict(lm2,validation2)
pred_lm2_validation<-(pred_lm2_validation)^2
library(caret)
RMSE(pred_lm2_validation,validation$kda_ratio)



pred_lm1_validation<-predict(lm1,validation2)
pred_lm1_validation<-(pred_lm1_validation)^2
library(caret)
RMSE(pred_lm1_validation,validation$kda_ratio)



pred_lm1_test<-predict(lm1,test5)
pred_lm1_test<-(pred_lm1_test)^2



#randomforest optimization which automatically chooses the parameters 
res0 <- rf_opt(train_data = train5,
                               train_label = kda_ratio,
                               test_data = validation2,
                               test_label = kda_ratio,
                               mtry_range = c(1L, ncol(iris_train) - 1),
                               num_tree = 10L,
                               init_points = 10,
                               n_iter = 1)



library(mlr)

task <- makeRegrTask(data = train5, target = "kda_ratio")
res = hyperopt(task, learner = "regr.randomForest")




#quantile regression neural networks
control <- trainControl(method="repeatedcv", number=5, repeats=3)
model <- train(kda_ratio~., data=train5, method="qrnn", preProcess="scale", trControl=control)



#k nearest neighbours
noOfNeigh <- 1
pred=knn.reg(bankdata_trainwithoutclass, bankdata_train$Personal.Loan, k = noOfNeigh)
library(class)
(chemometrics)

dotadata_trainwithoutclass<-train5[,-c(1,2,40,39)]
dotadata_validationwithoutclass<-validation2[,-c(1,2,40,39)]
dotadata_testwithoutclass<-test5[,-c(1,2,39,38)]
#
#dotadata_trainwithoutclass = subset(dotadata_trainwithoutclass,select=-c(kda_ratio))
#dotadata_validationwithoutclass = subset(dotadata_validationwithoutclass,select=-c(kda_ratio))
#noOfNeigh <- 3
#pred=knn(dotadata_trainwithoutclass,dotadata_validationwithoutclass,train5$kda_ratio, k = noOfNeigh)








predictions= data.frame("id"= test1$id, "kda_ratio"= pred_lm1_test)

sum(is.na(predictions))
#names(predictions)[1]<-paste("id")
write.csv(predictions, "predictions2.csv", row.names = F)

kda_train<-train5
kda_test<-test5
kda_validation<-validation2

#converting user_id and hero_id in to factors
kda_train$user_id<-as.factor(kda_train$user_id)
kda_train$hero_id<-as.factor(kda_train$hero_id)



kda_test$user_id<-as.factor(kda_test$user_id)
kda_test$hero_id<-as.factor(kda_test$hero_id)


kda_validation$user_id<-as.factor(kda_validation$user_id)
kda_validation$hero_id<-as.factor(kda_validation$hero_id)



#--------------------------------------------
#h2o can reduces the time of model which will runs 
library(h2o)
h2o.init(nthreads = -1, max_mem_size = "3g")
train.hex <- as.h2o(x = dotadata_trainwithoutclass)
test.hex <- as.h2o(dotadata_testwithoutclass)
validation.hex<-as.h2o(dotadata_validationwithoutclass)



y = 'kda_ratio'
x = setdiff(names(train.hex), y)


nfolds <- 4
#deep learning 
set.seed(34)
model_ai<-h2o.deeplearning(
  activation = 'Rectifier',
  training_frame=train.hex,
  x=x, 
  y=y, 
  overwrite_with_best_model=F,    
  hidden=c(50,50),          
  epochs=500,                      
  score_validation_samples=10000, 
  score_duty_cycle=0.025,         
  adaptive_rate=F,                
  rate=0.02, 
  rate_annealing=2e-6,            
  momentum_start=0.4,             
  momentum_stable=0.4, 
  momentum_ramp=1e7, 
  l1=1e-5,
  max_w2=10                      
) 


pred_validation_ai = h2o.predict(model_ai, validation.hex) # Predict on h2o object

pred_validation_ai<-(pred_validation_ai)^2

RMSE(pred_validation_ai,validation.hex)




pred_test_gbm<-predict(model_ai,test.hex)
pred_test_gbm<-(pred_test_gbm)^2
#pred_train_class = as.data.frame(pred_train)
pred_test_gbm<-as.data.frame(pred_test_gbm)

predictions= data.frame("id"= test1$id, "kda_ratio"= pred_test_gbm)

sum(is.na(predictions))
names(predictions)[2]<-paste("kda_ratio")
names(predictions)[1]<-paste("id")
write.csv(predictions, "predictions2.csv", row.names = F)


#random forest in h20
my_rf <- h2o.randomForest(x = setdiff(names(train.hex), "kda_ratio"),y =  "kda_ratio",
                          training_frame = train.hex,
                          nfolds = 15,fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,seed = 1)





h2o.varimp(my_rf)

predict_rf_train.hex = h2o.predict(my_rf, newdata = train.hex[,setdiff(names(train.hex), "kda_ratio")])

predict_rf.hex = h2o.predict(my_rf, newdata = test.hex[,setdiff(names(test.hex), "kda_ratio")])
predict_rf_test<-as.data.frame(predict_rf.hex)

predict_rf_validation.hex = h2o.predict(my_rf, newdata = validation.hex[,setdiff(names(validation.hex), "kda_ratio")])
predict_rf_validation.hex<-(predict_rf_validation.hex)^2
pred_validation_rf<-as.data.frame(predict_rf_validation.hex)
RMSE(pred_validation_rf,validation$kda_ratio)

#rmse_dt1 <- sqrt(mean((predict_rf_validation.hex - train.hex$kda_ratio)^2))
#rmse_dt1


predictions= data.frame("id"= test1$id, "kda_ratio"= pred_test_gbm)

sum(is.na(predictions))
names(predictions)[2]<-paste("kda_ratio")
names(predictions)[1]<-paste("id")
write.csv(predictions, "predictions2.csv", row.names = F)
h2o.shutdown()
y


#final model is linear regression which gives the least error for dota2 data 

