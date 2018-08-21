# install xgboost package, see R-package in root folder
require(xgboost)
require(methods)
library(mlr)
###############################reading file#############################################################################

str(train_fina_2)
str(test_final)
train_final$damage_grade<-as.character(train_final$damage_grade)
train_final$damage_grade<-ifelse(train_final$damage_grade=="Grade 1",1,train_final$damage_grade)
train_final$damage_grade<-ifelse(train_final$damage_grade=="Grade 2",2,train_final$damage_grade)
train_final$damage_grade<-ifelse(train_final$damage_grade=="Grade 3",3,train_final$damage_grade)
train_final$damage_grade<-ifelse(train_final$damage_grade=="Grade 4",4,train_final$damage_grade)
train_final$damage_grade<-ifelse(train_final$damage_grade=="Grade 5",5,train_final$damage_grade)
write.csv(train_final,"train_new_feature.csv")
train_fina_2$has_repair_started<-as.factor(train_fina_2$has_repair_started)
train_final$count_families<-as.numeric(as.character(train_final$count_families))
train_final_xg<-createDummyFeatures(train_final[,!colnames(train_final) %in% c("damage_grade","building_id")])
test_final_xg<-createDummyFeatures(test_final[,!colnames(train_final) %in% c("damage_grade","building_id")])

test_final
train_final_xg<-createDummyFeatures(train_final[,colnames(train_final) %in% columns])


#train_final_xg<-createDummyFeatures(train_final[,!colnames(train_final) %in% c("building_id")])
#train_final_xg$damage_grade<-as.character(train_final$damage_grade)
sum(is.na(train_final))
sum(is.na(test_final))

write.csv(test_final,"test_building.csv")
write.csv(train_final,"train_building.csv")

train_final$has_repair_started<-NULL

test_final$count_families<-as.numeric(as.character(test_final$count_families))
train_final_xg$damage_grade<-ifelse(train_final_xg$damage_grade=="Grade 1",1,train_final_xg$damage_grade)
train_final_xg$damage_grade<-ifelse(train_final_xg$damage_grade=="Grade 2",2,train_final_xg$damage_grade)
train_final_xg$damage_grade<-ifelse(train_final_xg$damage_grade=="Grade 3",3,train_final_xg$damage_grade)
train_final_xg$damage_grade<-ifelse(train_final_xg$damage_grade=="Grade 4",4,train_final_xg$damage_grade)
train_final_xg$damage_grade<-ifelse(train_final_xg$damage_grade=="Grade 5",5,train_final_xg$damage_grade)

which( colnames(train_final_xg)=="damage_grade" )
###############################converting data to matrix#############################################################################
label <- as.numeric(train_final$damage_grade)
data <- as.matrix(train_final_xg)
#data1<-as.matrix(testing1[,-52])
data1<-as.matrix(test_final_xg)
#str(train_final_xg)
#model_weights <- ifelse(train$target == 1,(1/table(train_final$damage_grade)[2]) * 0.6*6451,(1/table(train$target)[1]) * 0.7*6451)
###########################################input maxit data to xgboost####################################################
xgmat <- xgb.DMatrix(data, label = label,missing = NA,silent = FALSE)
                    

###########################################Parameter set####################################################
set.seed(123)

numberOfClasses <- length(unique(label))
param<-list(objective = 'multi:softmax',
"Xgb.best1:eta" = 0.377,
num_class = 6,
#"Xgb.best1:max_depth" = 6,
"eval_metric" = "mlogloss",
"eval_metric" = "merror",
"eval_metric" = "ams@0.10",
"silent" = 1,
"nthread" = 16,
# nrounds=498,
#"max_depth"=15,
"lambda"=0.133, 
"lambda_bias"=0.432, 
"alpha"=0.17, 
"eta"= 0.377, 
#"subsample"= 0.905, 
"min_child_weight"= 3.08,
"colsample_bytree"= 0.647,
"gamma"=15)

param<-list(objective = 'multi:softmax',
            "Xgb.best1:eta" = 0.5,
            num_class = 6,
            "Xgb.best1:max_depth" = 7,
            "eval_metric" = "mlogloss",
            "eval_metric" = "ams@0.70",
            "silent" = 1,
            "nthread" = 16,
            # nrounds=498,
            "max_depth"=6,
            "lambda"=0.5, 
            "lambda_bias"=0.56, 
            "alpha"=0.6, 
            "subsample"= 0.905, 
            "min_child_weight"= 3.08,
            "colsample_bytree"= 0.9)


str(train_final_xg)
###########################################train the data###################################################


Xgb.best1 = xgb.train(param, xgmat,nrounds = 50,watchlist =list("train" = xgmat),verbose = TRUE,print_every_n = T,);

test_final$has_repair_started<-as.factor(test_final$has_repair_started)

test_final_xg<-createDummyFeatures(test_final)

str(test_final_xg)
y<-setdiff(colnames(train_final),colnames(test_final))

x<-test_final_xg[,colnames(test_final_xg) %in% column]

data1<-as.matrix(x)

column<-Xgb.best1$feature_names

pred = predict(Xgb.best1, newdata = data1)
pred_train = predict(Xgb.best1, newdata = data)
library(caret)
confusionMatrix(data = pred,reference = train_final_xg$damage_grade)
u1 <- union(pred_train,train_final$damage_grade)
t1 <- table(factor(pred_train, u1), factor(train_final$damage_grade, u1))
b<-confusionMatrix(t1)
b$overall
b$byClass

importance_matrix <- xgb.importance(model = Xgb.best1)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
columns<-importance_matrix[importance_matrix$Importance>0.0001253726,]$Feature

submission<-read.csv(file.choose())

submission$damage_grade<-pred

submission$damage_grade<-ifelse(submission$damage_grade==1,"Grade 1",submission$damage_grade)
submission$damage_grade<-ifelse(submission$damage_grade==2,"Grade 2",submission$damage_grade)
submission$damage_grade<-ifelse(submission$damage_grade==3,"Grade 3",submission$damage_grade)
submission$damage_grade<-ifelse(submission$damage_grade==4,"Grade 4",submission$damage_grade)
submission$damage_grade<-ifelse(submission$damage_grade==5,"Grade 5",submission$damage_grade)
setwd("C:\\Users\\Venkatasubramanian\\Desktop\\dataset\\Dataset")

write.csv(submission,"sample_submission_xgboost.csv",row.names = FALSE)


setwd("C:\\Users\\Venkatasubramanian\\Desktop\\try")
df <- data.frame(Id=test_ID,SalePrice=preds1)
write.csv(df,"submission.csv",row.names=FALSE)

testing1<-createDummyFeatures(testing)

