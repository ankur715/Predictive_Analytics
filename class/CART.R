# Ankur Patel
# DS 640 - Predictive Analytics

# Prepare the data
risk = read.csv("ClassifyRisk.txt",
                stringsAsFactors=FALSE, header=TRUE, sep=",")
choose = runif(dim(risk)[1], 0, 1)
train = risk [which(choose <= 0.75),]
test = risk [which(choose > 0.75),]
View(test)

# Original CART model for predicting risk
library(rpart)
cart.o = rpart(formula = risk ~ marital_status+mortgage+loans+income+age,
               data = train,
               method = "class")
p.0 = predict(cart.o, newdata = test)
pred1 = ifelse(p.0[,1] > p.0[,2], "Pred: bad loss", "Pred: good risk")
o.t = table(pred1, test$risk)

# Bagging model (5 base models)
s1 = train[sample(dim(train)[1], replace = TRUE),]
s2 <- train[sample(dim(train)[1], replace = TRUE),]
s3 <- train[sample(dim(train)[1], replace = TRUE),]
s4 <- train[sample(dim(train)[1], replace = TRUE),]
s5 <- train[sample(dim(train)[1], replace = TRUE),]

#CART
cart1 = rpart(risk ~ marital_status+mortgage+loans+income+age,
              data = s1, method = "class")
p1 = predict(cart1, newdata = test)
pred1 = ifelse(p1[,1] > p1[,2], "Pred: bad loss", "Pred: good risk")
#########
cart2 = rpart(risk ~ marital_status+mortgage+loans+income+age,
              data = s2, method = "class")
p2 = predict(cart2, newdata = test)
pred2 = ifelse(p2[,1] > p2[,2], "Pred: bad loss", "Pred: good risk")
#########
cart3 = rpart(risk ~ marital_status+mortgage+loans+income+age,
              data = s3, method = "class")
p3 = predict(cart3, newdata = test)
pred3 = ifelse(p3[,1] > p3[,2], "Pred: bad loss", "Pred: good risk")
#########
cart4 = rpart(risk ~ marital_status+mortgage+loans+income+age,
              data = s4, method = "class")
p4 = predict(cart4, newdata = test)
pred4 = ifelse(p4[,1] > p4[,2], "Pred: bad loss", "Pred: good risk")
#########
cart5 = rpart(risk ~ marital_status+mortgage+loans+income+age,
              data = s5, method = "class")
p5 = predict(cart5, newdata = test)
pred5 = ifelse(p5[,1] > p5[,2], "Pred: bad loss", "Pred: good risk")
#########

#predict
preds = c(pred1, pred2, pred3, pred4, pred5)
recs = as.integer(names(preds)); fin.pred = rep(0, dim(test)[1])
for(i in 1:dim(test)[1]){
  t = table(preds[which(recs==as.integer(rownames(test))[i])])
  fin.pred[i] = names(t)[t == max(t)]
}
bag.t = table(fin.pred, test$risk) # Contingency table

# Boosting model (5 iterations)
cart6 = rpart(risk ~ marital_status+mortgage+loans+income+age,
              data = train, method = "class")
p6 = predict(cart6, newdata = train)
pred6 = ifelse(p6[,1] > p6[,2], "bad loss", "good risk")
moreweight = train$risk != pred6
new.weights = ifelse(moreweight==TRUE, 2, 1)
#########
cart7 = rpart(risk ~ marital_status+mortgage+loans+income+age,
              data = train, method = "class")
p7 = predict(cart7, newdata = train)
pred7 = ifelse(p7[,1] > p7[,2], "bad loss", "good risk")
moreweight = train$risk != pred7
new.weights = ifelse(moreweight==TRUE, 2, 1)
#########

cart10 = rpart(risk ~ marital_status+mortgage+loans+income+age,
               weights = new.weights, data = train, method = "class")
p10 = predict(cart10, newdata = test)
pred10 = ifelse(p10[,1] > p10[,2], "Pred: bad loss", "Pred: good risk")
boost.t = table(pred10, test$risk) # Contingency table

# Compare models
# Compare contingency tables
o.t
bag.t
boost.t
# Compare errors
(o.t[2]+o.t[3])/ sum(o.t)
(bag.t[2]+bag.t[3])/ sum(bag.t)
(boost.t[2]+boost.t[3])/ sum(boost.t)

###############################
#Output and Conclusions
# After the Original CART model ran for predicting risk using factors of arital_status+mortgage+loans+income+age, 
# the confusion matrix was created for target values of bad loss and good risk. Initially, 5 Bagging base models were created
# and the recursive partitioning and regression trees function was run on the factors listed earlier with the Bagging models. After 
# Bagging, the model showed better results than Original CART since it improved the prediction for good risk. Then, a Boosting model of 5 iteration
# was used to improve the model predictions by iterating to correct its predecessor. It gave 100% TP for good risk!

# Below are the output results for that conclusion.
