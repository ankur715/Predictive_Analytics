df = read.csv(file.choose())
df
summary(df)
df = df[3:5]
df$Purchased = factor(df$Purchased, levels = c(0,1))
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(df$Purchased, SplitRatio = 0.75)
training_set = subset(df, split==TRUE)
test_set = subset(df, split==FALSE)
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])
classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set)



prob_pred = predict(classifier, type='response', newdata=test_set[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)
cm = table(test_set[,3], y_pred>0.5)
cm
library(class)
y_pred_knn = knn(train = training_set[,-3],
                 test = test_set[,-3],
                 cl=training_set[,3],
                 k=5,
                 prob=TRUE)
cm_knn = table(test_set[,3], y_pred_knn)
cm_knn

feature scaling
fitting logistic, knn training
predicting test
making confusion matrix
