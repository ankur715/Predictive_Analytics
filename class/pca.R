df = read.csv("Wine.csv")
library(caTools)
set.seed((123))
split = sample.split(df$Customer_Segment, SplitRatio=0.8)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)
training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])
df
library(caret)
library(e1071)
pca = preProcess(x = training_set[-14], method="pca", pcaComp=2)
training_set = predict(pca, training_set)
training_set = training_set[c(2,3,1)]
test_set = predict(pca, test_set)
test_set = test_set[c(2,3,1)]
classifier = svm(formula=Customer_Segment~.,
                 data=training_set,
                 type="C-classification",
                 kernel="linear")
y_pred = predict(classifier, newdata=test_set[-3])
cm = table(test_set[,3], y_pred)
cm
