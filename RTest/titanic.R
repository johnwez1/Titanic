#Titanic Rtest file, apologies that code may be a little messy, I just went with the syntax that worked for me since I haven't used R in a while. 

#Loadup rpart
library(rpart)

# load file
mydata = read.csv("titanic.csv")

#split into 70% training and 30% test
cut = 0.7*length(mydata[,1])
cut = round(cut)
train = mydata[1:cut,]
test = mydata[(cut+1):(length(mydata[,1])),]

#at this point we can transform the data if we wish. However, we first seek a simple model as we do not want things overcomplicated. Intuitively we could reject factors that do not effect survival, say the name (although there is possibly subtle features we could extract).

#rpart "class" method ensures continuous and categorical data is handled appropriately. Pclass will be treated as a number, even though it is really a category, I am fine with this because the categories represent financial status: 1st > 2nd > 3rd.

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

summary(fit)

#now apply to the test set
testpredict <- predict(fit, test, type = "class")
#confusion matrix:
trueanswer = test$Survived
cm = table(trueanswer, testpredict)

correct = cm[1, 1] + cm[2, 2]
incorrect = cm[1, 2] + cm[2, 1]

print("Correct: ")
print(correct)
print("Incorrect: ")
print(incorrect)

#finally, apply to the prediction set and save the submission
predict = read.csv("predict.csv")
submission <- predict(fit, predict, type = "class")
submit <- data.frame(PassengerId = predict$PassengerId, Survived = submission)
write.csv(submit, file = "submission.csv", row.names = FALSE)
