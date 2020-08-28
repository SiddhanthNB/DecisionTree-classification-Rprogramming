# Importing the dataset
dataset = read.csv('agaricus-lepiota.csv', header = FALSE)

# Giving arbitrary column names
colnames(dataset) <- c("Class","cap.shape","cap.surface","cap.color","bruises","odor","gill.attachment","gill.spacing",
                         "gill.size","gill.color","stalk.shape","stalk.root","stalk.surface.above.ring",
                         "stalk.surface.below.ring","stalk.color.above.ring","stalk.color.below.ring","veil.type","veil.color",
                         "ring.number","ring.type","print","population","habitat")
head(dataset)

# Define the factor names for "Class"
levels(dataset$Class) <- c("Edible","Poisonous")

# Define the factor names for "odor"
levels(dataset$odor) <- c("Almonds","Anise","Creosote","Fishy","Foul","Musty","None","Pungent","Spicy")
# Define the factor names for "print"
levels(dataset$print) <- c("Black","Brown","Buff","Chocolate","Green","Orange","Purple","White","Yellow")
head(dataset)

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Class, SplitRatio = 0.80)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting classifier to the Training set
library(rpart)
classifier = rpart(formula = Class ~ .,data = training_set, method = "class")
                   
# Predicting the Test set results
result <- predict(classifier, newdata = test_set[,-1],type = "class")

head(result)
head(test_set$Class) 
#in the output 'e' stands for edible and 'p' for poisonous.


# Making the Confusion Matrix
cm = base::table(test_set$Class, result)
cm 

# Visualizing confusion matrix
fourfoldplot(cm, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")


