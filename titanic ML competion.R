# titanic competition 
# On April 15, 1912, during her maiden voyage, the widely considered "unsinkable"
# RMS Titanic sank after colliding with an iceberg. Unfortunately, there weren't 
# enough lifeboats for everyone onboard, resulting in the death 
# of 1502 out of 2224 passengers and crew.

# Death - 1502, Alive - 722 (we want to predict)

# Data Dictionary
# survival -	Survival	0 = No, 1 = Yes
# pclass -	Ticket class	1 = 1st, 2 = 2nd, 3 = 3rd
# sex -	Sex	
# Age	- Age in years	
# sibsp	- # of siblings / spouses aboard the Titanic	
# parch	- # of parents / children aboard the Titanic	
# ticket -	Ticket number	
# fare	- Passenger fare	
# cabin	- Cabin number	
# embarked	- Port of Embarkation	C = Cherbourg, Q = Queenstown, S = Southampton


library(dplyr)
library(ggplot2)
library(cowplot)
library(rpart)
library(rpart.plot)
library(partykit)
library(randomForest)


# lets import the data first
train <- read.csv(file.choose(),sep = ",",header = T, na.strings = "")
test <- read.csv(file.choose(),sep = ",",header = T, na.strings = "")

str(train);str(test)

# lets combine the train and test, this should be the first step while working on any data 

combined <- bind_rows(train,test)
# this will create the survived col on its own in the test dataset
# and will coerce NA values on its own.

summary(combined)

# Lets dive into the EDA
# both univariate and multi
# hist of Age
combined %>% 
  ggplot(aes(Age))+geom_histogram(binwidth = 2,color="white") # the age is positively skewed

# hist between age and survived col ( target variable)
# in multivariate analysis we usually take or should take the target variable as one variable

combined %>% 
  ggplot(aes(Age,fill=factor(Survived)))+geom_histogram(binwidth = 2) 
# why fill is not working ?? because it should be factor
# lets take train for this plot as test data has NA and so it is showing in the plot

train %>% 
  ggplot(aes(Age,fill=factor(Survived)))+geom_histogram(binwidth = 2) 
# looks from 15-45 age category people survived more and its expected as they can jump and move 
# around easily right..!!! so we can see the data is real based

# gender vs survived

train %>% 
  ggplot(aes(Sex,fill=factor(Survived)))+geom_bar(stat = "count",position = "dodge") 

# female survived more than men - we can do the labling and all later

# gender vs age vs survived - hist()

train %>% 
  ggplot(aes(Age,fill=factor(Survived)))+geom_histogram(bins = 30,color="white")+
  facet_grid(.~Sex)+labs(x="Age",y="Count",title = "Sex vs Survived vs Age") +
  scale_fill_discrete(name="Survived")

# pclass vs survived vs gender

train %>% 
  ggplot(aes(Pclass,fill=factor(Survived)))+geom_bar(stat = "count",position = "dodge")+
  scale_fill_discrete(name="Survived")+facet_grid(.~Sex)

# we can use plot_grid to plot all these together to see the pattern more

# fare va pclass - geom_jitter as both are descrete col
train %>% 
  ggplot(aes(Fare,Pclass))+geom_jitter(aes(color=factor(Survived)))
# we can see that passenger of class 1 survived more whereas of class 3 died more

# make jitter plot for age vs sex vs pclass
train %>% 
  ggplot(aes(Age,Pclass))+geom_jitter()+facet_grid(.~Sex)

# to find the mode of age
mode_of_age <- table(train$Age)
mode_of_age
names(mode_of_age)[mode_of_age==max(mode_of_age)]

# so the mode is 24 yr
range(train$Fare)

# create a boxplot of age vs pclass
boxplot(train$Age~train$Pclass)

# Interpretation of boxplot

# create a hist of Fare...
hist(train$Fare,xlab = "Fare",ylab = "Frequency",main = "Fare per Person",col = "coral",
     breaks = 40,xlim = c(0,250))

# Feature Engineering ####
# in name we can extract the Name title

LastName <- sapply(strsplit(combined$Name,", "),head,1) # head is the function here
FirstName <- sapply(strsplit(combined$Name,", "),tail,1)
Titles <- sapply(strsplit(FirstName,". "),head,1)

# lets put titles in the combined data
combined <- mutate(combined,Titles)

# now lets convert the less frequent titles into others
sort(table(combined$Titles),decreasing = T)

combined$Titles[combined$Titles == "Ms"] <- "Miss"
combined$Titles[combined$Titles == "Mlle"] <- "Miss"
combined$Titles[combined$Titles == "Mme"] <- "Mrs"
combined[combined$Titles == "th","Titles"] <- "Mrs"

officer <- c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev')
royalty <- c('Dona', 'Lady', 'the Countess','Sir', 'Jonkheer')

combined$Titles[combined$Titles %in% royalty] <- "Royalty"
combined$Titles[combined$Titles %in% officer] <- "Officer"

# lets create a new variable family var
combined$family <- combined$SibSp+combined$Parch+1

# categories the family var. single, small, large
combined$family_Size[combined$family==1] <- "Single" # another way to make a new variable
combined$family_Size[combined$family>1 & combined$family<5] <- "Small"
combined$family_Size[combined$family>4] <- "Large"

table(combined$family_Size)

# lets make few more variables
# lets make a col for age_cat - Adult,Children
combined$Age_cat <- ifelse(combined$Age<18,"Children","Adult")

# lets make a col of mothers and others
combined$Mother <- ifelse(combined$Sex=="female" & combined$family>1 & combined$Age>18 & combined$Titles=="Mrs","Yes","No")


str(combined)
# so lets convert the chr to factors
combined[sapply(combined, is.character)] <- lapply(combined[sapply(combined, is.character)],
                                                   as.factor)
str(combined)

# Missing Value treatment ####

# Now to impute the age, since we are unable to find any pattern so we will predict the
# age basis other col
colnames(combined)
predict_age <- rpart(Age~Pclass+Sex+SibSp+Parch+Fare+Embarked+Titles,
                     data = combined[!is.na(combined$Age),],method = "anova")
plot(as.party(predict_age))
rpart.plot(predict_age,tweak = 1)

# predict the age of missing value
combined$Age[is.na(combined$Age)] <- predict(predict_age,
                                             newdata = combined[is.na(combined$Age),])
combined$Age <- round(combined$Age,2)
# missing values of fare
sapply(combined, function(x) sum(is.na(x)))

combined[!complete.cases(combined$Fare),]

mean(combined[combined$Pclass==3 & combined$Age >= 60,"Fare"],na.rm = T)
# lets check the median
median(combined[combined$Pclass==3 & combined$Age >= 60,"Fare"],na.rm = T)

combined[is.na(combined$Fare),"Fare"] <- median(combined[combined$Pclass==3 & combined$Age >= 60,"Fare"],
                                                na.rm = T)

# missing values in Embarked

combined[!complete.cases(combined$Embarked),]

# so what all col. we can take that will help us in predicting the embarked missing values

table(combined$Embarked[combined$Pclass==1]) # from where do most of pclass == 1 passengers board
# Southhampton with most pclass 1 

combined[is.na(combined$Embarked),"Embarked"] <- "S"

# imputation of cabin 
na.cabin <- combined[!complete.cases(combined$Cabin),]

# lets find out the cabin for class 1,2 and 3
which.max(table(combined$Cabin[combined$Pclass==1])) # location with max data

which.max(table(combined$Cabin[combined$Pclass==2])) # location with max data

which.max(table(combined$Cabin[combined$Pclass==3])) # location with max data

combined[is.na(combined$Cabin) & combined$Pclass==1,"Cabin"] <- "C23 C25 C27"
combined[is.na(combined$Cabin) & combined$Pclass==2,"Cabin"] <- "D"
combined[is.na(combined$Cabin) & combined$Pclass==3,"Cabin"] <- "G6"

# all the missing values have been dealt

# now split the combined data back into train and test

train_new <- combined[1:891,]
test_new <- combined[892:1309,]

test_new$Survived <- NULL # drop the target var. from the test data

# ML Algorithms ####
# decision tree
colnames(train_new)
titanic_tree <- rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Titles+family+family_Size,
                      data = train_new,method = "class")
library(partykit)
plot(as.party(titanic_tree))

my_predict <- predict(titanic_tree,newdata = test_new,type = "class")

ID <- test_new$PassengerId
solution <- data.frame(PassengerId=ID,Survived=my_predict)

write.csv(solution,file = "Submission.csv",row.names = F)

# kaggle pred score - 0.77

# randomForest 
library(randomForest)
# it works on factor data i.e the survived becomes factor(Survived)
# it is random so we need set.seed()


train_new$Pclass <- factor(train_new$Pclass)
test_new$Pclass <- factor(test_new$Pclass)


sed.seed(100)
titanic_rf <- randomForest(as.factor(Survived)~Pclass+Sex+Age+Fare+Embarked+Titles+
                             family_Size+Age_cat,
                           data = train_new,method = "rf",importance=T)

predict_rf <- predict(titanic_rf,newdata = test_new)

ID <- test_new$PassengerId
solution.rf <- data.frame(PassengerId=ID,Survived=predict_rf)

write.csv(solution.rf,file = "Submission.csv",row.names = F)

# kaggple score - 0.77



# Validating our model ####
train_trial <- train_new
train_trial$Survived <- NULL # remove the actual survived col from the trainig data

# apply the prediction on this data using the model we trained from the same data
train_predict <- predict(titanic_tree,train_trial,type = "class")

# preparation for submission
ID <- train_trial$PassengerId
solution <- data.frame(PassengerId=ID,Survived=train_predict)


# lets validate the info. whether we got a good accuracy or not -
library(caret)
DTCM <- confusionMatrix(train_predict,as.factor(train_new$Survived)) #original

library(scales)
DTCM

percent(as.numeric(DTCM$overall[1])) #this will give the accuracy 

# randomForest validation - works with int,num,FACTORS
train_predict.rf <- predict(titanic_rf,train_trial,type = "response")

RFCM <- confusionMatrix(train_predict.rf,as.factor(train_new$Survived))
RFCM
percent(as.numeric(RFCM$overall[1])) #this will give the accuracy 





# preparing data for pca i.e making the factor variable to numeric dummy variables
# first lets remove those col which don't have any patterns like name ticket cabin
colnames(combined)
combined <- combined[,-c(4,9,11)]

# PCA - dimension reduction technique
# lets remove the ID and Survived col
combined2 <- combined[,-c(1,2)]

# lets convert the factor col into OHE
install.packages("dummies")
library(dummies)
new_combined_data <- dummy.data.frame(combined2,
                                      names = names(which(sapply(combined,function(x) is.factor(x)))),
                                      sep = "_")
# now we divide the data into train and test
pca.train <- new_combined_data[1:nrow(train_new),]
pca.test <- new_combined_data[-(1:nrow(train_new)),]

# time to apply pca 
princ_comp <- prcomp(pca.train,scale. = T)
princ_comp$center

# center and scale refers to the respective mean and stdev of the variables that are used for normalisation 
# prior to implementing PCA
round(princ_comp$center,1)
round(princ_comp$scale,2)

# lets find the components that explain the max information
stdev <- princ_comp$sdev
pr_var <- stdev^2
round(pr_var,2)

# lets find the proportion 
prop_varex <- pr_var/sum(pr_var)
round(prop_varex,2)

# lets plot and check how many can tell the var
plot(prop_varex,xlab = "Principal Components",ylab = "prop of var",type = "b")

plot(cumsum(prop_varex),xlab = "Principal Components",ylab = "prop of var",type = "b")

# around 15 components will explain around 95% of the data

# predictive modeling
# prepare traing data - target var
train.data <- data.frame(Survived=train_new$Survived,princ_comp$x[,c(1:16)])


# lets transform the test in pca
test.data <- predict(princ_comp,newdata = pca.test)
# prin_comp helped to predict all the PCAs
test.data <- as.data.frame(test.data)

# apply decision tree
rpart.model <- rpart(Survived~.,train.data)

# lets make prediction 
rpart.pred <- predict(rpart.model,newdata = test.data)
rpart.pred <- ifelse(rpart.pred<0.50,0,1)

solution.pca <- data.frame(PassengerId=test_new$PassengerId,Survived=rpart.pred)
write.csv(solution,file = "Submission.csv",row.names = F)

table(solution$Survived)
table(solution.pca$Survived)

# lets use random forest
tree_rf <- randomForest(as.factor(Survived)~.,train.data,method = "rf",importance=T)
predict.rf.pca <- predict(tree_rf,newdata = test.data)

table(predict.rf.pca)

solution.pca <- data.frame(PassengerId=test_new$PassengerId,Survived=predict.rf.pca)
write.csv(solution,file = "Submission.csv",row.names = F)

