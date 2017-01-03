# import training and testing data sets
train = read.csv("train.csv")
test = read.csv("test.csv")

# combine both data sets so all feature engineering will be done on both on one go
test$Survived = rep('none', nrow(test))
data = rbind(train, test)
str(data)

# change survived and class into factors
data$Survived = as.factor(data$Survived)
data$Pclass = as.factor(data$Pclass)
train$Survived = as.factor(train$Survived)
train$Pclass = as.factor(train$Pclass)

table(train$Survived)
# 342/891 did not survive (38.3%)
table(train$Pclass)
# 1st class: 216 
# 2nd class: 184
# 3rd class: 491

library(ggplot2)

ggplot(train, aes(x=Pclass, fill=Survived))+
  geom_bar()+
  xlab("Passenger Class")+
  ylab("Survival count")+
  ggtitle("Passenger Class vs Survivability")+
  labs(fill="Survived")

# see what names actually looks like
head(data$Name)

length(unique(as.character(data$Name)))
# 1307 unique names from 1309 passengers -> 2 duplicates

# confirm duplicated names are different passengers
duped.names = as.character(data$Name[which(duplicated(as.character(data$Name)))])
duped.names
data[data$Name %in% duped.names,]

# function to extract the salutation from each passengers name
library(stringr)
fcn.salutation = function(name){
  if (str_detect(name, 'Mrs.') == TRUE){
    return ('Mrs.') 
  }
  else if (str_detect(name, 'Mr.') == TRUE){
    return ('Mr.') 
  }
  else if (str_detect(name, 'Miss.') == TRUE){
    return ('Miss.')
  }
  else if (str_detect(name, 'Master.') == TRUE){
    return ('Master.')
  }
  else { return ('Other.')}
}

# add a salutation feature to the dataframe as a factor
salutation = numeric(nrow(data))
for (i in 1:nrow(data)){
  salutation[i] = fcn.salutation(as.character(data$Name[i]))
}

data$salutation = as.factor(salutation)

# plot class+salutation vs survivability
ggplot(data[1:891,], aes(x=salutation, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  xlab("salutation")+
  ylab("Survival count")+
  ggtitle("Passenger class + salutation vs surivability")

# feature returning the size of each passengers family (and themselves)
data$family.count = as.factor(c(data$SibSp + data$Parch + 1))

ggplot(data[1:891,], aes(x=family.count, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+salutation)+
  xlab("family size")+
  ylab("survival count")+
  ggtitle("Passenger class + family size + saluation vs survivability")


# data analysis on "Other." variable in the salutation feature
# function that returns all salutations 
fcn.other = function(name){
  full.name = unlist(str_split(name, ","))
  last.name = full.name[seq(2,length(full.name),2)]
  new.salutation = sapply(str_split(last.name, " "), "[", 2)
  return (new.salutation)
}

new.salutation = fcn.other(data$Name)
unique(new.salutation)
# the, Dona, Lady, Mlle, Ms, Mme, Jonkheer, Don, Sir, Dr, Rev, Col, Capt, Major


data[which(new.salutation %in% c('the', 'Dona.', 'Lady.')),] # Mrs.
data[which(new.salutation %in% c('Mlle.', 'Ms.', 'Mme.')),] # Mrs./Miss
data[which(new.salutation %in% c('Jonkheer.', 'Don.', 'Sir.')),] # Mr.
data[which(new.salutation %in% c('Dr.')),] # Mr.
data[which(new.salutation %in% c('Rev.')),] # Rev.
data[which(new.salutation %in% c('Col.', 'Capt.', 'Major.')),] # Officer

# change variables replaced by "Other." to generalize better for our model
new.salutation[new.salutation %in%
                 c('Mme.', 'Mlle.', 'the', 'Dona.', 'Lady.', 'Ms.')] = 'Mrs.'
#new.salutation[new.salutation %in% 'Ms.'] = 'Miss.'
new.salutation[new.salutation %in% c('Jonkheer.', 'Don.', 'Sir.')] = 'Mr.'
new.salutation[new.salutation %in% c('Col.', 'Capt.','Major.')] = 'Officer.'

# add our new saluation feature to the dataframe as a factor
data$new.salutation = as.factor(new.salutation)

# Random forest model with 100 trees
library(randomForest)
# fit with class, family size and the new salutation feature
feature.model = c('Pclass', 'family.count', 'new.salutation')
input = data[1:891, feature.model]
output = train$Survived


model = randomForest(x=input, y=output, ntree = 100, importance = TRUE)
model
# 16.84% OOB

# predict the passengers from the test data set from our new trained model
target = data[892:1309, feature.model]
pred = predict(model, target)

rf.submission = data.frame(PassengerId=rep(892:1309), Survived=pred)
write.csv(rf.submission, "rf_submission_12-29-16",row.names = FALSE)
