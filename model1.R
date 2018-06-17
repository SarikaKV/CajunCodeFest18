library(rpart)
library(glm2)
library(DMwR)
library(caret)
setwd('D:/CajunCodeFest18/Datasets/')
deaths <- read.csv('Opioid_Demographics.csv')
model1_data <- read.csv('Patient_Characteristics_Survey__PCS___2013.csv')
newData <- SMOTE(Drug.Substance.Related.Disorder ~ ., model1_data, perc.over = 600,perc.under=100)
table(newData$Drug.Substance.Related.Disorder)

dataset2_mod <- read.csv('MEPS_Medical_Conditions_-_2015.csv')
merge(dataset1_mod, dataset2_mod, by = "PERSON.ID..DUID...PID.")
dataset1_mod <- read.csv('updated_Medical_Expenditure_Panel_Survey__MEPS__Prescribed_Medicines_File_-2015.csv')
cccode<- read.csv('CCCode.csv')
names(dataset1_mod)
write.csv(dataset1_mod,file = 'updated_dataset1.csv')
min(dataset2_mod$PERSON.ID..DUID...PID.)
min(dataset1_mod$PERSON.ID..DUID...PID.)
model1_data$Principal.Diagnosis.Class
sort(unique(dataset1_mod$MODIFIED.CLINICAL.CLASS.CODE))
sort(unique(dataset1_mod$MODIFIED.CLINICAL.CLASS.CODE.1))
sort(unique(dataset1_mod$MODIFIED.CLINICAL.CLASS.CODE.2))
dataset1_mod$Drug_disorder <- "NO"
dataset1_mod$Drug_disorder[(dataset1_mod$MODIFIED.CLINICAL.CLASS.CODE == 661) | 
               (dataset1_mod$MODIFIED.CLINICAL.CLASS.CODE.1 == 661) |
               (dataset1_mod$MODIFIED.CLINICAL.CLASS.CODE.2 == 661),] <- "YES"
table(dataset1_mod$Drug_disorder)
merged <-merge(dataset1_mod, cccode, by.y = "VALUE",by.x ="MODIFIED.CLINICAL.CLASS.CODE")
unique(merged$LABEL)
model1_data <- model1_data[model1_data$Drug.Substance.Related.Disorder != "UNKNOWN",]
table(model1_data$Drug.Substance.Related.Disorder)
#Remove unwanted columns
drops <- c("Sno","Survey.Year","SSI.Cash.Assistance","No.Chronic.Med..Condition",
           "Endocrine.Condition","Kidney.Disease")
model1_data <-model1_data[ , !(names(model1_data) %in% drops)]
nrow(model1_data)
cor(model1_data)
names(dataset2_mod)
model1_data$Medicaid.Insurance
#Train and test split
## 75% of the sample size
smp_size <- floor(0.75 * nrow(model1_data))
set.seed(123)
train_ind <- sample(seq_len(nrow(model1_data)), size = smp_size)
train <- model1_data[train_ind, ]
test <- model1_data[-train_ind, ]
#Decision tree
fit <- rpart(Drug.Substance.Related.Disorder ~ .,
             data=train,
             method="class")
plot(fit)
text(fit)

#Logistic Regression
model <- glm(Drug.Substance.Related.Disorder ~.,family=binomial(link='logit'),data=model1_data)
summary(model)
anova(model,test="Chisq")

fitted.results <- predict(model,newdata=subset(test,type='response'))
fitted.results <- ifelse(fitted.results > 0.5,"YES","NO")

misClasificError <- mean(fitted.results != test$Drug.Substance.Related.Disorder)
print(paste('Accuracy',1-misClasificError))

