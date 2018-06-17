library(dplyr)
input <-read.csv('Medical_Expenditure_Panel_Survey__MEPS__Prescribed_Medicines_File_-2015.csv')
names(input)
as.dataframe(input %>% group_by(PERSON.ID..DUID...PID.) %>%
  summarise(num_drugs = n()) %>%
  arrange(-num_drugs))
#Train and test split
## 75% of the sample size
smp_size <- floor(0.75 * nrow(input))
set.seed(123)
train_ind <- sample(seq_len(nrow(input)), size = smp_size)
train <- input[train_ind, ]
test <- input[-train_ind, ]

#Opioid drug table merge
opioid_drugs <- read.csv('Opioid_Prescriptions.csv')
names(opioid_drug_names)
opioid_drug_names <- as.data.frame(unique(opioid_drugs$drugname))
write.csv(unique(opioid_drugs$drugname),file = 'opioid_drug_names.csv')
foo <- data.frame(do.call('rbind', strsplit(as.character(opioid_drug_names$`unique(opioid_drugs$drugname)`),' ',fixed=TRUE)))
foo <- data.frame(do.call('rbind', strsplit(as.character(foo$X1),'-',fixed=TRUE)))
unique(foo$X1)

foo2 <- data.frame(do.call('rbind', strsplit(as.character(input$MEDICINE.NAME..IMPUTED.),' ',fixed=TRUE)))
input$AnyDrug <- input$MEDICINE.NAME..IMPUTED. %in% unique(foo$X1)
table(input$AnyDrug)
head(foo2)
foo2$AnyDrug <-FALSE
foo2$AnyDrug[which(foo2$X1 %in% unique(foo$X1) | 
                     (foo2$X2 %in% unique(foo$X1)) | 
                     (foo2$X3 %in% unique(foo$X1)) |
                     (foo2$X4 %in% unique(foo$X1)) |
                     (foo2$X5 %in% unique(foo$X1)) |
                     (foo2$X6 %in% unique(foo$X1)) |
                     (foo2$X7 %in% unique(foo$X1)) |
                     (foo2$X8 %in% unique(foo$X1)) |
                     (foo2$X9 %in% unique(foo$X1)) |
                     (foo2$X10 %in% unique(foo$X1)) |
                     (foo2$X11 %in% unique(foo$X1)) 
                     ), ]



