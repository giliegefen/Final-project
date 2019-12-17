library(readr)
library(glmnet)
library(dplyr)
library(janitor)
library(ggplot2)
library(ggpubr)
library(fitdistrplus)
library(nortest)

###############chi2#################

data <- read_csv("Desktop/PRO/edu_sal_trans.csv")
data <- filter(data, data$`Q11_Page Submit` >= 0 & paying_amount>=-200 & paying_amount<=200)
#data <- filter(data, data$paying_amount!=0)

data <- data[is.na(data$paying_amount)]
data <- remove_empty(data,which = c("cols"))
uniquelength <- sapply(data,function(x) length(unique(x)))
data <- subset(data, select=uniquelength>1)

data$paying_amount <- data$paying_amount+1000
x<-(data$paying_amount)

View(x)
myFit<-fitdist(x,"norm") # fitting a normal distrbution 
poisgof <-gofstat(myFit, discrete=TRUE)%>%print()
#poisgof1 <-gofstat(myFit, discrete=FALSE)%>%print()
#x<-data$bigMac
#myFit<-fitdist(x,"norm")
#poisgof <-gofstat(myFit, discrete=FALSE)%>%print()

#data$rank <- NA
#data$rank<- rank(data$paying_amount, ties.method = c("average"))
#x<-as.integer(data$rank)
#myFit<-fitdist(x,"norm")
#poisgof <-gofstat(myFit, discrete=FALSE)%>%print()




data$log <- NA
data$log1 <- NA
data$log2 <- NA

data$log1<- log(-data$user_pay_no0,2)
data$log2<- log(data$comp_pay_no0,2)
data$log1[data$log1 <= 0] <- 0
data$log2[data$log2 <= 0] <- 0
data$log1 <- (-data$log1)



paste_noNA <- function(x,sep=", ") {
  gsub(", " ,sep, toString(x[!is.na(x) & x!="" & x!="NA"] ) ) }

sep=" "
data$log <- apply( data[ , c(23:24) ] , 1 , paste_noNA , sep=sep)

#data$log <- paste(data$log1,data$log2)
#data$log[data$log <= 0] <- 0

x<-as.double(data$log)
View(x)
myFit<-fitdist(x,"norm")
poisgof <-gofstat(myFit, discrete=TRUE)%>%print()


data$rank <- NA
data$rank<- rank(data$paying_amount_no0, ties.method = c("average"))
x<-as.integer(data$rank)
myFit<-fitdist(x,"norm")
poisgof <-gofstat(myFit, discrete=TRUE)%>%print()


########################hist###########################

data <- read_csv("Desktop/PRO/edu_sal_trans.csv")
data <- filter(data, data$`Q11_Page Submit` >= 0 & paying_amount>=-200 & paying_amount<=200)
data <- filter(data, data$paying_amount_no0!=0)


data <- data[is.na(data$paying_amount_no0)]
data <- remove_empty(data,which = c("cols"))
uniquelength <- sapply(data,function(x) length(unique(x)))
data <- subset(data, select=uniquelength>1)
data$pay <- "neg"
data$pay[is.na(data$user_pay_no0)] <- "pos"

x_mean=mean(data$paying_amount_no0)
x_median=median(data$paying_amount_no0)
x_sd=sqrt(var(data$paying_amount_no0))
a <- ggplot(data, aes(x = paying_amount_no0))

a + geom_histogram(aes(color = pay, fill = pay, y = ..density..,),
                   alpha = 0.5, position = "identity") +
  geom_density()+
  stat_function(fun = function(x) {dnorm(x, x_mean, x_sd)}, color = "red") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

d<-rnorm(data$paying_amount_no0, mean(data$paying_amount_no0), sqrt(var(data$paying_amount_no0)))
y<- data$paying_amount_no0
qqnorm(y)
qqline(y, col = 2)
qqplot(y, rt(300, df = 5))

qqnorm(data$paying_amount_no0, pch = 1, frame = FALSE)
qqline(data$paying_amount_no0, col = "steelblue", lwd = 2)

boxplot(data$paying_amount,d, horizontal=TRUE, main="paying amount")
x<-data$paying_amount
p<-dnorm(data$paying_amount, mean(data$paying_amount), sqrt(var(data$paying_amount)))
res<-chisq.test(x, p)
res

#################################logistic reg##################
data <- read_csv("Desktop/PRO/edu_sal_trans.csv")
data <- filter(data, data$`Q11_Page Submit` >= 15 & paying_amount>=-750 & paying_amount<=750)
data <- data[is.na(data$paying_amount) | is.na(data$user_pay),]
data$paying_amount <-cut(data$paying_amount, c(0,5.5,751), right=FALSE, labels=c(0:1))
#data$bigMac <-cut(-data$bigMac, c(0,5,751), right=FALSE, labels=c(0:1))
#data <- filter(data, paying_amount!=0)

data <- remove_empty(data,which = c("cols"))
uniquelength <- sapply(data,function(x) length(unique(x)))
data <- subset(data, select=uniquelength>1)
#x <- data[c(2,4,9,10,11)]
x <- data[c(2,4,9,10,11)]

y<-data$paying_amount
model <- glm(y~., data = x, family = "binomial")
summary(model)
sink("Desktop/PRO/summary_logistic.txt")
print(summary(model))
sink()


###################### 3 col model###################
data <- read_csv("Desktop/PRO/edu_sal_trans.csv")
#data <- read_csv("Desktop/PRO/nozeros.csv")
data <- filter(data, data$`Q11_Page Submit` >= 15 & paying_amount>=-750 & paying_amount<=750)
data <- data[is.na(data$paying_amount)]
#data$paying_amount<-cut(data$paying_amount, c(1,-2,-750), right=FALSE, labels=c(1:0))
#View(data) #read and view file

#data <- data[!(is.na(data$user_pay_no0)),]
#data <- filter(data, user_pay_no0!=0)

data <- data[!(is.na(data$comp_pay_no0)),]
data <- filter(data, comp_pay_no0!=0)

data <- remove_empty(data,which = c("cols"))
uniquelength <- sapply(data,function(x) length(unique(x)))
data <- subset(data, select=uniquelength>1)

x <- data[c(1,2,3,4,7,8,9,10,11,16)]
y<-data$paying_amount_no0
model <- lm(y~., data = x)
summary(model)
sink("Desktop/PRO/summary_3col.txt")
print(summary(model))
sink()

#########################linear reg####################
data <- read_csv("Desktop/PRO/out.csv")
names(data)[4]<-paste("Q11_Page_Submit")
data <- filter(data, Q11_Page_Submit >= 15 & paying_amount>=-750 & paying_amount<=750)
data <- data[is.na(data$paying_amount)]
data$md_condition <- as.integer(data$`md condition in past year`|data$`md condition now`)
data$`Q8_More than 60,000$` <- as.integer(data$`Q8_60,000$-75,000$`|data$`Q8_75,000$-90,000$`|data$`Q8_90,000$-115,000$`|data$`Q8_More than 115,000$`)

data <- data[!(is.na(data$user_pay)),]
data <- filter(data, user_pay!=0)

data <- remove_empty(data,which = c("cols"))
uniquelength <- sapply(data,function(x) length(unique(x)))
data <- subset(data, select=uniquelength>1)
#x <- data[c(-2, -3,-4,-9, -10,-12:-22)]
x <- data[c(-2, -3,-4,-9, -10,-12:-22,-32,-33)]
#x <- data[c(-2, -3,-4,-9, -10)]




#x <- data[c(-2, -3,-4,-9, -10,-11,-14:-24,-34:-36,-38)]
#x <- data[c(-2, -3,-4,-9, -10,-11,-14:-24)]
#x <- data[c(-2, -3,-4,-9, -10,-11)]


y<-data$user_pay
model <- lm(y~., data = x)
summary(model)
#sink("Desktop/PRO/summary_linear.txt")
#print(summary(model))
#sink()


#################multi ques##################

data <- read_csv("Desktop/PRO/edu_sal_rand.csv")

data <- remove_empty(data,which = c("cols"))
uniquelength <- sapply(data,function(x) length(unique(x)))
data <- subset(data, select=uniquelength>1)

set.seed(100)
trainingRows <- sample(1:nrow(data), 0.75*nrow(data))
training <- data[trainingRows, ]
test <- data[-trainingRows, ]

train_x <- training[c(1,2,3,4,7,8,9,10,11,16)]
test_x <- test[c(1,2,3,4,7,8,9,10,11,16)]

train_y<-training$question2
test_y<-test$question2

multinomModel<-multinom(train_y~.,data=train_x)
predicted_class <- predict (multinomModel, test_x)
table(predicted_class, test_y)
mean(as.character(predicted_class) != as.character(test_y))

###################stepwise################

data <- read_csv("Desktop/PRO/edu_sal.csv")
data <- filter(data, data$`Q11_Page Submit` >= 15 & paying_amount>=-750 & paying_amount<=750)
data <- data[is.na(data$paying_amount)]
data <- data[!(is.na(data$comp_pay)),]
data <- filter(data, comp_pay!=0)



data <- remove_empty(data,which = c("cols"))
uniquelength <- sapply(data,function(x) length(unique(x)))
data <- subset(data, select=uniquelength>1)

data$log <- NA
data$log<- log(data$comp_pay,2)
data$log[data$log <= 0] <- 0

data$rank <- NA
data$rank<- rank(data$paying_amount, ties.method = c("min"))
x<-as.integer(data$rank)

x <- data[c(1,2,3,4,7,8,9,10,11,16)]
y<-data$paying_amount
#model <- lm(y~., data = x)
#summary(model)

step(lm(y~., data = x),direction="backward")


