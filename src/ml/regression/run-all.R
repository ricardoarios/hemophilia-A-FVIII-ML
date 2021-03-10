###testing all methods 
rm(list=ls())

#load setup
source("src/preprocessing/load-data-reg.R")
source("src/ml/regression/rf.R")
source("src/ml/regression/svr.R")
source("src/ml/regression/glmnet.R")

# using k = 1 just as a proof of concept
# It means a test fold was created by randomly selecting 10% of instances
# Even using numeric labels, we designed a function to estratify them.
# The reminaing 90% of instances were considered to adjust the models.
# Such an adjust was obtained using 10-fold cross validation.
k=1

#### creating train/test data
cv.test<-sort(cv.10[[k]])
cv.train<-sort(unlist(cv.10))
cv.train<-cv.train[which(!(cv.train %in% cv.test))]

train<-dataHemophilia[cv.train, ] %>% droplevels()
train.task <- mlr::makeRegrTask(data = train, target = "group")

test<-dataHemophilia[cv.test, ] %>% droplevels()
test.task <- mlr::makeRegrTask(data = test, target = "group")

#### end train/test creation

########Testing GLMnet############

output<-glm.reg(train.task, test.task)

cat("*************GLMnet results*************\n")
cat("Severity index:", calc.severity(pred=output), "\n")
cat("Same Severity: ", same.severity(pred=output),
    "\n")
cat("Number of expected severe cases:", 
    which(output$data$truth %>% cut(breaks=c(-Inf, 0.9999999, Inf), labels=c("a","b"))=="a") %>% length(),
    "\n")
cat("Number of obtained severe cases:", 
    which(output$data$response %>% cut(breaks=c(-Inf, 0.9999999, Inf), labels=c("a","b"))=="a") %>% length(),
    "\n")

pdf(file="/tmp/glm.pdf")
dat = data.frame(Prediction=output$data$response, Truth=output$data$truth)
ggplot(dat, aes(x=Prediction, y=Truth)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
dev.off()

pdf(file="/tmp/glm-LOESS.pdf")
dat = data.frame(Prediction=output$data$response, Truth=output$data$truth)
ggplot(dat, aes(x=Prediction, y=Truth)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()            # Add a loess smoothed fit curve with confidence region
#> `geom_smooth()` using method = 'loess'
dev.off()


########Testing Random Forest############
output<-randomForest.reg(train.task, test.task)

cat("*************Random Forest results*************\n")
cat("Severity index:", calc.severity(pred=output), "\n")
cat("Same Severity: ", same.severity(pred=output),
    "\n")
cat("Number of expected severe cases:", 
    which(output$data$truth %>% cut(breaks=c(-Inf, 0.9999999, Inf), labels=c("a","b"))=="a") %>% length(),
    "\n")
cat("Number of obtained severe cases:", 
    which(output$data$response %>% cut(breaks=c(-Inf, 0.9999999, Inf), labels=c("a","b"))=="a") %>% length(),
    "\n")

pdf(file="/tmp/rf.pdf")
dat = data.frame(Prediction=output$data$response, Truth=output$data$truth)
ggplot(dat, aes(x=Prediction, y=Truth)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
dev.off()


pdf(file="/tmp/rf-LOESS.pdf")
dat = data.frame(Prediction=output$data$response, Truth=output$data$truth)
ggplot(dat, aes(x=Prediction, y=Truth)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()            # Add a loess smoothed fit curve with confidence region
#> `geom_smooth()` using method = 'loess'
dev.off()

########Testing SVM - Radial############
output<-svm.reg(train.task, test.task, pol=FALSE)

cat("*************SVM (radial) results*************\n")
cat("Severity index:", calc.severity(pred=output), "\n")
cat("Same Severity: ", same.severity(pred=output),
    "\n")
cat("Number of expected severe cases:", 
    which(output$data$truth %>% cut(breaks=c(-Inf, 0.9999999, Inf), labels=c("a","b"))=="a") %>% length(),
    "\n")
cat("Number of obtained severe cases:", 
    which(output$data$response %>% cut(breaks=c(-Inf, 0.9999999, Inf), labels=c("a","b"))=="a") %>% length(),
    "\n")

pdf(file="/tmp/svm.pdf")
dat = data.frame(Prediction=output$data$response, Truth=output$data$truth)
ggplot(dat, aes(x=Prediction, y=Truth)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region
dev.off()

pdf(file="/tmp/svm-LOESS.pdf")
dat = data.frame(Prediction=output$data$response, Truth=output$data$truth)
ggplot(dat, aes(x=Prediction, y=Truth)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()            # Add a loess smoothed fit curve with confidence region
#> `geom_smooth()` using method = 'loess'
dev.off()



