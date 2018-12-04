# In this session we are trying to explain the effect of sample size 
#and order of linear regression equations on the error

#We shall consider the cars dataset for this
file1 = read.csv('C:\\Users\\hp\\Downloads\\Cars1.csv')

#let us explore the dataset
dim(file1)
names(file1)
summary.data.frame(file1)

# Part 1. We shall model polynomial linear regression of order 7 
#on the train data of samples of various sizes 

# we shall randomly divide the data into train and test

set.seed(0)
rand = sample(1:nrow(file1),350)
train1 = file1[rand, ]
test1= file1[-rand, ]

dim.data.frame(test1)

#from train data we shall take a sample of size 10

set.seed(1)
rand = sample(1:nrow(train1),10)
n1 = train1[rand, ]
dim(n1)

#fit linear regression model of order 7

m7 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5) + I(Weight^6) 
         + I(Weight^7), n1)
m7

#TRAIN AND TEST ACCURACY
n1_t=sum(m7$residuals^2)
pred = predict(m7, newdata=test1)
n1_e=sum((pred-test1$MPG)^2)

n1_e

#from train data we shall take a sample of size 20

set.seed(2)
rand = sample(1:nrow(train1),20)
n2 = train1[rand, ]
dim(n1)
#fit linear regression model of order 7

m7 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5) + I(Weight^6) 
         + I(Weight^7), n2)
m7

#TRAIN AND TEST ACCURACY
n2_t=sum(m7$residuals^2)
pred = predict(m7, newdata=test1)
n2_e=sum((pred-test1$MPG)^2)

n2_e

#from train data we shall take a sample of size 30

set.seed(3)
rand = sample(1:nrow(train1),30)
n3 = train1[rand, ]
dim(n3)

#fit linear regression model of order 7

m7 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5) + I(Weight^6) 
         + I(Weight^7), n3)
m7

#TRAIN AND TEST ACCURACY
n3_t=sum(m7$residuals^2)
pred = predict(m7, newdata=test1)
n3_e=sum((pred-test1$MPG)^2)

n3_e

#from train data we shall take a sample of size 50

set.seed(4)
rand = sample(1:nrow(train1),50)
n4 = train1[rand, ]
dim(n4)

#fit linear regression model of order 7

m7 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5) + I(Weight^6) 
         + I(Weight^7), n4)
m7

#TRAIN AND TEST ACCURACY
n4_t=sum(m7$residuals^2)
pred = predict(m7, newdata=test1)
n4_e=sum((pred-test1$MPG)^2)

n4_e

#from train data we shall take a sample of size 100

set.seed(5)
rand = sample(1:nrow(train1),100)
n4 = train1[rand, ]
dim(n3)

#fit linear regression model of order 7

m7 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5) + I(Weight^6) 
         + I(Weight^7), n4)
m7

#TRAIN AND TEST ACCURACY
n5_t=sum(m7$residuals^2)
pred = predict(m7, newdata=test1)
n5_e=sum((pred-test1$MPG)^2)

n5_e

#from train data we shall take a sample of size 200

set.seed(6)
rand = sample(1:nrow(train1),200)
n6 = train1[rand, ]
dim(n6)

#fit linear regression model of order 7

m7 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5) + I(Weight^6) 
         + I(Weight^7), n6)
m7

#TRAIN AND TEST ACCURACY
n6_t=sum(m7$residuals^2)
pred = predict(m7, newdata=test1)
n6_e=sum((pred-test1$MPG)^2)

n6_e

#from train data we shall take a sample of size 300

set.seed(7)
rand = sample(1:nrow(train1),300)
n7 = train1[rand, ]
dim(n7)

#fit linear regression model of order 7

m7 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5) + I(Weight^6) 
         + I(Weight^7), n7)
m7

#TRAIN AND TEST ACCURACY
n7_t=sum(m7$residuals^2)
pred = predict(m7, newdata=test1)
n7_e=sum((pred-test1$MPG)^2)

n7_e
#plotting test error vs sample size
y1=c(n1_e,n2_e,n3_e,n4_e,n5_e,n6_e,n7_e)
y1
y2=c(n1_t,n2_t,n3_t,n4_t,n5_t,n6_t,n7_t)
y2
x=c(10,20,30,50,100,200,300)


plot(x, y1, main="Test Error vs. Sample Size", 
     xlab="Sample size", ylab="Test error", pch=18, col="blue")
lines(x, y1[order(x)], col='black', type='l',pch=50) 

plot(x, y2, main="Train Error vs. Sample Size", 
     xlab="Sample size", ylab="Train error", pch=18, col="blue")
lines(x, y2[order(x)], col='black', type='l',pch=50) 

#-------------------------end of part 1---------------------------------------#

# Part-2: We shall now fix a sample size and plot regression models of 
#increasing order and fit all the models and observe

# select a random sample of size 20
set.seed(10)
rand = sample(1:nrow(file1),20)
train1 = file1[rand, ]
dim(train1)

# fit regression model of order1
m1<-lm(MPG~Weight,train1)
m1
#PLOTTING THE MODEL OVER THE DATA
plot(train1$Weight,train1$MPG, main="Regression fit of order1",xlab="Weight",ylab="Miles per Gallon",pch=19, cex=0.5)
lines(sort(train1$Weight), fitted(m1)[order(train1$Weight)], col='red', type='l') 


# fit regression model of order2
m2<-lm(MPG~Weight+I(Weight^2),train1)
m2
#PLOTTING THE MODEL OVER THE DATA
plot(train1$Weight,train1$MPG, main="Regression fit of order2",xlab="Weight",ylab="Miles per Gallon",pch=19, cex=0.5)
lines(sort(train1$Weight), fitted(m2)[order(train1$Weight)], col='red', type='l') 

# fit regression model of order3
m3<-lm(MPG~Weight+I(Weight^2)+I(Weight^3),train1)
m3
#PLOTTING THE MODEL OVER THE DATA
plot(train1$Weight,train1$MPG, main="Regression fit of order3",xlab="Weight",ylab="Miles per Gallon",pch=19, cex=0.5)
lines(sort(train1$Weight), fitted(m3)[order(train1$Weight)], col='red', type='l') 

# fit regression model of order4
m4<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4),train1)
m4
#PLOTTING THE MODEL OVER THE DATA
plot(train1$Weight,train1$MPG, main="Regression fit of order4",xlab="Weight",ylab="Miles per Gallon",pch=19, cex=0.5)
lines(sort(train1$Weight), fitted(m4)[order(train1$Weight)], col='red', type='l') 

# fit regression model of order5
m5<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5),train1)
m5
#PLOTTING THE MODEL OVER THE DATA
plot(train1$Weight,train1$MPG, main="Regression fit of order5",xlab="Weight",ylab="Miles per Gallon",pch=19, cex=0.5)
lines(sort(train1$Weight), fitted(m5)[order(train1$Weight)], col='red', type='l') 

# fit regression model of order6
m6<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6),train1)
m6
#PLOTTING THE MODEL OVER THE DATA
plot(train1$Weight,train1$MPG, main="Regression fit of order6",xlab="Weight",ylab="Miles per Gallon",pch=19, cex=0.5)
lines(sort(train1$Weight), fitted(m6)[order(train1$Weight)], col='red', type='l') 

# fit regression model of order7
m7<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7),train1)
m7
#PLOTTING THE MODEL OVER THE DATA
plot(train1$Weight,train1$MPG, main="Regression fit of order7",xlab="Weight",ylab="Miles per Gallon",pch=19, cex=0.5)
lines(sort(train1$Weight), fitted(m7)[order(train1$Weight)], col='red', type='l') 

# fit regression model of order7
m8<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7)+I(Weight^8),train1)
m8
#PLOTTING THE MODEL OVER THE DATA
plot(train1$Weight,train1$MPG, main="Regression fit of order8",xlab="Weight",ylab="Miles per Gallon",pch=19, cex=0.5)
lines(sort(train1$Weight), fitted(m8)[order(train1$Weight)], col='red', type='l') 

#Plot all the regression lines
plot(train1$Weight, train1$MPG, main="Linear Regression line fits", 
     xlab="Weight of Car", ylab="Miles Per Gallon", pch=18, col="black")
lines(sort(train1$Weight), fitted(m1)[order(train1$Weight)], col='blue', type='l', pch=19) 
lines(sort(train1$Weight), fitted(m2)[order(train1$Weight)], col='brown', type='l', pch=19) 
lines(sort(train1$Weight), fitted(m3)[order(train1$Weight)], col='green', type='l', pch=19) 
lines(sort(train1$Weight), fitted(m4)[order(train1$Weight)], col='yellow', type='l', pch=19) 
lines(sort(train1$Weight), fitted(m5)[order(train1$Weight)], col='maroon', type='l', pch=19) 
lines(sort(train1$Weight), fitted(m6)[order(train1$Weight)], col='magenta', type='l', pch=19) 
lines(sort(train1$Weight), fitted(m7)[order(train1$Weight)], col='orange', type='l', pch=19) 
lines(sort(train1$Weight), fitted(m8)[order(train1$Weight)], col='black', type='l', pch=19) 
legend(x=4134.017, y=30.40786, legend=c("Order 1", "Order 2","Order 3", "Order 4","Order 5", "Order 6","Order 7", "Order 8"),
       col=c("blue", "brown", "green","yellow","maroon","magenta","orange","black"), lty=1, cex=0.8)
locator(1)
#----------------------------end of part 2---------------------------------------------------#

#Part-3: Here we take 4 random samples of same size from the train dataset and 
#fit linear regression of ascending orders for each sample. Then we plot
#the test error against increasing model complexity for each sample

# select a random sample of size 30 for test

set.seed(12)
rand = sample(1:nrow(file1),30)
test2 = file1[rand, ]
dim(test2)

# select a random sample of size 30 for first sample n1a
set.seed(13)
rand = sample(1:nrow(file1),30)
n1a = file1[rand, ]
dim(n1a)

# fit regression model of order1
m1a<-lm(MPG~Weight,n1a)
m1a
#TRAIN AND TEST ACCURACY
sum(m1a$residuals^2)
pred = predict(m1a, newdata=test2)
e11=sum((pred-test2$MPG)^2)
e11

# fit regression model of order2
m2a<-lm(MPG~Weight+I(Weight^2),n1a)
m2a
#TRAIN AND TEST ACCURACY
sum(m2a$residuals^2)
pred = predict(m2a, newdata=test2)
e12=sum((pred-test2$MPG)^2)
e12
# fit regression model of order3
m3a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3),n1a)
m3a
#TRAIN AND TEST ACCURACY
sum(m3a$residuals^2)
pred = predict(m3a, newdata=test2)
e13=sum((pred-test2$MPG)^2)
e13

# fit regression model of order5
m5a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5),n1a)
m5a
#TRAIN AND TEST ACCURACY
sum(m5a$residuals^2)
pred = predict(m5a, newdata=test2)
e15=sum((pred-test2$MPG)^2)
e15

# fit regression model of order7
m7a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7),n1a)
m7a
#TRAIN AND TEST ACCURACY
sum(m7a$residuals^2)
pred = predict(m7a, newdata=test2)
e17=sum((pred-test2$MPG)^2)
e17

# fit regression model of order9
m9a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7)+I(Weight^8)+I(Weight^9),n1a)
m9a
#TRAIN AND TEST ACCURACY
sum(m9a$residuals^2)
pred = predict(m9a, newdata=test2)
e19=sum((pred-test2$MPG)^2)
e19



# select a random sample of size 30 for second sample n2a
set.seed(14)
rand = sample(1:nrow(file1),30)
n2a = file1[rand, ]
dim(n2a)

# fit regression model of order1
m1a<-lm(MPG~Weight,n2a)
m1a
#TRAIN AND TEST ACCURACY
sum(m1a$residuals^2)
pred = predict(m1a, newdata=test2)
e21=sum((pred-test2$MPG)^2)
e21

# fit regression model of order2
m2a<-lm(MPG~Weight+I(Weight^2),n2a)
m2a
#TRAIN AND TEST ACCURACY
sum(m2a$residuals^2)
pred = predict(m2a, newdata=test2)
e22=sum((pred-test2$MPG)^2)
e22
# fit regression model of order3
m3a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3),n2a)
m3a
#TRAIN AND TEST ACCURACY
sum(m3a$residuals^2)
pred = predict(m3a, newdata=test2)
e23=sum((pred-test2$MPG)^2)
e23

# fit regression model of order5
m5a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5),n2a)
m5a
#TRAIN AND TEST ACCURACY
sum(m5a$residuals^2)
pred = predict(m5a, newdata=test2)
e25=sum((pred-test2$MPG)^2)
e25

# fit regression model of order7
m7a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7),n2a)
m7a
#TRAIN AND TEST ACCURACY
sum(m7a$residuals^2)
pred = predict(m7a, newdata=test2)
e27=sum((pred-test2$MPG)^2)
e27

# fit regression model of order9
m9a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7)+I(Weight^8)+I(Weight^9),n2a)
m9a
#TRAIN AND TEST ACCURACY
sum(m9a$residuals^2)
pred = predict(m9a, newdata=test2)
e29=sum((pred-test2$MPG)^2)
e29


# select a random sample of size 30 for third sample n3a
set.seed(15)
rand = sample(1:nrow(file1),30)
n3a = file1[rand, ]
dim(n3a)

# fit regression model of order1
m1a<-lm(MPG~Weight,n3a)
m1a
#TRAIN AND TEST ACCURACY
sum(m1a$residuals^2)
pred = predict(m1a, newdata=test2)
e31=sum((pred-test2$MPG)^2)
e31

# fit regression model of order2
m2a<-lm(MPG~Weight+I(Weight^2),n3a)
m2a
#TRAIN AND TEST ACCURACY
sum(m2a$residuals^2)
pred = predict(m2a, newdata=test2)
e32=sum((pred-test2$MPG)^2)
e32
# fit regression model of order3
m3a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3),n3a)
m3a
#TRAIN AND TEST ACCURACY
sum(m3a$residuals^2)
pred = predict(m3a, newdata=test2)
e33=sum((pred-test2$MPG)^2)
e33

# fit regression model of order5
m5a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5),n3a)
m5a
#TRAIN AND TEST ACCURACY
sum(m5a$residuals^2)
pred = predict(m5a, newdata=test2)
e35=sum((pred-test2$MPG)^2)
e35

# fit regression model of order7
m7a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7),n3a)
m7a
#TRAIN AND TEST ACCURACY
sum(m7a$residuals^2)
pred = predict(m7a, newdata=test2)
e37=sum((pred-test2$MPG)^2)
e37

# fit regression model of order9
m9a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7)+I(Weight^8)+I(Weight^9),n3a)
m9a
#TRAIN AND TEST ACCURACY
sum(m9a$residuals^2)
pred = predict(m9a, newdata=test2)
e39=sum((pred-test2$MPG)^2)
e39

# select a random sample of size 30 for fourth sample n4a
set.seed(16)
rand = sample(1:nrow(file1),30)
n4a = file1[rand, ]
dim(n4a)

# fit regression model of order1
m1a<-lm(MPG~Weight,n4a)
m1a
#TRAIN AND TEST ACCURACY
sum(m1a$residuals^2)
pred = predict(m1a, newdata=test2)
e41=sum((pred-test2$MPG)^2)
e41

# fit regression model of order2
m2a<-lm(MPG~Weight+I(Weight^2),n4a)
m2a
#TRAIN AND TEST ACCURACY
sum(m2a$residuals^2)
pred = predict(m2a, newdata=test2)
e42=sum((pred-test2$MPG)^2)
e42
# fit regression model of order3
m3a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3),n4a)
m3a
#TRAIN AND TEST ACCURACY
sum(m3a$residuals^2)
pred = predict(m3a, newdata=test2)
e43=sum((pred-test2$MPG)^2)
e43

# fit regression model of order5
m5a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5),n4a)
m5a
#TRAIN AND TEST ACCURACY
sum(m5a$residuals^2)
pred = predict(m5a, newdata=test2)
e45=sum((pred-test2$MPG)^2)
e45

# fit regression model of order7
m7a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7),n4a)
m7a
#TRAIN AND TEST ACCURACY
sum(m7a$residuals^2)
pred = predict(m7a, newdata=test2)
e47=sum((pred-test2$MPG)^2)
e47

# fit regression model of order9
m9a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7)+I(Weight^8)+I(Weight^9),n4a)
m9a
#TRAIN AND TEST ACCURACY
sum(m9a$residuals^2)
pred = predict(m9a, newdata=test2)
e49=sum((pred-test2$MPG)^2)
e49

#plotting error vs model complexity for different samples of same size

x=c(1,2,3,5,7,9)
y1=c(e11,e12,e13,e15,e17,e19)
y2=c(e21,e22,e23,e25,e27,e29)
y3=c(e31,e32,e33,e35,e37,e39)
y4=c(e41,e42,e43,e45,e47,e49)

plot(x, y1, main="Test Error vs. Model Complexity (N1)", 
     xlab="Model complexity", ylab="Test error", pch=18, col="blue")
lines(x, y1[order(x)], col='black', type='l',pch=50) 
plot(x, y2, main="Test Error vs. Model Complexity(N2)", 
     xlab="Model complexity", ylab="Test error", pch=18, col="blue")
lines(x, y2[order(x)], col='blue', type='l',pch=50)
plot(x, y3, main="Test Error vs. Model Complexity(N3)", 
     xlab="Model complexity", ylab="Test error", pch=18, col="blue")
lines(x, y3[order(x)], col='brown', type='l',pch=50)
plot(x, y4, main="Test Error vs. Model Complexity(N4)", 
     xlab="Model complexity", ylab="Test error", pch=18, col="blue")
lines(x, y4[order(x)], col='green', type='l',pch=50)


plot(x, y1, main="Test Error vs. Model Complexity", 
     xlab="Model complexity", ylab="Test error", pch=18, col="blue")
lines(x, y1[order(x)], col='black', type='l',pch=50) 
lines(x, y2[order(x)], col='blue', type='l',pch=50)
lines(x, y3[order(x)], col='brown', type='l',pch=50)
lines(x, y4[order(x)], col='green', type='l',pch=50)
legend(x=1.924261, y=179593.7, legend=c("N1", "N2","N3","N4"),
       col=c("black", "blue","brown","green"), lty=1, cex=0.8)
locator(1)


#--------------------end of part 3-----------------------------------------#

#Part-4, here we do the same as above but take only one sample size n=100
# select a random sample of size 100 for fourth sample n4a
set.seed(17)
rand = sample(1:nrow(file1),100)
nb = file1[rand, ]
dim(nb)

# fit regression model of order1
m1a<-lm(MPG~Weight,nb)
m1a
#TRAIN AND TEST ACCURACY
sum(m1a$residuals^2)
pred = predict(m1a, newdata=test2)
eb1=sum((pred-test2$MPG)^2)
eb1

# fit regression model of order2
m2a<-lm(MPG~Weight+I(Weight^2),nb)
m2a
#TRAIN AND TEST ACCURACY
sum(m2a$residuals^2)
pred = predict(m2a, newdata=test2)
eb2=sum((pred-test2$MPG)^2)
eb2
# fit regression model of order3
m3a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3),nb)
m3a
#TRAIN AND TEST ACCURACY
sum(m3a$residuals^2)
pred = predict(m3a, newdata=test2)
eb3=sum((pred-test2$MPG)^2)
eb3

# fit regression model of order5
m5a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5),nb)
m5a
#TRAIN AND TEST ACCURACY
sum(m5a$residuals^2)
pred = predict(m5a, newdata=test2)
eb5=sum((pred-test2$MPG)^2)
eb5

# fit regression model of order7
m7a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7),nb)
m7a
#TRAIN AND TEST ACCURACY
sum(m7a$residuals^2)
pred = predict(m7a, newdata=test2)
eb7=sum((pred-test2$MPG)^2)
eb7

# fit regression model of order9
m9a<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7)+I(Weight^8)+I(Weight^9),nb)
m9a
#TRAIN AND TEST ACCURACY
sum(m9a$residuals^2)
pred = predict(m9a, newdata=test2)
eb9=sum((pred-test2$MPG)^2)
eb9

#plotting the error vs model complexity for n=100
x=c(1,2,3,5,7,9)
yb=c(e11,e12,e13,e15,e17,e19)
plot(x, yb, main="Test Error vs. Model Complexity for n=100", 
     xlab="Model complexity", ylab="Test error", pch=18, col="blue")
lines(x, yb[order(x)], col='black', type='l',pch=50) 

#------------------------end of part-4---------------------------------------#

#Part-5, we shall plot both train error and test erron against model complexity

#let us take a random sample of n=50 for test and train data

set.seed(18)
rand = sample(1:nrow(file1),50)
test3 = file1[rand, ]
dim(test3)

set.seed(19)
rand = sample(1:nrow(file1),50)
train3 = file1[rand, ]
dim(train3)

# fit regression model of order1
m1<-lm(MPG~Weight,train3)
m1
#TRAIN AND TEST ACCURACY
etr1=sum(m1$residuals^2)
etr1
pred = predict(m1, newdata=test3)
ets1=sum((pred-test3$MPG)^2)
ets1

# fit regression model of order2
m2<-lm(MPG~Weight+I(Weight^2),train3)
m2
#TRAIN AND TEST ACCURACY
etr2=sum(m2$residuals^2)
etr2
pred = predict(m2a, newdata=test3)
ets2=sum((pred-test3$MPG)^2)
ets2
# fit regression model of order3
m3<-lm(MPG~Weight+I(Weight^2)+I(Weight^3),train3)
m3
#TRAIN AND TEST ACCURACY
etr3=sum(m3$residuals^2)
etr3
pred = predict(m3, newdata=test3)
ets3=sum((pred-test3$MPG)^2)
ets3

# fit regression model of order5
m5<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5),train3)
m5
#TRAIN AND TEST ACCURACY
etr5=sum(m5$residuals^2)
etr5
pred = predict(m5, newdata=test3)
ets5=sum((pred-test3$MPG)^2)
ets5

# fit regression model of order7
m7<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7),train3)
m7
#TRAIN AND TEST ACCURACY
etr7=sum(m7$residuals^2)
etr7
pred = predict(m7, newdata=test3)
ets7=sum((pred-test3$MPG)^2)
ets7

# fit regression model of order9
m9<-lm(MPG~Weight+I(Weight^2)+I(Weight^3)+I(Weight^4)+I(Weight^5)+I(Weight^6)+I(Weight^7)+I(Weight^8)+I(Weight^9),train3)
m9
#TRAIN AND TEST ACCURACY
etr9=sum(m9$residuals^2)
etr9
pred = predict(m9, newdata=test3)
ets9=sum((pred-test3$MPG)^2)
ets9

#plotting the error vs model complexity for n=100
x=c(1,2,3,5,7,9)
y=c(700,800,900,1000,1200,1400)
ytr=c(etr1,etr2,etr3,etr5,etr7,etr9)
ytr
yts=c(ets1,ets2,ets3,ets5,ets7,ets9)
yts


plot(x,y, main="Error vs. Model Complexity for n=100",type="n", 
     xlab="Model complexity", ylab="Error", pch=18, col="blue")
lines(x, ytr[order(x)], col='black', type='l',pch=50)
lines(x, yts[order(x)], col='red', type='l',pch=50)
legend(x=7.387975, y=1118.558, legend=c("Train Error", "Test Error"),
       col=c("black", "red"), lty=1, cex=0.8)
locator(1)
