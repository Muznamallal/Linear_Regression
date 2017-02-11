setwd("D:/MS/DataMining/HW1")

train <- read.csv("DirectMarketing.csv")

View(train)

summary(train)

dim(train)

train.clean = (na.omit(train))
train.clean = train

train.clean$History<- factor (train.clean$History)
train.clean$History<- as.character(train.clean$History)
train.clean$History[is.na(train.clean$History)] <- "None"

train.clean$History <- factor (train.clean$History, levels=c("None","Low","Medium","High"))
levels(train.clean$History)


View(train.clean)

train.clean[is.na(train.clean)] <- 

attach(train.clean)
dim(train.clean)

summary(train.clean)

standardDevs = c(Salary = sd(train.clean$Salary), Children = sd(train.clean$Children), Catalogs = sd(train.clean$Catalogs), AmountSpent = sd(train.clean$AmountSpent))

standardDevs

library("ggplot2")

ggplot(train.clean, aes(x = AmountSpent)) + geom_density()

ggplot(train.clean, aes(x = Salary)) + geom_density()

ggplot(train.clean, aes(x = Catalogs)) + geom_density()

model1 = lm(AmountSpent ~ Catalogs)
plot(model1)

cor(train.clean$Salary, train.clean$AmountSpent)

cor(log(train.clean$Salary), log(train.clean$AmountSpent))

cor(train.clean$Children, train.clean$AmountSpent)

cor(train.clean$Catalogs, train.clean$AmountSpent)


ggplot(train.clean, aes(x = Salary, y = AmountSpent)) + geom_point(shape=1) +  geom_smooth(method=lm, se=FALSE)

ggplot(train.clean, aes(x = Children, y = AmountSpent)) + geom_point(shape=1) +  geom_smooth(method=lm, se=FALSE)

ggplot(train.clean, aes(x = log(Catalogs), y = log(AmountSpent))) + geom_point(shape=1) +  geom_smooth(method=lm, se=FALSE)


#Density calc for categorical variables

ggplot(train.clean, aes(x=AmountSpent, color=Age) )+ geom_density(alpha = 0.5)

ggplot(train.clean, aes(x=AmountSpent, color=Gender) )+ geom_density(alpha = 0.5)

ggplot(train.clean, aes(x=AmountSpent, color=OwnHome) )+ geom_density(alpha = 0.5)

ggplot(train.clean, aes(x=AmountSpent, color=Married) )+ geom_density(alpha = 0.5)

ggplot(train.clean, aes(x=AmountSpent, color=Married) )+ geom_density(alpha = 0.5)+facet_grid(~Children)

ggplot(train.clean, aes(x=AmountSpent, color=Location) )+ geom_density(alpha = 0.5)

ggplot(train.clean, aes(x=AmountSpent, color=History) )+ geom_density(alpha = 0.5)

#Categrical mean comparison
#Age vs AmountSpent
Age.Young = subset(train.clean, train.clean$Age=='Young')
Age.Middle =  subset(train.clean, train.clean$Age=='Middle')
Age.Old =  subset(train.clean, train.clean$Age=='Old')

ggplot(train.clean, aes(x = Age, y = AmountSpent, color=Age)) + stat_boxplot(geom = "errorbar", width = 0.3) + geom_boxplot()
mean(Age.Young$AmountSpent)
mean(Age.Middle$AmountSpent)
mean(Age.Old$AmountSpent)

#ANOVA test
oneway.test(train.clean$AmountSpent ~ train.clean$Age, var.equal = FALSE)

#Gender vs AmountSpent
Gender.Female = subset(train.clean, train.clean$Gender=='Female')
Gender.Male =  subset(train.clean, train.clean$Gender=='Male')

ggplot(train.clean, aes(x = Gender, y = AmountSpent, color=Gender)) + stat_boxplot(geom = "errorbar", width = 0.3) + geom_boxplot()
mean(Gender.Female$AmountSpent)
mean(Gender.Male$AmountSpent)

#ANOVA test
oneway.test(train.clean$AmountSpent ~ train.clean$Gender, var.equal = FALSE)

#OwnHome vs AmountSpent
OwnHome.Own = subset(train.clean, train.clean$OwnHome=='Own')
OwnHome.Rent = subset(train.clean, train.clean$OwnHome=='Rent')

ggplot(train.clean, aes(x = OwnHome, y = AmountSpent, color=OwnHome)) + stat_boxplot(geom = "errorbar", width = 0.3) + geom_boxplot()
mean(OwnHome.Rent$AmountSpent)
mean(OwnHome.Own$AmountSpent)

#ANOVA test
oneway.test(train.clean$AmountSpent ~ train.clean$OwnHome, var.equal = FALSE)

#Married vs AmountSpent
Married.Single = subset(train.clean, train.clean$Married=='Single')
Married.Married = subset(train.clean, train.clean$Married=='Married')

ggplot(train.clean, aes(x = Married, y = AmountSpent, color=Married)) + stat_boxplot(geom = "errorbar", width = 0.3) + geom_boxplot()
mean(Married.Single$AmountSpent)
mean(Married.Married$AmountSpent)

#ANOVA test
oneway.test(train.clean$AmountSpent ~ train.clean$Married, var.equal = FALSE)

#Location vs AmountSpent
Location.Close = subset(train.clean, train.clean$Location=='Close')
Location.Far = subset(train.clean, train.clean$Location=='Far')

ggplot(train.clean, aes(x = Location, y = AmountSpent, color=Location)) + stat_boxplot(geom = "errorbar", width = 0.3) + geom_boxplot()
mean(Location.Close$AmountSpent)
mean(Location.Far$AmountSpent)

#ANOVA test
oneway.test(train.clean$AmountSpent ~ train.clean$Location, var.equal = FALSE)

#History vs AmountSpent
History.Low = subset(train.clean, train.clean$History=='Low')
History.Medium = subset(train.clean, train.clean$History=='Medium')
History.High = subset(train.clean, train.clean$History=='High')

ggplot(train.clean, aes(x = History, y = AmountSpent, color=History)) + stat_boxplot(geom = "errorbar", width = 0.3) + geom_boxplot()
mean(History.Low$AmountSpent)
mean(History.Medium$AmountSpent)
mean(History.High$AmountSpent)

#ANOVA test
oneway.test(train.clean$AmountSpent ~ train.clean$History, var.equal = FALSE)

#3
fitFull = lm(AmountSpent ~ Catalogs + Salary  + Children + History + Age+ Gender+ Location+ Married+ OwnHome, data=train.clean)
summary(fitFull)

model.mse = mean(residuals(fitFull)^2)
model.mse
rmse = sqrt(model.mse)
rmse

#http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ for ggplot multiplots
multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}

p1 <- ggplot(train.clean, aes(x = Salary, y = AmountSpent)) + geom_point(shape=1) +  geom_smooth(method=lm, se=FALSE)
p2 <- ggplot(train.clean, aes(x = log(Salary), y = log(AmountSpent))) + geom_point(shape=1) +  geom_smooth(method=lm, se=FALSE)

multiplot(p1,p2, cols = 2)

fitSal = lm(log(AmountSpent) ~ log(Salary), data=train.clean)
summary(fitSal)
plot(fitSal)
abline(fitSal, lwd = 3, col="red")

fit = lm(AmountSpent ~ Children, data=train.clean)
summary(fit)
plot(fit)

fit = lm(AmountSpent ~ Catalogs, data=train.clean)
summary(fit)
plot(fit)

fit = lm(AmountSpent ~ Catalogs + Salary+ Children, data=train.clean)
summary(fit)

fit = lm(AmountSpent ~ Catalogs + Salary, data=train.clean)
summary(fit)

fit = lm(AmountSpent ~ Salary + Children, data=train.clean)
summary(fit)

library(MASS)

fitFull = lm(AmountSpent ~ Catalogs + Salary  + Children + History + Age+ Gender+ Location+ Married+ OwnHome, data=train.clean)
stepAIC(fitFull, direction="backward")

#Final models
#Model without using any polynomial terms
fitNonPoly = lm(AmountSpent ~ Catalogs + Salary + Children + History + Gender + Location, data=train.clean)
summary(fitNonPoly)

model.mse = mean(residuals(fitNonPoly)^2)
model.mse
rmse = sqrt(model.mse)
rmse

## leave-one-out cross validation
n = length(train.clean$AmountSpent)
error = dim(n)
for (k in 1:n) {
  train1 = c(1:n)
  train2 = train1[train1!=k] ## pick elements that are different from k
  m2 = lm(AmountSpent ~  Catalogs + Salary + Children + History + Gender + Location, data=train.clean[train2 ,])
  pred = predict(m2, newdat=train.clean[-train2 ,])
  obs = train.clean$AmountSpent[-train2]
  error[k] = obs-pred
}
me=mean(error)
me 
rmse=sqrt(mean(error^2))
rmse

#Let us try putting log transformations on Salary and AmountSpent and see what the results are
poly.fitLog <- lm(AmountSpent ~ poly(Salary, degree = 5) +poly(Catalogs, degree = 3)+ poly(Children, degree = 3)+ History + poly(Age, degree=2) + Location, data = train.clean)

summary(poly.fitLog)
model.mse = mean(residuals(poly.fitLog)^2)
model.mse
rmse = sqrt(model.mse)
rmse

## leave-one-out cross validation
n = length(train.clean$AmountSpent)
error = dim(n)
for (k in 1:n) {
  train1 = c(1:n)
  train2 = train1[train1!=k] ## pick elements that are different from k
  m2 = lm(log(AmountSpent) ~   poly(log(Salary), degree = 5) + poly(Catalogs, degree = 3)+ Location  + poly(Children, degree = 3) + History + Gender , data=train.clean[train2 ,])
  pred = predict(m2, newdat=train.clean[-train2 ,])
  obs = train.clean$AmountSpent[-train2]
  error[k] = obs-pred
}
me=mean(error)
me 

rmse=sqrt(mean(error^2))
rmse

#Finally let us try without log
#if we use degree = 10 for salary we get a better fit, but it can tend to be over fitting as based on the cv so we set it as 5
poly.fitFinal <- lm(AmountSpent ~ poly(Salary, degree = 6) +poly(Catalogs, degree = 3)+Age +poly(Children, degree = 3)+ History + Gender + Location, data = train.clean)
summary(poly.fitFinal)

model.mse = mean(residuals(poly.fitFinal)^2)
model.mse
rmse = sqrt(model.mse)
rmse

model.stepAIC <-stepAIC(poly.fitFinal, direction="forward")
summary(model.stepAIC)
plot(poly.fitFinal)

## leave-one-out cross validation
n = length(train.clean$AmountSpent)
error = dim(n)
for (k in 1:n) {
  train1 = c(1:n)
  train2 = train1[train1!=k] ## pick elements that are different from k
  m2 = lm(AmountSpent ~   poly(Salary, degree = 6) + poly(Catalogs, degree = 3)+ Location+ Age + poly(Children, degree = 2) + History , data=train.clean[train2 ,])
  pred = predict(m2, newdat=train.clean[-train2 ,])
  obs = train.clean$AmountSpent[-train2]
  error[k] = obs-pred
}
me=mean(error)
me 

rmse=sqrt(mean(error^2))
rmse


library(locfit)

loc.fit <- locfit(log(AmountSpent)~lp(log(Salary),nn=0.50),data=train.clean)
plot(loc.fit)
loc.fit

alpha <-seq(0.20,1,by=0.01)
n1=length(alpha)
g=matrix(nrow=n1,ncol=4)
for (k in 1:length(alpha)) {
  g[k,]<-gcv(AmountSpent~lp(Salary,nn=alpha[k]),data=train.clean)
}
head(g)
g
plot(g[,4]~g[,3],ylab="GCV",xlab="degrees of freedom")

