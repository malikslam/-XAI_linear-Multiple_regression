

bike <- na.omit(read.csv(file.choose()))
dim(bike) #731  12

# checking missing (na) row
bike[rowSums(is.na(bike)) > 0,]

# checking columns
names(bike)

# Simple Linear Regression

# Regression of number of rented bike(cnt) on  temperature (tmp)
# cnt = B0 + B1*temp + e

lm.fit<-lm(cnt ~ temp, data = bike)
summary(lm.fit)

plot(lm.fit)
# plot(lm.fit) #diagnostic test

plot(temp,cnt)
abline(lm.fit)


# obtain confident interval for coefficient for the Estimate
confint(lm.fit, level = 0.95)

# predicting the cnt with the model
pred <- predict(lm.fit,bike)

# checking estimated values vs actual value 
pred[1:10]
bike$cnt[1:10]


#comparing predicated vs actual values
plot(bike$cnt,type = "l", lty = 1.8, col ="red")
lines(pred,type = "l", col ="blue")

# =====
# Simple Linear Regression with 2 level categorical variables

# Regression of number of rented bike(cnt) on  holiday (tmp)
# cnt = B0 + B1*holiday + e

unique(bike$holiday) # "NO HOLIDAY" & "HOLIDAY" level categories
 
lm.fit_2<-lm(cnt ~ holiday, data = bike)
summary(lm.fit_2)

plot(lm.fit_2)

# baseline category is "HOLIDAY"
# the average number number of rented bike for HOLIDAY is estimated to be 3735, 
# whereas NO HOLIDAY are estimated a total of 3735 + 792.1 = 4527.1 


# ====

# Linear / Multiple Regression

# checking available columns
names(bike)

#pick columns of interest
data.interest<- subset(bike,select = c(1,4,6,7,8,9,10,11,12)) 
names(data.interest)

# Regression of number of rented bike(cnt) on  "season","holiday", "workingday",
#                       "weathersit","temp","hum", windspeed", "days_since_2011"

MR.model_1 = lm(cnt ~ ., data = data.interest)
summary(MR.model_1)

plot(MR.model_1) # diagnostic test

# obtain confident interval for coefficient for the Estimate
confint(MR.model_1, level = 0.95)

# predicting the cnt with the model
ML.pred_1 <- predict(MR.model_1,bike)

# checking estimated values vs actual value 
ML.pred_1[1:10]
bike$cnt[1:10]

#comparing predicated vs actual values
plot(bike$cnt,type = "l", lty = 1.8, col ="red")
lines(ML.pred_1,type = "l", col ="blue")
