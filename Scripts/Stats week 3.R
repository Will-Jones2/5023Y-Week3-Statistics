library(tidyverse)
library(readr)
library(patchwork)
library(broom)
#Q. Which is our predictor/independent variable and which is our response/dependent variable
#Predictor: Wood density, Response: Timber hardness

wood_density <- read_csv("Data/wood_density.csv")

# CI is wider at the ends than it is in the middle. This is simply because if we imagine this line has the ability to wiggle around the data points, we can imagine it has more freedom to move at the ends (hence wider CI) than the centre where it passes through a pivot point.
#making the cl on a ggplot and adding geomsmooth is the same as the least squares method 

wood_density %>% ggplot(aes(x=Density,
                            y=Hardness))+
  geom_point()+
  geom_smooth(method="lm")
#y=β0+β1x, There will also be error ϵ in our model (also referred to as unexplained residual variation). This is represented as 1 - R2.
#y=β0+β1x+ϵ
#how to get teh coefficients e.g. te intercept and density 
density_model <- lm(Hardness~Density, data=wood_density)
density_model

#Hardness=−1160.5+57.51(Density)

#unlike last session where value of the intercept was the mean of one of our categories, and the slope was the difference between them, there are no categorical means at which to set the intercept, instead the value of the intercept is the timber hardness when wood density (x) = zero

#-1160.5 is a strange number as you cannot have negative hardense and density ect= A Wood density of zero should obviously be seen as an impossible value. I think this would be some sort of intangible ghost wood. The second odd things is that wood with a density of zero would produce timber with a ?negative hardness? and at this point my head starts to hurt!
#this does not matter as if we use a sensible x value we get a sensible y

# calculate the predicted hardness when density is 24.7
coef(density_model)[1]+
  coef(density_model)[2]*
  24.7
#Add predictions to our datapoints
#It can be very useful to be able to add the predictions from our dataset, so that we can compare our model’s predicted values to our realised values. We can do this with the function fitted() on our model to pull out the models estimates of the dependent variable based on each value of the independent value.
fitted(density_model)

#this will show us the model fitted values
#Residuals
#We can now also calculate the difference between these predictions or model-fitted values and the observed values - these values are know as the residuals
#do this by minusing the model fitted values and the values we saw:
484-259.9152
#there is a 224.08 difference

#add the models predictions and residuals back onto our original dataframe.
wood_density_augmented <- wood_density %>% 
  mutate(predictions=fitted(density_model)) %>% 
  mutate(residuals=Hardness-predictions)
Checking the overall explanatory power of a model

#For each observation in our dataframe we now have:the observed Timber Hardness, our predicted Hardness value, the difference between these two values, We can plot each of these in turn.

p1 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=Hardness))+
  geom_line()+
  ggtitle("Full Data")#actucal data 

p2 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=predictions))+
  geom_line()+
  ggtitle("Linear trend")#actual values subbed for the model fitted ones 

p3 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=residuals))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining pattern")#putting the error so the differences between real and model fitted
#should be a flat line if it is to fit well^

p1+p2+p3

#A model which explains our data perfectly would produce a perfect prediction fit, and the residual line would be a perfect flat line. Any patterns in our residuals are what is left-over after taking away the pattern explained by the linear model.Visualising residuals can be very useful for understanding the fit of our model. Slopes, patterns or trends here can indicate whether our model has done a good job.

#Tidy your models with (a) broom
library(broom)
broom::glance(density_model)#Constructs a concise one-row summary of the model. This typically contains values such as R^2, adjusted R^2, your F values, degrees of freedom and P
broom::tidy(density_model, conf.int=TRUE)#Constructs a small tibble with most of the models summary data in it. Very similar to our summary() output.
broom::augment(density_model, wood_density, interval="confidence") #Takes computations from our model fit and adds them back onto our original dataframe.
#.fitted = predictions of the model
#.resid = residuals
#.upper is the 95% confidence interval upper value for our fit line
#.lower is the 95% confidence interval lower value for our fit line

#Confidence in the slope of our regression
plot1 <- broom::augment(density_model, wood_density, interval="confidence") %>% ggplot(aes(x=Density, y=Hardness))+geom_line(aes(x=Density, y=.fitted))+geom_line(aes(x=Density, y=.upper), linetype="dashed")+geom_line(aes(x=Density, y=.lower), linetype="dashed")+geom_point() +ggtitle("Manually fitting linear model \n and confidence intervals")

plot2 <- wood_density %>% ggplot(aes(x=Density, y=Hardness))+geom_smooth(method=lm)+geom_point()+ggtitle("Geom smooth method to plotting \n a linear model")

plot1+plot2


#write up
#Wood density is an excellent predictor of timber hardness. On average for every pound per cubic foot increase in the density of wood, we see a 57.5 point increase in the Janka “hardness scale” (F1,34= 637, P <0.001, R^2 = 0.94).
# can i see this 


