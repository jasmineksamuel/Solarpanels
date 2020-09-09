---
title: "Solar Power Plant EDA & Irradiation Predictor"
author: "Jasmine Samuel"
date: "9/3/2020"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
### We are going to be looking at solar plant energy data. We will rely heavily on EDA for this analysis
### We have data from two plants with the same metrics being measured 

### Let's define the variables
#### DATE_TIME: Refers to the date_time in 15 minute intervals 
#### PLANT_ID: Refers to the specific plant ID- there are only two plants that have been sampled
#### SOURCE_KEY(generator): Refers to the inverter id 
#### SOURCE_KEY(sensor): Sensor panel id
#### AMBIENT_TEMPERATURE: Temperature at the plant
#### MODULE_TEMPERATURE: Temperature of the sensor
#### IRRADIATION: Amount of solar IRRADIATION in 15 minute intervals. Solar irradiation refers to the conversion of solar light to solar energy.
#### DC_POWER: Amount of direct current power generated in 15 minute intervals 
#### AC_POWER: Amount of alternating current power generated in 15 minute intervals
#### DAILY_YIELD: Cumulative sum of power generated at that point in the day
#### TOTAL_YIELD: Total yield for that inverter up until that point 

#### Let's start by importing relevant packages & reviewing the data

```{r packages}
library('ggplot2')
library('dplyr')
library('olsrr')
library('ggpubr')
library('gridExtra')
```

#### Reading in the 4 seperate files 
```{r data}
plant_g_1 <-  read.csv(file.path('/Users/jasminesamuel/Downloads/836676_1428586_bundle_archive/Plant_1_Generation_Data.csv'))
plant_g_2 <- read.csv(file.path('/Users/jasminesamuel/Downloads/836676_1428586_bundle_archive/Plant_2_Generation_Data.csv'))
plant_s_1 <- read.csv(file.path('/Users/jasminesamuel/Downloads/836676_1428586_bundle_archive/Plant_1_Weather_Sensor_Data.csv'))
plant_s_2 <- read.csv(file.path('/Users/jasminesamuel/Downloads/836676_1428586_bundle_archive/Plant_2_Weather_Sensor_Data.csv'))

```
### Looking at the data structure and getting familar with the data & variables


```{r generator structure}
str(plant_g_1) # solar plant generator data 
str(plant_g_2)
```

```{r sensor data}
str(plant_s_1) # solar plant sensor data 
str(plant_s_2) # solar plant sensor data 
```
```{r}
head(plant_g_1)
head(plant_g_2)
head(plant_s_1)
head(plant_s_2)
```
#### Let's see if any null variables exist
```{r}
lapply(plant_g_1,function(x) { length(which(is.na(x)))})
lapply(plant_g_2,function(x) { length(which(is.na(x)))})
lapply(plant_s_1,function(x) { length(which(is.na(x)))})
lapply(plant_s_2,function(x) { length(which(is.na(x)))})



```
#### Look's like we don't have any null values!


### Data Preprocessing
####  We have some changes to make to our data structure to make it more ideal for analysis.
####  -Right now we have 4 different data files, let's combine them into one file. 
#### -factor level data needs to be recoded 
#### -Clean up the time stamp data- data reported in 15 minute intervals 

#### PLANT_ID refers to the id of the plant. Let's change PLANT_ID from character to factor

```{r factor}
plant_g_1$PLANT_ID <- factor(plant_g_1$PLANT_ID)
plant_g_2$PLANT_ID <- factor(plant_g_2$PLANT_ID)
plant_s_1$PLANT_ID <- factor(plant_s_1$PLANT_ID)
plant_s_2$PLANT_ID <- factor(plant_s_2$PLANT_ID)
```
#### SOURCE_KEY have different values for sensor and generator id- we aren't going to be able to merge data on this factor and we can't use this variable as a factor to compare sensor vs generator data let's drop this and have plant_id as the main factor variable 

```{r factor drop}
plant_g_1$SOURCE_KEY <- NULL
plant_g_2$SOURCE_KEY <- NULL
plant_s_1$SOURCE_KEY <- NULL
plant_s_2$SOURCE_KEY <- NULL

```
#### DATE_TIME reported in 15 minute intervals. Lets just aggregate our observations by day 

```{r}
plant_g_1$DATE <- as.Date(plant_g_1$DATE_TIME, format= '%d-%m-%Y')
plant_g_1$DATE[1:10] 
plant_g_1$DATE_TIME<- NULL# we can drop DATE_TIME
```

``` {r}
plant_g_2$DATE <- as.Date(plant_g_2$DATE_TIME, format = '%Y-%m-%d')
plant_g_2$DATE[1:10]
plant_g_2$DATE_TIME <- NULL 

```
```{r}
plant_s_1$DATE <- as.Date(plant_s_1$DATE_TIME , format = '%Y-%m-%d')
plant_s_1$DATE[1:10]
plant_s_1$DATE_TIME <- NULL 
```

```{r}
plant_s_2$DATE <- as.Date(plant_s_2$DATE_TIME, format = '%Y-%m-%d')
plant_s_2$DATE[1:10]
plant_s_2$DATE_TIME <- NULL
```
#### Now that we have dates cleaned we can aggregate data by day using dplyr package


```{r}
plant_g_1 <- plant_g_1 %>%
  group_by(DATE, PLANT_ID) %>%
  summarise_all(sum)
plant_g_1

plant_g_2 <- plant_g_2 %>%
  group_by(DATE, PLANT_ID) %>%
  summarise_all(sum)
plant_g_2

plant_s_1 <- plant_s_1 %>%
  group_by(DATE,PLANT_ID) %>%
  summarise_all(sum)
plant_s_1

plant_s_2 <- plant_s_2 %>%
  group_by(DATE, PLANT_ID) %>%
  summarise_all(sum)
plant_s_2

```
#### Now lets combine all the data into one dataframe 

```{r}
plant_generator <- union(plant_g_1,plant_g_2)
plant_sensor <- union(plant_s_1, plant_s_2)
plant_generator
plant_sensor

df <- merge(plant_generator,plant_sensor, by = c('PLANT_ID','DATE'))


```

### Data 
#### DATE_TIME(date time): Refers to the date_time in 15 minute intervals 
#### PLANT_ID(factor): Refers to the specific plant ID- there are only two plants that have been sampled
#### AMBIENT_TEMPERATURE(num): Temperature at the plant
#### MODULE_TEMPERATURE(num): Temperature of the sensor
#### IRRADIATION(num): Amount of solar IRRADIATION in 15 minute intervals. Solar irradiation refers to the conversion of solar light to solar energy.
#### DC_POWER(num): Amount of direct current power generated in 15 minute intervals 
#### AC_POWER(num): Amount of alternating current power generated in 15 minute intervals
#### DAILY_YIELD(num): Cumulative sum of power generated at that point in the day
#### TOTAL_YIELD(num): Total yield for that inverter up until that point 

#### Review the dataframe and it's structure 

```{r data review}
df
str(df)
head(df)

```
#### Basic statistics 
```{r}
summary(df)

```



### EDA:Visualize & Understand data
#### Let's start by look at distributions & outliers of all variables

```{r}
v1 <- ggdensity(df$DAILY_YIELD,fill = 'skyblue4', main = 'Distribution of daily yield')
v2 <- ggqqplot(df$DAILY_YIELD, col='springgreen4')
v3 <- ggboxplot(df$DAILY_YIELD, fill = 'yellow3', col= "snow4", main ='Daily yield outliers')
grid.arrange(v1, v2,v3, nrow=3)
```


#### Daily yield approx normally distributed with some possibly extreme values & appears to be linear

```{r}
v4 <- ggdensity(df$AC_POWER, fill = 'skyblue4', main = 'Distribution of alternating current')
v5 <- ggqqplot(df$AC_POWER, col='springgreen4')
v6 <- ggboxplot(df$AC_POWER, fill = 'yellow3', col= "snow4", main ='Anlternating current outliers')
grid.arrange(v4,v5,v6, nrow=3)
```

#### AC_POWER is approaching normality & appears to be linear

```{r}
v7 <- ggdensity(df$DC_POWER, fill = 'skyblue4', main = 'Distribution of direct current')
v8 <- ggqqplot(df$DC_POWER, col='springgreen4')
v9 <- ggboxplot(df$DC_POWER,  fill = 'yellow3', col= "snow4", main ='Direct current outliers')
grid.arrange(v7, v8, v9, nrow=3)
```

#### DC_POWER appearing bimodal & possibly s curve

```{r}
v10 <- ggdensity(df$TOTAL_YIELD, fill = 'skyblue4', main = 'Distribution of total yield')
v11 <- ggqqplot(df$TOTAL_YIELD, col='springgreen4')
v12 <- ggboxplot(df$TOTAL_YIELD, fill = 'yellow3', col= "snow4", main ='total yield outliers')
grid.arrange(v10, v11, v12, nrow=3)
```

#### TOTAL_YIELD appearing bimodel and possible s curve

```{r}
v13 <- ggdensity(df$AMBIENT_TEMPERATURE, fill = 'skyblue4', main = 'Distribution of ambient temperature')
v14 <- ggqqplot(df$AMBIENT_TEMPERATURE, col='springgreen4')
v15 <- ggboxplot(df$AMBIENT_TEMPERATURE, fill = 'yellow3', col= "snow4", main ='Ambient temperature outliers')
grid.arrange(v13, v14, v15, nrow=3)
```


#### AMBIENT_TEMPERATURE is linear, could be approaching normal, not extreme outliers

```{r}
v16 <- ggdensity(df$MODULE_TEMPERATURE, fill = 'skyblue4', main = 'Distribution of module temperature')
v17 <- ggqqplot(df$MODULE_TEMPERATURE, col='springgreen4')
v18 <- ggboxplot(df$MODULE_TEMPERATURE, fill = 'yellow3', col= "snow4", main ='Module temperature outliers')
grid.arrange(v16,v17,v18, nrow=3)
```

#### MODULE_TEMPERATURE linear, approaching normal 

```{r}
v19 <- ggdensity(df$IRRADIATION, fill = 'skyblue4', main = 'Distribution of irradiation')
v20 <- ggqqplot(df$IRRADIATION, col='springgreen4')
v21 <- ggboxplot(df$IRRADIATION, fill = 'yellow3', col= "snow4", main ='Irradiation outliers')
grid.arrange(v19, v20, v21, nrow=3)
```


#### IRRADIATION linear



#### Looking at each variables distributions gives us a sense of what variables are normally distributed and linear- giving us a better sense of what kind of possible tests and questions we want to ask about our data 

#### Here are a few questions I have and plan to explore during this EDA
#### Which power plant performs better?
#### Are there significant differences between types of power?
#### How does irradiation relate to different energy 
#### Does a relationship between temp, irradiation, and energy exist?
#### Are there any relevant predictors of daily yield? 

keeping in mind that DC_power, and total_yielded_power have very non linear and normal distributions
I will use the below sections to address and answer all these questions as well as any new questions that arise 


### Q1:  Which power plant performs better?
####  Let's start by looking at the different power plants 

```{r}
boxplot(df$DAILY_YIELD~df$PLANT_ID, main= 'Plant 1 vs. Plant 2:Examining Outliers', xlab='plant id', ylab='daily yield',col= c('springgreen4','skyblue4'))
```

#### What is the average daily yield per plant?

```{r}
dy_means <- aggregate(df$DAILY_YIELD, by=list(df$PLANT_ID), FUN = mean)
dy_means
barplot(dy_means$x, names.arg = dy_means$Group.1, col= c('springgreen4','skyblue4'))
title("Mean daily energy yielded")

```



#### There does not appear to be a large difference between the daily energy yielded by plant. Let's run a test and see if there are mean differences 

```{r}
summary(aov(df$DAILY_YIELD~df$PLANT_ID))

```

#### There is no significant difference between energy yielded between plants.
#### What about ac_power?


```{r}
ac_mean <- aggregate(df$AC_POWER, by= list(df$PLANT_ID), FUN = mean)
ac_mean
barplot(ac_mean$x, names.arg=ac_mean$Group.1,col= c('springgreen4','skyblue4'))
title("Mean daily alternating current energy yielded ")

```


#### Each plant seems to yield a difference amount of alternating current energy 
#### Let's see if these differences are significant? 


```{r}
summary(aov(df$AC_POWER~df$PLANT_ID))


```
#### As suspected the mean daily alternating current yielded significantly differs between each plant 
#### Let's explore DC_POWER 

```{r}
dc_mean <- aggregate(df$DC_POWER, by= list(df$PLANT_ID), FUN= mean)
dc_mean
barplot(dc_mean$x, names.arg = dc_mean$Group.1,col= c('springgreen4','skyblue4'))
title("Mean daily direct current energy yielded ")
```


#### Similar to AC_POWER, each plant seems to generate different amounts of direct current on a daily basis 
#### Let's run a t test to seem if the differences are significant 


```{r}
plant_1 <- df[df$PLANT_ID == 4135001,]
plant_2 <- df[df$PLANT_ID != 4135001,]
t.test(plant_1$DC_POWER,plant_2$DC_POWER)
```

#### We see significant differences between dc for each plant. Overall there is a difference between alternating & direct current production for each plant.However, we do not see  significant difference in how much power total power is yielded per day.

### Q2: Are there significant differences between types of power?
#### What about differences between AC & DC power regardless of power plant? 
```{r}
# Looks like a difference between means. 
t.test(df$AC_POWER,df$AC_POWER)
```


#### We do not see a signifcant differences between alternating and direct current.
#### It looks like the differences are at the factor level or the plants. Plant 1 
#### to perform signifcantly better for producing direct and alternating current 


### Next let's investigate eraddation levels


#### what is the average eradation level? 

```{r}
mean(df$IRRADIATION)

```
#### What's the relationship of irradiation and time?
```{r}
plot(df$DATE,df$IRRADIATION, main = 'Daily Irradation Levels', xlab='Day',ylab='Irradation level', pch= 19, col='springgreen4')


```

#### We don't see a clear relationship between time & irradation levels. 
#### It's possible a relationship exists over longer periods of time For example
#### month over month, but  a limitation of this data is sample is only 34 days.

### Q3: How does irradiation relate to different energy 

```{r}
plot(df$IRRADIATION,df$DAILY_YIELD, main= 'Relationship between daily
     yielded power & Irradation', xlab= 'Irradation', ylab = 'Daily yielded power',pch= 19, col='springgreen4')
``` 



#### This relationship is moderately linear, with a few apparent outliers.


```{r}
plot(df$IRRADIATION,df$AC_POWER, main= 'Relationship between alternating 
current power & Irradation', xlab= 'Irradation', ylab = 'Alternating power current',pch= 19, col='springgreen4')
``` 

#### We see a somewhat linear, possible curve relationship.


#### how does irradation relate to different types of temperature? 

```{r}
plot(df$IRRADIATION,df$AMBIENT_TEMPERATURE, main= 'Relationship between ambient
temperature and irradation', xlab= 'Irradiation', ylab = ' Ambient Temperature',
pch= 19, col='springgreen4')
```

#### We see a slight curvlinear relationship, that may benefit in a transformation



```{r}
plot(df$IRRADIATION,df$MODULE_TEMPERATURE, main= 'Relationship between module
temperature and irradation', xlab= 'Irradiation', ylab = ' Module Temperature',
pch= 19, col='springgreen4')

```



#### We also so a linear relationship between irradation and module temperature 

#### Let's look at other possible relationships between variables 

```{r}
plot(df$AMBIENT_TEMPERATURE,df$MODULE_TEMPERATURE, main= 'Relationship between module
temperature and ambient temperature', xlab= 'Ambient temperature', ylab = ' Module Temperature',
pch= 19, col='springgreen4')


```
#### Unsurprisingly we see a strong linear relationship between temperature.

```{r}
plot(df$MODULE_TEMPERATURE,df$AC_POWER, main= 'Relationship between module
temperature and alternating current', xlab= 'Ambient temperature', ylab = 'Alternating current',
pch= 19, col='springgreen4')


```
```{r}
plot(df$MODULE_TEMPERATURE,df$DAILY_YIELD, main= 'Relationship between module
temperature and daily yield', xlab= 'Ambient temperature', ylab = 'Daily yield',
pch= 19, col='springgreen4')


```
#### We see a pretty similar relationship for energy yields and ambient temperature

```{R}
plot(df$AMBIENT_TEMPERATURE,df$MODULE_TEMPERATURE, main= 'Relationship between module
temperature and ambient temperature', xlab= 'Ambient temperature', ylab = ' Module Temperature',
pch= 19, col='springgreen4')


```
#### Let's look at the correlation between variables 

```{r}
df_cor<- cor(df[,unlist(lapply(df, is.numeric))])
library(corrplot)
col<- colorRampPalette(c('yellow3','springgreen4','skyblue4'))(20)
corrplot(df_cor, method= 'square', type='upper', col=col,tl.col="snow4", tl.srt=45)
```

#### let's define a highly correlated variable at  r=.70+
#### dvs: ac_power, daily_yield, ambient_temp and module_temp correlates? 
#### ac power & daily yield are moderately correlated r=.52
#### ac power & module & ambient temp have low correlations r< .30
#### module * ambient temp are highly correlated r = .89

### Q4:Does a relationship between temp, irradiation, and energy exist?
#### Through our EDA we saw irradiation has a linear relationship with a few variables. Can we use temp and energy yielded to predict irradation levels?



### To deal with multicorrelates we will search procedures- step forward to help identify the best model


#### Let's start with building a multiple linear regression model with all the variables 
#### we investigated with irradiation
```{r}
irridation <- lm(IRRADIATION ~AC_POWER+MODULE_TEMPERATURE+DAILY_YIELD+AMBIENT_TEMPERATURE, data=df)
irridation
summary(irridation)
```

#### The model is significant p <.01 overall and yields R-squared of 85%, and a relatively lower residual standard error.
#### y = -1.099  -1.181(ac_power)  + 2.575(module_temperature) + 4.989(daily_yield) - 1.879(ambient_temperature)
#### Further investigation of the model that daily yield ,module & ambient temperature are significant 
#### predictors. 

#### Let's use search procedures to find the best version of this model. 
#### using the olsrr package we can explore different search procedures
#### we'll emplore step forward for ease



```{r}
ols_step_forward_p(irridation)

```

#### Using step forward procedures we see that the best predictors of irridation are module temperature, ambient temperature and daily yielded energy. 

#### A few things to consider while looking at the selection summary R squared refers to the amount of variability that can be attributed to the predictor(irridation). We have decent r squared= with daily yiled & ambient temp



```{r}
irridation_best <- lm(IRRADIATION~MODULE_TEMPERATURE+AMBIENT_TEMPERATURE+DAILY_YIELD, data=df)
irridation_best
summary(irridation_best)

```

#### We see that daily_yield, module temp and ambient temp are the best predictors of irridation levels
#### y = -1.161 + 2.51(module_temperature) + 4.615(daily_yield) - 1.79(ambient_temperature)

#### Let's investigate assumptions of this linear regression and this model by looking at residuals 

```{r}
plot(irridation_best)
```


#### Looking at the residual vs fitted plot will help us understand if our model is appropriate for glm. 

#### The residuals are random around the 0 line suggesting that the relationship is reasonably linear. 
#### The residuals roughly form a horizontal band around the 0 line, suggesting that the variances of the error terms are equal.
#### We do see some residuals that stand out- suggesting that outliers do exist.

#### Looking at the QQ plot we see that the model roughly fits a straight line 

#### Scale location plots indicate spread of points across the predicted values range. 
#### Ideally there should be a horizontal line, indicating homoscedasticity.
#### This model is homoscedastic, meaning it has an uniform variance and it becomes heteroscedastic around 23

#### Residuals vs. leverage 
#### We aren't seeing any points that fall within cook's distance, which suggest 
#### there are not observations that have high leverage or are potientially influencing 
#### our model 


#### Overall this model meets the assumptions of linear regression! 


### Q5: Are there any relevant predictors of daily yield? 

#### Next see if we can predict total energy from the type energy types 
#### Let's examine the relationship between energy & Alternating current
```{r}
plot(df$DAILY_YIELD,df$AC_POWER, main= 'Relationship between daily yield &
alternating current', xlab= 'Daily Energy Yield', ylab= 'Daily Alternating Current Yielded', 
pch= 19, col='springgreen4')



```


#### There is weak linear relationship, perhaps treating outliers will give us a better relationship 


```{r}
plot(df$DAILY_YIELD, df$DC_POWER, main= 'Relationship between daily yield &
direct current', xlab= 'Daily Energy Yield', ylab= 'Daily Direct Current Yielded' ,
pch= 19, col='springgreen4' )


```


#### There appears to be a linear relationship with a large amount of outliers 

#### we may not be able to use direct current as a linear predictor of daily yield, but we can still look at AC_POWER


```{r}

energy <- lm(df$DAILY_YIELD~df$AC_POWER)
energy 
summary(energy)

```
#### Overall the model is insignificant, has a low R squared, and an extremely high residual standard error suggesting this model is not a great fit for linear regression. 


### Summary 

Overall the data yielded some interesting findings as we explored the data. We were able to address these questions 

1.  Which power plant performs better?
Power plant 1(4135001) performs better on average than plant 2(4136001). We did find a significant difference in how much direct current and the alternating current was yielded on a daily basis per plant. However, there is no evidence to support that there was a significant difference between plants for overall energy yielded.

2. Are there significant differences between types of power?
There were no significant differences in how much alternating vs. direct current is created on average. 

3. How does irradiation relate to different energy 
We see a positive linear relationship between irradiation levels and alternating current. This suggests that higher irradiation levels yield higher alternating current power produced. However, we did not see much of a relationship between direct current and irradiation. 

4. Does a relationship between temp, irradiation, and energy exist?
We were able to find that irradiation levels can be predicted by different variables such as temperature, and total energy yielded. This is an interesting finding because irradiation is the conversion of solar light to solar energy. More irradiance allows us to convert and yield more energy. We can use this model to better understand how much irradiation we need to yield a certain amount of energy and identify the best conditions for the conversion of solar to solar energy. If we have target energy yield goals we can use this model to identify what irradiation conditions are needed to meet those goals.


5. Are there any relevant predictors of daily yield? 
Unfortunately, we were not able to find any significant predictors of daily yield. We did see a slight positive relationship between alternating current and daily yield, which suggests that higher alternating current output results in higher energy yielded overall. 

Limitations
We have a limited amount of observations (34 days), and we would except changes to things like the overall distributions to become more normalized as well as identify long term trends such as how seasons impact energy produced. Additionally other
geospatial data would be interesting to incorporate. 




