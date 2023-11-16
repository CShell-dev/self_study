library(COVID19)
#COVID19::covid19(country = c("USA", "CHN") , 
#                 level = 1, 
#                 wb = c('NY.GDP.MKTP.KD.ZG'))-> test.data
#view(test.data)

rm(list=ls()) 
library(readxl)
CodesForCountry <- read_excel("CodesForCountry.xlsx")
View(CodesForCountry)

# add food data
library(tidyverse)
library(readr)


#  Read in data
# Then fix colnames to be more R friendly (no spaces, no commas)
Fat_Supply <- read_csv("Fat_Supply_Quantity_Data.csv")
nrow(Fat_Supply)
ncol(Fat_Supply)
dimnames(Fat_Supply)
head(Fat_Supply)
names(Fat_Supply)<- make.names(names(Fat_Supply),unique = TRUE)

####Food_Supply_kcal###
Food_Supply_kcal <- read_csv("Food_Supply_kcal_Data.csv")
nrow(Food_Supply_kcal)
ncol(Food_Supply_kcal)
dimnames(Food_Supply_kcal)
names(Food_Supply_kcal)<- make.names(names(Food_Supply_kcal),unique = TRUE)

#####Food_Supply_Quantity ######
Food_Supply_Quantity <- read_csv("Food_Supply_Quantity_kg_Data.csv")
row(Food_Supply_Quantity)
ncol(Food_Supply_Quantity)
dimnames(Food_Supply_Quantity)
names(Food_Supply_Quantity)<- make.names(names(Food_Supply_Quantity),unique = TRUE)

#######Protein_Supply_Quantity ######
Protein_Supply_Quantity <- read_csv("Protein_Supply_Quantity_Data.csv")
row(Protein_Supply_Quantity)
ncol(Protein_Supply_Quantity)
dimnames(Protein_Supply_Quantity)
names(Protein_Supply_Quantity)<- make.names(names(Protein_Supply_Quantity),unique = TRUE)
#view(Protein_Supply_Quantity)
#Protein_Supply_Quantity%>%filter(Country=="United States of America")%>%select(Country,Deaths:Population)
#View(Fat_Supply)
#Food_Supply_kcal[,-(25:32)]
#View(Fat_Supply[,-(25:32)])
#Joining data together to create the nutrition data
Fat_Supply[,-(25:32)]%>% #removing last columns from 25->32
  left_join(.,Food_Supply_kcal[,-(25:32)],by="Country",
            suffix=c("_Fat","_Food"))%>%
  left_join(.,Protein_Supply_Quantity[,-32],by="Country")->extradata


View(extradata)
#extradata%>%filter(Country=="United States of America")%>%select(Country,Deaths:Population)

### left join food data with country codes and add the 3 letters code columns
left_join(extradata, 
          CodesForCountry,
          by = c("Country" = "Country_Name")) -> agdata_codes
View(agdata_codes)
#View(agdata_codes%>%select(Country, ISO_Code))
agdata_codes$ISO_Code
agdata_codes$ISO_3 <- substr(agdata_codes$ISO_Code, 1, 3)   # add a new column ISO_3 to remove the extra space of "LVA " for example
agdata_codes$ISO_3
View(agdata_codes)                            

## has manually fixed in the CodesForCountry.xlsx and then check by the following code  
View(agdata_codes%>%select(Country, ISO_Code)%>%filter(is.na(ISO_Code)))


#covid.countries <- na.omit(agdata_codes$ISO_Code)
# remove the extra empty space in country code otherwise it won't create the data using covid 19 function
# becuase the country code has to be only 3 strings without extra space
covid.countries_raw <- agdata_codes$ISO_Code
covid.countries_raw 
covid.countries=substr(covid.countries_raw,1,3)
covid.countries
ISO_Code=substr(covid.countries_raw,1,3)
agdata_codes->fooddata
view(fooddata)
fooddata$ISO_3

## 2.  Determine which World Bank Indicators would be helpful, find their weird variable names
### using this web https://www.kaggle.com/benhamner/indicators-in-data 
# first read all the WBIs from the excel file after removing all the non-existing world bank indices in the COVID-19 library from the excel file

namestToshow <- read_excel("index_name.xlsx")
view(namestToshow)
library(COVID19)
COVID19::covid19(country = covid.countries, 
                 level = 1, 
                 wb = namestToshow$indexname)-> testsh.data  
view(testsh.data)




## just keep the most recent record-4 days based on date for each world bank index for each country
library(data.table)
setDT(testsh.data)[,.SD[which.max(date)-4],keyby=iso_alpha_3]->testshnew.data  
view(testshnew.data)

## 3. find out the missing data percentage for all the world bank indices.
total=nrow(testshnew.data)  # this is how we get 169
# sapply(testshnew.data, function(x) sum(is.na(x)))  calculate how many missing data for each index

# only choose the columns withe low missing data rate
wordvomit=sapply(testshnew.data, function(x) sum(is.na(x)))*100/169
useful_index<-wordvomit[wordvomit<10]%>%names()
useful_index[(19:32)]
## 4.  Use the covid19 function to create a dataset with the covid19 and world bank data
# COVID19::covid19(country = covid.countries, 
#                  level = 1, 
#                  wb=c("SH.ANM.CHLD.ZS","SH.ANM.NPRG.ZS","SH.DTH.COMM.ZS","SH.DTH.IMRT","SH.DTH.INJR.ZS", "SH.DTH.MORT","SH.DTH.NCOM.ZS","SH.DTH.NMRT", "SH.DYN.MORT",
#                       "SH.DYN.MORT.FE","SH.DYN.MORT.MA","SH.DYN.NMRT","SH.IMM.IDPT","SH.IMM.MEAS","SH.MED.NUMW.P3","SH.MED.PHYS.ZS","SH.MMR.DTHS","SH.MMR.RISK",
#                       "SH.MMR.RISK.ZS","SH.PRG.ANEM","SH.STA.BRTC.ZS","SH.STA.DIAB.ZS","SH.STA.MMRT", "SH.TBS.CURE.ZS", "SH.TBS.DTEC.ZS", "SH.TBS.INCD"))->testwb.data
COVID19::covid19(country = covid.countries, 
                 level = 1, 
                 wb=c(useful_index[(19:32)]))->testwb.data

view(testwb.data)
# plug the indices with low missing data rates back to COVID function to populate the COVID data for each country
setDT(testwb.data)[,.SD[which.max(date)-4],keyby=iso_alpha_3]->oneday.data 
oneday_index.data<-oneday.data%>%select(iso_alpha_3,useful_index[(19:32)])
view(oneday_index.data)
wbfinal_rate.data<-oneday.data%>%select(iso_alpha_3,confirmed,deaths,vaccines,people_vaccinated,people_fully_vaccinated,population)%>%mutate(conf.rate =confirmed/(population/1000),
                                                                                                                                             death.rate=deaths/(population/1000),
                                                                                                                                             vacc.rate=vaccines/(population/1000),
                                                                                                                                             fullyvacc.rate=people_fully_vaccinated/(population/1000))%>%view()

wbfinal_rate_only<-wbfinal_rate.data%>%select(iso_alpha_3,conf.rate,death.rate,vacc.rate,fullyvacc.rate)
view(wbfinal_rate_only)  
#mutate(across(c(confirmed,deaths,vaccines,people_vaccinated,people_fully_vaccinated),~.x/(population*.001),"{.col}.rate"))%>%
# mutate(conf.rate =confirmed/(population/1000),
#        death.rate=deaths/(population/1000),
#        vacc.rate=vaccines/(population/1000),
#        fullyvacc.rate=people_fully_vaccinated/(population/1000))%>%
#          




# just keep the most recent record based on date for each world bank index for each country after updating the useful and low missing percentage for each index
# setDT(testwb.data)[,.SD[which.max(date)],keyby=iso_alpha_3]->wbfinal.data
# view(wbfinal.data)


#select(agdata_codes,"Country","ISO_3","Confirmed","Active","Recovered","Undernourished","Obesity" ,"Deaths")->fooddata

# make sure the country codes are 3 strings only otherwise they won't join
wbfinal_rate_only.data$iso_alpha_3
fooddata$ISO_3
## 4.  Combine with food supply data

left_join(fooddata,
          wbfinal_rate_only,
          by = c("ISO_3" = "iso_alpha_3")) -> full_temp_data
view(full_temp_data)
left_join(full_temp_data,
          oneday_index.data,
          by = c("ISO_3"="iso_alpha_3")) -> fulldata
view(fulldata)
#summary(fulldata)
# delete ISO_3 and id in the final dataset and only keep useful columns for further regression
sapply(fulldata, function(x) sum(is.na(x)))*100/170

# final = fulldata[,!(names(fulldata) %in% c("Alcoholic.Beverages_Fat","Aquatic.Products..Other_Fat","ISO_3","ISO_Code","id","date", "recovered","tests","iso_alpha_2","iso_numeric","iso_currency",
#                                            "icu","vent","school_closing","workplace_closing", "cancel_events",
#                                            "gatherings_restrictions","vaccines","people_vaccinated","people_fully_vaccinated","hosp",
#                                            "transport_closing","stay_home_restrictions","internal_movement_restrictions", "international_movement_restrictions",
#                                            "information_campaigns", "testing_policy","contact_tracing","facial_coverings","vaccination_policy", "elderly_people_protection",
#                                            "government_response_index", "stringency_index","containment_health_index","economic_support_index",
#                                            "administrative_area_level","administrative_area_level_1","administrative_area_level_2","administrative_area_level_3",
#                                            "key_local","key_google_mobility", "key_apple_mobility","key_nuts","key_jhu_csse","key_gadm",
#                                            "SH.DYN.MORT.FE","SH.DYN.MORT.MA","SH.MMR.RISK.ZS","SH.DTH.NCOM.ZS"))] 
# 
# 
# 
# View(final)
wordvomit2=sapply(fulldata, function(x) sum(is.na(x)))*100/170
useful_index2<-wordvomit2[wordvomit2<10]%>%names()
# cols2<-fulldata%>%names()
final_data<-fulldata[-c(1,2,5,19,20,28,51,72,73,74,75,76,78,79,82,83,87,90,91)]
view(final_data)
#final = fulldata[,!(names(fulldata) %in% c("ISO_Code","iso_alpha_3","ISO_3","id"))]
#sapply(final,is.factor)
#sapply(extradata,is.factor)
##5.  Rerun Lasso models.
######################
#using lassoing
########################
#final<- na.omit(fulldata) 
#View(final)
#View(extradata)

##final_test=final[,!(names(final) %in% c("Country","Undernourished","death"))]
#view(final_test)
final_data%>%names()
#final_x_data=final_data[,!(names(final_data) %in% c("death.rate"))]
# remove death_rate which is the target
x_data=final_data[-c(67)]
x_cols=x_data%>%names()
x_cols
final_noycorr<-na.omit(x_data)
#vif(final_noycorr)
view(x_data)
# remove all rows having NA
final_corr<- na.omit(x_data) 
#view(final_corr)
cor(final_corr)
install.packages("psych")
library(psych)
corPlot(final_corr, cex = 1.5)

#view(final_corr)
#head(final_corr)
#view(y)
# model.full=lm(death.rate~.-conf.rate-Animal.Products_Fat-Animal.fats_Fat-Cereals...Excluding.Beer_Fat-Treenuts_Fat-Offals_Fat-SH.IMM.IDPT
#               -Vegetables_Fat-SH.DYN.MORT-Meat_Fat-Oilcrops_Fat-egetal.Products_Fat-SH.MMR.DTHS-Eggs_Fat-Fish..Seafood_Fat
#               -Meat_Fat-Miscellaneous_Fat-Obesity-SH.DTH.NMRT-SH.DTH.IMRT-SH.DTH.MORT-SH.DTH.NMRT-SH.MMR.DTHS,data=final_data) 

# we used stepwise process to reduce the columns either by chuncks or one by one
model.full=lm(formula=death.rate~Alcoholic.Beverages+Oilcrops+Stimulants
              +Vegetable.Oils+Vegetable.Oils+SH.MED.NUMW.P3+SH.MED.PHYS.ZS+SH.DTH.INJR.ZS,data=final_data) 
#stepwise
#model.full=lm(formula =death.rate~Pulses_Fat+Spices_Fat,data=final_data) 
summary(model.full)
#summary(model.full)$coefficients[,4]<0.05
# print the coefficients for each x variables which are significent 
data.frame(summary(model.full)$coef[summary(model.full)$coef[,4] <= .1, 4])
#install.packages("car")
# VIF values are reasonable small
library(car)
car::vif(model.full)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          1.19695    0.34745   3.445 0.000738 ***
#   Alcoholic.Beverages  1.23379    0.33747   3.656 0.000352 ***
#   Oilcrops            -0.20653    0.05985  -3.451 0.000724 ***
#   Stimulants           0.44249    0.21294   2.078 0.039395 *  
#   Vegetable.Oils       9.03111    3.72371   2.425 0.016468 *  
#   SH.MED.NUMW.P3      -0.11590    0.02840  -4.080 7.24e-05 ***
#   SH.MED.PHYS.ZS       0.31207    0.06250   4.993 1.61e-06 ***
#   SH.DTH.INJR.ZS      -0.06198    0.02459  -2.520 0.012760 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9838 on 152 degrees of freedom
# (10 observations deleted due to missingness)
# Multiple R-squared:  0.4526,	Adjusted R-squared:  0.4273 
# F-statistic: 17.95 on 7 and 152 DF,  p-value: < 2.2e-16

#vif(model.full) question here, why NAN
#alias(model.full)
#install.packages("car")
library(car)
car::vif(model.full)

# dropping some predictor variables with high VIF
#library("plyr")   

# reading data from R stored session
#mydata = final_corr

# Checking number of  rows and columns in data
#dim(mydata)

# Loading required packages
#library(car)
#library(plyr)


#install.packages("plyr")

# Fit a linear model to the data
#fit=lm(Deaths~.-Confirmed-Active-Recovered-latitude-longitude,data=final_corr) 


# Calculating VIF for each independent variable
#vif(fit)

# Set a VIF threshold. All the variables having higher VIF than threshold
#are dropped from the model
#threshold=3

# Sequentially drop the variable with the largest VIF until
# all variables have VIF less than threshold
#drop=TRUE


# aftervif=data.frame()
# while(drop==TRUE) {
#   vfit=vif(fit)
#   aftervif=rbind.fill(aftervif,as.data.frame(t(vfit)))
#   if(max(vfit)>threshold) { fit=
#     update(fit,as.formula(paste(".","~",".","-",names(which.max(vfit))))) }
#   else { drop=FALSE }}

# Model after removing correlated Variables
# print(fit)
# summary(fit)
# How variables removed sequentially
#t_aftervif= as.data.frame(t(aftervif))


# Final (uncorrelated) variables with their VIFs
# vfit_d= as.data.frame(vfit)
# view(vfit_d)
# Exporting variables



# taking log for y
# x = scale(model.matrix(Deaths~.+Alcoholic.Beverages+Animal.Products+Animal.fats+Aquatic.Products..Other+Cereals...Excluding.Beer+Eggs+Fish..Seafood+Fruits...Excluding.Wine
#                        +Meat+Milk...Excluding.Butter+Offals+Oilcrops+Pulses+Spices+Starchy.Roots+Stimulants+Sugar.Crops+Sugar...Sweeteners+Treenuts+Vegetal.Products  
#                         +Vegetable.Oils+Vegetables+Miscellaneous+Obesity,data=final)[, -1])

x = scale(model.matrix(death.rate~Alcoholic.Beverages+Oilcrops+Stimulants
                       +Vegetable.Oils+Vegetable.Oils+SH.MED.NUMW.P3+SH.MED.PHYS.ZS+SH.DTH.INJR.ZS,
                       data=final_data)[, -1])
check=which(complete.cases(final_data))

y1.log = scale(log(final_data[check,]$death.rate+.00001))
library(glmnet)
lasso.cv.out1 = cv.glmnet(x, y1.log, alpha = 1, nfolds = 10)


y1.log
best.lam1 = lasso.cv.out1$lambda.min
best.lam1
log(best.lam1)
#summary(lasso.cv.out1)
#[1] -2.169011
# obtain the coefficient from Lasso
lasso.coef1 = predict(glmnet(x, y1.log, alpha = 1, lambda = best.lam1, 
                             standardize = FALSE, intercept = FALSE),
                      type = "coefficients")
lasso.coef1

#attach(mtcars)
#par(mfrow=c(3,2,2,2))
#par(mar=c(1,1,1,1))

yhat1 = predict( glmnet(x, y1.log, alpha = 1, lambda = best.lam1, 
                        standardize = FALSE, intercept = FALSE),
                 type = "response",
                 newx=x)
#yhat1 #predictions for death rate, negative deaths
y1.log 
# attr(,"scaled:center")
# [1] -4.854542
# attr(,"scaled:scale")
# [1] 2.214167
### quewtion here, how do we get the the coefficients?
drhat=yhat1*1.680283-0.7240206
par(mar=c(0.1,0.1,1,1))
par(mfrow=c(2,2))
plot(lasso.cv.out1)
plot(exp(yhat1*1.680283-0.7240206),exp(y1.log*1.680283-0.7240206))
hist(exp(yhat1*1.680283-0.7240206)-exp(y1.log*1.680283-0.7240206))
hist(y1.log)










