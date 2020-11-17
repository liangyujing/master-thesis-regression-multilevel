---
title: "Aha and Heart Rate Variability"
output:
  pdf_document: default
  word_document: default
---

```{r}
library(readxl)
library(naniar)
library(tidyverse) 
library(MESS)
library(lmerTest)
library(lme4)
library(BayesFactor)
library(cluster)
library(usdm)
library(outliers)
library(ggplot2)
library(GGally)
library(tidyr)
library(psych)

```



```{r}
########################################
#GET RAW BEHAVIORAL DATA FROM CSV FILES 
#######################################

setwd("C:/Users/18829.DESKTOP-PG2BS5Q/Desktop/MT/Data/data/data/")
PP <- c(1:62)

# Read 1st PP

data = read_excel("1_insight_PsychoPhysics.xlsx")

data<- data %>% 
  transmute(
    subject = participant,
    age = leeftijd,
    language = specificieer,
    trial_counter = CRAT_exp_trials.thisTrialN,
    solution_location = location,
    CRAT1 = CRAT1,
    correct_answer = corrAns,
    AlternativeCorrect_answer = second_correct,
    answer = inputText,
    ACC = correct,
    RT = CRAT_response.rt,
    confidence = confidence_rating.response,
    solution_type = solution_strategy_response.keys)
# Read remaining PPs

for (i in PP[-1]){
  
  datastr <- paste("C:/Users/18829.DESKTOP-PG2BS5Q/Desktop/MT/Data/data/data/",i,   "_insight_PsychoPhysics.xlsx", sep = "")
  
  data_n = read_excel(datastr)
  
  data_n<- data_n %>% 
  transmute(
    subject = participant,
    age = leeftijd,
    language = specificieer,
    trial_counter = CRAT_exp_trials.thisTrialN,
    solution_location = location,
    CRAT1 = CRAT1,
    correct_answer = corrAns,
    AlternativeCorrect_answer = second_correct,
    answer = inputText,
    ACC = correct,
    RT = CRAT_response.rt,
    confidence = confidence_rating.response,
    solution_type = solution_strategy_response.keys)
  
  data <- rbind(data, data_n)
  
}
```

```{r}
data$trial_counter <- as.integer(factor(data$trial_counter,levels=unique(data$trial_counter))) 
data$subject <- as.character(as.factor(data$subject))

summary(data)
```

```{r}
#######################################
#Delete all practice trials
######################################
experimental_df<-data%>%
  tidyr::drop_na(trial_counter)

#############################
#Replace all cells with None by NA
##############################
experimental_df<- experimental_df%>% dplyr::na_if("None")
experimental_df<- experimental_df%>% dplyr::na_if("")

```


```{R}
####################################
#Place all values on one line per trial/solved word puzzle
library(dplyr)
experimental_df<- experimental_df %>%
  group_by(subject, trial_counter)%>%
  summarise_all(funs(first(na.omit(.))))

experimental_df$solution_type<-as.factor(experimental_df$solution_type)

summary(experimental_df)
```

```{r}
#################################
#Delete all word puzzles that were not solved
###################################
experimental_df<-experimental_df%>%
  tidyr::drop_na(answer)
  
```

```{r}
###########################################
#create factor of solution-type and convert RT and confidence to numeric values with a certain amount of decimal places left
experimental_df$solution_type<-as.factor(experimental_df$solution_type)
experimental_df$RT<-as.numeric(experimental_df$RT, digits = 3)
experimental_df$confidence<-as.numeric(experimental_df$confidence, digits = 2)
summary(experimental_df)

#Some preliminary descriptives for correct and incorrectly solved trials
solutioneBy(experimental_df$confidence, group=experimental_df$solution_type,mat=FALSE,type=3)
solutioneBy(experimental_df$RT, group=experimental_df$solution_type,mat=FALSE,type=3)
describeBy(experimental_df$ACC, group=experimental_df$solution_type,mat=FALSE,type=3)

#idem only for correctly solved trials
preliminary_data<-experimental_df%>%
  filter(ACC == 1)%>%
  group_by(solution_type)%>%
  summarise(mean_conf = mean(confidence), mean_RT = mean(RT))
  
```

```{r}
############################################
#create number of correctly solved insight and non-insight word puzzles per participant 
###########################################
number_insight<-experimental_df%>%
  filter(solution_type == 1, ACC == 1)%>%
  group_by(subject)%>%
  count(ACC)

#number of correctly non-insight
number_NonInsight<-experimental_df%>%
  filter(solution_type == 2, ACC == 1)%>%
  group_by(subject)%>%
  count(ACC)

```

```{r}
###########################################################
#attach the number of trials solved with insight and non-insight to the df
#######################################################################
experimental_df$nrInsight <- NaN
for (s in unique(number_insight$subject)){
  experimental_df$nrInsight[experimental_df$subject==s] <- number_insight$n[number_insight$subject==s]
} 

experimental_df$nrNonInsight <- NaN
for (s in unique(number_NonInsight$subject)){
  experimental_df$nrNonInsight[experimental_df$subject==s] <- number_NonInsight$n[number_NonInsight$subject==s]
} 
```


```{r}
##############################
#Seperate Bilingual PP
################################
#Create a variable to seperate the natural Dutch speaking participants from the participants with another mother tongue
experimental_df$mother_tongue<- ifelse(experimental_df$language == "Nederlands" | experimental_df$language == "nederlands" | experimental_df$language == "dutch", 1, 0)

#check all subjects available
num_subj<-experimental_df%>%
  count(subject)
  
```

```{R}
###############################################
#add baseline RMSSD to data frame
#################################################
#create df with HRV data
artiifact_HRV_df = read_excel("C:/Users/18829.DESKTOP-PG2BS5Q/Desktop/MT/Data/data/data/artiifact_HRV.xlsx")

#replace NaN in df with real NaN R-wise
artiifact_HRV_df<- artiifact_HRV_df%>%dplyr::na_if("NaN")

#add to df with behavioral data RMSSD, High-frequency HRV, IBI reject
experimental_df$baseline_RMSSD <- NaN
for (s in unique(artiifact_HRV_df$subject)){
  experimental_df$baseline_RMSSD[experimental_df$subject==s] <- artiifact_HRV_df$Artiifact_RMSSD[artiifact_HRV_df$subject==s]
} 

experimental_df$HF_HRV <- NaN
for (s in unique(artiifact_HRV_df$subject)){
  experimental_df$HF_HRV[experimental_df$subject==s] <- artiifact_HRV_df$Artiifact_HF[artiifact_HRV_df$subject==s]
} 

experimental_df$num_reject_IBI <- NaN
for (s in unique(artiifact_HRV_df$subject)){
  experimental_df$num_reject_IBI[experimental_df$subject==s] <- artiifact_HRV_df$rejected_IBI[artiifact_HRV_df$subject==s]
} 

summary(experimental_df)
```

```{R}
########################################
#Create data frames to perform analysis on
#####################################
#first create additional variable total number solved
experimental_df<-experimental_df%>%
  mutate(total_correct_solved = nrInsight + nrNonInsight)

#create two data frames one with correct and incorrect responses and one with only correct responses
mixedef_correct<-experimental_df %>%
  filter(ACC == 1) %>% 
  filter(solution_type != 3) %>% 
  mutate(baseline_RMSSD = as.numeric(baseline_RMSSD),
         HF_HRV = as.numeric(HF_HRV),
         num_reject_IBI = as.numeric(num_reject_IBI))

mixedef_correct_incorrect<-experimental_df %>%
  filter(solution_type != 3) %>% 
  mutate(baseline_RMSSD = as.numeric(baseline_RMSSD),
         HF_HRV = as.numeric(HF_HRV),
         num_reject_IBI = as.numeric(num_reject_IBI))
summary(mixedef_correct_incorrect)
```
```{r}
#change solution-type to 0 = non-insight and 1 = insight
mixedef_correct<-mixedef_correct%>%
  mutate(solution_type = if_else(solution_type == 2, 0, 1))

#create factor of solution-type
mixedef_correct$solution_type<-factor(mixedef_correct$solution_type, levels = c(0, 1), labels = c("Non-Insight", "Insight"))
mixedef_correct<-mixedef_correct%>%
  filter(subject != "37669" & subject != "39823" & baseline_RMSSD > 0)
summary(mixedef_correct)
#correct incorrect
mixedef_correct_incorrect<-mixedef_correct_incorrect%>%
  mutate(solution_type = if_else(solution_type == 2, 0, 1))

#create factor of solution-type
mixedef_correct_incorrect$solution_type<-factor(mixedef_correct_incorrect$solution_type, levels = c(0, 1), labels = c("Non-Insight", "Insight"))

#exclude participant number 37669 only 17% solved correct and only with insight, also bilangual with Turkish as mothertongue
#subject 39823 was by accident included but did not adhere to the instructions, she/he slept to little, sleep<6h
mixedef_correct_incorrect<-mixedef_correct_incorrect%>%
  filter(subject != "37669" & subject != "39823" & baseline_RMSSD > 0)

#reverse ACC score
mixedef_correct_incorrect<-mixedef_correct_incorrect%>%
  mutate(ACC_reverse = if_else(ACC == 0, 1, 0))

#create factor of the reverse ACC score
mixedef_correct_incorrect$ACC_reverse<-factor(mixedef_correct_incorrect$ACC_reverse, levels = c(0, 1), labels = c("correct", "Incorrect"))
```



```{r}
#Transform HRV measures, to increase progressing to normal distribution, so that model estimation is beter afterwards
#correct incorrect
mixedef_correct_incorrect<-mixedef_correct_incorrect%>%
  mutate(lnRT = log10(RT),
         lnconfidence = 1/(max(confidence+1) - confidence))
mixedef_correct<-mixedef_correct%>%
  mutate(lnRT = log10(RT),
         lnconfidence = 1/(max(confidence+1) - confidence))
         
summary(mixedef_correct_incorrect)
describe.by(mixedef_correct_incorrect$baseline_RMSSD, group=mixedef_correct_incorrect$ACC)
describe.by(mixedef_correct$baseline_RMSSD, group=mixedef_correct$solution_type)

describe.by(mixedef_correct_incorrect$lnconfidence, group=mixedef_correct_incorrect$ACC)
view(describe.by(mixedef_correct_incorrect$solution_type, group=mixedef_correct_incorrect$ACC))
view(describe.by(mixedef_correct_incorrect$baseline_RMSSD, group=mixedef_correct_incorrect$solution_type))

view(describe.by(mixedef_correct$baseline_RMSSD, group=mixedef_correct$solution_type))
table(mixedef_correct$RT, group=mixedef_correct$solution_type)
table(mixedef_correct$solution_type)

```



```{r}
##############################################
#mixed effect logistic and linear regression
#############################################
#Null models linear and logistic
# ACC
model_empty_ACC<-glmer(ACC ~ (1|CRAT1) + (1|subject),
                  data = mixedef_correct_incorrect, 
                  family = binomial("logit"), 
                  control=glmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=2e5)))
summary(model_empty_ACC)

####RT
model_empty_linear_RT<-lmer(lnRT ~ (1|CRAT1) + (1|subject), data = mixedef_correct, 
                  REML = FALSE, 
                  control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(model_empty_linear_RT)

#### confidence
model_empty_linear_con<-lmer(lnconfidence ~ (1|CRAT1) + (1|subject), data = mixedef_correct, 
                  REML = FALSE, 
                  control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(model_empty_linear_con)

#Calculate the IntraClass Correlation =ICC
#for word puzzles
ICC_WP <- 0.2349/(0.2349 + 0.6983 + 3.29)
ICC_WP
#this is 6%, small variation between word puzzles

ICC_subject <- 0.6983/(0.2349 + 0.6983 + 3.29)
ICC_subject
#this is 17%, sufficient variations between participants
library("lme4")

```


```{r fig.height = 6, fig.width = 8, fig.align = "center"}
## RT
#build model
model_RT1<-lmer(lnRT~solution_type*baseline_RMSSD + (1|CRAT1) + (1|subject), data = mixedef_correct, 
                  REML = FALSE, 
                  control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(model_RT1)
anova(model_RT1, model_empty_linear_RT)
confint(model_RT1,parm="beta_",method="Wald")

###################
##visaulize
library(effects)
model_RT1_1 <- effect('solution_type*baseline_RMSSD', model_RT1,
                        se=TRUE)
model_RT1_1_DF<-as.data.frame(model_RT1_1)
head(model_RT1_1_DF)
model_RT1_1_DF$solution_type<-factor(model_RT1_1_DF$solution_type,
              levels=c("Non-Insight","Insight"),
              labels=c("Analysis", "Insight"))

Plot_RT<-ggplot(data=model_RT1_1_DF, aes(x=baseline_RMSSD, y=fit, group=solution_type))+
    geom_line(size=2, aes(color=solution_type))+
    geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=solution_type),alpha=.2)+
    xlim(c(0, 150)) +
    ylab("Solution time")+
    xlab("Average of baseline HRV")+
    ggtitle("Solution type and baseline HRV as solution time Predictors")+
    theme_bw()+
    theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
Plot_RT


# ASSUMPTIONS LMM #

###################

# See Field and Winter
# Get predicted values

mixedef_correct$fitted <- fitted(model_RT1)
RTass<-data.frame(mixedef_correct)


# 1. LINEARITY

plot(fitted(model_RT1),residuals(model_RT1)); abline(h=0)

# lines(lowess(fitted(M_CxB_fullrandom),residuals(M_CxB_fullrandom)))

# Okayish

 

# 2. MULITCOLLINEARITY

vif(model_RT1)

# VIF > 10 than predictor has too strong linear relationship with other predictor

# Mutlicollinearity may be a problem when interaction cross levels in hierarchy (cross-level interactions) --> solution: centering

 

# 3. HOMOSCEDASTICITY (variance of    your data should be approximately              equal    across the          range    of your predicted           values)

plot(fitted(model_RT1),residuals(model_RT1)^2); abline(h=0)

# Not to great... Larger residuals with large fitted values

 

# 4. NORMALLY DISTRIBUTED RESIDUALS (least important)

hist(residuals(model_RT1), breaks = 60)

qqnorm(residuals(model_RT1))

qqline(residuals(model_RT1))

# Not to great...

 

# 4. ABSENCE OF INFLUENTIAL POINTS

# Outliers

# outlier =  standardized residual (residual/sd) > 3.29.

# More than 1% larger than 2.6--> level of error unacceptable

# More than 5% larger than 2 --> level of error unacceptable

RTass$stand.resid <- resid(model_RT1, scaled=T) # Compute standardized residuals

RTass$large.stand.resid <- RTass$stand.resid > 2 | RTass$stand.resid < -2 # Check if stand resid is too large

sum(RTass$large.stand.resid, na.rm = T)/length(RTass$large.stand.resid[!is.na(RTass$large.stand.resid)]) # % of stand resid are too large

# --> 2.7% > 2.6  Problematic?

# --> 4.8% > 2    Okay

 

largeResid <- RTass[!is.na(RTass$large.stand.resid)&RTass$large.stand.resid,] # Check which stand resid are too large

 

# Influential cases

library(influence.ME)

inflcases<-influence(model_RT1, group = "subject") # used to compute measures of influential data
inflCRAT1<-influence(model_RT1, group = "CRAT1")
 

dfbetas(inflcases)
dfbetas(inflCRAT1)    # cut-off = 2/sqrt(n) = 0.2030692 (?.2981424); n=# subjects. https://journal.r-project.org/archive/2012-2/RJournal_2012-2_Nieuwenhuis~et~al.pdf

m<-plot(inflcases,       # cut off = values        that       are different     by          at              least      half       of          the        absolute             value    of          the              slope. Winter

     which="dfbetas",

     xlab="DFbetaS",

     ylab="subject")

m

n<-plot(inflCRAT1,       # cut off = values        that       are different     by          at              least      half       of          the        absolute             value    of          the              slope. Winter

     which="dfbetas",

     xlab="DFbetaS",

     ylab="subject")

n


cooks.distance(inflcases, sort=T)# overall influence of a case on the model: > 1 is concern (Field)
cooks.distance(inflCRAT1, sort=T)

# > 4/97 = 0.04123711, n=# subjects. https://journal.r-project.org/archive/2012-2/RJournal_2012-2_Nieuwenhuis~et~al.pdf

plot(inflcases, which="cook",

    cutoff=(4/45), sort=TRUE,

     xlab="Cook's Distance",

     ylab="Subject")

sigtest(inflcases, test=-1.96)

plot(inflCRAT1, which="cook",

    cutoff=(4/68), sort=TRUE,

     xlab="Cook's Distance",

     ylab="Subject")

sigtest(inflCRAT1, test=-1.96)


help(sigtest)



```



```{r}
model_confidence<-lmer(lnconfidence~solution_type*baseline_RMSSD + (1|CRAT1) + (1|subject), 
                       data = mixedef_correct, 
                  REML = FALSE, 
                  control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(model_confidence)
eta_sq(model_confidence)

confint(model_confidence,parm="beta_",method="Wald")
anova(model_confidence, model_empty_linear_con)



##visaulize
library(effects)
model_con_1 <-
effect('solution_type*baseline_RMSSD', model_confidence,
se = TRUE)
model_con1_1_DF <- as.data.frame(model_con_1)
head(model_con1_1_DF)
model_con1_1_DF$solution_type <- factor(
model_con1_1_DF$solution_type,
levels = c("Non-Insight", "Insight"),
labels = c("Analysis", "Insight")
)

Plot_con <-
ggplot(data = model_con1_1_DF, aes(x = baseline_RMSSD, y = fit, group =
solution_type)) +
geom_line(size = 2, aes(color = solution_type)) +
geom_ribbon(aes(
ymin = fit - se,
ymax = fit + se,
fill = solution_type
), alpha = .2) +
ylab("Confidence") +
xlab("Average of baseline HRV") +
xlim(c(0, 150)) +
ggtitle("Solution type and baseline HRV as confidence Predictors") +
theme_bw() +
theme(
text = element_text(size = 12),
legend.text = element_text(size = 12),
legend.direction = "horizontal",
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
legend.position = "top"
)
Plot_con

###################

# ASSUMPTIONS LMM #

###################

# See Field and Winter
# Get predicted values

mixedef_correct$fitted <- fitted(model_confidence)
RTass <- data.frame(mixedef_correct)

# 1. LINEARITY

plot(fitted(model_confidence),residuals(model_confidence)); abline(h=0)

# lines(lowess(fitted(M_CxB_fullrandom),residuals(M_CxB_fullrandom)))

# Okayish

 

# 2. MULITCOLLINEARITY

vif(model_confidence)

# VIF > 10 than predictor has too strong linear relationship with other predictor

# Mutlicollinearity may be a problem when interaction cross levels in hierarchy (cross-level interactions) --> solution: centering

 

# 3. HOMOSCEDASTICITY (variance of    your data should be approximately              equal    across the          range    of your predicted           values)

plot(fitted(model_confidence),residuals(model_confidence)^2); abline(h=0)

# Not to great... Larger residuals with large fitted values

 

# 4. NORMALLY DISTRIBUTED RESIDUALS (least important)

hist(residuals(model_confidence), breaks = 60)

qqnorm(residuals(model_confidence))

qqline(residuals(model_confidence))

# Not to great...

 

# 4. ABSENCE OF INFLUENTIAL POINTS

# Outliers

# outlier =  standardized residual (residual/sd) > 3.29.

# More than 1% larger than 2.6--> level of error unacceptable

# More than 5% larger than 2 --> level of error unacceptable

RTass$stand.resid <- resid(model_confidence, scaled=T) # Compute standardized residuals

RTass$large.stand.resid <- RTass$stand.resid > 2 | RTass$stand.resid < -2 # Check if stand resid is too large

sum(RTass$large.stand.resid, na.rm = T)/length(RTass$large.stand.resid[!is.na(RTass$large.stand.resid)]) # % of stand resid are too large

# --> 2.7% > 2.6  Problematic?

# --> 4.8% > 2    Okay

 

largeResid <- RTass[!is.na(RTass$large.stand.resid)&RTass$large.stand.resid,] # Check which stand resid are too large

 

# Influential cases

library(influence.ME)

inflcases<-influence(model_confidence, group = "subject") # used to compute measures of influential data
inflCRAT1<-influence(model_confidence, group = "CRAT1")
 

dfbetas(inflcases)
dfbetas(inflCRAT1)    # cut-off = 2/sqrt(n) = 0.2030692 (?.2981424); n=# subjects. https://journal.r-project.org/archive/2012-2/RJournal_2012-2_Nieuwenhuis~et~al.pdf

m<-plot(inflcases,       # cut off = values        that       are different     by          at              least      half       of          the        absolute             value    of          the              slope. Winter

     which="dfbetas",

     xlab="DFbetaS",

     ylab="subject")

m

n<-plot(inflCRAT1,       # cut off = values        that       are different     by          at              least      half       of          the        absolute             value    of          the              slope. Winter

     which="dfbetas",

     xlab="DFbetaS",

     ylab="subject")

n


cooks.distance(inflcases, sort=T)# overall influence of a case on the model: > 1 is concern (Field)
cooks.distance(inflCRAT1, sort=T)

# > 4/97 = 0.04123711, n=# subjects. https://journal.r-project.org/archive/2012-2/RJournal_2012-2_Nieuwenhuis~et~al.pdf

plot(inflcases, which="cook",

    cutoff=(4/45), sort=TRUE,

     xlab="Cook's Distance",

     ylab="Subject")

sigtest(inflcases, test=-1.96)

plot(inflCRAT1, which="cook",

    cutoff=(4/68), sort=TRUE,

     xlab="Cook's Distance",

     ylab="Subject")

sigtest(inflCRAT1, test=-1.96)

```



```{r}
model_ACC<-glmer(ACC~solution_type*baseline_RMSSD + (1|CRAT1) + (1|subject), data = mixedef_correct_incorrect, 
                  family = binomial("logit"), 
                  control=glmerControl(optimizer="Nelder_Mead",optCtrl=list(maxfun=2e5)))
summary(model_ACC)

#compare
anova(model_ACC, model_empty_ACC)

confint(model_ACC,parm="beta_",method="Wald")


summary(mixedef_correct_incorrect$ACC)

##visaulize
library(effects)
model_acc_1 <- effect('solution_type*baseline_RMSSD', model_ACC,
                        se=TRUE)
model_acc_1_DF<-as.data.frame(model_acc_1)
head(model_acc_1_DF)
model_acc_1_DF$solution_type<-factor(model_acc_1_DF$solution_type,
              levels=c("Non-Insight","Insight"),
              labels=c("Analysis", "Insight"))

Plot_acc<-ggplot(data=model_acc_1_DF, aes(x=baseline_RMSSD, y=fit, group=solution_type))+
    geom_line(size=2, aes(color=solution_type))+
    geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=solution_type),alpha=.2)+
    ylab("Odds of solving the problem correctly")+
    xlab("Average of baseline HRV")+
    ggtitle("Solution type and baseline HRV as accuracy predictors")+
    theme_bw()+
    theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")

Plot_acc

##################################
#Model Diagnostics
#################################
#check multicollinearity
library(performance)
#check multicolinearity
check_collinearity(model_ACC)
#ok all less than 4

####################################
#Random intercept diagnostics
####################################
#Plot random effect with dotplot
library(lattice)
dotplot_model3<- dotplot(ranef(model_ACC, condVar = TRUE))
dotplot_model3

#distribution of random intercepts SDs CRAT1
Rand_model3_CRAT<-ranef(model_ACC, condVar = TRUE, whichel = "CRAT1")
Rand_model3_CRAT_df<-as.data.frame(Rand_model3_CRAT)

qplot(Rand_model3_CRAT_df$condval, geom = "histogram") +#standardized values of the slopes per word puzzle
  stat_bin(bins = 10)
#This looks like normal distributed

#distribution of random intercepts SDs subject
Rand_model3_subject<-ranef(model_full_3, condVar = TRUE, whichel = "subject")
Rand_model3_subject_df<-as.data.frame(Rand_model3_subject)

qplot(Rand_model3_subject_df$condval, geom = "histogram") +#standardized values of the slopes per word puzzle
  stat_bin(bins = 10)
#This looks, more or less, like normal distributed

###################################################
#main model diagnostics
####################################################
#residual deviance analysis
library(gridExtra)
plot_model3_1 <- plot(model_full_3,id=0.05,idLabels=~.obs)
plot_model3_2 <- plot(model_full_3,ylim=c(-1.5,1),type=c("p","smooth"))
grid.arrange(plot_model3_1,plot_model3_2,nrow=1)
#Seems some outliers in the model, 

#three step outlier deletion for three cut-offs 1.96/2.58/3
#deviation within 95% boundary = 1.96SD
new_model3_outlier_196<- subset(mixedef_correct_incorrect,
                        abs(resid(model_full_3,"pearson"))<1.96)

refit_model3__196 <- update(model_full_3, data = new_model3_outlier_196)
summary(refit_model3__196)
#Results remain the same, RT becomes sign

#deviation within 99% boundary = 2.58SD
new_model3_outlier_258<- subset(mixedef_correct_incorrect,
                             abs(resid(model_full_3,"pearson"))<2.58)

refit_model3_258 <- update(model_full_3, data = new_model3_outlier_258)
summary(refit_model3_258)
#results remain the same, in general

#deviation within 99.7% boundary = 3SD
new_model3_outlier_300<- subset(mixedef_correct_incorrect,
                             abs(resid(model_full_3,"pearson"))<3)

refit_model3_300 <- update(model_full_3, data = new_model3_outlier_300)
summary(refit_model3_300)
#results remain the same

#by-subject model influence
library(influence.ME)
fit_model3_BySubjectINfluence <-  influence(model_full_3, group="subject")

#Create data frame with cooks distance for each subject
cd_model3=cooks.distance(fit_model3_BySubjectINfluence)
#General rule-of-thumb value above 3*mean cooks distance can be considered influential cases
mean(cd_model3)*3

#Histogram of cook's distance:
qplot(cd_model3) + xlab("Histogram Cook's distance (subjects)")

#which subjects are potential influential cases
cd_model3[order(-cd_model3),,drop = FALSE]

#refit model without influential subject which 38455
fit_model3_MinSubject38455 <- update(model_full_3, . ~ ., data=filter(mixedef_correct_incorrect, subject!=38455))
summary(fit_model3_MinSubject38455)
#RT*RMSSD is changes somewhat to being almost borderline sign, results remain generally the same (seems to be something there for this interactioneffect)

#by-CRAT1 model influence
fit_model3_ByCRAT1INfluence <-  influence(model_full_3, group="CRAT1")

#Create data frame with cooks distance for each CRAT1
cd_model3_CRA=cooks.distance(fit_model3_ByCRAT1INfluence)
#General rule-of-thumb value above 3*mean cooks distance can be considered influential cases
mean(cd_model3_CRA)*3#0.0433

#Histogram of cook's distance:
qplot(cd_model3_CRA) + xlab("Histogram Cook's distance (CRAT)")

#which CRAT are potential influential cases
cd_model3_CRA[order(-cd_model3_CRA),,drop = FALSE]
#CRAT wandeling, regen, rust

#refit model without influential CRAT1
fit_model3_MinCRAT1 <- update(model_full_3, . ~ ., data=filter(mixedef_correct_incorrect,
                                                                           CRAT1!= "wandeling" & CRAT1!= "regen" & CRAT1 != "rust"))
summary(fit_model3_MinCRAT1)
#result remains the same

#effect size
library(sjstats)
eta_sq(model_ACC)
eta_sq(model_RT1)
eta_sq(model_con_1)



```















