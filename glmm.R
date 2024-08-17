##02 generalized linear mixed effects model
library(lme4)#lmer() 
library(nlme)#lme()
library(lmtest) #lrtest()fuction
library(sjPlot) #for tab_model() function
library(MuMIn)#r.squredGLMM()
library(sjstats)# for icc() function
library(ciTools)#generate CI using add_ci() function
library(arm) #se_coef(), fix_coef
library(multcomp) #multicompare glht() function
library(pbkrtest) #Krmodcomp() function
library(simr)#power analysis powerSim() and powerCurve()
library(ggplot2)
library(effects)
library(glmmTMB)
library(lmerTest)
library(afex)
library(tidyverse)
library(DHARMa)
#########################################################################

##############################################################################
# Count-data GLMMs
aaa <- datAlpha
aaa$indx <- c(rep(1,6),2,2,3,2) # batch of sampling
summary(aaa)
library(GGally)
ggpairs(dplyr::select(aaa,-group,-indx)) # check to choose model

head(aaa)
dim(aaa)

#############################################################################
#model for poisson data

### null models
glmm03.null<-glmer(Observed~1 + (1|group), data=aaa,family=poisson(link=log))
summary(glmm03.null)

#ICC check
performance::icc(glmm03.null)

tab_model(glmm03.null)

### adding predictors in fixed effects: varying intercept model
glmm03.intercept<-glmer(Observed~group + (1|indx), data=aaa,family=poisson(link='log'))
glmm03.intercept <- glmer(Shannon~group + (1|indx), data=aaa,family=gaussian(link='identity'))
glmm03.intercept <- glmer(Simpson~group + (1|indx), data=aaa,family=Gamma(link='inverse'))
glmm03.intercept <- glmer(Simpson~group + (1|indx), data=aaa,family=gaussian(link='identity'))

summary(glmm03.intercept)
tab_model(glmm03.intercept)
lmtest::lrtest(glmm03.null,glmm03.intercept)
anova(glmm03.null,glmm03.intercept)

## diagnostics
##overdispersion test
DHARMa::testZeroInflation(glmm03.intercept)
testDispersion(glmm03.intercept)
 
#testDispersion(glmm03.intercept) #DHARMa
performance::check_overdispersion(glmm03.intercept)

## can add an "individual level random effect"
## (this is a common fix in overdispersed poisson models)
aaa$individual <- factor(1:nrow(aaa))
summary(aaa)
glmm03.intercept.b <- glmer(Observed~group + (1|indx)+(1|individual), 
                            data=aaa,family=poisson(link=log))
glmm03.intercept.b <- glmer(Shannon~group + (1|indx)+(1|individual), 
                            data=aaa,family=gaussian(link='identity'))
glmm03.intercept.b <- glmer(Simpson~group + (1|indx)+(1|individual), 
                            data=aaa,family=Gamma(link='inverse'))
glmm03.intercept.b <- glmer(Pielou~group + (1|indx)+(1|individual), 
                            data=aaa,family=gaussian(link='identity'))

summary(glmm03.intercept.b)
testDispersion(glmm03.intercept.b)
performance::check_overdispersion(glmm03.intercept.b)
performance::check_model(glmm03.intercept.b)

lmtest::lrtest(glmm03.intercept.b2,glmm03.intercept.a)
anova(glmm03.intercept.b,glmm03.intercept)

tab_model(glmm03.intercept.nb)

## Now check residuals
plot(glmm03.intercept.b, resid(., 'pearson')~fitted(.), 
     type=c('p', 'smooth'), abline=0) 


## we can use negative binomial for overdispersion
glmm03.intercept.nb <- glmer.nb(Observed~group+(1|indx),
                                 data=aaa,family=poisson(link=log))
summary(glmm03.intercept.nb)

testDispersion(glmm03.intercept.nb)
performance::check_overdispersion(glmm03.intercept.nb)

## AIC test
AIC(glmm03.intercept,glmm03.intercept.nb)
