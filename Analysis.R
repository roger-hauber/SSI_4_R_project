### Script for Data Analysis EEG-4

#all libraries
library(R.matlab)
library(languageR)
library(lme4)
library(MASS)
library(ggplot2) #cookbook for R/ Graphs
library(hexbin)
library(memisc)
library(reshape)
library(reshape2) #melt and cast -> restructure and aggregate data
library(data.table)
library(coin) #for permutation tests
library(psych)
library(doBy)
library(heplots)
library(plyr) #necessary for ddply
library(matrixStats) 
library(foreign) 
library(Hmisc)
library(lmerTest)
library(ez)
library (stringr)
library(gridExtra)
library(grid)
library(RWiener)
library(remef)
library(tidyverse)
library(desc)

#set working directory

setwd("log") # log file data path


fl <- list.files() #lists files in folder
testf  <- grep("", fl) # here, the log files are named as "1.txt", "2.txt" and so on. between the "" one can specify a string for file selection (e.g., "test" or "learning", if the files are named "VP1_test.txt" and "VP1_learning.txt)
for (i in testf) # loop reading txt-file by txt-file as table
{ tmp <- (read.table(fl[i] , header = TRUE, sep = "", #dec = ",", bad... kills everything!
                     fill = TRUE))
if (i==1)
{a1 <- tmp} # at the first iteration, create a1
else {a1 <- rbind(a1,tmp)} ### rbind glues currently loaded file (tmp) to the end of dataframe a1
}

str(a1) #check the data

### a couple of changes to the data before dropping any rows

#mean centered ordinal position
a1$g_ng_count_cent[a1$g_ng == "g1" & !a1$Cond == 206] <- scale(a1$g_ng_count[a1$g_ng == "g1" & !a1$Cond == 206], scale = FALSE, center = TRUE)
a1$g_ng_count_cent[a1$g_ng == "g2" & !a1$Cond == 206] <- scale(a1$g_ng_count[a1$g_ng == "g2" & !a1$Cond == 206], scale = FALSE, center = TRUE)

#add a variable for position in category
# you can actually use the ordinAll variable minus 100

a1$pos_in_cat <- a1$ordinaAll-100

a1$g_ng_count_1_5 <- a1$g_ng_count_cent+3

# variable pos_in_cat minus the g_ng_count expresses the amount of partner go trials preceding each go trial of the part

a1$alt_g_ng <- a1$pos_in_cat-a1$g_ng_count_1_5

# and mean center that variable

a1$alt_g_ng_cent <- scale(a1$alt_g_ng, scale = FALSE)


#recoded DG variable
a1$DG <- factor(a1$DG, levels = c(51:54))
a1$DG <- recode_factor(a1$DG, "51" = "1", "52" = "2", "53" = "3", "54" = "4")

#mean centered DG variable
a1$DG_cent <- scale(as.numeric(a1$DG), scale = FALSE, center = TRUE)


### drop fillers, errors, and premature/missing reaction times (0-200) and "no-gos"

a2 <- a1[!a1$Cond == 206 & a1$errorcode == 0 & a1$RT > 200 & a1$RT < 2000 & !a1$g_ng == "n", ]

### now separate the data of the individual participants from the same session 
#(dropping the go trials of participants in the other participants joint naming condition, i.e. cond = 201 and cond = 203)

a3 <- rbind(a2[a2$Cond %in% c("200", "202"), ], a2[a2$Cond %in% c("204", "205"), ])

#check the data
str(a3)

#change subject variable to actual subject number via session nr *2 / session nr *2-1

for (i in 1:length(unique(a3$Session))) {

  a3$subject[a3$Cond %in% c("200", "202") & a3$Session == i] <- i*2-1 

  a3$subject[a3$Cond %in% c("204", "205") & a3$Session == i] <- i*2 

}
#and reorder by subject (ascending)

a3 <- a3[order(a3$subject), ]


## now calculate error rate (ie trial loss rate for each subject)
# done via commparing the actual number of trials per participant with the minimum (applying 20 % criterion, i.e. trials >= 287)

#special case: drop all rows from subject 1 before 276 (ie. before first break) because of auditory masking problems

a3 <- a3[!(rownames(a3) %in% c(1:275) & a3$subject == 1), ]

#compute the number of valid trials per subject
for (i in 1:length(unique(a3$subject))) {
  a3$valid[a3$subject == i] <- length(a3$subject[a3$subject == i])
}

# subset according to criterion
a4 <- a3[a3$valid >= 287, ]

#a4 <- a3

#quick check which and how many subjects were excluded because of trial loss

excluded <- setdiff(unique(a3$subject), unique(a4$subject))
length(excluded)


# set factors
a4$Cond <- factor(a4$Cond, levels = as.character(unique(a4$Cond)))
a4$Cond <- recode_factor(a4$Cond, "202" = "SN", "205" = "SN", "200" = "JN", "204" = "JN")


a4$subject <- as.factor(a4$subject)

a4$KatCode <- as.factor(a4$KatCode)

#add a numeric code for each item and set it to factor

a4$ItemID <- if_else(a4$Nr < 10, as.integer(paste0(as.character(a4$KatCode), "0", as.character(a4$Nr))), 
                     as.integer(paste0(as.character(a4$KatCode), as.character(a4$Nr))))

a4$ItemID <- as.factor(a4$ItemID)

a4$Item <- as.factor(a4$Item)

#change Kueche in üebergeordnete Kategorie == Werkzeug to "Werkzeug_Kueche"
a4$Semantische.Kategorie <- as.character(a4$Semantische.Kategorie)

a4[a4$Semantische.Kategorie == "Kueche" & a4$uebergeordnete.Kategorie == "Werkzeug", "Semantische.Kategorie"] <- "Werkzeug_Kueche"

a4$Semantische.Kategorie <- as.factor(a4$Semantische.Kategorie)

##### Analysis of behavioral data 



# set contrasts
contrasts(a4$Cond) <- contr.sum(2)
contrasts(a4$Cond) <- c(0.5, -0.5)
contrasts(a4$DG) <- contr.sdif(4) ### NOTE: maybe mean-center DG? Otherwise random structure explodes

#possible transformations: check boxcox of the regular linear model and transform accordingly..
boxcox(lm(RT~Cond*g_ng_count*DG, data = a4)) #in this case: log(RT)

#polynomial contrast for g_ng_count
a4$g_ng_count <- as.factor(a4$g_ng_count)


contrasts(a4$g_ng_count) <- contr.poly(5)


#### first model

#using buildmer approach from alex (taken from Annas Script)
library(buildmer)

build_mod0 <- buildmer(log(RT)~ Cond*g_ng_count_cent*DG+(Cond*g_ng_count_cent*DG|subject) +(Cond*g_ng_count_cent*DG|KatCode) + 
                         (Cond*g_ng_count_cent*DG|ItemID),
                       data = a4,
                       REML = FALSE, # or TRUE
                       control = lmerControl(calc.derivs = FALSE,
                                             optimizer = "bobyqa",
                                             optCtrl = list(maxfun = 2e5)),
                       buildmerControl = buildmerControl(ddf = "Satterthwaite", # or "Kenward-Roger"
                                                         direction = c("backward", "backward"),
                                                         elim = function(logp) exp(logp) >= .20))

### 

mod0 <- lmer(log(RT) ~ Cond*g_ng_count_cent*DG +(Cond*g_ng_count_cent|ItemID) + (Cond*g_ng_count_cent|subject) + (Cond*g_ng_count_cent|KatCode), 
             data = a4, control = lmerControl(calc.derivs = FALSE), REML=FALSE)

summary(mod0)

# write the table to a doc
mod_0_table <- RTF(file = "mod_0_table.doc")

addParagraph(mod_0_table, "Modell mit allen Stufen von Ordinal Position\n")
addTable(mod_0_table, as.data.frame(cbind(rownames(summary(mod0)$coefficients), 
                                          round(summary(mod0)$coefficients, 2))))
addParagraph(mod_0_table, "Modell ohne Ordinal Position 5\n")
addTable(mod_0_table, as.data.frame(cbind(rownames(summary(mod_1_4)$coefficients), 
                                          round(summary(mod_1_4)$coefficients, 2))))
done(mod_0_table)


# do a model without ord_pos 5

mod_1_4 <- lmer(log(RT) ~ Cond*g_ng_count_cent*DG +(Cond*g_ng_count_cent|ItemID) + (Cond*g_ng_count_cent|subject) + (Cond*g_ng_count_cent|KatCode), 
                data = a4[a4$g_ng_count_cent %in% c(-2, -1, 0, 1), ], control = lmerControl(calc.derivs = FALSE), REML=FALSE)
#do a PCA of random effects
PCA <- rePCA(mod0)

# PCA suggests only 5 parameters for ItemID and subject and 4 parameters for KatCode
# but first lets force correlation parameters to zero using || in the model syntax

mod1 <- lmer(log(RT) ~ Cond*g_ng_count_cent*DG_cent + (Cond*g_ng_count_cent*DG_cent || ItemID) + (Cond*g_ng_count_cent*DG_cent || subject) + (Cond*g_ng_count_cent*DG_cent || KatCode), a4, REML = FALSE)

mod2 <- lmer(log(RT) ~ Cond*g_ng_count_cent*DG_cent + (1|ItemID) + (0 + Cond|ItemID) + (0 + g_ng_count_cent|ItemID) + (0 + DG_cent|ItemID), a4, REML = FALSE)

#nested models
## for SN only
mod1 <- lmer(log(RT)~ g_ng_count_cent*DG_cent+(g_ng_count_cent|subject) +(1|ItemID),  a4[a4$Cond == "SN", ], REML=FALSE)

## for JN only
mod2 <- lmer(log(RT)~ g_ng_count_cent*DG_cent+(g_ng_count_cent|subject) +(1|ItemID),  a4[a4$Cond == "JN", ], REML=FALSE)


######

#Models of data but with alternative g_ng
#polynomial contrast for alt_g_ng
a4$alt_g_ng <- as.factor(a4$alt_g_ng)


contrasts(a4$alt_g_ng) <- contr.poly(6)


build_mod0 <- buildmer(log(RT)~ Cond*alt_g_ng*DG_cent+(Cond*alt_g_ng*DG_cent|subject) +(Cond*alt_g_ng*DG_cent|KatCode) + 
                         (Cond*alt_g_ng*DG_cent|ItemID),
                       data = a4,
                       REML = FALSE, # or TRUE
                       control = lmerControl(calc.derivs = FALSE,
                                             optimizer = "bobyqa",
                                             optCtrl = list(maxfun = 2e5)),
                       buildmerControl = buildmerControl(ddf = "Satterthwaite", # or "Kenward-Roger"
                                                         direction = c("backward", "backward"),
                                                         elim = function(logp) exp(logp) >= .20))
# Which model did we end up with?
mod_build <- build_mod0@model
formula(mod_build)

## let's do it without buildmer (leave out DG from random structure)

alt_mod_0 <- lmer(log(RT) ~ Cond*alt_g_ng_cent*DG_cent + (1 | ItemID) + 
                    (1 | subject) + 
                    (1 | KatCode), a4, REML = FALSE)


############ LMMs

mod1 <- lmer(log(RT) ~ Cond*g_ng_count_cent*DG_cent + ( 1| Item) + 
               (1 | subject) + 
               (1 | Semantische.Kategorie), a4, REML = FALSE)

# let's use influence.ME to look at which items, categories and subjects influence the estimates disproportionately

library(influence.ME)

#subject
influence.subject <- influence(mod1, "subject")
or.test.s <- influence.subject$or.test #get original t-values
alt.test.s <- influence.subject$alt.test #get modified t-values when omitting one subject
or.fixed.s <- influence.subject$or.fixed
alt.fixed.s <- influence.subject$alt.fixed #get modified fixed effects when omitting one subject
or.se.s <- influence.subject$or.se
alt.se.s <- influence.subject$alt.se #get modified se when omitting one subject
or.vcov.s <- influence.subject$or.vcov
alt.vcov.s <- influence.subject$alt.vcov #get modified covariances when omitting one subject

#item
influence.target <- influence(mod1, "Item")
or.test.t <- influence.target$or.test
alt.test.t <- influence.target$alt.test
or.fixed.t <- influence.target$or.fixed
alt.fixed.t <- influence.target$alt.fixed
or.se.t <- influence.target$or.se
alt.se.t <- influence.target$alt.se
or.vcov.t <- influence.target$or.vcov
alt.vcov.t <- influence.target$alt.vcov

#categories
influence.cat <- influence(mod1, "Semantische.Kategorie")
or.test.c <- influence.target$or.test
alt.test.c <- influence.target$alt.test
or.fixed.c <- influence.target$or.fixed
alt.fixed.c <- influence.target$alt.fixed
or.se.c <- influence.target$or.se
alt.se.c <- influence.target$alt.se
or.vcov.c <- influence.target$or.vcov
alt.vcov.c <- influence.target$alt.vcov

# Cookâs Distance is a measure indicating to what extent model parameters are influenced by the influential data (of individual subjects and trials).
# Cookâs D is calculated by removing the ith data point from the model and recalculating the regression. It summarizes how much all the values in the regression model change when the ith observation is removed. 
#Cookâs D can be interpreted as the standardized average squared difference between the estimates with and without unit j.
##Belsley, Kuh, and Welsch recommend 4/n (=28/360/36) as a size-adjusted cutoff (subjects: 0.143; targets: 0.011; categories: 0.111)
cooks.s <- cooks.distance(influence.subject)
any(cooks.s > 0.143 | cooks.s < -0.143) #True
which(cooks.s > 0.143 |  cooks.s < -0.143) # Subject 14

cooks.t <- cooks.distance(influence.target)
any(cooks.t > 0.011 | cooks.t < -0.011) #True
items_cook <- rownames(cooks.t)[which(cooks.t > 0.011 |  cooks.t < -0.011)] # IAktenkoffer, Bier, Rochen, Saxophon, Schloss

cooks.c <- cooks.distance(influence.cat)
any(cooks.c > 0.111 | cooks.c < -0.111) #False

#DFBETAS (standardized difference of the beta) is a measure that standardizes the absolute difference in parameter estimates between our original model and our models from which a (potentially influential) subset of data (individual subjects and trials) is removed.
#A value forDFBETAS is calculated for each parameter in the model separately. 
#One can interpret DFBETAS as the standardized difference between the estimate with and without unit j.2
#Belsley, Kuh, and Welsch recommend 2/sqrt(n) (28/360/36) as a size-adjusted cutoff (subjects: 0.378; targets: 0.105, categories: 0.333)

dfbetas.s <- dfbetas(influence.subject)
any(dfbetas.s[,5] > 0.378 | dfbetas.s[,5] < -0.378) 
which(dfbetas.s[,5] > 0.378 | dfbetas.s[,5] < -0.378) #7 16 18

dfbetas.t <- dfbetas(influence.target)
any(dfbetas.t[,5] > 0.105 | dfbetas.t[,5] < -0.105) 
df_beta_prob_items <- names(which(dfbetas.t[,5] > 0.105 | dfbetas.t[,5] < -0.105))

dfbetas.c <- dfbetas(influence.cat)
any(dfbetas.t[,4] > 0.333 | dfbetas.t[,4] < -0.333)#False
which(dfbetas.t[,4] > 0.333 | dfbetas.t[,4] < -0.333)

### pc change
##pchange: percentile change, helps to interpret the magnitude of the individual data units exert on our model estimates
#The percentage change in parameter estimates between our original model and the model from which a (potentially influential) subset of data is removed.

pchange.subject <- pchange(influence.subject)
pchange.target <- pchange(influence.target)
pchange.cat <- pchange(influence.cat)

## sigtest: tests whether excluding the influence of a single case (subject or target) changes the statistical significance of any or more variables in the model. 
# For each of the cases that are evaluated, the test statistic of each variable is compared to a test-value specified by the user. (t = -1.96). For the purpose of this test, the parameter is regarded to statistically significant if the test statistic of the model exceeds the specified value.
# The"sigtest"function reports for each variable the test statistic after deletion of each evaluated case, whether or not this updated teststatistic results in statistical significance based on the user-specified value, and whether or not this new statistical significance differs from the significance in the original model. 
# So, in other words,if a parameter was statistically significant in the original model, but is not longer significant afterthe deletion of a specific case from the model, this is indicated by the output of the"sigtest"function. It is also indicated when an estimate was not significant originally, but reached statisticalsignificance after deletion of a specific case

sigtest.subject <- sigtest(influence.subject, test=-1.96, parameters = c(5))
sig.inter_Cond_OrdPos.s <- data.frame(sigtest.subject[5]) # all false

sigtest.target <- sigtest(influence.target, test=-1.96, parameters = c(5))
sig.inter_Cond_OrdPos.t <- data.frame(sigtest.target[5]) # all false

sigtest.cat <- sigtest(influence.cat, test=-1.96, parameters = c(5))
sig.inter_Cond_OrdPos.c <- data.frame(sigtest.cat[5])#all false

##### Plots of Influence Analysis
### set path for saving plots
setwd("..")

pdf(file="dfbetas_subject.pdf")
plot(influence.subject, cutoff=2/sqrt(28),xlab="DFbeta",ylab="Subject", cex = 0.5, scales=list(cex=0.5), sub = "Estimate of Condition X Ordinal Position")
dev.off()

pdf(file="cook_subject.pdf")
plot(influence.subject, which = "cook", cutoff=4/28, sort = TRUE, xlab="Cooks Distance",ylab="Subject", sub = "All Estimates in the model")
dev.off()

pdf(file="pchange_subject.pdf")
plot(influence.subject, which = "pchange", xlab="Change of estimate in %",ylab="Subject", cex = 0.5, scales=list(cex=0.5), sub = "All Estimates in the model")
dev.off()

pdf(file="sigtest_subject.pdf")
plot(influence.subject, which = "sigtest", parameters = c(5), cutoff = -1.96)
dev.off()

pdf(file="dfbetas_target.pdf")
plot(influence.target, cutoff=2/sqrt(360),xlab="DFbeta",ylab="target", cex = 0.5, scales=list(cex=0.5), sub = "Estimate of Condition X Ordinal Position")
dev.off()

pdf(file="cook_target.pdf")
plot(influence.target, which = "cook", cutoff=4/360, sort = TRUE, xlab="Cooks Distance",ylab="target", sub = "All Estimates in the model")
dev.off()

pdf(file="pchange_target.pdf")
plot(influence.target, which = "pchange", xlab="Change of estimate in %",ylab="target", cex = 0.5, scales=list(cex=0.5), sub = "All Estimates in the model")
dev.off()

pdf(file="sigtest_target.pdf")
plot(influence.target, which = "sigtest", parameters = c(5), cutoff = -1.96)
dev.off()

pdf(file="dfbetas_cat.pdf")
plot(influence.cat, cutoff=2/sqrt(36),xlab="DFbeta",ylab="target", cex = 0.5, scales=list(cex=0.5), sub = "Estimate of Condition X Ordinal Position")
dev.off()

pdf(file="cook_cat.pdf")
plot(influence.cat, which = "cook", cutoff=4/36, sort = TRUE, xlab="Cooks Distance",ylab="target", sub = "All Estimates in the model")
dev.off()

pdf(file="pchange_cat.pdf")
plot(influence.cat, which = "pchange", xlab="Change of estimate in %",ylab="target", cex = 0.5, scales=list(cex=0.5), sub = "All Estimates in the model")
dev.off()

pdf(file="sigtest_cat.pdf")
plot(influence.cat, which = "sigtest", parameters = c(5), cutoff = -1.96)
dev.off()