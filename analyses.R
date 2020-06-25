# Load packages -------------------------------

if(!require(pacman)) install.packages("pacman")
pacman::p_load("haven", "dplyr", "lme4", "reshape2", "ggplot2", "wesanderson", "afex", "emmeans")

select <- dplyr::select
filter <- dplyr::filter

# parameter for graphs
pd = position_dodge(.7)

# write the path to your project folder
setwd('C://Users//Elena//Documents//AA_PhD//Projects//BRAC01_BRAC02//BRAC01-FirstOnline')

# define paths to further subfolders in the project folders (create them first)
dataDir = "data//"
figDir = "figures//"
tabDir = "tables//"

# a .R file with custom functions - define the path to it if different from the working directory
source("C://Users//Elena//Documents//AA_PhD//Projects//expra2020_faces//modelsFun.R")

 
# Load and prepare Data -------------------------------

d <- read.csv(paste0(dataDir, "temp_B1_B2_ageEstimated", ".csv"), sep = ";", dec = ",")

# change variables class

d$task_R <- as.factor(d$task_R)
d$ANSWER_R <- as.factor(d$ANSWER_R)
d$context_R <- as.factor(d$context_R)
d$Attempt <- as.factor(d$Attempt)
d$cocoa <- as.factor(d$cocoa)
d$counterbalance <- as.factor(d$counterbalance)
d$age <- as.numeric(d$age)
d$sex <- as.factor(d$sex)
d$psyKnowledge <- as.factor(d$psyKnowledge)
d$education <- as.factor(d$education)
d$handedness <- as.factor(d$handedness)
d$motherTongue <- as.factor(d$motherTongue)
d$country <- as.factor(d$country)
d$prolific <- as.factor(d$prolific)
d$map_horiAA300 <- as.factor(d$map_horiAA300)
d$rt <- as.numeric(d$rt)

# detect if it's brac01 or brac02 or between subjs from the colour of frame and cue

#d <- d[d$exp == "BRAC2",]

if (length(unique(d$framecolor)) == 3){
  B = "B1B2"
} else if (unique(d$framecolor)[1] == "black"){
  B = "B1"
} else if (unique(d$cuecolor)[1] == "black"){
  B = "B2"
}
print(B)

# Prepare the 2 datsets for RTs and errors analyses

# create a column useful afterwards
d$respRepetitions <- 0

# dataset clening for rts analyses
drt <- d[(d$task_R != 99 & d$rt > 200 & !is.na(d$Attempt == 1)),]
drt <- drt[!(drt$error == 1 | drt$error_R == 1),]

drt$logRT <- log(drt$rt)

for (j in unique(drt$pp)){
  drt[drt$pp == j, "respRepetitions"] <- sum(drt$pp == j & drt$ANSWER_R == 0)
}

# dataset cleaing for error analyses
de <- d[d$task_R != 99 & d$rt > 200,]

# count resp repetitions
for (j in unique(de$pp)){
  de[de$pp == j, "respRepetitions"] <- sum(de$pp == j & de$ANSWER_R == 0)
}

drt$respRepetitions <- as.numeric(drt$respRepetition)
de$respRepetitions <- as.numeric(de$respRepetition)

# RTs -------------------------------

# model with control variables

if (B == "B1" | B == "B2"){
  # same model for the 2 studies; either country or prolific, not both
  mod2 <- lmer(log(rt) ~ task_R*ANSWER_R*context_R*cocoa + blockNum + age + sex + map_horiAA300 +
                 handedness + prolific + respRepetitions + (1|pp), data= drt, REML=F)
} else if (B == "B1B2"){
  mod2 <- lmer(log(rt) ~ task_R*ANSWER_R*context_R*cocoa*exp + blockNum + age + sex + 
                 handedness + prolific + map_horiAA300 + respRepetitions + (1|pp),
               data= drt, REML=F)
}

#mod1 <- lmer(log(rt) ~ task_R*ANSWER_R*context_R*cocoa + (1|pp), data= drt, REML=F)

summary(mod2)
# Note:
# in B1B2, block num doens't interact signifcantly with ANS*task - so no evidence they learnt 
# about different frequency of resp sw versus resp rep

# save the table
#write.csv2(write_pvalues(mod2, "exp"), file= paste0(tabDir, B, "_mod2", ".csv"))

# calculate marginal means
if (B == "B1" | B == "B2"){
  estMeans <- emmeans(
    mod2, c("task_R", "cocoa", "context_R", "ANSWER_R"), lmer.df = "satterthwaite", data = drt)
  # estM_taskContext <- emmeans(
  #   mod2, c("task_R", "context_R"), lmer.df = "satterthwaite", data = drt)
  estM_taskContextCocoa <- emmeans(
    mod2, c("task_R", "context_R", "cocoa"), lmer.df = "satterthwaite", data = drt, type = "response")
} else if (B== "B1B2"){
  estMeans <- emmeans(
    mod2, c("task_R", "cocoa", "context_R", "exp"), lmer.df = "satterthwaite", data = drt) 
}

#write.table(estMeans, paste0(tabDir, B, "_estMeansRts", ".csv"), dec = ".", sep = ";", row.names = F)

# estMeans$exp.media <- exp(estMeans$emmean)
# estMeans$exp.se <- exp(estMeans$SE)


# Check what emmeans does

# # new df with predicted values and relevant variables
# pred <- fitted(mod2)
# dnew <- cbind(pred, drt[,c("task_R", "ANSWER_R", "context_R", "cocoa")])
# 
# tst <- group_my(dnew, pred, task_R, ANSWER_R, context_R, cocoa)


# Plot Rts -------- 

# general plot with all of the conditions - no experiment

png(paste0(figDir, B, "mod2_rts", ".png"), width = 1200, height = 1000, res = 200)
ggplot(as.data.frame(estMeans), aes(x= task_R, y = emmean, fill = ANSWER_R)) +
#ggplot(as.data.frame(estMeans), aes(x= task_R, y = emmean, group = ANSWER_R)) +
  geom_col(width = 0.7, color = "black", position = pd) +
  # geom_line(aes(color = ANSWER_R))+
  # geom_point(aes(color = ANSWER_R))+
  #geom_errorbar(aes(ymin  = exp.media - exp.se, ymax  = exp.media + exp.se), width = 0.3, size  = 0.7, position = pd,color = "black") +
  geom_errorbar(aes(ymin  = emmean  - SE, ymax  = emmean + SE), width = 0.3, size  = 0.7, position = pd,color = "black") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  coord_cartesian(ylim=c(6.3,6.8)) +
  ylab("Mean RTs") +
  xlab("Task sequence") +
  facet_wrap(~context_R + cocoa)
dev.off()

# plot Task and Context (and cocoa)

png(paste0(figDir, B, "mod2_TCCo", ".png"), width = 1200, height = 1000, res = 200)
ggplot(as.data.frame(estM_taskContextCocoa), aes(x= task_R, y = response, fill = context_R)) +
  geom_col(width = 0.7, color = "black", position = pd) +
  #geom_errorbar(aes(ymin  = exp.media - exp.se, ymax  = exp.media + exp.se), width = 0.3, size  = 0.7, position = pd,color = "black") +
  # geom_line(aes(color = context_R))+
  # geom_point(aes(color = context_R))+
  geom_errorbar(aes(ymin  = response  - SE, ymax  = response + SE), width = 0.3, size  = 0.7, position = pd,color = "black") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  coord_cartesian(ylim=c(540,800)) +
  scale_fill_manual(values= c("yellow", "green"))+
  facet_wrap(~ cocoa)+
  ylab("Mean RTs") +
  xlab("Task sequence")
dev.off()

# plot everything apart from response

png(paste0(figDir, B, "mod2_noANS", ".png"), width = 1200, height = 1000, res = 200)
ggplot(as.data.frame(estMeans), aes(x= task_R, y = emmean, fill = context_R)) +
  geom_col(width = 0.7, color = "black", position = pd) +
  #geom_errorbar(aes(ymin  = exp.media - exp.se, ymax  = exp.media + exp.se), width = 0.3, size  = 0.7, position = pd,color = "black") +
  # geom_line(aes(color = context_R))+
  # geom_point(aes(color = context_R))+
  geom_errorbar(aes(ymin  = emmean  - SE, ymax  = emmean + SE), width = 0.3, size  = 0.7, position = pd,color = "black") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  coord_cartesian(ylim=c(6.3,6.9)) +
  scale_fill_manual(values= c("purple", "lightblue"))+
  facet_wrap(~ exp + cocoa)+
  ylab("Mean RTs") +
  xlab("Task sequence")
dev.off()

# -------- 
# Contrasts
if (B== "B1" | B== "B2"){
  postHoc <- contrast(estMeans, "consec", simple = "each", combine = TRUE, adjust = "mvt")
  ciao <- pairs(estMeans, simple = "task_R")
  ciao1 <- pairs(estMeans, by = c("cocoa", "context_R"))
} else if (B == "B1B2") {
  postHoc <- contrast(estMeans, "consec", simple = "each", combine = TRUE, adjust = "mvt")
  }

# save contrasts table
write.table(postHoc, paste0(tabDir, B, "_postHoc_Rts", ".csv"), dec = ".", sep = ";", row.names = F)
# -------- 


# Errors -------------------------------

# model withOUT control variables
if (B == "B1" | B == "B2"){
  #mode1 <- glm(error ~ task_R*ANSWER_R*context_R + context_R*cocoa, family="binomial", data= de)
  mode1 <- glmer(error ~ task_R*ANSWER_R*context_R*cocoa + (1|pp), family="binomial", control=glmerControl(optimizer="bobyqa"), data= de)
} else if (B == "B1B2") {
  mode1 <- glmer(error ~ task_R*ANSWER_R*context_R*cocoa*exp + (1|pp), 
                 family="binomial", control=glmerControl(optimizer="bobyqa"), data= de)
  
}

summary(mode1)
#OddRatio_tab(mode1)

# calculate marginal means
if (B == "B1" | B == "B2"){
  estMeans <- emmeans(
    mode1, c("task_R", "cocoa", "context_R", "ANSWER_R"), lmer.df = "satterthwaite", data = de)
  #estM_taskContext <- emmeans(mod2, c("task_R", "context_R"), lmer.df = "satterthwaite", data = drt)
} else if (B == "B1B2"){
  estMeans <- emmeans(
    mode1, c("task_R", "cocoa", "context_R", "ANSWER_R"), lmer.df = "satterthwaite", data = de)
}

# Plot errors -------- 

# general plot with all of the conditions - no experiment

png(paste0(figDir, B, "mode1_err", ".png"), width = 1200, height = 1000, res = 200)
ggplot(as.data.frame(estMeans), aes(x= task_R, y = abs(emmean), fill = ANSWER_R)) +
  geom_col(width = 0.7, color = "black", position = pd) +
  #geom_errorbar(aes(ymin  = exp.media - exp.se, ymax  = exp.media + exp.se), width = 0.3, size  = 0.7, position = pd,color = "black") +
  geom_errorbar(aes(ymin  = abs(emmean)  - SE, ymax  = abs(emmean) + SE), width = 0.3, size  = 0.7, position = pd,color = "black") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  #coord_cartesian(ylim=c(-3.5, -1.5)) +
  coord_cartesian(ylim=c(1.5, 3.5)) +
  ylab("Mean logOR") +
  xlab("Task sequence") +
  facet_wrap(~context_R + cocoa)
dev.off()

# plot with emmip
png(paste0(figDir, B, "mode1_err", ".png"), width = 1200, height = 1000, res = 200)
emmip(estMeans, ANSWER_R ~ task_R | context_R + cocoa) +
geom_errorbar(aes(ymin  = yvar  - SE, ymax  = yvar + SE), width = 0.2, size  = 0.1, 
              position = pd,color = "black") +
theme_bw() 
dev.off()

# -------- 
# Contrasts
if (B== "B1" | B== "B2"){
  postHoc <- contrast(estMeans, "consec", simple = "each", combine = TRUE, adjust = "mvt")
}