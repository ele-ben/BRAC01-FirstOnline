# -----------------
# try if this works for your, otherwise load each package with the line below
if(!require(pacman)) install.packages("pacman")
pacman::p_load("haven", "dplyr", "lme4", "reshape2", "ggplot2", "wesanderson", "afex", "emmeans")

select <- dplyr::select
filter <- dplyr::filter

# write the path to your project folder
setwd('C://Users//Elena//Documents//AA_PhD//Projects//BRAC01_BRAC02//BRAC01-FirstOnline')

# define paths to further subfolders in the project folders (create them first)
dataDir = "data//"
figDir = "figures//"
tabDir = "tables//"

# a .R file with custom functions - define the path to it if different from the working directory
source("C://Users//Elena//Documents//AA_PhD//Projects//expra2020_faces//modelsFun.R")


# -------------------------------
# load data

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

# -------------------------------

# detect if it's brac01 or brac02 or between subjs from the colour of frame and cue

d <- d[d$exp == "BRAC1",]

if (length(unique(d$framecolor)) == 3){
  B = "B1B2"
} else if (unique(d$framecolor)[1] == "black"){
  B = "B1"
} else if (unique(d$cuecolor)[1] == "black"){
  B = "B2"
}
print(B)

# -------------------------------
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

# -------------------------------

# RTs

# model with control variables

if (B == "B1" | B == "B2"){
  # same model for the 2 studies; either country or prolific, not both
  mod2 <- lmer(logRT ~ task_R*ANSWER_R*context_R*cocoa + blockNum + age + sex + map_horiAA300 +
                 handedness + prolific + respRepetitions + (1|pp), data= drt, REML=F)
} else{
  mod2 <- lmer(log(rt) ~ task_R*ANSWER_R*context_R*cocoa*exp + blockNum + age + sex + 
                 handedness + prolific + map_horiAA300 + respRepetitions + (1|pp),
               data= drt, REML=F)
}

mod1 <- lmer(log(rt) ~ task_R*ANSWER_R*context_R*cocoa + (1|pp),
             data= drt, REML=F)

summary(mod2)
# Note:
# in B1B2, block num doens't interact signifcantly with ANS*task - so no evidence they learnt 
# about different frequency of resp sw versus resp rep

# save the table
write.csv2(write_pvalues(mod2, "exp"), file= paste0(tabDir, B, "_mod2", ".csv"))

# calculate marginal means
if (B == "B1"){
  estMeans <- as.data.frame(
    emmeans(mod1, c("task_R", "cocoa", "context_R", "ANSWER_R"), lmer.df = "satterthwaite"))
}
estMeans <- as.data.frame(
  emmeans(mod1, c("task_R", "cocoa", "context_R", "ANSWER_R"), lmer.df = "satterthwaite"))
estMeans$exp.media <- exp(estMeans$emmean)
estMeans$exp.se <- exp(estMeans$SE)

#dnew <- cbind(fitted(mod1), drt[,c("task_R", "ANSWER_R", "context_R", "cocoa")])

# general plot with all of the conditions - no experiment
# barplot
pd = position_dodge(.7)
#png(paste0(figDir, B, "mod2_rts", ".png"), width = 1200, height = 1000, res = 200)
ggplot(estMeans, aes(x= task_R, y = exp.media, fill = ANSWER_R)) +
  geom_col(width = 0.7, color = "black", position = pd) +
  geom_errorbar(aes(ymin  = exp.media - exp.se, ymax  = exp.media + exp.se), width = 0.3, size  = 0.7, position = pd,color = "black") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  #coord_cartesian(ylim=c(6.25,6.75)) +
  ylab("Mean RTs") +
  xlab("Task sequence") +
  facet_wrap(~context_R + cocoa)
#dev.off()

# -------------------------------

# -------------------------------

# Errors