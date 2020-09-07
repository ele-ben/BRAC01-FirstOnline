# Load packages -------------------------------

if(!require(pacman)) install.packages("pacman")
pacman::p_load("haven", "dplyr", "lme4", "reshape2", "ggplot2", "wesanderson", "afex", "emmeans", "tables")

select <- dplyr::select
filter <- dplyr::filter

# parameter for graphs
pd = position_dodge(.1)

# write the path to your project folder
setwd('C://Users//Elena//Documents//AA_PhD//Projects//BRAC01_BRAC02//BRAC01-FirstOnline')

# define paths to further subfolders in the project folders (create them first)
dataDir = "data//"
figDir = "figures//"
tabDir = "tables//"
logbookDir = paste0(dataDir, "logbooks//")

# a .R file with custom functions - define the path to it if different from the working directory
source("C://Users//Elena//Documents//AA_PhD//Projects//expra2020_faces//modelsFun.R")

# Load and prepare Data ----------------------------------------------------------------------------------------

# Load B1 and pick 2 people form rwth only
d_pro <- read.csv(paste0(dataDir, "B1_Pro", ".csv"), sep = ";", dec = ",")
d_pro$prolific <- 1

d_rwth <- read.csv(paste0(dataDir, "B1_RWTH", ".csv"), sep = ";", dec = ",")
d_rwth$prolific <- 0

# Pick 2 pps with horiAA_1st300

# We got too many with same counterbalance:
# I pick LU1 and LY8, the first male and a female with no warnings in the logbook
rwthLB <- read.csv2(paste0(logbookDir, "logbook_B1_RWTH", ".csv"))

pps2keep <- rwthLB[rwthLB$mapping != "BRAC1_horiAA_1st300" | rwthLB$pp == "LU1" | rwthLB$pp == "LY8", "pp"]
pps2rem <- setdiff(unique(d_rwth$pp), pps2keep)

for (ppRem in pps2rem){
  d_rwth <- d_rwth[!d_rwth$pp == ppRem, ]
}

d1 <- rbind(d_rwth, d_pro)
names(d1)[names(d1) == "cuecolor_R"] <- "context_R"
d1$exp <- "BRAC1"

# Load BRAC2
d_pro2 <- read.csv(paste0(dataDir, "B2_Pro", ".csv"), sep = ";", dec = ",")
d_pro2$prolific <- 1

d_rwth2 <- read.csv(paste0(dataDir, "B2_RWTH", ".csv"), sep = ";", dec = ",")
d_rwth2$prolific <- 0

d2 <- rbind(d_rwth2, d_pro2)
names(d2)[names(d2) == "framecolor_R"] <- "context_R"
d2$exp <- "BRAC2"

dtot <- rbind(d1, d2)

# remove partial datasets
rm(list = c("d_pro", "d_pro2", "d_rwth", "d_rwth2", "d1", "d2"))

# Pick the experiment! ----------------------------------------------------------------------------------------

#B = "B1B2"
B = "B1"
#B = "B2"

# Subset the dataset based on the B
if(B == "B1"){d <- dtot[dtot$exp == "BRAC1",]
}else if (B == "B2"){d <- dtot[dtot$exp == "BRAC2",]
}else if (B== "B1B2"){d <- dtot}

rownames(d) <- NULL

# Add more variables ---------------------------------------------------------------------------------------
# # Congruency
# d$congruency <- 99
# 
# for (i in 1:dim(d)[1]){
#   currPP = d[i, "pp"]
#   currStim = d[i, "stimulus"]
#   currTask = d[i, "task"]
#   if (unique(d[d$pp == currPP & d$stimulus == currStim & d$task != currTask, "ANSWER"]) == d[i, "ANSWER"]){
#     d$congruency[i] <- 1
#   } else {
#     d$congruency[i] <- 0
#   }
# }
# 
# d$congruency <- as.factor(d$congruency)
# 
# # if previous trial was congruent or not
# d <- sequence_relation(d, "congruency", 96, suffix = "post", Lag = 1, type = "error")
# 
# d$congruency_post <- as.factor(d$congruency_post)

# Change variables class ---------------------------------------------------------------------------------------

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
#d$map_horiAA300 <- as.factor(d$map_horiAA300)
d$rt <- as.numeric(d$rt)

# Prepare the 2 datsets for RTs and errors analyses -------------------------------------------------------

# create a column useful afterwards
d$respRepetitions <- 0

# fix the missing values of sex and handedness (1 pp) with most likely value
d$handedness[d$handedness == "Other (please specify)"] <- "right-handed"
d$sex[d$sex == "I'd rather not say"] <- "male"

# dataset cleaning for rts analyses
cat("Fast trials were", sum(d$rt < 200), "\n")
drt <- d[(d$task_R != 99 & d$rt > 200 & !is.na(d$Attempt == 1)),]
drt <- drt[!(drt$error == 1 | drt$error_R == 1),]
cat("In total removed", (dim(d)[1] - dim(drt)[1])/dim(d)[1], "of the observations \n")

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
# stadardize these for error model --> helps it to converge
de$respRepetitions <- (de$respRepetitions - mean(de$respRepetitions))/sd(de$respRepetitions)

# Plot errors distribution across pps -----------------------------------------------------------------------

# calculate mean for each pp
errM <- group_my(d, error, pp, task_R, ANSWER_R, context_R, cocoa)
pps <- group_my(errM, meanerror, pp)
cat("Mean error rate was",  mean(pps$meanmeanerror), "plus or minus", sd(pps$meanmeanerror))

png(paste0(figDir, B, "_errorsDistr", ".png"), width = 1500, height = 1000, res = 200)
hist(pps$meanmeanerror, col=45, main = "Distribution of error rates", xlab= "error rate", 
     breaks = seq(0,0.3,0.01))
abline(v= mean(pps$meanmeanerror) + sd(pps$meanmeanerror)*2, col= "darkblue", lwd= 2, lty = "dashed")
dev.off()

# Remove outliers -------------------------------------------------------------------------------------------
# who are the worst ones

ppsWorse <- as.data.frame(pps[pps$meanmeanerror > (mean(pps$meanmeanerror) + sd(pps$meanmeanerror)*2), "pp"])

for (ppRem in ppsWorse$pp){
  #print(ppRem)
  drt <- drt[!drt$pp == ppRem, ]
  de <- de[!de$pp == ppRem, ]
}

# Distribution RTs -----------------------------------------------------------------------------------------

png(paste0(figDir, B, "_RTsDistr", ".png"), width = 1500, height = 1000, res = 200)
par(mfrow=c(1,2))
hist(drt$rt, freq= F, col= "pink", main = "", xlab= "RTs values")
curve(dnorm(x, mean(drt$rt), sd(drt$rt)), add= T, col= 2, lwd= 2)
hist(log(drt$rt), freq= F, col= "pink", main = "", xlab= "log RTs")
curve(dnorm(x, mean(log(drt$rt)), sd(log(drt$rt))), add= T, col= 2, lwd= 2)
dev.off()


# RTs Models - Preliminary ---------------------------------------------------------------------------------------------

# Check hierarchical structure of the data
modLin <- lm(log(rt) ~ 1, data= drt)

modEmpty <- lmer(log(rt) ~ 1 + (1|pp), data= drt, REML=F)
summary(modEmpty)

modEmpty1 <- lmer(log(rt) ~ 1 + (1|pp) + (1|stimulus), data= drt, REML=F)
summary(modEmpty1)

# B1= empty1 better
# B2= empty1 better
# between empty better
anova(modEmpty1, modEmpty)

# RTs Models - Run ---------------------------------------------------------------------------------------------

# Model with control variables

if (B == "B1"){
  mod1 <- lmer(log(rt) ~ task_R*ANSWER_R*context_R*cocoa + (1|pp) + (1|stimulus), data= drt, REML=F)
  # either country or prolific, not both
  mod2 <- lmer(log(rt) ~ task_R*ANSWER_R*context_R*cocoa + blockNum + sex + Participant.Browser + 
                 Participant.OS +handedness + prolific + respRepetitions + (1|pp) + (1|stimulus), 
               data= drt, REML=F)
} else if (B == "B2") {
  # here we don't have map_hori_AA
  #mod1 <- lmer(log(rt) ~ task_R*ANSWER_R*context_R*cocoa + (1|pp), data= drt, REML=F)
  mod2 <- lmer(log(rt) ~ task_R*ANSWER_R*context_R*cocoa + blockNum + sex + Participant.Browser + 
                 Participant.OS +handedness + prolific + respRepetitions + (1|pp) + (1|stimulus), 
               data= drt, REML=F)
  # try congruency
  mod2bis <- lmer(log(rt) ~ task_R*ANSWER_R*context_R*cocoa + ANSWER_R*congruency + blockNum + sex + 
                 Participant.Browser + Participant.OS +handedness + prolific + respRepetitions + 
                 (1|pp) + (1|stimulus), data= drt, REML=F)
  mod2tris <- lmer(log(rt) ~ task_R*ANSWER_R*context_R*cocoa + ANSWER_R*congruency_post + blockNum + sex + 
                    Participant.Browser + Participant.OS +handedness + prolific + respRepetitions + 
                    (1|pp) + (1|stimulus), data= drt, REML=F)
} else if (B == "B1B2"){
  mod2 <- lmer(log(rt) ~ task_R*ANSWER_R*context_R*cocoa*exp + blockNum + sex + Participant.Browser + 
                 Participant.OS +handedness + prolific + respRepetitions + (1|pp) + (1|stimulus),
               data= drt, REML=F)
  mod1 <- lmer(log(rt) ~ task_R*ANSWER_R*context_R*cocoa*exp + (1|pp) + (1|stimulus),
               data= drt, REML=F)
}

summary(mod2)
summary(mod1)
summary(mod2tris)
# Note:
# in B1B2, block num doens't interact signifcantly with ANS*task - so no evidence they learnt 
# about different frequency of resp sw versus resp rep

# save the table
write.table(write_pvalues(mod2, "exp"), file= paste0(tabDir, B, "_mod2", ".csv"), sep = ";", dec = ".")

# Emmeans ---------------------------------------------------------------------------------------------

# Calculate marginal means
if (B == "B1" | B == "B2"){
  estMeans <- emmeans(mod2, c("task_R", "ANSWER_R", "context_R", "cocoa"), lmer.df = "satterthwaite", 
                      data = drt, type = "response")
} else if (B== "B1B2"){
  estMeans <- emmeans(
    mod2, c("task_R", "ANSWER_R", "cocoa", "context_R", "exp"), lmer.df = "satterthwaite", data = drt,
    type = "response") 
}

# Check what emmeans does - not the same as the one below, it takes into account the other variables and, for each
# it first creates a group for each comb of ALL the variables, and then average them acc.ng the require vars


# Plot predicted Rts ----------------------------------------------------------------------------------------

# Plot Estimates above true values 
if (B == "B1" | B== "B2"){
  e <-emmip(estMeans, ANSWER_R ~ task_R | context_R + cocoa)
  xy <- e$data[,c(8,9)]
  
  # Calculate raw means, the order cpndtns arguments is to match the one of emmip data
  gbl <- group_my(drt, rt, pp, task_R, ANSWER_R, context_R, cocoa)
  condtns <- group_my(gbl, meanrt, task_R, cocoa, context_R, ANSWER_R)
  names(condtns)[names(condtns) == "meanmeanrt"] <- "yvar"
  condtns <- cbind(condtns, xy)
  
  #draw it
  png(paste0(figDir, B, "_mod2", ".png"), width = 1200, height = 1000, res = 200)  
  emmip(estMeans, ANSWER_R ~ task_R | context_R + cocoa)+
    geom_point(data = condtns, aes(color = ANSWER_R, alpha = 0.85))+
    theme_bw()
  dev.off()
  
} else if(B == "B1B2"){
  
  # get data of the emmip graph and extract a useful column
  e <-emmip(estMeans, ANSWER_R ~ task_R | exp + cocoa + context_R)
  xy <- e$data[,c(9,10)]
  
  # Calculate raw means, the order cpndtns arguments is to match the one of emmip data
  gbl <- group_my(drt, rt, pp, task_R, ANSWER_R, context_R, cocoa, exp)
  condtns <- group_my(gbl, meanrt, task_R, context_R, cocoa, exp, ANSWER_R)
  names(condtns)[names(condtns) == "meanmeanrt"] <- "yvar"
  condtns <- cbind(condtns, xy)
  
  #draw it
  png(paste0(figDir, B, "_mod2", ".png"), width = 1200, height = 1000, res = 200)  
  emmip(estMeans, ANSWER_R ~ task_R | exp + cocoa + context_R)+
    geom_point(data = condtns, aes(color = ANSWER_R, alpha = 0.85))+
    theme_bw()
  dev.off()
  
}

# # Plot with emmip
# 
# png(paste0(figDir, B, "_mod2_rt", ".png"), width = 1200, height = 1000, res = 200)
# emmip(estMeans, ANSWER_R ~ task_R | exp + cocoa + context_R) +
#   #geom_errorbar(aes(ymin  = yvar  - SE, ymax  = yvar + SE), width = 0.2, size  = 0.1, 
#                 #position = pd,color = "black") +
#   theme_bw() 
# dev.off()

# Plot raw Rts ----------------------------------------------------------------------------------------

#prepare the dataset to plot

# Calculate raw means, the order cpndtns arguments is to match the one of emmip data
gbl <- group_my(drt, rt, pp, task_R, ANSWER_R, context_R, cocoa)
condtns <- group_my(gbl, meanrt, task_R, cocoa, context_R, ANSWER_R)
names(condtns)[names(condtns) == "meanmeanrt"] <- "yvar"

if (B == "B1" | B == "B2"){
  
  png(paste0(figDir, B, "_RT_raws.png"), width = 1200, height = 1000, res = 200)
  ggplot(condtns, aes(x= task_R, y = yvar, group = ANSWER_R, colour = ANSWER_R)) +
    geom_errorbar(aes(ymin  = yvar - se, ymax  = yvar + se), width = 0.3, size  = 0.3, 
                  position = position_dodge(0.3), color = "black") +
    geom_line(position = position_dodge(0.3)) +
    geom_point(aes(colour = ANSWER_R), position = position_dodge(0.3)) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    facet_wrap(~context_R + cocoa)+
    ylab("Mean RTs")
  dev.off()
  
} else if (B == "B1B2"){

  png(paste0(figDir, B, "_RT_raws.png"), width = 2000, height = 1000, res = 200)
  ggplot(condtns, aes(x= task_R, y = yvar, group = ANSWER_R, colour = ANSWER_R)) +
    geom_errorbar(aes(ymin  = yvar - se, ymax  = yvar + se), width = 0.3, size  = 0.3, 
                  position = position_dodge(0.3), color = "black") +
    geom_line(position = position_dodge(0.3)) +
    geom_point(aes(colour = ANSWER_R), position = position_dodge(0.3)) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    facet_wrap(~exp + context_R + cocoa)+
    ylab("Mean RTs")
  dev.off()
  
}

# Prepare Contrasts RTs ---------------------------------------------------------------------------------------

# Build custom contrasts
# https://aosmith.rbind.io/2019/04/15/custom-contrasts-emmeans/
# check also this: https://stats.stackexchange.com/questions/165125/lsmeans-r-adjust-for-multiple-comparisons-with-interaction-terms

# Compare the Delta of the task sw cost in resp rep and resp sw --> same as interaction of the mdel
# take the estMeans and build vectors that "pick" the specific condition

trep <- rep(0, 16)
trep[seq(1, 16, 2)] <- 1

rrep <- rep(0, 16)
rrep[c(1,2,5,6,9,10,13,14)] <- 1

reprep <- as.numeric(trep==rrep & trep == 1)
swrep <- as.numeric(trep!=rrep & trep == 0)
repsw <- as.numeric(trep!=rrep & trep == 1)
swsw <- as.numeric(trep==rrep & trep == 0)

# binding must be done in the pre-made function to see in the different panels
#swCost <- contrast(estMeans, method = list("Sw_costs" = tsw - trep))

# RR_cocoa: Check if resp sw cost in task switch are different in cocoa = 300 and = 0
swrep0 <- c(swrep[c(1:8)], rep(0,8))
swrep300 <- c(rep(0,8), swrep[c(9:16)])

swsw0 <- c(swsw[c(1:8)], rep(0,8))
swsw300 <- c(rep(0,8), swsw[c(9:16)])

# DR_bind: Check distractor-resp binding in task rep: delta betw resp rep and sw when context rep or sw
# repreprep = task rep + resp rep + context rep
repreprep <- c(reprep[1:4], rep(0,4), reprep[9:12], rep(0,4))
repswrep <- c(repsw[1:4], rep(0,4), repsw[9:12], rep(0,4))

reprepsw <- c(rep(0,4), reprep[5:8], rep(0,4), reprep[13:16])
repswsw <-  c(rep(0,4), repsw[5:8], rep(0,4), repsw[13:16])

# respSw_cocoa: Check diff in the lowest panel: resp switch & context switch in task rep with cocoa 300 or 0
zero <- rep(0,16)
zero[7] <- 1
trec <- rep(0,16)
trec[15] <- 1

# check if task-resp binding is significantly smaller in panel 3 than in 1 in Study 1 (no)
repreprep0 <- c(repreprep[1:8], rep(0,8))
swreprep0 <- c(0, 1, rep(0, 14))

repswrep0 <- c(repswrep[1:8], rep(0,8))
swswrep0 <- c(rep(0,3), 1, rep(0, 12))

reprepsw0 <- c(reprepsw[1:8], rep(0,8))
swrepsw0 <- c(rep(0,5), 1, rep(0, 10))

repswsw0 <- c(repswsw[1:8], rep(0,8)) 
swswsw0 <- c(rep(0,7), 1, rep(0, 8))

# check if task-resp binding is significantly smaller in panel 4 than in 2 in Study 2 ()
sumM <- summary(estMeans)
repreprep300 <- as.numeric(sumM$task_R == 0 & sumM$ANSWER_R == 0 & sumM$context_R == 0 & sumM$cocoa == 300)
swreprep300 <- as.numeric(sumM$task_R == 1 & sumM$ANSWER_R == 0 & sumM$context_R == 0 & sumM$cocoa == 300)

repswrep300 <- as.numeric(sumM$task_R == 0 & sumM$ANSWER_R == 1 & sumM$context_R == 0 & sumM$cocoa == 300)
swswrep300 <- as.numeric(sumM$task_R == 1 & sumM$ANSWER_R == 1 & sumM$context_R == 0 & sumM$cocoa == 300)

reprepsw300 <- as.numeric(sumM$task_R == 0 & sumM$ANSWER_R == 0 & sumM$context_R == 1 & sumM$cocoa == 300)
swrepsw300 <- as.numeric(sumM$task_R == 1 & sumM$ANSWER_R == 0 & sumM$context_R == 1 & sumM$cocoa == 300)

repswsw300 <- as.numeric(sumM$task_R == 0 & sumM$ANSWER_R == 1 & sumM$context_R == 1 & sumM$cocoa == 300)
swswsw300 <- as.numeric(sumM$task_R == 1 & sumM$ANSWER_R == 1 & sumM$context_R == 1 & sumM$cocoa == 300)

# Run Contrasts RTs -------------------------------------------------------------------------------------------
# According to the study, different post-hoc
if (B == "B1"){
  
  bindingEffect <- contrast(emmeans(mod2, ~ task_R*ANSWER_R | context_R + cocoa), 
                            interaction = "pairwise", type = "response")
  taskXcontext <- contrast(emmeans(mod2, ~ task_R*context_R| ANSWER_R + cocoa), 
                           interaction = "pairwise", type = "response")
  DR_bind <- contrast(emmeans(mod2, ~ context_R*ANSWER_R | task_R + cocoa), 
                            interaction = "pairwise", type = "response")
  
  # Run the contrasts all together to adjust for multiple comparisons
  # binding must be done outside
  tot <- contrast(estMeans, method = list("RR_cocoa" = (swrep0 - swsw0) - (swrep300 - swsw300),
                                          #"DR_bind" = (repswrep - repreprep) - (repswsw -reprepsw),
                                          "respSw_cocoa" = zero - trec,
                                          "bindingPanel1vs3" = ((swreprep0-repreprep0) - (swswrep0-repswrep0))
                  - ((swrepsw0-reprepsw0) - (swswsw0-repswsw0)),
                                          "RRcost_taskSw_contSw_0" = swrepsw0 - swswsw0,
                  "delta_RRcost_swsw_0vs300" = (swrepsw0 - swswsw0) - (swrepsw300 - swswsw300),
                  "respRep_swsw_0vs300" = swrepsw300 - swrepsw0,
                  "taskSwCost_repsw_0vs300" = (swrepsw0 - reprepsw0) - (swrepsw300 - reprepsw300),
                  "taskSwCost_respsw0_contextSwvsRep" = (swswsw0 - repswsw0) - (swswrep0 - repswrep0),
                  "taskSwCost_swsw0_0vs300" = (swswsw0 - repswsw0) - (swswsw300 - repswsw300),
                  "delta_RRbenefit_0taskrep_contSwVsRep" = (repswrep0 - repreprep0) - (repswsw0 - reprepsw0),
                  # try if this adjust here works!                                                  
                  adjust = "holm"))

  }else if (B== "B2"){
    
  bindingEffect <- contrast(emmeans(mod2, ~ task_R*ANSWER_R | context_R + cocoa), 
                            interaction = "pairwise", type = "response")
  cocoa <- contrast(emmeans(mod2, ~ cocoa|task_R + ANSWER_R + context_R + cocoa), 
                            interaction = "pairwise", type = "response")
  # added after preparing brac presentation
  taskXcontext <- contrast(emmeans(mod2, ~ task_R*context_R| ANSWER_R + cocoa), 
                           interaction = "pairwise", type = "response")
  DR_bind <- contrast(emmeans(mod2, ~ context_R*ANSWER_R | task_R + cocoa), 
                      interaction = "pairwise", type = "response")
  tot <- contrast(estMeans, method = list("bindingPanel2vs4" = ((swreprep300-repreprep300) - 
                        (swswrep300-repswrep300)) - ((swrepsw300-reprepsw300) - (swswsw300-repswsw300))))

} else if (B == "B1B2") {
  #postHoc <- contrast(estMeans, "consec", simple = "each", combine = TRUE, adjust = "mvt")
  bindingEffect <- contrast(emmeans(mod2, ~ task_R*ANSWER_R | context_R + cocoa + exp), 
                            interaction = "pairwise", type = "response")
  taskExp <- contrast(emmeans(mod2, ~ task_R*exp | context_R + cocoa + ANSWER_R), 
                            interaction = "pairwise", type = "response")
  }

# save contrasts table
write.table(postHoc, paste0(tabDir, B, "_postHoc_Rts", ".csv"), dec = ".", sep = ";", row.names = F)



#cocoaEff <- contrast(estMeans, method = list("zeroMinusTrec" = zero - trec))


# Run ANOVA RTs ---------------------------------------------------------------------------------------

# Function from afex package that uses car:Anova and gives F test

# Check which type of Sum of Squares we're using: If 3 is cool
if (afex_options("type") != 3){cat("!!! you are about to run and ANOVA type", afex_options("type"), "!!!")}

# make sure you understand this before running between-subj analyses:
#afex_options("check_contrasts")
if (B == "B1" | B == "B2"){
  
  aov_nice <- aov_ez("pp", "rt", drt, within=c("task_R", "ANSWER_R", "context_R", "cocoa"),
                     return="nice", anova_table = list(es = "pes"), fun_aggregate = mean, include_aov = T)
  
  # anova wihout response relation
  aov_noresp <- aov_ez("pp", "rt", drt, within=c("task_R", "context_R", "cocoa"),
                       return="nice", anova_table = list(es = "pes"), fun_aggregate = mean, include_aov = T)
  
} else if (B == "B1B2"){
  
  aov_nice <- aov_ez("pp", "rt", drt, within=c("task_R", "ANSWER_R", "context_R", "cocoa"), between = "exp",
                     return="nice", anova_table = list(es = "pes"), fun_aggregate = mean, include_aov = T)
  
  # anova wihout response relation
  aov_noresp <- aov_ez("pp", "rt", drt, within=c("task_R", "context_R", "cocoa"), between = "exp",
                       return="nice", anova_table = list(es = "pes"), fun_aggregate = mean, include_aov = T)
}


# save output
write.table(export_aovNice(aov_nice), file= paste0(tabDir, B, "_anova_RTs", ".csv"), sep = ";", dec = ".",
            row.names = F)
write.table(export_aovNice(aov_noresp), file= paste0(tabDir, B, "_aovNoResp_RTs", ".csv"), sep = ";", dec = ".",
            row.names = F)

# The nice above is not identical to car::Anova unless the control variables are removed: particularly is 
# the variable coding the number of response repetition trials that, when added, swaps the significance from context 
# relation to response relation
# ANV <- car::Anova(lmer(rt ~ task_R*ANSWER_R*context_R*cocoa + blockNum + sex + Participant.Browser +
#                          Participant.OS + respRepetitions + handedness + prolific + (1|pp) + (1|stimulus), data = drt, REML = F), type = "III")
# print(ANV)

# Run post-hocs -------------------------------------------------------------------------

# calculate means in each condition
meansXpp <- as.data.frame(group_my(drt, rt, pp, task_R, ANSWER_R, context_R, cocoa))
#meansXcond <- as.data.frame(group_my(meansXpp, meanrt, task_R, ANSWER_R, context_R, cocoa))

# Task x Resp interaction 
# with emmeans
# eMM <- emmeans(aov_nice, ~ task_R*ANSWER_R*context_R*cocoa)
# bindingEffect <- contrast(emmeans(aov_nice, ~ task_R*ANSWER_R | context_R + cocoa), 
#                          interaction = "pairwise", type = "response")

# with t-tests
# retrieve vectors of the means
lev = c(0,1)
for (t in lev){for (r in lev){for (c in lev){for (co in c(0, 300)){
  nam <- paste0("t", t, ".r", r, ".c", c, ".", co)
  assign(nam, meansXpp[meansXpp$task_R == t & meansXpp$ANSWER_R == r & meansXpp$context_R == c & 
                        meansXpp$cocoa == co, "meanrt"])
  }}}}

# test
if (B == "B1"){
  
  postHocLst <- list(
    # dissect task x resp interaction
    t.test(t1.r0.c0.0 - t0.r0.c0.0, t1.r1.c0.0 - t0.r1.c0.0, var.equal = T, paired =  T),
    t.test(t1.r0.c1.0 - t0.r0.c1.0, t1.r1.c1.0 - t0.r1.c1.0, var.equal = T, paired =  T),
    t.test(t1.r0.c0.300 - t0.r0.c0.300, t1.r1.c0.300 - t0.r1.c0.300, var.equal = T, paired =  T),
    t.test(t1.r0.c1.300 - t0.r0.c1.300, t1.r1.c1.300 - t0.r1.c1.300, var.equal = T, paired =  T),
    # dissect task x context interaction
    t.test(t1.r0.c0.0 - t0.r0.c0.0, t1.r0.c1.0 - t0.r0.c1.0, var.equal = T, paired =  T),
    t.test(t1.r1.c0.0 - t0.r1.c0.0, t1.r1.c1.0 - t0.r1.c1.0, var.equal = T, paired =  T),
    t.test(t1.r0.c0.300 - t0.r0.c0.300, t1.r0.c1.300 - t0.r0.c1.300, var.equal = T, paired =  T),
    t.test(t1.r1.c0.300 - t0.r1.c0.300, t1.r1.c1.300 - t0.r1.c1.300, var.equal = T, paired =  T),
    # cfr. the first delta in task switch costs above with the 3rd:
    # is the modulating effect of context in resp repetitions sign.ly different in cocoa 0 vs 300?
    t.test(((t1.r0.c0.0 - t0.r0.c0.0) - (t1.r0.c1.0 - t0.r0.c1.0)), 
           ((t1.r0.c0.300 - t0.r0.c0.300) - (t1.r0.c1.300 - t0.r0.c1.300)), var.equal = T, paired =  T)
  )
} else if (B == "B2"){
  
  postHocLst <- list(
    # dissect task x resp interaction
    t.test(t1.r0.c0.0 - t0.r0.c0.0, t1.r1.c0.0 - t0.r1.c0.0, var.equal = T, paired =  T),
    t.test(t1.r0.c1.0 - t0.r0.c1.0, t1.r1.c1.0 - t0.r1.c1.0, var.equal = T, paired =  T),
    t.test(t1.r0.c0.300 - t0.r0.c0.300, t1.r1.c0.300 - t0.r1.c0.300, var.equal = T, paired =  T),
    t.test(t1.r0.c1.300 - t0.r0.c1.300, t1.r1.c1.300 - t0.r1.c1.300, var.equal = T, paired =  T),
    # dissect task x context interaction
    t.test(t1.r0.c0.0 - t0.r0.c0.0, t1.r0.c1.0 - t0.r0.c1.0, var.equal = T, paired =  T),
    t.test(t1.r1.c0.0 - t0.r1.c0.0, t1.r1.c1.0 - t0.r1.c1.0, var.equal = T, paired =  T),
    t.test(t1.r0.c0.300 - t0.r0.c0.300, t1.r0.c1.300 - t0.r0.c1.300, var.equal = T, paired =  T),
    t.test(t1.r1.c0.300 - t0.r1.c0.300, t1.r1.c1.300 - t0.r1.c1.300, var.equal = T, paired =  T)
  )
}
  

# Adjust p-values for multiple comparisons
# Collect p-values
pvec <- vector()
for (ii in 1:length(postHocLst)) {pvec <- c(pvec, postHocLst[[ii]]$p.value)}
# Adjust them
adjPValues <- p.adjust(pvec, method = "fdr")

# Collect post-hocs in a df
postHoc_info <- c("Comparison", "Mean of differences", "df", "t-value", "adj. p-value", "p-value")
postHocDf <- data.frame(matrix(NA, nrow = length(postHocLst), ncol = length(postHoc_info)))
names(postHocDf) <- postHoc_info
# Fill in the columns
postHocDf[,"adj. p-value"] <- round(adjPValues, 4)
for (ii in 1:length(postHocLst)){
  postHocDf[ii,"Comparison"] <- postHocLst[[ii]]$data.name
  postHocDf[ii,"Mean of differences"] <- round(postHocLst[[ii]]$estimate, 2)
  postHocDf[ii,"df"] <- postHocLst[[ii]]$parameter
  postHocDf[ii,"t-value"] <- round(postHocLst[[ii]]$statistic, 2)
  postHocDf[ii,"p-value"] <- round(postHocLst[[ii]]$p.value, 4)
}

#export post-hoc table
write.table(postHocDf, file= paste0(tabDir, B, "_anova_postHoc_RTs", ".csv"), sep = ";", dec = ".",
            row.names = F)

# Errors -------------------------------------------------------------------------------------------------

# Check Hierarchical structure

modLog <- glm(error ~ 1, family="binomial", data= de)
summary(modLog)

modeEmpty <- glmer(error ~ 1 + (1|pp), family="binomial", control = glmerControl(optimizer="bobyqa"), 
                   data= de)
summary(modeEmpty)

modeEmpty1 <- glmer(error ~ 1 + (1|pp) + (1|stimulus), family="binomial", control = glmerControl(optimizer="bobyqa"), 
                               data= de)
summary(modeEmpty1)

# B1 with pp only
# B2 : with stimulus; empty better than logit and with stimulus better than w/o
# B1B2: with stimulus
anova(modeEmpty, modeEmpty1)

# model withOUT control variables
if (B == "B1"){
  # mode1 <- glmer(error ~ task_R*ANSWER_R*context_R*cocoa + (1|pp), family="binomial", 
  #                control=glmerControl(optimizer="bobyqa"), data= de)
  mode2 <- glmer(error ~ task_R*ANSWER_R*context_R*cocoa + blockNum + sex + Participant.Browser + 
                   Participant.OS +handedness + prolific + respRepetitions + (1|pp), family="binomial", 
                 control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)), data= de)
} else if (B == "B2") {
  mode1 <- glmer(error ~ task_R*ANSWER_R*context_R*cocoa + (1|pp) + (1|stimulus), family="binomial", 
                 control=glmerControl(optimizer="bobyqa"), data= de)
  mode2 <- glmer(error ~ task_R*ANSWER_R*context_R*cocoa + blockNum + sex + Participant.Browser + 
                   Participant.OS +handedness + prolific + respRepetitions + (1|pp), family="binomial", 
                 control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)), data= de)
} else if (B == "B1B2") {
  # since "maxfun < 10 * length(par)^2 is not recommended." I set maxfun to 2e5
  mode2 <- glmer(error ~ task_R*ANSWER_R*context_R*cocoa*exp + blockNum + sex + Participant.Browser + 
                   Participant.OS +handedness + prolific + respRepetitions + (1|pp) + (1|stimulus), 
                 family="binomial", control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)), data= de)
  }

summary(mode1)
summary(mode2)

# save output
write.table(OddRatio_tab(mode2), file= paste0(tabDir, B, "_mode2_err", ".csv"), sep = ";", dec = ".")

# Calculate marginal means ----------------------------------------------------------------------------------
if (B == "B1" | B == "B2"){
  estMeans <- emmeans(mode2, c("task_R", "ANSWER_R", "context_R", "cocoa"), lmer.df = "satterthwaite",
                      data = de, type = "response")
} else if (B == "B1B2"){
  estMeans <- emmeans(mode2, c("task_R", "ANSWER_R", "context_R", "cocoa", "exp"), lmer.df = "satterthwaite",
                      data = de, type = "response")
}


# Plot predicted errors -------------------------------------------------------------------------------------

if (B == "B1" | B== "B2"){
  e <-emmip(estMeans, ANSWER_R ~ task_R | context_R + cocoa)
  xy <- e$data[,c(8,9)]
  
  # Calculate raw means, the order cpndtns arguments is to match the one of emmip data
  gbl <- group_my(de, error, pp, task_R, ANSWER_R, context_R, cocoa)
  condtns <- group_my(gbl, meanerror, task_R, cocoa, context_R, ANSWER_R)
  names(condtns)[names(condtns) == "meanmeanerror"] <- "yvar"
  condtns <- cbind(condtns, xy)
  
  #draw it
  png(paste0(figDir, B, "_mode2", ".png"), width = 1200, height = 1000, res = 200)  
  emmip(estMeans, ANSWER_R ~ task_R | context_R + cocoa)+
    geom_point(data = condtns, aes(color = ANSWER_R, alpha = 0.85))+
    theme_bw()
  dev.off()
  
} else if(B == "B1B2"){
  # get data of the emmip graph and extract a useful column
  e <-emmip(estMeans, ANSWER_R ~ task_R | exp + cocoa + context_R)
  xy <- e$data[,c(9,10)]
  
  # Calculate raw means
  gbl <- group_my(de, error, pp, task_R, ANSWER_R, context_R, cocoa, exp)
  condtns <- group_my(gbl, meanerror, task_R, context_R, cocoa, exp, ANSWER_R)
  names(condtns)[names(condtns) == "meanmeanerror"] <- "yvar"
  condtns <- cbind(condtns, xy)
  
  #draw it
  png(paste0(figDir, B, "_mode2", ".png"), width = 1200, height = 1000, res = 200)  
  emmip(estMeans, ANSWER_R ~ task_R | exp + cocoa + context_R)+
    geom_point(data = condtns, aes(color = ANSWER_R, alpha = 0.85))+
    #scale_color_manual(values= c("ivory3", "ivory4"))+
    theme_bw()
  dev.off()
  
}

# Plot raw errors -----------------------------------------------------------------------------------------

# Calculate raw means, the order cpndtns arguments is to match the one of emmip data
gbl <- group_my(de, error, pp, task_R, ANSWER_R, context_R, cocoa)
condtns <- group_my(gbl, meanerror, task_R, cocoa, context_R, ANSWER_R)
names(condtns)[names(condtns) == "meanmeanerror"] <- "yvar"

if (B == "B1" | B == "B2"){
  
  png(paste0(figDir, B, "_ER_raws.png"), width = 1200, height = 1000, res = 200)
  ggplot(condtns, aes(x= task_R, y = yvar, group = ANSWER_R, colour = ANSWER_R)) +
    geom_errorbar(aes(ymin  = yvar - se, ymax  = yvar + se), width = 0.3, size  = 0.3, 
                  position = position_dodge(0.3), color = "black") +
    geom_line(position = position_dodge(0.3)) +
    geom_point(aes(colour = ANSWER_R), position = position_dodge(0.3)) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    facet_wrap(~context_R + cocoa)+
    ylab("Mean errors")
  dev.off()
  
}

# Contrasts Errors -------------------------------------------------------------------------------------------- 

# For custom contrasts
tsw <- factor2numeric(summary(estMeans)[,"task_R"])
rsw <- factor2numeric(summary(estMeans)[,"ANSWER_R"])
csw <- factor2numeric(summary(estMeans)[,"context_R"])
co0 <- c(rep(1,8), rep(0,8))

if (B== "B1"){
  #postHoc <- contrast(estMeans, "consec", simple = "each", combine = TRUE, adjust = "mvt")
  bindingEffect <- contrast(emmeans(mode2, ~ task_R*ANSWER_R | context_R + cocoa), 
                            interaction = "pairwise", type = "response")
  taskXcontext <- contrast(emmeans(mode2, ~ task_R*context_R| ANSWER_R + cocoa), 
                           interaction = "pairwise", type = "response")
  DR_bind <- contrast(emmeans(mode2, ~ context_R*ANSWER_R | task_R + cocoa), 
                      interaction = "pairwise", type = "response")
  
  #Custom contrasts
  
  # Check how response switch benefit changes
  
  # is response switch benefit different from 0 in cocoa300 context rep?
  rsw_crep_300 <- as.numeric(rsw != csw & rsw == 1 & co0 == 0)
  rrep_crep_300 <- as.numeric(rsw == csw & rsw == 0 & co0 == 0)
  
  # is resp sw benefit diff between cocoa 0 and 300 in context switch?
  rsw_csw_300 <- as.numeric(rsw == csw & rsw == 1 & co0 == 0)
  rrep_csw_300 <- as.numeric(rsw != csw & rsw == 0 & co0 == 0)
  
  rsw_csw_0 <- as.numeric(rsw == csw & rsw == 1 & co0 == 1)
  rrep_csw_0 <- as.numeric(rsw != csw & rsw == 0 & co0 == 1)
  
  # is resp sw benefit diff in context sw versus rep when cocoa300?
  rsw_crep_300 <- as.numeric(rsw != csw & rsw == 1 & co0 == 0)
  rrep_crep_300 <- as.numeric(rsw == csw & rsw == 0 & co0 == 0)
  
  # distractor response binding? Gia guardato nel modello e negli altri post-hoc
  tsw <- factor2numeric(summary(estMeans)[,"task_R"])
  repswrep <- as.numeric(rsw != tsw & rsw == 1 & csw == 0)
  repreprep <- as.numeric(rsw == tsw & rsw == 0 & csw == 0)
  repswsw <- as.numeric(rsw != tsw & rsw == 1 & csw == 1)
  reprepsw <- as.numeric(rsw == tsw & rsw == 0 & csw == 1)
  
  # respSw_cocoa1: resp switch in task rep in the bottom panel
  zero <- rep(0,16)
  zero[7] <- 1
  trec <- rep(0,16)
  trec[15] <- 1
  
  # respSw_cocoa2: resp swith in task rep in the top panel
  zero2 <- rep(0,16)
  zero2[3] <- 1
  trec2 <- rep(0,16)
  trec2[11] <- 1
  
  # respResp_cocoa3: resp swith in task rep in the top panel
  zero3 <- rep(0,16)
  zero3[1] <- 1
  trec3 <- rep(0,16)
  trec3[9] <- 1
  
  
  tot <- contrast(estMeans, method = list("swBenefit_300Rep" = (rsw_crep_300 - rrep_crep_300),
              "swBenefit_bottPanels" = (rsw_csw_300 - rrep_csw_300) - (rsw_csw_0 -rrep_csw_0),
              "swBenefit_leftPanels" = (rsw_csw_300 - rrep_csw_300) - (rsw_crep_300 - rrep_crep_300),
              "respSw_cocoa" = zero - trec,
              "respSw_cocoa2" = zero2 - trec2,
              "respResp_cocoa3" = zero3 - trec3))

  } else if (B == "B2"){
    bindingEffect <- contrast(emmeans(mode2, ~ task_R*ANSWER_R | context_R + cocoa), 
                              interaction = "pairwise", type = "response")
    contCocoa <- contrast(emmeans(mode2, ~ context_R*cocoa | task_R + ANSWER_R ), 
                              interaction = "pairwise", type = "response")
    # for the hypothese this would make more sense maybe:
    DR_bind <- contrast(emmeans(mode2, ~ context_R*ANSWER_R | task_R + cocoa),
                         interaction = "pairwise", type = "response")
    taskXcontext <- contrast(emmeans(mode2, ~ task_R*context_R| ANSWER_R + cocoa), 
                             interaction = "pairwise", type = "response")
    # Custom contrasts
    
    # cont_300: task & rep switch & 300, compare context rep & sw
    reprep300_sw <- as.numeric(rsw == tsw & rsw == 0 & co0 == 0 & csw == 1)
    reprep300_rep <- as.numeric(rsw == tsw & rsw == 0 & co0 == 0 & csw == 0)
    
    # cont_3001: task sw, resp rep & 300, compare context rep & sw
    swrep300_sw <- as.numeric(rsw != tsw & rsw == 0 & co0 == 0 & csw == 1)
    swrep300_rep <- as.numeric(rsw != tsw & rsw == 0 & co0 == 0 & csw == 0)
    
    tot <- contrast(estMeans, method = list("cont_300" = reprep300_sw - reprep300_rep,
                                            "cont_3001" = swrep300_sw - swrep300_rep))
  
} else if (B== "B1B2"){
  bindingEffect <- contrast(emmeans(mode2, ~ task_R*ANSWER_R | context_R + cocoa + exp), 
                            interaction = "pairwise", type = "response")
  contCocoa <- contrast(emmeans(mode2, ~ context_R*cocoa | task_R + ANSWER_R + exp), 
                        interaction = "pairwise", type = "response")
  # more aggreated emmeans to calculate context sw cost independently from the study:
  estMeans <- emmeans(mode2, c("task_R", "ANSWER_R", "context_R", "cocoa"), lmer.df = "satterthwaite", 
                      data = de, type = "response")
  }


# Run ANOVA on errors -------------------------------------------------------------------------------------

if (B == "B1" | B == "B2"){
  
  # group 0,1 variable into continuos error rate
  meansXpp <- as.data.frame(group_my(de, error, pp, task_R, ANSWER_R, context_R, cocoa))
  aov_nice <- aov_ez("pp", "meanerror", meansXpp, within=c("task_R", "ANSWER_R", "context_R", "cocoa"),
                     return="nice", anova_table = list(es = "pes"), fun_aggregate = mean)
  
  # run anova without response relation
  meansXpp1 <- as.data.frame(group_my(de, error, pp, task_R, context_R, cocoa))
  aov_noresp <- aov_ez("pp", "meanerror", meansXpp1, within=c("task_R", "context_R", "cocoa"),
                     return="nice", anova_table = list(es = "pes"), fun_aggregate = mean)
  
} else if (B == "B1B2"){
  
  meansXpp <- as.data.frame(group_my(de, error, pp, task_R, ANSWER_R, context_R, cocoa, exp))
  aov_nice <- aov_ez("pp", "meanerror", meansXpp, within=c("task_R", "ANSWER_R", "context_R", "cocoa"), 
                     between = "exp", return="nice", anova_table = list(es = "pes"), fun_aggregate = mean)
  
  # anova wihout response relation
  meansXpp1 <- as.data.frame(group_my(de, error, pp, task_R, context_R, cocoa, exp))
  aov_noresp <- aov_ez("pp", "meanerror", meansXpp1, within=c("task_R", "context_R", "cocoa"), between = "exp",
                       return="nice", anova_table = list(es = "pes"), fun_aggregate = mean)
}
# save output
write.table(export_aovNice(aov_nice), file= paste0(tabDir, B, "_anova_ER", ".csv"), sep = ";", 
            dec = ".", row.names = F)
write.table(export_aovNice(aov_noresp), file= paste0(tabDir, B, "_aovNoResp_ER", ".csv"), sep = ";", 
            dec = ".", row.names = F)

# Run Post-Hocs -------------------------------------------------------------------------------------------

# isolate mean vectors
lev = c(0,1)
for (t in lev){for (r in lev){for (c in lev){for (co in c(0, 300)){
  nam <- paste0("t", t, ".r", r, ".c", c, ".", co)
  assign(nam, meansXpp[meansXpp$task_R == t & meansXpp$ANSWER_R == r & meansXpp$context_R == c & 
                         meansXpp$cocoa == co, "meanerror"])
  }}}}

# tests
if (B == "B1" | B == "B2"){
  postHocLst <- list(
  # dissect task x resp interaction: task switch costs in response rep vs sw
  t.test(t1.r0.c0.0 - t0.r0.c0.0, t1.r1.c0.0 - t0.r1.c0.0, var.equal = T, paired =  T),
  t.test(t1.r0.c1.0 - t0.r0.c1.0, t1.r1.c1.0 - t0.r1.c1.0, var.equal = T, paired =  T),
  t.test(t1.r0.c0.300 - t0.r0.c0.300, t1.r1.c0.300 - t0.r1.c0.300, var.equal = T, paired =  T),
  t.test(t1.r0.c1.300 - t0.r0.c1.300, t1.r1.c1.300 - t0.r1.c1.300, var.equal = T, paired =  T),
  # dissect resp x context x cocoa: resp switch benefit in context rep vs sw
  t.test(t0.r1.c0.0 - t0.r0.c0.0, t0.r1.c1.0 - t0.r0.c1.0, var.equal = T, paired =  T),
  t.test(t1.r1.c0.0 - t1.r0.c0.0, t1.r1.c1.0 - t1.r0.c1.0, var.equal = T, paired =  T),
  t.test(t0.r1.c0.300 - t0.r0.c0.300, t0.r1.c1.300 - t0.r0.c1.300, var.equal = T, paired =  T),
  t.test(t1.r1.c0.300 - t1.r0.c0.300, t1.r1.c1.300 - t1.r0.c1.300, var.equal = T, paired =  T)
  )
}

# collect p-values
pvec <- vector()
for (ii in 1:length(postHocLst)) {pvec <- c(pvec, postHocLst[[ii]]$p.value)}

#p.adjust
adjPValues <- p.adjust(pvec, method = "fdr")

# Collect post-hocs in a df
postHoc_info <- c("Comparison", "Mean of differences", "df", "t-value", "adj. p-value", "p-value")
postHocDf <- data.frame(matrix(NA, nrow = length(postHocLst), ncol = length(postHoc_info)))
names(postHocDf) <- postHoc_info
postHocDf[,"adj. p-value"] <- round(adjPValues, 4)
for (ii in 1:length(postHocLst)){
  postHocDf[ii,"Comparison"] <- postHocLst[[ii]]$data.name
  postHocDf[ii,"Mean of differences"] <- round(postHocLst[[ii]]$estimate, 2)
  postHocDf[ii,"df"] <- postHocLst[[ii]]$parameter
  postHocDf[ii,"t-value"] <- round(postHocLst[[ii]]$statistic, 2)
  postHocDf[ii,"p-value"] <- round(postHocLst[[ii]]$p.value, 4)
}

#export post-hoc table
write.table(postHocDf, file= paste0(tabDir, B, "_anova_postHoc_ER", ".csv"), sep = ";",
            dec = ".", row.names = F)

# Draw a table with all of the Raw means -------------------------------------------------------------------

predRT <- summary(emmeans(mod2, c("task_R", "ANSWER_R", "context_R", "cocoa", "exp"), lmer.df = "satterthwaite", 
                data = drt, type = "response"))
write.table(predRT[,c(1:6)], paste0(tabDir,"predRT.csv"), row.names = F, sep = ";", dec = ".")

predER <- summary(emmeans(mode2, c("task_R", "ANSWER_R", "context_R", "cocoa", "exp"), lmer.df = "satterthwaite", 
                data = de, type = "response"))
write.table(predER[,c(1:6)], paste0(tabDir,"predER.csv"), row.names = F, sep = ";", dec = ".")

rawRTs1 <- group_my(drt, rt, task_R, ANSWER_R, context_R, cocoa, exp, pp)
rawRTs <- group_my(rawRTs1, meanrt, exp, cocoa, context_R, ANSWER_R, task_R)
rawRTs <- as.data.frame(rawRTs[, c("task_R", "ANSWER_R", "context_R", "cocoa", "exp", "meanmeanrt", "n", "se")])
write.table(rawRTs, paste0(tabDir,"rawRTs.csv"), row.names = F, sep = ";", dec = ".")

rawER1 <- group_my(de, error, task_R, ANSWER_R, context_R, cocoa, exp, pp)
rawER <- group_my(rawER1, meanerror, exp, cocoa, context_R, ANSWER_R, task_R)
rawER <- as.data.frame(rawER[, c("task_R", "ANSWER_R", "context_R", "cocoa", "exp", "meanmeanerror", "n", "se")])
write.table(rawER, paste0(tabDir,"rawER.csv"), row.names = F, sep = ";", dec = ".")



# anova rts
aov_nice <- aov_ez("pp", "rt", drt, within=c("task_R", "ANSWER_R", "context_R", "cocoa"), between = c("exp"),
                   return="nice", fun_aggregate = mean)
