# Load packages -------------------------------

if(!require(pacman)) install.packages("pacman")
pacman::p_load("haven", "dplyr", "lme4", "reshape2", "ggplot2", "RColorBrewer", "afex", "emmeans", "tables")

select <- dplyr::select
filter <- dplyr::filter

# write the path to your project folder
setwd('C://Users//Elena//Documents//AA_PhD//Projects//BRAC01_BRAC02//BRAC1-2-FirstOnline')

# define paths to further subfolders in the project folders (create them first)
dataDir = "data//"
figDir = "figures//"
tabDir = "tables//"
logbookDir = paste0(dataDir, "logbooks//")

# a .R file with custom functions - define the path to it if different from the working directory
source("C://Users//Elena//Documents//AA_PhD//R//modelsFun.R")

# parameter and theme for graphs
my_theme <- theme_minimal() + theme(axis.title = element_text(face = "bold"),
                                    text = element_text(size=16),
                                    axis.line = element_line(colour = "black"))
pd = position_dodge(.1)

# Load and prepare Data --------------------------------------------------------------------------------------

B = "B1"
#B = "B2"
#B = "B1B2"

if (B == "B1" | B == "B1B2"){
  # Online data B1
  
  # Load B1 and pick 2 people form rwth only
  d_pro <- read.csv(paste0(dataDir, "B1_Pro", ".csv"), sep = ";", dec = ",")
  d_pro$prolific <- 1
  
  d_rwth <- read.csv(paste0(dataDir, "B1_RWTH", ".csv"), sep = ";", dec = ",")
  d_rwth$prolific <- 0
  
  # Pick 2 pps with horiAA_1st300
  
  # We got too many with same counterbalance:
  # I pick LU1 and LY8, the first male and a female with no warnings in the logbook
  rwthLB <- read.csv2(paste0(logbookDir, "logbook_B1_RWTH", ".csv"))
  
  pps2rem <- rwthLB[rwthLB$mapping == "BRAC1_horiAA_1st300" & rwthLB$pp != "LU1" & rwthLB$pp != "LY8", "pp"]
  
  for (ppRem in pps2rem){
    d_rwth <- d_rwth[!d_rwth$pp == ppRem, ]
  }
  
  d1 <- rbind(d_rwth, d_pro)
  d1$version <- "online"
  
  names(d1)[names(d1) == "cuecolor_R"] <- "context_R"
  names(d1)[names(d1) == "ANSWER_R"] <- "correctResp_R"
  names(d1)[names(d1) == "handedness"] <- "hand"
  # Lab data B1
  
  # change project folder
  setwd('C://Users//Elena//Documents//AA_PhD//Projects//BRAC01_BRAC02//BRAC1-2-labVersion')
  
  cleanData <- list.files(dataDir, pattern = paste0("B1_lab(.)+.csv"))
  dlab <- read.csv2(paste0(dataDir, cleanData), dec = ".")
  dlab$version <- "lab"
  
  names(dlab)[names(dlab) == "colour_R"] <- "context_R"
  
  # # remove the last 4 blocks to have same number of trials
  # dlab <- dlab[dlab$blockNum < 5,]
  
  # change it back!
  setwd('C://Users//Elena//Documents//AA_PhD//Projects//BRAC01_BRAC02//BRAC1-2-FirstOnline')
  
  # get commong coloumns and bind them in the final dataset
  common_cols <- intersect(names(d1), names(dlab))
  dlab$pp <- as.factor(dlab$pp)

  d <- rbind(subset(d1, select = common_cols), subset(dlab, select = common_cols))
  # remove partial datasets
  rm(list = c("d_pro", "d_rwth", "dlab", "d1"))
  
} 

if (B == "B2"| B == "B1B2"){
  # Online data B2
  
  # Load B1 and pick 2 people form rwth only
  d_pro2 <- read.csv(paste0(dataDir, "B2_Pro", ".csv"), sep = ";", dec = ",")
  d_pro2$prolific <- 1
  
  d_rwth2 <- read.csv(paste0(dataDir, "B2_RWTH", ".csv"), sep = ";", dec = ",")
  d_rwth2$prolific <- 0
  
 
  # bind the online datsets
  d2 <- rbind(d_rwth2, d_pro2)
  d2$version <- "online"
  
  names(d2)[names(d2) == "framecolor_R"] <- "context_R"
  names(d2)[names(d2) == "ANSWER_R"] <- "correctResp_R"
  
  # Lab data B2
  
  # change project folder
  setwd('C://Users//Elena//Documents//AA_PhD//Projects//BRAC01_BRAC02//BRAC1-2-labVersion')
  
  cleanData <- list.files(dataDir, pattern = paste0("B2_lab(.)+.csv"))
  dlab2 <- read.csv2(paste0(dataDir, cleanData), dec = ".")
  dlab2$version <- "lab"
  
  names(dlab2)[names(dlab2) == "colour_R"] <- "context_R"
  
  # # remove the last 4 blocks to have same number of trials
  # dlab2 <- dlab2[dlab2$blockNum < 5,]
  
  # change it back!
  setwd('C://Users//Elena//Documents//AA_PhD//Projects//BRAC01_BRAC02//BRAC1-2-FirstOnline')
  
  # get commong coloumns and bind them in the final dataset
  common_cols <- intersect(names(d2), names(dlab2))
  dlab2$pp <- as.factor(dlab2$pp)
  # d <- rbind(d1[, c("task_R")], dlab[, c("task_R")])
  # or...
  d2 <- rbind(subset(d2, select = common_cols), subset(dlab2, select = common_cols))
  # remove partial datasets
  rm(list = c("d_pro2", "d_rwth2", "dlab2"))
  
}

# v1 <- lapply(d2[,common_cols], class)
# vl <- lapply(dlab2[,common_cols], class)

if (B == "B2"){d <- d2} else if (B == "B1B2"){
  # identify exp 1 and 2
  d$exp <- "B1"
  d2$exp <- "B2"
  # change names of lab subj 2 not to match with 1
  d2$pp <- paste0(d2$pp, "_2")
  # bind exp 1 and exp 2
  d <- rbind(d, d2)
}


# Change variables class 

d$pp <- as.factor(d$pp)
d$task_R <- as.factor(d$task_R)
d$correctResp_R <- as.factor(d$correctResp_R)
d$context_R <- as.factor(d$context_R)
d$cocoa <- as.factor(d$cocoa)
d$version <- as.factor(d$version)
d$age <- as.numeric(d$age)
d$sex <- as.factor(d$sex)
d$rt <- as.numeric(d$rt)
#d$exp <- as.factor(d$exp)

#d[d2$cocoa == 0,]

# Dataset cleaning for rts analyses
cat("Fast trials were", sum(d$rt < 200, na.rm = T), "\n")
drt <- d[(d$task_R != 99 & d$rt > 200 & !is.na(d$rt)),]
drt <- drt[!(drt$error == 1 | drt$error_R == 1),]

# dataset cleaing for error analyses
de <- d[d$task_R != 99 & (d$rt > 200 | is.na(d$rt)),]

# export datasets for machinelearning analyses
de_means <- group_my(de, error, pp, task_R, correctResp_R, context_R, cocoa, age, sex, version)
de_means$meanerror <- as.numeric(de_means$meanerror)
write.csv2(de_means, paste0(dataDir, "error_means.csv"), row.names = F)

write.csv2(de, paste0(dataDir, "error_ML.csv"), row.names = F)

drt_means <- group_my(drt, rt, pp, task_R, correctResp_R, context_R, cocoa, age, sex, version)
write.csv2(drt_means, paste0(dataDir, "rt_means.csv"), row.names = F)

# Remove outliers -------------------------------------------------------------------------------------------
# who are the worst ones

# calculate mean for each pp
errM <- group_my(d, error, pp, task_R, correctResp_R, context_R, cocoa)
pps <- group_my(errM, meanerror, pp)
cat("Mean error rate was",  mean(pps$meanmeanerror), "plus or minus", sd(pps$meanmeanerror))

# OR 19%??????????
ppsWorse <- as.data.frame(pps[pps$meanmeanerror > 0.2, "pp"])

for (ppRem in ppsWorse$pp){
  #print(ppRem)
  d <- d[!d$pp == ppRem, ]
  drt <- drt[!drt$pp == ppRem, ]
  de <- de[!de$pp == ppRem, ]
}

errM <- group_my(d, error, pp, task_R, correctResp_R, context_R, cocoa)
pps <- group_my(errM, meanerror, pp)
cat("W/o outliers, mean error rate is ",  mean(pps$meanmeanerror), "plus or minus", sd(pps$meanmeanerror))


cat("For rts analyses, removed ", 1 - (nrow(drt)/ nrow(d)), " of the (no outlier) dataset.")
cat("For error analyses, removed ", 1 - (nrow(de)/ nrow(d)), " of the (no outlier) dataset.")

# Plot expeirments ---------------------------------------------------------------------

# Calculate raw means
gbl <- group_my(drt, rt, pp, task_R, correctResp_R, context_R, cocoa, version)
condtns1 <- group_my(gbl, meanrt, task_R, cocoa, context_R, correctResp_R)
#condtns1 <- group_my(gbl, meanrt, task_R, cocoa, context_R, correctResp_R, version)
condtns1$colour_R <- factor(condtns1$context_R,
                            labels = c("Context Repet.", "Context Switch"))
condtns1$cocoa <- factor(condtns1$cocoa,
                         labels = c("Onset Asynchr. 0 ms", "Onset Asynchr. 300 ms"))
condtns1$correctResp_R <- factor(condtns1$correctResp_R,
                                 labels = c("Repet.", "Switch"))
condtns1$task_R <- factor(condtns1$task_R, 
                          labels = c("Repet.", "Switch"))
condtns1 <- rename(condtns1, Response = correctResp_R)

# Plot & save it
#png(paste0(figDir, B, "_comparison_Rts.png"), width = 1300, height = 1300, res = 200)
png(paste0(figDir, B, "_merged_A+B.png"), width = 1300, height = 1300, res = 200)
ggplot(condtns1, aes(x= task_R, y = meanmeanrt, group = Response, colour = Response)) +
  geom_errorbar(aes(ymin  = meanmeanrt - se, ymax  = meanmeanrt + se), width = 0.3, size  = 0.3, 
                position = pd, color = "black") +
  geom_line(position = pd) +
  geom_point(aes(colour = Response), position = pd) +
  my_theme +
  facet_grid(rows = vars(colour_R), cols = vars(cocoa))+
  #facet_grid(rows = c(vars(colour_R), vars(cocoa)), cols = vars(version))+
  ylab("Mean RTs")+
  xlab("Task")+
  coord_cartesian(ylim = c(700, 950))
dev.off()

# one plot for each cocoa value
for (co in c("Onset Asynchr. 0 ms", "Onset Asynchr. 300 ms")){
  # get only cocoa 0 or 300
  condtns <- condtns1[condtns1$cocoa == co,]
  # Plot it
  plott <- ggplot(condtns, aes(x= task_R, y = meanmeanrt, group = Response, colour = Response)) +
    geom_errorbar(aes(ymin  = meanmeanrt - se, ymax  = meanmeanrt + se), width = 0.3, size  = 0.3, 
                  position = position_dodge(0.3), color = "black") +
    geom_line(position = position_dodge(0.3)) +
    geom_point(aes(colour = Response), position = position_dodge(0.3)) +
    my_theme +
    facet_grid(rows = c(vars(colour_R)), cols = vars(version))+
    ylab("Mean RTs")+
    xlab("Task")+
    ggtitle(co)+
    coord_cartesian(ylim = c(500, 900))
  # save it - ggplot requires print
  namee <- paste0(figDir, B, "_comparison_Rts_cocoa", gsub("Onset Asynchr. ", "", co), ".png")
  png(namee, width = 1200, height = 1300, res = 200)
  print(plott)
  dev.off()
}

# one plot where cocoas are merged together
condtns1 <- group_my(gbl, meanrt, task_R, context_R, correctResp_R)
condtns1$colour_R <- factor(condtns1$context_R,
                            labels = c("Context Repet.", "Context Switch"))
condtns1$correctResp_R <- factor(condtns1$correctResp_R,
                                 labels = c("Repet.", "Switch"))
condtns1$task_R <- factor(condtns1$task_R, 
                          labels = c("Repet.", "Switch"))
condtns1 <- rename(condtns1, Response = correctResp_R)
# plot&save
png(paste0(figDir, B, "_merged_A+B_cocoas_merged.png"), width = 1300, height = 900, res = 200)
ggplot(condtns1, aes(x= task_R, y = meanmeanrt, group = Response, colour = Response)) +
  geom_errorbar(aes(ymin  = meanmeanrt - se, ymax  = meanmeanrt + se), width = 0.3, size  = 0.3, 
                position = pd, color = "black") +
  geom_line(position = pd) +
  geom_point(aes(colour = Response), position = pd) +
  my_theme +
  facet_grid(cols = vars(colour_R))+
  ylab("Mean RTs")+
  xlab("Task")+
  coord_cartesian(ylim = c(700, 950))
dev.off()

# one plot where resp relations are merged together
condtns1 <- group_my(gbl, meanrt, task_R, context_R, cocoa)
condtns1$cocoa <- factor(condtns1$cocoa,
                            labels = c("0 ms", "300 ms"))
condtns1 <- rename(condtns1, Asynchrony = cocoa)
condtns1$context_R <- factor(condtns1$context_R,
                                 labels = c("Context Repet.", "Context Switch"))
condtns1$task_R <- factor(condtns1$task_R, 
                          labels = c("Repet.", "Switch"))
condtns1 <- rename(condtns1, Context = context_R)
# plot&save
png(paste0(figDir, B, "_merged_A+B_resp_merged_teap.png"), width = 1300, height = 900, res = 200)
ggplot(condtns1, aes(x= task_R, y = meanmeanrt, group = Asynchrony, colour = Asynchrony)) +
  geom_errorbar(aes(ymin  = meanmeanrt - se, ymax  = meanmeanrt + se), width = 0.3, size  = 0.3, 
                position = pd, color = "black") +
  scale_colour_manual(values=c("mediumblue", "darkorange"))+
  geom_line(position = pd) +
  geom_point(aes(colour = Asynchrony), position = pd) +
  my_theme +
  facet_grid(cols = vars(Context))+
  ylab("Mean RTs")+
  xlab("Task")+
  coord_cartesian(ylim = c(700, 950))
dev.off()

# one plot for Teap 2021 with task switch costs in the two context relations values only
condtns1 <- group_my(gbl, meanrt, task_R, context_R)
condtns1$context_R <- factor(condtns1$context_R,
                             labels = c("Context Repet.", "Context Switch"))
condtns1$task_R <- factor(condtns1$task_R, 
                          labels = c("Repet.", "Switch"))
condtns1 <- rename(condtns1, Context = context_R)
# plot&save
png(paste0(figDir, B, "_merged_A+B_task-sw-cost_teap.png"), width = 1000, height = 900, res = 200)
ggplot(condtns1, aes(x= task_R, y = meanmeanrt)) +
  geom_errorbar(aes(ymin  = meanmeanrt - se, ymax  = meanmeanrt + se), width = 0.3, size  = 0.3, 
                position = pd, color = "black") +
  geom_line(position = pd) +
  geom_point() +
  my_theme +
  facet_grid(cols = vars(Context))+
  ylab("Mean RTs")+
  xlab("Task")+
  coord_cartesian(ylim = c(700, 950))
dev.off()


# ANOVA on Rts -----------------------------------------------------------------

aov_nice <- aov_ez("pp", "rt", drt, within=c("task_R", "correctResp_R", "context_R", "cocoa"),
                   between = "version", return="nice", anova_table = list(es = "pes"), fun_aggregate = mean)

if (B == "B1B2"){
  
  aov_nice <- aov_ez("pp", "rt", drt, within=c("task_R", "correctResp_R", "context_R", "cocoa"),
                     between = "exp", return="nice", anova_table = list(es = "pes"), fun_aggregate = mean)
  grep("^[a-zA-Z_01]+$", aov_nice$Effect, value = T)
  varsVec <- c("exp", "task", "resp", "context", "cocoa")
  
  write.table(export_aovNice(aov_nice, varsVec), file= paste0(tabDir, B, "_labOnline_RTs", ".csv"),
              sep = ";", dec = ".", row.names = F)
}


# names of anova vars
grep("^[a-zA-Z_01]+$", aov_nice$Effect, value = T)
varsVec <- c("version", "task", "resp", "context", "cocoa")

write.table(export_aovNice(aov_nice, varsVec), file= paste0(tabDir, B, "_comparison_RTs", ".csv"),
            sep = ";", dec = ".", row.names = F)

# Calculate some means
meansXpp <- as.data.frame(group_my(drt, rt, pp, task_R, correctResp_R, context_R, cocoa, version))

respCocoa <- group_my(meansXpp, meanrt, correctResp_R, version)
TRC <- group_my(meansXpp, meanrt, task_R, correctResp_R, context_R)

# Post-hocs --------------------------------------------------------------------------------------

group_my(meansXpp, meanrt, task_R)
group_my(meansXpp, meanrt, correctResp_R)
group_my(meansXpp, meanrt, context_R)


# task x resp x context

# calculate aggregated means
means1 <- as.data.frame(group_my(meansXpp, meanrt, pp, task_R, correctResp_R, context_R))

# Anova on resp rep & switch separately

only1 <- means1[means1$correctResp_R == 0,]
only2 <- means1[means1$correctResp_R == 1,]

aov1 <- aov_ez("pp", "meanmeanrt", only1, within=c("task_R", "context_R"),
                return="nice", anova_table = list(es = "pes"))

aov2 <- aov_ez("pp", "meanmeanrt", only2, within=c("task_R", "context_R"),
                  return="nice", anova_table = list(es = "pes"))
aov1
aov2

group_my(only1, meanmeanrt, task_R, context_R)

# task x context x onset asynchrony

# calculate aggregated means
means1 <- as.data.frame(group_my(meansXpp, meanrt, pp, task_R, context_R, cocoa))

# Anova on resp rep & switch separately

only1 <- means1[means1$cocoa == 0,]
only2 <- means1[means1$cocoa == 300,]

aov1 <- aov_ez("pp", "meanmeanrt", only1, within=c("task_R", "context_R"),
               return="nice", anova_table = list(es = "pes"))

aov2 <- aov_ez("pp", "meanmeanrt", only2, within=c("task_R", "context_R"),
               return="nice", anova_table = list(es = "pes"))
aov1
aov2

group_my(only1, meanmeanrt, task_R, context_R)

# check experiment x corresp
group_my(meansXpp, meanrt, correctResp_R, version)

# check exp x correcsp x context x cocoa

means1 <- as.data.frame(group_my(meansXpp, meanrt, pp, correctResp_R, context_R, cocoa, version))

# Anova on resp rep & switch separately

only1 <- means1[means1$version == "lab",]
only2 <- means1[means1$version == "online",]

aov1 <- aov_ez("pp", "meanmeanrt", only1, within=c("correctResp_R", "context_R", "cocoa"),
               return="nice", anova_table = list(es = "pes"))

aov2 <- aov_ez("pp", "meanmeanrt", only2, within=c("correctResp_R", "context_R", "cocoa"),
               return="nice", anova_table = list(es = "pes"))
aov1
aov2

only2 <- only1[only1$cocoa == 300,]
only1 <- only1[only1$cocoa == 0,]

aov1 <- aov_ez("pp", "meanmeanrt", only1, within=c("correctResp_R", "context_R"),
               return="nice", anova_table = list(es = "pes"))

aov2 <- aov_ez("pp", "meanmeanrt", only2, within=c("correctResp_R", "context_R"),
               return="nice", anova_table = list(es = "pes"))
aov1
aov2

group_my(only2, meanmeanrt, correctResp_R, context_R)



# ANOVA on errors ---------------------------------------------------------------------------------

meansXpp <- as.data.frame(group_my(de, error, pp, task_R, correctResp_R, context_R, cocoa, version))

aov_nice <- aov_ez("pp", "error", de, within=c("task_R", "correctResp_R", "context_R", "cocoa"),
                   between = "version", return="nice", anova_table = list(es = "pes"), fun_aggregate = mean)

# names of anova vars
grep("^[a-zA-Z_01]+$", aov_nice$Effect, value = T)
varsVec <- c("version", "task", "resp", "context", "cocoa")

write.table(export_aovNice(aov_nice, varsVec), file= paste0(tabDir, B, "_comparison_err", ".csv"),
            sep = ";", dec = ".", row.names = F)

if (B == "B1B2"){
  meansXpp <- as.data.frame(group_my(de, error, pp, task_R, correctResp_R, context_R, cocoa, exp))
  
  aov_nice <- aov_ez("pp", "error", de, within=c("task_R", "correctResp_R", "context_R", "cocoa"),
                     between = "exp", return="nice", anova_table = list(es = "pes"), fun_aggregate = mean)
  
  varsVec <- c("exp", "task", "resp", "context", "cocoa")
  
  write.table(export_aovNice(aov_nice, varsVec), file= paste0(tabDir, B, "_labOnline_err", ".csv"),
              sep = ";", dec = ".", row.names = F)
}

# Post-hocs errors --------------------------------

group_my(meansXpp, meanerror, task_R)
group_my(meansXpp, meanerror, correctResp_R)
group_my(meansXpp, meanerror, context_R)
group_my(meansXpp, meanerror, cocoa, correctResp_R)

# plot errors --------------------------------------------------------

# Calculate raw means
gbl <- group_my(de, error, pp, task_R, correctResp_R, context_R, cocoa, version)
condtns1 <- group_my(gbl, meanerror, task_R, cocoa, context_R, correctResp_R, version)
condtns1$colour_R <- factor(condtns1$context_R,
                            labels = c("Context Repet.", "Context Switch"))
condtns1$cocoa <- factor(condtns1$cocoa,
                         labels = c("Onset Asynchr. 0 ms", "Onset Asynchr. 300 ms"))
condtns1$correctResp_R <- factor(condtns1$correctResp_R,
                                 labels = c("Repet.", "Switch"))
condtns1$task_R <- factor(condtns1$task_R, 
                          labels = c("Repet.", "Switch"))
condtns1 <- rename(condtns1, Response = correctResp_R)


# one plot for each cocoa value
for (co in c("Onset Asynchr. 0 ms", "Onset Asynchr. 300 ms")){
  # get only cocoa 0 or 300
  condtns <- condtns1[condtns1$cocoa == co,]
  # Plot it
  plott <- ggplot(condtns, aes(x= task_R, y = meanmeanerror, group = Response, colour = Response)) +
    geom_errorbar(aes(ymin  = meanmeanerror - se, ymax  = meanmeanerror + se), width = 0.3, size  = 0.3, 
                  position = position_dodge(0.3), color = "black") +
    geom_line(position = position_dodge(0.3)) +
    geom_point(aes(colour = Response), position = position_dodge(0.3)) +
    my_theme+
    facet_grid(rows = c(rows = vars(colour_R)), cols = vars(version))+
    ylab("Mean error")+
    xlab("Task")+
    #coord_cartesian(ylim = c(0.3, 0.11))+
    ggtitle(co)
  # save it - ggplot requires print
  png(paste0(figDir, B, "_comparison_err_cocoa", gsub("Onset Asynchr. ", "", co), ".png"),
      width = 1200, height = 1300, res = 200)
  print(plott)
  dev.off()
}

# Parse Rts distributions  -----------------------------------------------------------------
# 
# For each participant, in each combiantion of experimental conditions (task relation * resp
# relation * context relation * cocoa = 16 combinations) we tested whether it was possible to
# reject the hypothesis that rts in block 1, 2 were extracted from a different distribution
# than rts in blocks 1 to 4. If this is the case, it means that collecting more trials
# after the 1st and the 2nd blocks changed the shape of the distribution, or, put 
# differently, that trials from two blocks are not enough to approximate real distribution
# of rts of that participant in that condition. On the other hand, if the hypohtesis is
# not rejected, than we have no reasons to conclude that trials from 4 blocks better
# approximate the real distribution 
# 

# Check how individual Rts distrib change with increasing trials

drt <- dlab[(dlab$task_R != 99 & dlab$rt > 200 & !is.na(dlab$rt)),]
drt <- drt[!(drt$error == 1 | drt$error_R == 1),]

ppsvec <- unique(drt$pp)
#ppsvec <- c("32")
store.tests <- matrix(nrow = length(ppsvec)*16, ncol = 4)
lev = c(0,1)

row = 0
for (p in 1:length(ppsvec)){
  #conto = 0
  for (t in lev){for (r in lev){for (c in lev){for (co in c(0, 300)){
    #conto = conto + 1
    row = row + 1
    for (Slice in c(1,2,3)){
      pname <- ppsvec[p]
      # get for that pp, in that condition in those blocks the rts vector
      one <- drt[drt$pp == pname & drt$blockNum <= Slice*2 & drt$task_R == t & drt$correctResp_R == r &
                   drt$context_R == c & drt$cocoa == co, "rt"]
      #print(one)
      # save lenght of vector one in 4th column for the first smallest slice
      if (Slice == 1){store.tests[row, 4] = length(one)}
      # and with another 25% more of the data
      two <- drt[drt$pp == pname & drt$blockNum <= Slice*2+2 & drt$task_R == t & drt$correctResp_R == r &
                   drt$context_R == c & drt$cocoa == co, "rt"]
      # test the 2 distributions
      kolmo.test <- ks.test(one, two)
      #print(kolmo.test)
      if (kolmo.test$p.value < 0.05){store.tests[row, Slice] = 1
      } else if (kolmo.test$p.value > 0.05){store.tests[row, Slice] = 0}
    }
  }
    
  }}}
}


store.tests[rowSums(store.tests[, c(1,2,3)]) > 0,]
which(rowSums(store.tests) > 0)

write.csv(store.tests, paste0(dataDir, "rtDistrAcrrBlocks.csv"))

ppsvec <- unique(drt$pp)
ppsvec <- c("32")
store.tests <- matrix(nrow = length(ppsvec)*16, ncol = 4)
lev = c(0,1)

row = 0
for (p in 1:length(ppsvec)){
  #conto = 0
  for (t in lev){for (r in lev){for (c in lev){for (co in c(0, 300)){
    #conto = conto + 1
    row = row + 1
    for (Slice in c(1,2,3)){
      pname <- ppsvec[p]
      # and with another 25% more of the data
      two <- drt[drt$pp == pname & drt$blockNum <= Slice*2+2 & drt$task_R == t & drt$correctResp_R == r &
                   drt$context_R == c & drt$cocoa == co, "rt"]
      # test the 2 distributions
      kolmo.test <- ks.test(two, "lognormal")
      #print(kolmo.test)
      if (kolmo.test$p.value < 0.05){store.tests[row, Slice] = 1
      } else if (kolmo.test$p.value > 0.05){store.tests[row, Slice] = 0}
    }
  }
    
  }}}
}



# P-values trends  ----------------------------------------------------------------------

# Observe p values of the task resp context & task context cocoa across participants

# get pps listt
ppslst <- sample(unique(drt$pp))
# sequence of participants to add, add 3 by 3 to save anovas
ppsAfter9 <- seq(1,length(ppslst)-9, 3)
# prepare vectors to fill with pvalues fromt he 2 interactions
vec1 <- rep(0, length(ppsAfter9)-9)
vec2 <- rep(0, length(ppsAfter9)-9)
#pre-allocate loop counter
j = 1
for (i in ppsAfter9){
  #print(i)
  drt_temp <- drt[drt$pp %in% ppslst[1:(9+i)],]
  #print( ppslst[1:9+i])
  aov_nice <- aov_ez("pp", "rt", drt_temp, within=c("task_R", "correctResp_R", "context_R", "cocoa"),
                     return="nice", anova_table = list(es = "pes"), fun_aggregate = mean)
  
  vec1[j] <- as.numeric(aov_nice[aov_nice$Effect == "task_R:context_R:cocoa", "p.value"])
  vec2[j] <- as.numeric(aov_nice[aov_nice$Effect == "task_R:correctResp_R:context_R", "p.value"])
  j=j+1
}

#is.na(vec) <- 0
png(paste0(figDir, "between-Exp_pvalues_curves.png"))
par(mfrow = c(2, 1))
plot(ppsAfter9, vec1, type = "l", xlab = "how many participants more than 9",
     ylab = "p-value", main = "task x context x onset asynchr.", ylim = c(0, 1))
points(ppsAfter9, vec1, pch = 19, col = "dodgerblue")
abline(h = 0.05, lty = "dashed", col = "red")

plot(ppsAfter9, vec2, type = "l", xlab = "how many participants more than 9",
     ylab = "p-value", main = "task x response x context", ylim = c(0, 1))
points(ppsAfter9, vec2, pch = 19, col = "aquamarine3")
abline(h = 0.05, lty = "dashed", col = "red")
dev.off()


# Analyses as 2:1 cue:task mapping exp ------------------------------------------------------------

# first was odn eon B1 only and version did not interact, thus removed from the anova 
# and run the same on merged B1B2 and with exp as variable

d$cue.task.rel <- 99

for (i in 1:dim(d)[1]){
  if (d$context_R[i] == 0 & d$task_R[i] == 0){
    d$cue.task.rel[i] = "doubleRep" 
  } else if (d$context_R[i] == 1 & d$task_R[i] == 0){
    d$cue.task.rel[i] = "cueSw"
    } else if (d$context_R[i] == 1 & d$task_R[i] == 1){
      d$cue.task.rel[i] = "doubleSwitch"
      } 
}

# Dataset cleaning for rts analyses
drt <- d[d$task_R != 99 & d$rt > 200 & !is.na(d$rt),]
drt <- drt[!(drt$error == 1 | drt$error_R == 1),]
drt <- drt[drt$cue.task.rel != 99,]

drt$cue.task.rel <- factor(drt$cue.task.rel, levels=c("doubleRep", "cueSw", "doubleSwitch"))

cat("Removed ", 1 - nrow(drt)/nrow(d), " of the initial dataset. \n")

# ANOVA on cueRep, doubleRep ect

# rts anova
meansxpp <- as.data.frame(group_my(drt, rt, pp, cue.task.rel, exp))

aov_nice <- aov_ez("pp", "meanrt", meansxpp, within=c("cue.task.rel"), between = "exp",
                   return="nice", anova_table = list(es = "pes"))

meansxppc <- as.data.frame(group_my(drt, rt, pp, cue.task.rel, cocoa, exp))

aov_nice <- aov_ez("pp", "meanrt", meansxppc, within=c("cue.task.rel", "cocoa"), between = "exp",
                   return="nice", anova_table = list(es = "pes"))

# plot aggregated over cocoa
condtns1 <- group_my(meansxpp, meanrt, cue.task.rel, exp)

png(paste0(figDir, B, "_2to1Analyses.png"), width = 1300, height = 1300, res = 200)
ggplot(condtns1, aes(x= cue.task.rel, y = meanmeanrt, group = exp, colour = exp)) +
  geom_errorbar(aes(ymin  = meanmeanrt - se, ymax  = meanmeanrt + se), width = 0.3, size  = 0.3, 
                position = pd, color = "black") +
  scale_colour_manual(values=c("black", "red"))+
  geom_line(position = pd) +
  geom_point(aes(colour = exp), position = pd) +
  my_theme+
  ylab("Mean RTs")+
  xlab("Cue & Task Relation")+
  coord_cartesian(ylim = c(00, 900))
dev.off()

# divide by cocoa
condtns2 <- group_my(meansxppc, meanrt, cue.task.rel, cocoa, exp)

png(paste0(figDir, B, "_2to1Analyses_withcocoa.png"), width = 1400, height = 1300, res = 200)
ggplot(condtns2, aes(x= cue.task.rel, y = meanmeanrt, group = exp, colour = exp)) +
  geom_errorbar(aes(ymin  = meanmeanrt - se, ymax  = meanmeanrt + se), width = 0.3, size  = 0.3, 
                position = pd, color = "black") +
  scale_colour_manual(values=c("black", "red"))+
  geom_line(position = pd) +
  geom_point(aes(colour = exp), position = pd) +
  my_theme+
  ylab("Mean RTs")+
  xlab("")+
  coord_cartesian(ylim = c(600, 900))+
  facet_grid(cols = vars(cocoa))
dev.off()

# rts anova with response relation

#meansxppr <- as.data.frame(group_my(drt, rt, pp, cue.task.rel, correctResp_R, cocoa, exp))
meansxppr <- as.data.frame(group_my(drt, rt, pp, cue.task.rel, correctResp_R, cocoa))

aov_nice <- aov_ez("pp", "meanrt", meansxppr, within=c("cue.task.rel", "correctResp_R", "cocoa"),
                   return="nice", anova_table = list(es = "pes"))

varsVec <- c("trial type", "resp", "cocoa")
write.table(export_aovNice(aov_nice, varsVec), file= paste0(tabDir, B, "_2CT_analyses_RTs", ".csv"),
            sep = ";", dec = ".", row.names = F)

# divide by response relation and make the Brac1 graph
condtns3 <- group_my(meansxppr, meanrt, cue.task.rel, correctResp_R, cocoa)
condtns3$correctResp_R <- factor(condtns3$correctResp_R,
                              labels = c("Repet.", "Switch"))
condtns3 <- rename(condtns3, Response = correctResp_R)
condtns3$cocoa <- factor(condtns3$cocoa,
                       labels = c("onset asynchr. 0 ms", "onset asynchr. 300 ms"))

#for (e in unique(d$exp)){
#condtns2 <- condtns3[condtns3$exp == e,]
png(paste0(figDir, B, "_2to1Analyses_withcocoa&resp.png"), width = 1700, height = 1300, res = 200)
ggplot(condtns3, aes(x= cue.task.rel, y = meanmeanrt, group = Response, colour = Response)) +
  geom_errorbar(aes(ymin  = meanmeanrt - se, ymax  = meanmeanrt + se), width = 0.3, size  = 0.3, 
                position = pd, color = "black") +
  #scale_colour_manual(values=c("black", "red"))+
  geom_line(position = pd) +
  geom_point(aes(colour = Response), position = pd) +
  my_theme+
  ylab("Mean RTs")+
  xlab("")+
  coord_cartesian(ylim = c(700, 1000))+
  facet_grid(cols = vars(cocoa))

dev.off()
#}


#post-hocs

for (l in c(levels(drt$cue.task.rel))){for (r in c(levels(drt$correctResp_R))){for (c in c(0, 300)){
  nam <- paste0(l, "_r", r, "_c", c)
  assign(nam, meansxppr[meansxppr$cue.task.rel == l & meansxppr$correctResp_R == r &
                          meansxppr$cocoa == c, "meanrt"])
  print(nam)
}}}

postHocLst <- list(
  "in resp rep, cocoa 0, cueSw - doubleRep" =
    t.test(cueSw_r0_c0, doubleRep_r0_c0, var.equal = T, paired = T, alternative = "g"),
  "in resp sw, cocoa 0, cueSw - doubleRep" =
    t.test(cueSw_r1_c0, doubleRep_r1_c0, var.equal = T, paired = T, alternative = "g"),
  "in resp rep, cocoa 300, cueSw - doubleRep" =
    t.test(cueSw_r0_c300, doubleRep_r0_c300, var.equal = T, paired = T, alternative = "g"),
  "in resp sw, cocoa 300, cueSw - doubleRep" =
    t.test(cueSw_r1_c300, doubleRep_r1_c300, var.equal = T, paired = T)
)

postHoc.output(postHocLst)

#further post-hocs
for (l in c(levels(drt$cue.task.rel))){for (e in c("B1", "B2")) {for (c in c(0, 300)){
  nam <- paste0(l, e, c)
  assign(nam, meansxppc[meansxppc$cue.task.rel == l & meansxppc$exp == e & 
                         meansxppc$cocoa == c, "meanrt"])
  print(nam)
}}}


postHocLst <- list(
  "in B1 benefit of cue rep over cue switch in 0ms" =
    t.test(cueSwB10, doubleRepB10, var.equal = T, paired = T, alternative = "g"),
  "in B1 benefit of cue rep over cue switch in 300ms" =
    t.test(cueSwB1300, doubleRepB1300, var.equal = T, paired = T, alternative = "t"),
  "in B2 benefit of cue rep over cue switch in 0ms" =
    t.test(cueSwB20, doubleRepB20, var.equal = T, paired = T, alternative = "g"),
  "in B2 benefit of cue rep over cue switch in 300ms" =
    t.test(cueSwB2300, doubleRepB2300, var.equal = T, paired = T, alternative = "t")
)

postHoc.output(postHocLst)

# Analyses as 2:1 on Errors -----------------------------------------------------------------------------


# dataset cleaing for error analyses
de <- d[d$task_R != 99 & (d$rt > 200 | is.na(d$rt)),]
de <- de[de$cue.task.rel != 99,]

de$cue.task.rel <- factor(de$cue.task.rel, levels=c("doubleRep", "cueSw", "doubleSwitch"))

cat("Removed ", 1 - nrow(de)/nrow(d), " of the initial dataset. \n")

# old anovas

meansxpp <- as.data.frame(group_my(de, error, pp, cue.task.rel, exp))

aov_nice <- aov_ez("pp", "meanerror", meansxpp, within=c("cue.task.rel"), between = "exp",
                   return="nice", anova_table = list(es = "pes"))

meansxppr <- as.data.frame(group_my(de, error, pp, cue.task.rel, cocoa, corresp))

aov_nice <- aov_ez("pp", "meanerror", meansxppr, within=c("cue.task.rel", "cocoa"), between = "exp",
                   return="nice", anova_table = list(es = "pes"))

# plot errors
# plot
condtns1 <- group_my(meansxpp, meanerror, cue.task.rel, exp)

png(paste0(figDir, B, "_2to1Analyses_err.png"), width = 1300, height = 1300, res = 200)
ggplot(condtns1, aes(x= cue.task.rel, y = meanmeanerror, group = exp, colour = exp)) +
  geom_errorbar(aes(ymin  = meanmeanerror - se, ymax  = meanmeanerror + se), width = 0.3, size  = 0.3, 
                position = pd, color = "black") +
  scale_colour_manual(values=c("black", "red"))+
  geom_line(position = pd) +
  geom_point(aes(colour = exp), position = pd) +
  my_theme+
  ylab("Mean Errors")+
  xlab("")+
  coord_cartesian(ylim = c(0.03, 0.11))
dev.off()

#plot with cocoa
condtns2 <- group_my(meansxppc, meanerror, cue.task.rel, cocoa, exp)

png(paste0(figDir, B, "_2to1Analyses_err_wCococa.png"), width = 1400, height = 1300, res = 200)
ggplot(condtns2, aes(x= cue.task.rel, y = meanmeanerror, group = exp, colour = exp)) +
  geom_errorbar(aes(ymin  = meanmeanerror - se, ymax  = meanmeanerror + se), width = 0.3, size  = 0.3, 
                position = pd, color = "black") +
  scale_colour_manual(values=c("black", "red"))+
  geom_line(position = pd) +
  geom_point(aes(colour = exp), position = pd) +
  my_theme+
  ylab("Mean Errors")+
  xlab("")+
  coord_cartesian(ylim = c(0.03, 0.11))+
  facet_grid(cols = vars(cocoa))
dev.off()


#post-hocs
# I already know for B1:
# 0% t(122) = - 1.68, adj. p = .095 
# 2% t(122) = 6.72, adj. p < .0001 
# let's check B2 only:
# 0% t(248) = 0.02.68, adj. p = .982 
# 2% t(248) = 8.89, adj. p < .0001 

# Anova on also response relation
meansxppr <- as.data.frame(group_my(de, error, pp, cue.task.rel, correctResp_R, cocoa))

aov_nice <- aov_ez("pp", "meanerror", meansxppr, within=c("cue.task.rel", "correctResp_R", "cocoa"),
                   return="nice", anova_table = list(es = "pes"))

varsVec <- c("trialType", "resp", "cocoa")
write.table(export_aovNice(aov_nice, varsVec), file= paste0(tabDir, B, "_2CT_analyses_err", ".csv"),
            sep = ";", dec = ".", row.names = F)

# divide by response relation and make the Brac1 graph
condtns3 <- group_my(meansxppr, meanerror, cue.task.rel, correctResp_R, cocoa)
condtns3$correctResp_R <- factor(condtns3$correctResp_R,
                                 labels = c("Repet.", "Switch"))
condtns3 <- rename(condtns3, Response = correctResp_R)
condtns3$cocoa <- factor(condtns3$cocoa,
                         labels = c("onset asynchr. 0 ms", "onset asynchr. 300 ms"))

png(paste0(figDir, B, "_2to1Analyses_withcocoa&resp_err.png"), width = 1700, height = 1300, res = 200)
ggplot(condtns3, aes(x= cue.task.rel, y = meanmeanerror, group = Response, colour = Response)) +
  geom_errorbar(aes(ymin  = meanmeanerror - se, ymax  = meanmeanerror + se), width = 0.3, size  = 0.3, 
                position = pd, color = "black") +
  #scale_colour_manual(values=c("black", "red"))+
  geom_line(position = pd) +
  geom_point(aes(colour = Response), position = pd) +
  my_theme+
  ylab("Mean Error Rate")+
  xlab("")+
  coord_cartesian(ylim = c(0.03, 0.11))+
  facet_grid(cols = vars(cocoa))
dev.off()

#post-hocs

for (l in c(levels(de$cue.task.rel))){for (r in c(0, 1)){for (c in c(0, 300)){
  nam <- paste0(l, "_r", r, "_c", c)
  assign(nam, meansxppr[meansxppr$cue.task.rel == l & meansxppr$correctResp_R == r &
                          meansxppr$cocoa == c, "meanerror"])
  print(nam)
}}}

postHocLst <- list(
  "in resp rep, cocoa 0, cueSw - doubleRep" =
    t.test(cueSw_r0_c0, doubleRep_r0_c0, var.equal = T, paired = T, alternative = "l"),
  "in resp sw, cocoa 0, cueSw - doubleRep" =
    t.test(cueSw_r1_c0, doubleRep_r1_c0, var.equal = T, paired = T, alternative = "g"),
  "in resp rep, cocoa 300, cueSw - doubleRep" =
    t.test(cueSw_r0_c300, doubleRep_r0_c300, var.equal = T, paired = T, alternative = "g"),
  "in resp sw, cocoa 300, cueSw - doubleRep" =
    t.test(cueSw_r1_c300, doubleRep_r1_c300, var.equal = T, paired = T, alternative = "l")
)

postHoc.output(postHocLst)

# old post-hocs
#meansxpp1 <- meansxpp[meansxpp$exp == "B2",]
meansxpp1 <- as.data.frame(group_my(meansxpp, meanerror, pp, cue.task.rel))

for (l in c(levels(drt$cue.task.rel))){
  nam <- paste0(l, "_means")
  assign(nam, meansxpp[meansxpp$cue.task.rel == l, "meanerror"])
  print(nam)
}
# !! change alternative !!
postHocLst <- list(
  "cueSw - doubleRep" =
    t.test(cueSw_means, doubleRep_means, var.equal = T, paired = T, alternative = "t"),
  "doubleSw - cueSw" =
    t.test(doubleSwitch_means, cueSw_means, var.equal = T, paired = T, alternative = "g")
)

postHoc.output(postHocLst)

# futher post-hocs with cocoa
#further post-hocs
for (l in c(levels(drt$cue.task.rel))){for (e in c("B1", "B2")) {for (c in c(0, 300)){
  nam <- paste0(l, e, c)
  assign(nam, meansxppc[meansxppc$cue.task.rel == l & meansxppc$exp == e & 
                          meansxppc$cocoa == c, "meanerror"])
  print(nam)
}}}


postHocLst <- list(
  "in B1 benefit of cue rep over cue switch in 0ms" =
    t.test(cueSwB10, doubleRepB10, var.equal = T, paired = T, alternative = "t"),
  "in B1 benefit of cue rep over cue switch in 300ms" =
    t.test(cueSwB1300, doubleRepB1300, var.equal = T, paired = T, alternative = "t"),
  "in B2 benefit of cue rep over cue switch in 0ms" =
    t.test(cueSwB20, doubleRepB20, var.equal = T, paired = T, alternative = "g"),
  "in B2 benefit of cue rep over cue switch in 300ms" =
    t.test(cueSwB2300, doubleRepB2300, var.equal = T, paired = T, alternative = "g")
)

postHoc.output(postHocLst)


# Analyses separately on cocoa 0 and 300 -------------------------------------------------------------------

# Rts
for (co in c(0, 300)){
  # get only cocoa 0 or 300
  drt1 <- drt[drt$cocoa == co,]

  aov_nice <- aov_ez("pp", "rt", drt1, within=c("task_R", "correctResp_R", "context_R"),
                     between = "version", return="nice", anova_table = list(es = "pes"), fun_aggregate = mean)
  aov_nice
  # names of anova vars
  grep("^[a-zA-Z_01]+$", aov_nice$Effect, value = T)
  varsVec <- c("version", "task", "resp", "context")
  
  write.table(export_aovNice(aov_nice, varsVec), file= paste0(tabDir, B, "_RTs_cocoa", co, ".csv"),
              sep = ";", dec = ".", row.names = F)
  
}

# post-hoc 0 ms
drt1 <- drt[drt$cocoa == 0,]
meansxpp <- group_my(drt1, rt, pp, task_R, correctResp_R, context_R, version)
group_my(meansxpp, meanrt, task_R)
group_my(meansxpp, meanrt, context_R)
group_my(meansxpp, meanrt, task_R, context_R)
group_my(meansxpp, meanrt, correctResp_R, version)

# post-hoc 300 ms
drt1 <- drt[drt$cocoa == 300,]
meansxpp <- group_my(drt1, rt, pp, task_R, correctResp_R, context_R, version)
group_my(meansxpp, meanrt, task_R)
group_my(meansxpp, meanrt, context_R)
group_my(meansxpp, meanrt, context_R, version)
group_my(meansxpp, meanrt, context_R, correctResp_R)

# 3-way resp, context, version:
meansrrep<- meansxpp[meansxpp$correctResp_R == 0,]
meansrsw <- meansxpp[meansxpp$correctResp_R == 1,]

aov_rrep <- aov_ez("pp", "meanrt", meansrrep, within=c("task_R", "context_R"),
                  between = "version",  return="nice", anova_table = list(es = "pes"))
aov_rsw <- aov_ez("pp", "meanrt", meansrsw, within=c("task_R", "context_R"),
                     between = "version", return="nice", anova_table = list(es = "pes"))

group_my(meansrsw, meanrt, version, context_R)
group_my(meansrrep, meanrt, version, context_R)


# Errors
for (co in c(0, 300)){
  # get only cocoa 0 or 300
  de1 <- de[de$cocoa == co,]
  
  meansXpp <- as.data.frame(group_my(de1, error, pp, task_R, correctResp_R, context_R, version))
  
  aov_nice <- aov_ez("pp", "meanerror", meansXpp, within=c("task_R", "correctResp_R", "context_R"),
                     between = "version", return="nice", anova_table = list(es = "pes"), fun_aggregate = mean)
  
  # names of anova vars
  grep("^[a-zA-Z_01]+$", aov_nice$Effect, value = T)
  varsVec <- c("version", "task", "resp", "context")
  
  write.table(export_aovNice(aov_nice, varsVec), file= paste0(tabDir, B, "_err_cocoa", co, ".csv"),
              sep = ";", dec = ".", row.names = F)
  
}

# post-hoc 0 ms
de1 <- de[de$cocoa == 0,]
meansxpp <- group_my(de1, error, pp, task_R, correctResp_R, context_R, version)
group_my(meansxpp, meanerror, task_R)
group_my(meansxpp, meanerror, correctResp_R)
group_my(meansxpp, meanerror, context_R)
group_my(meansxpp, meanerror, version, correctResp_R, task_R)

# post-hoc 300 ms
de1 <- de[de$cocoa == 300,]
meansxpp <- group_my(de1, error, pp, task_R, correctResp_R, context_R, version)
group_my(meansxpp, meanerror, task_R)
group_my(meansxpp, meanerror, correctResp_R)


# 3-way resp, context, version:
means1<- meansxpp[meansxpp$version == "online",]
means2 <- meansxpp[meansxpp$version == "lab",]

aov_1 <- aov_ez("pp", "meanerror", means1, within=c("task_R", "context_R", "correctResp_R"),
                   #between = "version",
                return="nice", anova_table = list(es = "pes"))
aov_2 <- aov_ez("pp", "meanerror", means2, within=c("task_R", "context_R", "correctResp_R"),
                  #between = "version",
                return="nice", anova_table = list(es = "pes"))

group_my(means1, meanerror, correctResp_R, context_R)


# Check samples: online vs lab --------------------------------------------------------------------------
labd <- d[d$version == "lab",]
onlined <- d[d$version == "online",]

#get ages for lab & online

agev.lab <- rep(0, length(unique(labd$pp)))

for (j in 1:length(unique(labd$pp))){
  pp = unique(labd$pp)[j]
  if (length(unique(labd[labd$pp == pp, "age"])) != 0){
    agev.lab[j] <- unique(labd[labd$pp == pp, "age"])
  }
}
agev.lab <- agev.lab[agev.lab != 0]

agev.online <- rep(0, length(unique(onlined$pp)))

for (j in 1:length(unique(onlined$pp))){
  pp = unique(onlined$pp)[j]
  if (length(unique(onlined[onlined$pp == pp, "age"])) != 0){
    agev.online[j] <- unique(onlined[onlined$pp == pp, "age"])
    print(unique(onlined[onlined$pp == pp, "age"]))
  }
}
agev.online <- agev.online[!is.na(agev.online)]

# test
t.test(agev.online, agev.lab, var.equal = T)

# check sex
d$sex <- factor(d$sex, labels = c("f", "f", "I'd rather not say", "m", "m"))
tbl <- round(table(d$sex, d$version)/384, 0)
tbl <- tbl[c(1,3),]
tbl[,1] <- tbl[,1]/2
chisq.test(tbl)

# check hand
d$hand <- factor(d$hand, labels = c("l", "l", "Other (please specify)", "r", "r"))
tbl <- round(table(d$hand, d$version)/384, 0)
tbl <- tbl[c(1,3),]
tbl[,1] <- tbl[,1]/2
chisq.test(tbl)
