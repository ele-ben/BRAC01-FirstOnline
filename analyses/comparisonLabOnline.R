# Load packages -------------------------------

if(!require(pacman)) install.packages("pacman")
pacman::p_load("haven", "dplyr", "lme4", "reshape2", "ggplot2", "RColorBrewer", "afex", "emmeans", "tables")

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

# Load and prepare Data --------------------------------------------------------------------------------------

B = "B1"
#B = "B2"

if (B == "B1"){
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
  
  # Lab data B1
  
  # change project folder
  setwd('C://Users//Elena//Documents//AA_PhD//Projects//BRAC01_BRAC02//BRAC02-labVersion')
  
  cleanData <- list.files(dataDir, pattern = paste0("B1_lab(.)+.csv"))
  dlab <- read.csv2(paste0(dataDir, cleanData), dec = ".")
  dlab$version <- "lab"
  
  names(dlab)[names(dlab) == "colour_R"] <- "context_R"
  
  # # remove the last 4 blocks to have same number of trials
  # dlab <- dlab[dlab$blockNum < 5,]
  
  # change it back!
  setwd('C://Users//Elena//Documents//AA_PhD//Projects//BRAC01_BRAC02//BRAC01-FirstOnline')
  
} else if (B == "B2"){
  # Online data B2
  
  # Load B1 and pick 2 people form rwth only
  d_pro <- read.csv(paste0(dataDir, "B2_Pro", ".csv"), sep = ";", dec = ",")
  d_pro$prolific <- 1
  
  d_rwth <- read.csv(paste0(dataDir, "B2_RWTH", ".csv"), sep = ";", dec = ",")
  d_rwth$prolific <- 0
  
 
  # bind the online datsets
  d1 <- rbind(d_rwth, d_pro)
  d1$version <- "online"
  
  names(d1)[names(d1) == "framecolor_R"] <- "context_R"
  names(d1)[names(d1) == "ANSWER_R"] <- "correctResp_R"
  
  # Lab data B2
  
  # change project folder
  setwd('C://Users//Elena//Documents//AA_PhD//Projects//BRAC01_BRAC02//BRAC02-labVersion')
  
  cleanData <- list.files(dataDir, pattern = paste0("B2_lab(.)+.csv"))
  dlab <- read.csv2(paste0(dataDir, cleanData), dec = ".")
  dlab$version <- "lab"
  
  names(dlab)[names(dlab) == "colour_R"] <- "context_R"
  
  # # remove the last 4 blocks to have same number of trials
  # dlab <- dlab[dlab$blockNum < 5,]
  
  # change it back!
  setwd('C://Users//Elena//Documents//AA_PhD//Projects//BRAC01_BRAC02//BRAC01-FirstOnline')
  
}

# get commong coloumns and bind them in the final dataset
common_cols <- intersect(names(d1), names(dlab))
dlab$pp <- as.factor(dlab$pp)
# d <- rbind(d1[, c("task_R")], dlab[, c("task_R")])
# or...
d <- rbind(subset(d1, select = common_cols), subset(dlab, select = common_cols))
# 
# v1 <- lapply(d1[,common_cols], class)
# vl <- lapply(dlab[,common_cols], class)

# remove partial datasets
rm(list = c("d_pro", "d_rwth", "dlab", "d1"))

# Change variables class 

d$task_R <- as.factor(d$task_R)
d$correctResp_R <- as.factor(d$correctResp_R)
d$context_R <- as.factor(d$context_R)
d$cocoa <- as.factor(d$cocoa)
d$version <- as.factor(d$version)
d$age <- as.numeric(d$age)
d$sex <- as.factor(d$sex)
d$rt <- as.numeric(d$rt)

#d[d$cocoa == 0,]

# Dataset cleaning for rts analyses
cat("Fast trials were", sum(d$rt < 200, na.rm = T), "\n")
drt <- d[(d$task_R != 99 & d$rt > 200 & !is.na(d$rt)),]
drt <- drt[!(drt$error == 1 | drt$error_R == 1),]

# dataset cleaing for error analyses
de <- d[d$task_R != 99 & (d$rt > 200 | is.na(d$rt)),]

# Remove outliers -------------------------------------------------------------------------------------------
# who are the worst ones

# calculate mean for each pp
errM <- group_my(d, error, pp, task_R, correctResp_R, context_R, cocoa)
pps <- group_my(errM, meanerror, pp)
cat("Mean error rate was",  mean(pps$meanmeanerror), "plus or minus", sd(pps$meanmeanerror))

# OR 20??????????
ppsWorse <- as.data.frame(pps[pps$meanmeanerror > 0.19, "pp"])

for (ppRem in ppsWorse$pp){
  #print(ppRem)
  d <- d[!d$pp == ppRem, ]
  drt <- drt[!drt$pp == ppRem, ]
  de <- de[!de$pp == ppRem, ]
}

# Plot expeirments ---------------------------------------------------------------------

# Calculate raw means
gbl <- group_my(drt, rt, pp, task_R, correctResp_R, context_R, cocoa, version)
condtns1 <- group_my(gbl, meanrt, task_R, cocoa, context_R, correctResp_R, version)
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
png(paste0(figDir, B, "_comparison_Rts.png"), width = 1300, height = 1300, res = 200)
ggplot(condtns1, aes(x= task_R, y = meanmeanrt, group = Response, colour = Response)) +
  geom_errorbar(aes(ymin  = meanmeanrt - se, ymax  = meanmeanrt + se), width = 0.3, size  = 0.3, 
                position = position_dodge(0.3), color = "black") +
  geom_line(position = position_dodge(0.3)) +
  geom_point(aes(colour = Response), position = position_dodge(0.3)) +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold")) +
  #facet_wrap(~ version + cocoa + context_R)+
  facet_grid(rows = c(vars(colour_R), vars(cocoa)), cols = vars(version))+
  ylab("Mean RTs")+
  xlab("Task")+
  coord_cartesian(ylim = c(500, 900))
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
    theme_minimal() +
    theme(axis.title = element_text(face = "bold"), text = element_text(size=16)) +
    facet_grid(rows = c(vars(colour_R)), cols = vars(version))+
    ylab("Mean RTs")+
    xlab("Task")+
    ggtitle(co)+
    coord_cartesian(ylim = c(500, 900))
  # save it - ggplot requires print
  png(paste0(figDir, B, "_comparison_Rts_cocoa", gsub("Onset Asynchr. ", "", co), ".png"),
      width = 1200, height = 1300, res = 200)
  print(plott)
  dev.off()
}


# ANOVA on Rts -----------------------------------------------------------------

aov_nice <- aov_ez("pp", "rt", drt, within=c("task_R", "correctResp_R", "context_R", "cocoa"),
                   between = "version", return="nice", anova_table = list(es = "pes"), fun_aggregate = mean)

aov_nice <- aov_ez("pp", "rt", drt, within=c("task_R", "correctResp_R", "context_R"),
                   between = "version", return="nice", anova_table = list(es = "pes"), fun_aggregate = mean)
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

# task x resp x context
# retrieve vectors of the means
lev = c(0,1)
for (t in lev){for (r in lev){for (c in lev){
  nam <- paste0("t", t, ".r", r, ".c", c)
  assign(nam, meansXpp[meansXpp$task_R == t & meansXpp$correctResp_R == r & 
                         meansXpp$context_R == c, "meanrt"])
}}}
# look into task switch costs and response rep ebenfit
# tests
postHocLst <- list(
  "effect of context on task switch costs in resp. rep." =
    t.test(t1.r0.c0 - t0.r0.c0, t1.r0.c1 - t0.r0.c1, var.equal = T, paired =  T),
  "effect of context on task switch costs in resp. sw." =
    t.test(t1.r1.c0 - t0.r1.c0, t1.r1.c1 - t0.r1.c1, var.equal = T, paired =  T),
  "effect of context on resp repetition in task rep" =
    t.test(t0.r0.c1, t0.r0.c0, var.equal = T, paired =  T)
)
# adjust p-values and return postHocs output
postHoc.df <- postHoc.output(postHocLst)
postHoc.df

# run 2 3-way anovas to check task, context ,cocoa intercation
only0 <- meansXpp[meansXpp$cocoa == 0,]
only300 <- meansXpp[meansXpp$cocoa == 300,]

aov_0 <- aov_ez("pp", "meanrt", only0, within=c("task_R", "correctResp_R", "context_R"),
                return="nice", anova_table = list(es = "pes"))

aov_300 <- aov_ez("pp", "meanrt", only300, within=c("task_R", "correctResp_R", "context_R"),
                  return="nice", anova_table = list(es = "pes"))

group_my(only0, meanrt, task_R, context_R)

# ANOVA on errors

meansXpp <- as.data.frame(group_my(de, error, pp, task_R, correctResp_R, context_R, cocoa, version))

aov_nice <- aov_ez("pp", "error", de, within=c("task_R", "correctResp_R", "context_R", "cocoa"),
                   between = "version", return="nice", anova_table = list(es = "pes"), fun_aggregate = mean)

# names of anova vars
grep("^[a-zA-Z_01]+$", aov_nice$Effect, value = T)
varsVec <- c("version", "task", "resp", "context", "cocoa")

write.table(export_aovNice(aov_nice, varsVec), file= paste0(tabDir, B, "_comparison_err", ".csv"),
            sep = ";", dec = ".", row.names = F)



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
    theme_minimal() +
    theme(axis.title = element_text(face = "bold"), text = element_text(size=16)) +
    facet_grid(rows = c(vars(colour_R)), cols = vars(version))+
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

# dataset cleaing for error analyses
de <- d[d$task_R != 99 & (d$rt > 200 | is.na(d$rt)),]
de <- de[de$cue.task.rel != 99,]

de$cue.task.rel <- factor(de$cue.task.rel, levels=c("doubleRep", "cueSw", "doubleSwitch"))

# ANOVA on cueRep, doubleRep ect

# rts anova
meansxpp <- as.data.frame(group_my(drt, rt, pp, cue.task.rel, version))

aov_nice <- aov_ez("pp", "meanrt", meansxpp, within=c("cue.task.rel"), between = "version",
                   return="nice", anova_table = list(es = "pes"))

meansxpp <- as.data.frame(group_my(drt, rt, pp, cue.task.rel, correctResp_R, version))

aov_nice <- aov_ez("pp", "meanrt", meansxpp, within=c("cue.task.rel", "correctResp_R"), between = "version",
                   return="nice", anova_table = list(es = "pes"))

# plot
condtns1 <- group_my(meansxpp, meanrt, cue.task.rel, version)

png(paste0(figDir, B, "_2to1Analyses.png"), width = 1300, height = 1300, res = 200)
ggplot(condtns1, aes(x= cue.task.rel, y = meanmeanrt, group = version, colour = version)) +
  geom_errorbar(aes(ymin  = meanmeanrt - se, ymax  = meanmeanrt + se), width = 0.3, size  = 0.3, 
                position = position_dodge(0.3), color = "black") +
  scale_colour_manual(values=c("#E69F00", "#56B4E9"))+
  geom_line(position = position_dodge(0.3)) +
  geom_point(aes(colour = version), position = position_dodge(0.3)) +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold"), text = element_text(size=16)) +
  ylab("Mean RTs")+
  xlab("Cue & Task Relation")+
  coord_cartesian(ylim = c(500, 900))
dev.off()

condtns2 <- group_my(meansxpp, meanrt, cue.task.rel, correctResp_R, version)

png(paste0(figDir, B, "_2to1Analyses_withResp.png"), width = 1300, height = 1300, res = 200)
ggplot(condtns2, aes(x= cue.task.rel, y = meanmeanrt, group = correctResp_R, colour = correctResp_R)) +
  geom_errorbar(aes(ymin  = meanmeanrt - se, ymax  = meanmeanrt + se), width = 0.3, size  = 0.3, 
                position = position_dodge(0.3), color = "black") +
  geom_line(position = position_dodge(0.3)) +
  geom_point(aes(colour = correctResp_R), position = position_dodge(0.3)) +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold")) +
  facet_grid(rows = c(cols = vars(version)))+
  ylab("Mean RTs")+
  xlab("Cue & Task Relation")+
  coord_cartesian(ylim = c(500, 900))
dev.off()

#post-hocs
meansxpp1 <- as.data.frame(group_my(meansxpp, meanrt, pp, cue.task.rel))

for (l in c(levels(drt$cue.task.rel))){
  nam <- paste0(l, "_means")
  assign(nam, meansxpp[meansxpp$cue.task.rel == l, "meanrt"])
  print(nam)
}

postHocLst <- list(
  "cueSw - doubleRep" =
    t.test(cueSw_means, doubleRep_means, var.equal = T, paired = T, alternative = "g"),
  "doubleSw - cueSw" =
    t.test(doubleSwitch_means, cueSw_means, var.equal = T, paired = T, alternative = "g")
)

postHoc.output(postHocLst)


# anova on Errors as 2:1 -----------------------------------------------------------------------------

meansxpp <- as.data.frame(group_my(de, error, pp, cue.task.rel, version))

aov_nice <- aov_ez("pp", "meanerror", meansxpp, within=c("cue.task.rel"), between = "version",
                   return="nice", anova_table = list(es = "pes"))

# plot errors
# plot
condtns1 <- group_my(meansxpp, meanerror, cue.task.rel, version)

png(paste0(figDir, B, "_2to1Analyses_err.png"), width = 1300, height = 1300, res = 200)
ggplot(condtns1, aes(x= cue.task.rel, y = meanmeanerror, group = version, colour = version)) +
  geom_errorbar(aes(ymin  = meanmeanerror - se, ymax  = meanmeanerror + se), width = 0.3, size  = 0.3, 
                position = position_dodge(0.3), color = "black") +
  scale_colour_manual(values=c("#E69F00", "#56B4E9"))+
  geom_line(position = position_dodge(0.3)) +
  geom_point(aes(colour = version), position = position_dodge(0.3)) +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold"), text = element_text(size=16)) +
  coord_cartesian(ylim = c(0.03, 0.11))+
  xlab("")+
  ylab("Mean Error Rate")
dev.off()


#post-hocs
meansxpp1 <- as.data.frame(group_my(meansxpp, meanerror, pp, cue.task.rel))

for (l in c(levels(drt$cue.task.rel))){
  nam <- paste0(l, "_means")
  assign(nam, meansxpp[meansxpp$cue.task.rel == l, "meanerror"])
  print(nam)
}

postHocLst <- list(
  "cueSw - doubleRep" =
    t.test(cueSw_means, doubleRep_means, var.equal = T, paired = T, alternative = "l"),
  "doubleSw - cueSw" =
    t.test(doubleSwitch_means, cueSw_means, var.equal = T, paired = T, alternative = "g")
)

postHoc.output(postHocLst)
