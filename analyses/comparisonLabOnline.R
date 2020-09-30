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

# get commong coloumns and bind them in the final dataset
common.cols <- intersect(names(d1), names(dlab))
d <- rbind(d1[, common.cols], dlab[, common.cols])

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


ppsWorse <- as.data.frame(pps[pps$meanmeanerror > 0.2, "pp"])

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
png(paste0(figDir, "comparison_Rts.png"), width = 1300, height = 1300, res = 200)
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


# ANOVA on Rts -----------------------------------------------------------------

aov_nice <- aov_ez("pp", "rt", drt, within=c("task_R", "correctResp_R", "context_R", "cocoa"),
                   between = "version", return="nice", anova_table = list(es = "pes"), fun_aggregate = mean)
# names of anova vars
grep("^[a-zA-Z_01]+$", aov_nice$Effect, value = T)
varsVec <- c("version", "task", "resp", "context", "cocoa")

write.table(export_aovNice(aov_nice, varsVec), file= paste0(tabDir, "comparison_RTs", ".csv"),
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

write.table(export_aovNice(aov_nice, varsVec), file= paste0(tabDir, "comparison_RTs", ".csv"),
            sep = ";", dec = ".", row.names = F)


# Parse Rts distributions  -----------------------------------------------------------------

# Check how individual Rts distrib change with increasing trials

drt <- dlab[(dlab$task_R != 99 & dlab$rt > 200 & !is.na(dlab$rt)),]
drt <- drt[!(drt$error == 1 | drt$error_R == 1),]

ppsvec <- unique(drt$pp)
store.tests <- matrix(nrow = length(ppsvec)*16, ncol = 3)
lev = c(0,1)

for (p in 1:length(ppsvec)){
  conto = 0
  for (t in lev){for (r in lev){for (c in lev){for (co in c(0, 300)){
    conto = conto + 1
    for (Slice in c(1,2,3)){
      pname <- ppsvec[p]
      # get for that pp, in that condition in those blocks the rts vector
      one <- drt[drt$pp == pname & drt$blockNum <= Slice*2 & drt$task_R == t & drt$correctResp_R == r &
                   drt$context_R == c & drt$cocoa == co, "rt"]
      # and with another 25% more of the data
      two <- drt[drt$pp == pname & drt$blockNum <= Slice*2+2 & drt$task_R == t & drt$correctResp_R == r &
                   drt$context_R == c & drt$cocoa == co, "rt"]
      # test the 2 distributions
      kolmo.test <- ks.test(one, two)
      if (kolmo.test$p.value < 0.05){store.tests[p, Slice] = 1}
      else if (kolmo.test$p.value > 0.05){store.tests[p, Slice] = 0}
    }
  }
    
  }}}}


store.tests[rowSums(store.tests) > 0,]
which(rowSums(store.tests) > 0)

