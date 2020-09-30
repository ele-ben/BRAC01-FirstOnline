# Preliminary settings --------------------

# load each packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load("haven", "dplyr", "lme4", "reshape2", "ggplot2", "wesanderson")

select <- dplyr::select
filter <- dplyr::filter

# write the path to your project folder
setwd('C://Users//Elena//Documents//AA_PhD//Projects//BRAC01_BRAC02//BRAC01-FirstOnline')

# define paths to further subfolders in the project folders (create them first)
dataDir = "data//"
figDir = "figures//"
tabDir = "tables//"
logbookDir = paste0(dataDir, "logbooks//")

# a .R file with custom functions - define the path to it if different from the working directory
source("C://Users//Elena//Documents//AA_PhD//Projects//expra2020_faces//modelsFun.R")

# Load data  ------------------------------

# 17308-v16 = OLD_BRAC01 - RWTH
# 18723-v2 = NEW_BRAC01 - RWTH
# 17326-v8 = BRAC02 - RWTH - Solo Mara!
# 17326-v13 = BRAC02 - RWTH
# 18619-v2 or v3 = BRAC02 - Prolific
# (18613-v2 = OLD_BRAC01 - Prolific)
# (18755-v2 = NEW_BRAC01 - Prolific)

# B1Pro1 = NEW + OLD BRAC01 - Prolific, first half of the sample

# task-me89 = BRAC02
# task-in6f = BRAC01

# Load Gorilla data  --------------------------------------------------------------

# Task/experiment data
exp = "18619"
version = "(.)*"
file_extesion = ".csv"

data1_files <- list.files(
  dataDir, pattern = paste0("^d(.)+", exp ,"(.)+task", version, file_extesion)
)

d <- read.csv2(paste0(dataDir, data1_files[1]), dec = ".", fileEncoding="UTF-8-BOM")
d <- d[c(1:nrow(d) - 1), ]

data_old <- read.csv2(paste0(dataDir, data1_files[2]))
#data_exp <- data_exp[c(1:nrow(data_exp) - 1), ]

# Modify datasets to allow merging

# check missing columns
setdiff(names(d), names(data_old))
setdiff(names(data_old), names(d))

# B1 rename the new counterbalance column as the old one
d <- subset(d, select = - counterbalance.9qvq)
names(d)[names(d) == "counterbalance.jqly"] <- "counterbalance.9qvq"

# B2 rename the new counterbalance column as the old one
d <- subset(d, select = - counterbalance.7ip1)
names(d)[names(d) == "counterbalance.a7a6"] <- "counterbalance"


# Load demographics data
demo1_files <- list.files(
  dataDir, pattern = paste0("^d(.)+", exp ,"(.)+questionnaire", version, file_extesion)
)

demo1 <- read.csv2(paste0(dataDir, demo1_files[1]), fileEncoding="UTF-8-BOM")
demo1 <- demo1[c(1:nrow(demo1) - 1), ]

demo2 <- read.csv2(paste0(dataDir, demo1_files[2]))

# modify to allow merging

# check missing columns
setdiff(names(demo1), names(demo2))
setdiff(names(demo2), names(demo1))

# rename the new counterbalance column as the old one
demo1 <- subset(demo1, select = - counterbalance.9qvq) # the one with the same names as the old is empty
names(demo1)[names(demo1) == "counterbalance.jqly"] <- "counterbalance.9qvq"

# B2 - rename the new counterbalance column as the old one
demo1 <- subset(demo1, select = - counterbalance.7ip1) # the one with the same names as the old is empty
names(demo1)[names(demo1) == "counterbalance.a7a6"] <- "counterbalance.9qvq"

# Merge datasets and choose which to parse in the script ---------------------------------
d <- rbind(d, data_old)
demo <- rbind(demo1, demo2) 
#quantiPps <-length(unique(d$Participant.Public.ID))
quantiPps <-length(unique(d$pp))

write.csv2(d, paste0(dataDir, "data_exp_", exp,"_", quantiPps, "pps_task-in6f.csv"), row.names = F)
write.csv2(demo, paste0(dataDir, "data_exp_", exp, "_", quantiPps, "pps_questionnaire-89vf.csv"), row.names = F)
#write.csv2(demo, paste0(dataDir, "data_exp_", exp, "_PlusMara_", "questionnaire-b579.csv"), row.names = F)

# write.csv2(d, paste0(dataDir, "data_exp_", "_RWTH_OldNew_", "task-in6f.csv"), row.names = F)
# write.csv2(demo, paste0(dataDir, "data_exp_", "_RWTH_OldNew_", "questionnaire-89vf.csv"), row.names = F)

#d <- data_exp
demo <- demo1

# Rename certain columns in exp data ------

names(d)[which(names(d) == "Event.Index")] <- "eventIndex"
names(d)[which(names(d) == "Participant.Public.ID")] <- "pp"
names(d)[which(names(d) == "Reaction.Time")] <- "rt"
names(d)[which(names(d) == "Zone.Type")] <- "zoneType"
names(d)[which(names(d) == "Zone.Name")] <- "zoneName"
names(d)[which(names(d) == "Spreadsheet Row")] <- "spreadsheetRow"
names(d)[which(names(d) == "Local.Date")] <- "localDate"
names(d)[which(names(d) == "checkpoint.t5ey")] <- "finalCheckpoint"
names(d)[which(names(d) == "checkpoint.7y1f")] <- "finalCheckpoint"
names(d)[which(names(d) == "Participant.Viewport.Size")] <- "ppViewportSize"
names(d)[which(names(d) == "counterbalance.9qvq")] <- "counterbalance"

names(d)[which(names(d) == "Incorrect")] <- "error"
names(d)[which(names(d) == "blockN")] <- "blockNum"
names(d)[which(names(d) == "trialN")] <- "trialNum"

d$pp <- as.factor(d$pp)
d$rt <- as.numeric(d$rt)
d$eventIndex <- as.numeric(d$eventIndex)
d$blockNum <- as.numeric(d$blockNum)
d$trialNum <- as.numeric(d$trialNum)

# demographics

names(demo)[which(names(demo) == "Question.Key")] <- "questionKey"
names(demo)[which(names(demo) == "Participant.Public.ID")] <- "pp"


# Inspect the dataset to check for undesiderable events --------

# If you don't want to go over the old pps... :
# reload the old logbook:
# - logbook_B1_Pro
# - logbook_B1_RWTH
# - logbook_B2_Pro
# - logbook_B2RWTH

oldLogbook <- read.csv2(paste0(logbookDir, "logbook_B2_Pro", ".csv"))

# detect the old pps and subtract them from the whole set of pps
oldPps <- oldLogbook$pp
#oldPps <- unique(data_exp[data_exp$Experiment.Version == 2, "Participant.Public.ID"])
newPps <- setdiff(unique(d$pp), oldPps)

# prepare some variables that will be filled in the loop below:
# for durations and logbook_df run the row with d$pp OR the one with newPps

# vector that collects duration of experiment for each pp. Then you can extract mean duration in minutes
#durations <- rep(0, length(unique(d$pp)))
durations <- rep(0, length(newPps))

# create the logbook, a df that will contain demographics and other info for each pp.
#logbook_df <- data.frame("pp" = unique(d$pp))
logbook_df <- data.frame("pp" = newPps)

# it also contains a column to signal if a pp is to remove (set to 0 (=accept) as default)
logbook_df$remove <- 0

# contains info about the counterbalance condition of each
logbook_df$mapping <- ""

# and a column for comments
logbook_df$message <- ""

# Loop over pps to check if some events happened...
#... run either the first or the second line:
#for (j in unique(d$pp)){
for (j in newPps){
  
  # preallocate empty strings that will be filled if sth went wrong
  screen_mess = ""
  weird_duration_mess = ""
  finish_mess = ""
  
  #check & save mapping/counterbalance
  #print(d[d$pp == j & d$display == "Instructions", c("counterbalance", "greatMap", "oddMap", "magnMap")][1,])
  logbook_df[logbook_df$pp == j, "mapping"] <- unique(d[d$pp == j, "counterbalance"])
  
  # did she re-read instructions?
  if (sum(d$pp == j & d$Response == "Re-read instructions", na.rm = T) != 0) {
    cat("pp ", j, "has read instructions more than once. \n")
  }
  
  # did she get to the end?
  if (unique(d[d$pp == j, "Checkpoint"]) != "demographics done" & unique(d[d$pp == j, "finalCheckpoint"]) != "after demographic"){
    cat("pp ", j, " has not gotten to the end.\n" )
    finish_mess = paste("pp ", j, " has not gotten to the end.")
  } else {
    # if she finished, calculate duration
    begin <- as.POSIXct(as.character(d[d$pp == j & d$eventIndex == 1, "UTC.Date"]),format="%d/%m/%Y %H:%M:%S")
    # choose the 1st or the 2nd couple or rows: the 1st counts duration from instr to end, the 2nd includes time to answer to demogr
    
    # duration excluding demographics
    endRow <- which(d$pp == j & d$breakMessage == "## You have completed block 4 out of 4.")
    end <- as.POSIXct(as.character(d[endRow, "UTC.Date"]), format="%d/%m/%Y %H:%M:%S")
    
    # duration including demographics
    # endRow <- which(demo$pp == j & demo$questionKey == "END QUESTIONNAIRE")
    # end <- as.POSIXct(as.character(demo[endRow, "localDate"]), format="%d/%m/%Y %H:%M:%S")
    
    duration <- difftime(end, begin, units = "mins")
    durations[which(unique(d$pp) == j)] <- duration
    if (duration > 35 | duration < 14) {
      cat("pp ", j, " took", duration, "minutes to complete the experiment.\n")
    }
    
    # if she finished, check for comments
    # did she left any comments?
    #if (!is.na(demo[demo$pp == j & demo$questionKey == "comment", "Response"])){
    if (demo[demo$pp == j & demo$questionKey == "comment", "Response"] != ""){
      cat("pp ", j, "left a comment:", demo[demo$pp == j & demo$questionKey == "comment", "Response"], "\n\n")
    }
  }
  
  
  # did she have big enouhg screen?
  screenSize <- unique(d[d$pp == j, "ppViewportSize"])
  #w = strsplit(as.character(screenSize), 'x')[1]
  # get screen height (the width shouldn't be a concern)
  h = strsplit(as.character(screenSize), 'x')[[1]][2]
  # the minum is derived by some trials using gorilla preview and simultaneosly this website:
  # https://whatismyviewport.com/
  if (as.integer(h) < 578) {
    cat("pp ", j, "screen height was", h,"while the min necessary is 578.\n" )
    screen_mess <- paste0("screen height was ", h," while the min necessary is 578;")
  }
  
  # did she have some weird trials durations?
  if (sum(d$pp == j & d$zoneType == "fixation" & d$rt > 1410, na.rm = T) > 0) {
    cat("pp", j, "has had",  sum(d$pp == j & d$zoneType == "fixation" & d$rt > 1410, na.rm = T), 
        "weird fixations durations \n\n")
    weird_duration_mess = paste(
      "has had",  sum(d$pp == j & d$zoneType == "fixation" & d$rt > 1410, na.rm = T), 
      "weird fixations durations;")
  }
  
  if (sum(d$pp == j & d$zoneName == "advancementZone" & d$display == "trial" & d$rt > 310, na.rm = T) > 0) {
    cat("pp ", j, "has had", sum(
      d$pp == j & d$zoneName == "advancementZone" & d$display == "trial" & d$rt > 310, na.rm = T),
      "weird cue durations \n\n")
    weird_duration_mess = paste(
      weird_duration_mess, "has had", sum(d$pp == j & d$zoneName == "advancementZone" & d$display == "trial" & d$rt > 310, na.rm = T), "weird cue durations;")
  }
  
  # did she have a loading delay somewhere?
  load_delays <- sum(d$pp == j & d$rt == "LOADING DELAY", na.rm = T)
  if (load_delays >0) {cat("pp ", j, " has had", load_delays, "loading delays.\n\n")
  }
  
  
  # fill the pp's logbook line with the info collected in the loop
  logbook_df[logbook_df$pp == j, "message1"] <- paste0(screen_mess, weird_duration_mess, finish_mess)
  # if there are info in the message column, ask if want ot keep the pp or not
  if (logbook_df[logbook_df$pp == j, "message1"] != ""){
    # the typed answer is written in the remove column
    logbook_df[logbook_df$pp == j, "remove"] <- remove_him(j)
  }
}

cat( "mean duration of the experiment was", mean(durations[durations != 0], na.rm = T), "minutes")

# Obtain a clean dataset ---------------------------

# keep zone 1 (no resp) and zone 3 and 4 (left an right resp) events of the trial part (no training)
dclean <- d[
  (d$zoneName == "Zone1" | d$zoneName == "Zone3" | d$zoneName == "Zone4") & d$display == "trial", 
  c("pp", "counterbalance", "blockNum", "trialNum", "rt", "Response", "Attempt", "error", "Timed.Out", 
    "ANSWER", "cuecolor", "framecolor", "task", "stimulus", "cocoa", "Participant.OS", "Participant.Browser",
    "ppViewportSize")
  ]

# Remove empty rows, but before printing how many NA are there. It happens only in xlsx files
print(sum(is.na(dclean$pp)))
dclean <- dclean[!is.na(dclean$pp), ]

# check if each pp has 384 trials, if not print & write in the logbook
pps2check <- vector()
for (j in unique(d$pp)){
#for (j in newPps){  
  #print(j)
  trialMess = ""
  finalMess = ""
  for (t in 0:95){
    rowsCount <- sum(dclean$pp == j & dclean$trialNum == t)
    if (rowsCount != 4){
      cat("pp", j, "had", rowsCount, "time trial number", t, "\n")
      trialMess = paste("had", rowsCount, "time trial number", t, ";")
      finalMess = paste0(finalMess, trialMess)
      logbook_df[logbook_df$pp == j, "message"] <- finalMess
      oldLogbook[oldLogbook$pp == j, "message"] <- finalMess
      pps2check <- c(pps2check, j)
    }
  }
}
  
  
# # pp 5d893d3fc993870001ccd14e had twice trial 16 in block 0
# wh <- which(dclean$pp == "5d893d3fc993870001ccd14e" & dclean$blockNum == 0 & dclean$trialNum == 16 & 
#               is.na(dclean$Response))
# dclean <- dclean[-c(wh),]


# Check the performance --------------------------------

# seek for fast responses, errors and not answered trials
#for (j in unique(dclean$pp)){
for (j in newPps){
  
  # prepare empty strings to fill logbook
  fast_mess = ""
  slow_mess = ""
  error_mess = ""
  miss_mess = ""
  
  # check different perofrmance markers
  # how many faster than 200 ms repsonses?
  if (sum(dclean$pp == j & dclean$rt < 200) > 38){
    cat("pp ", j, " did", sum(dclean$pp == j & dclean$rt < 200), "super fast resp \n")
    fast_mess = paste0(" did ", sum(dclean$pp == j & dclean$rt < 200), " super fast responses;")
  }
  # how many slower than 2000 ms repsonses?
  if (sum(dclean$pp == j & dclean$rt > 2000) > 38){
    cat("pp ", j, " did", sum(dclean$pp == j & dclean$rt > 2000), "very slow resp \n")
    slow_mess = paste0(" did ", sum(dclean$pp == j & dclean$rt > 2000), " very slow responses;")
  }
  # how many errors?
  if (sum(dclean$pp == j & dclean$error) > 38){
    cat("pp ", j, " did", sum(dclean$pp == j & dclean$error), "errors \n")
    error_mess = paste0(" did ", sum(dclean$pp == j & dclean$error), " errors;")
  }
  # how many misses?
  if (sum(dclean$pp == j & dclean$Attempt == 1, na.rm = T) < 346){
    cat("pp ", j, " did not answer to", sum(is.na(dclean$pp == j & dclean$Attempt == 0)), " trials \n")
    miss_mess = paste0(" did not answer to ", sum(is.na(dclean$pp == j & dclean$Attempt == 0)), " trials;")
  }
  
  # fill-in the logbook
  logbook_df[logbook_df$pp == j, "message2"] <- paste0(fast_mess, slow_mess, error_mess, miss_mess)
  # prompt the user with the remove qestion, only if the message2 is full and the remove is not 1 already!
  if (logbook_df[logbook_df$pp == j, "message2"] != "" & logbook_df[logbook_df$pp == j, "remove"] == 0){
    logbook_df[logbook_df$pp == j, "remove"] <- remove_him(j)
  }
}

# Manage acceptances and rejections --------------------------------------------------------------

# Export the pps ID of the accepted ones to upload in Prolific
acceptedPps <- logbook_df[logbook_df$remove == 0, "pp"]
# warning for the user
cat("You're accepting", length(acceptedPps), "participants")
write.table(acceptedPps, paste0(logbookDir, "accept_", Sys.Date(), ".csv"), sep = ",", col.names = F, 
            row.names = F, quote = F)

# export rejection message
rejectText <- data.frame("pp" = logbook_df[logbook_df$remove == 1, "pp"], "mail" = "")

texts <- read.csv2(paste0(logbookDir, "reject_mess_prolific", ".csv"), header = F, fileEncoding="UTF-8-BOM")

for (j in rejectText$pp){
  fix_mess <- gsub("did not", "you didn't", logbook_df[logbook_df$pp == j, "message2"])
  fix_mess <- gsub("did ", "you made ", fix_mess)
  fix_mess = gsub("; ", " and ", fix_mess)
  fix_mess = gsub(";", " ", fix_mess)
  rejectText[rejectText$pp == j, "mail"] <- paste0(texts[1,2], fix_mess, texts[2,2])
}

write.csv2(rejectText, paste0(logbookDir, "reject_", Sys.Date(), ".csv"), row.names = F, quote = F)

# Demographics ----------------------------------------

# Draw the interesting info and re.organize the demo ds

# select the interesting columns
demotemp <- demo[, c("pp", "questionKey", "Response")]
# transform the dataset from long to wide
demotemp1 <- dcast(demotemp, pp ~ questionKey, value.var = "Response")
# select only some of the answers (many are redudant)
demotemp1 <- demotemp1[, c("pp", "age", "sex", "country", "education", "response-2",
                           "response-3","psyKnowledge", "comment")]
# change columns names to clearer names
names(demotemp1)[which(names(demotemp1) == "response-2")] <- "handedness"
names(demotemp1)[which(names(demotemp1) == "response-3")] <- "motherTongue"
# remove na if there are
demotemp1 <- demotemp1[!is.na(demotemp1$pp),]

# Fix the open questions Country & Language -----------------

# Country 

demotemp1$country[demotemp1$country == "Wales"] <- "United Kingdom"
demotemp1$country[demotemp1$country == "wales"] <- "United Kingdom"
demotemp1$country[demotemp1$country == "England"] <- "United Kingdom"
demotemp1$country[demotemp1$country == "england"] <- "United Kingdom"
demotemp1$country[demotemp1$country == "English"] <- "United Kingdom"
demotemp1$country[demotemp1$country == "UK"] <- "United Kingdom"
demotemp1$country[demotemp1$country == "uk"] <- "United Kingdom"
demotemp1$country[demotemp1$country == "Northern Ireland"] <- "United Kingdom"
demotemp1$country[demotemp1$country == "United Kingdom (England)"] <- "United Kingdom"
demotemp1$country[demotemp1$country == "Scotland"] <- "United Kingdom"
demotemp1$country[demotemp1$country == "united kingdom"] <- "United Kingdom"
demotemp1$country[demotemp1$country == "United Kingsom"] <- "United Kingdom"
demotemp1$country[demotemp1$country == "UK/England"] <- "United Kingdom"
demotemp1$country[demotemp1$country == "england "] <- "United Kingdom"
demotemp1$country[demotemp1$country == "germany"] <- "Germany"
demotemp1$country[demotemp1$country == "italy"] <- "Italy"
demotemp1$country[demotemp1$country == "Netherlands"] <- "The Netherlands"
demotemp1$country[demotemp1$country == "poland"] <- "Poland"
demotemp1$country[demotemp1$country == "Portual"] <- "Portugal"
demotemp1$country[demotemp1$country == "portugal"] <- "Portugal"
demotemp1$country[demotemp1$country == "España"] <- "Spain"
demotemp1$country[demotemp1$country == "andalusia"] <- "Spain"
demotemp1$country[demotemp1$country == "United States"] <- "USA"
demotemp1$country[demotemp1$country == "Alabama"] <- "USA"
demotemp1$country[demotemp1$country == "france"] <- "France"
demotemp1$country[demotemp1$country == "Grrece"] <- "Greece"
demotemp1$country[demotemp1$country == "greece"] <- "Greece"
demotemp1$country[demotemp1$country == "Polska"] <- "Poland"

# MotherTongue 

demotemp1$motherTongue[demotemp1$motherTongue == "english"] <- "English"
demotemp1$motherTongue[demotemp1$motherTongue == "german"] <- "German"
demotemp1$motherTongue[demotemp1$motherTongue == "italian"] <- "Italian"
demotemp1$motherTongue[demotemp1$motherTongue == "Hungary"] <- "Hungarian"
demotemp1$motherTongue[demotemp1$motherTongue == "polish"] <- "Polish"
demotemp1$motherTongue[demotemp1$motherTongue == "Polish "] <- "Polish"
demotemp1$motherTongue[demotemp1$motherTongue == "Polish language"] <- "Polish"
demotemp1$motherTongue[demotemp1$motherTongue == "Poland"] <- "Polish"
demotemp1$motherTongue[demotemp1$motherTongue == "portuguese"] <- "Portuguese"
demotemp1$motherTongue[demotemp1$motherTongue == "Portuguese (PT)"] <- "Portuguese"
demotemp1$motherTongue[demotemp1$motherTongue == "português"] <- "Portuguese"
demotemp1$motherTongue[demotemp1$motherTongue == "spanish"] <- "Spanish"
demotemp1$motherTongue[demotemp1$motherTongue == "greek"] <- "Greek"

# unique(demotemp1$motherTongue)
# unique(demotemp1$country)


# Merge Demo with Logbook -----------------------

# Merge new pps in this demo dataframe with the logbook, matching the rows by pp ID
# all.x allows to include pp that did exp but not demographics
# At the same time we are merging only the pps in the logbook and not the old ones
#logbook_toExp <- merge(logbook_df, demotemp1, by = "pp", all.x = T)

# Or, if old logbook present:
logbook_toExp1 <- merge(logbook_df, demotemp1, by = "pp", all.x = T)

# try this line, if it says: "Error: object 'X' not found" then just skip this line and
# run the one below
oldLogbook <- subset(oldLogbook, select = - X)
# oldLogbook <- subset(oldLogbook, select = - fake_mapping)
# names(oldLogbook)[names(oldLogbook) == "response.2"] <- "handedness"
# names(oldLogbook)[names(oldLogbook) == "response.3"] <- "motherTongue"

# pile up the new and the old logbooks

# run the loop below if the current logbook contains old pps
for (ppRem in logbook_toExp1$pp){
  print(ppRem)
  oldLogbook <- oldLogbook[!oldLogbook$pp == ppRem, ]
}

#oldLogbook$message1 <- ""

logbook_toExp <- rbind(oldLogbook, logbook_toExp1)

# Now set to remove = 1 those pps who were rewarded, but must not be analysed

# Remove 5dada613823b7d0016484453 for taking part in brac2
logbook_toExp[logbook_toExp$pp == "5dada613823b7d0016484453", "remove"] <- 1
logbook_toExp[logbook_toExp$pp == "5dada613823b7d0016484453", "message"] <- "for taking part in brac2"

# # Print messages of the rewarded ones
# rewardedPps <- logbook_toExp[logbook_toExp$remove == 0, "pp"]
# for (j in rewardedPps){
#   print(logbook_toExp[logbook_toExp == j, "message"])
# }

# Fix mapping for wrong expeirment rwth brac1

# for (j in oldPps){
#   logbook_toExp[logbook_toExp$pp == j, "message1"] <- "had BRAC1_horiAA_1st300 mapping"
#   logbook_toExp[logbook_toExp$pp == j, "mapping"] <- "BRAC1_horiAA_1st300"
# }

if (unique(dclean$framecolor)[1] == "black"){
  B = "B1"
} else if (unique(dclean$cuecolor)[1] == "black"){
  B = "B2"
}
B

# save this dataframe for future inspections

# - _Pro.csv
# - _RWTH.csv

write.csv2(logbook_toExp, paste0(logbookDir, "logbook_", B, "_Pro.csv"), row.names = F)


# Refine cleaned dataset --------------------

# Remove the pps to be removed

#load the logbook (if not done already)
#logbook_toExp <- read.csv2(paste0(logbookDir, "logbook_B1_Pro.csv"))

# find pps to remove from the loogbook (that now includes old and new pps)
pps2remove <- logbook_toExp[logbook_toExp$remove == 1, "pp"]
# warning for the user
cat("You're about to remove", length(pps2remove), "participants")
# remove them
for (ppRem in pps2remove){
  print(ppRem)
  dclean <- dclean[!dclean$pp == ppRem, ]
}

rownames(dclean) <- NULL

# Add Sequence variables

# create a column to signal rep or sw for each relevant variable (task, response, context..)
# different colour column if BRAC1 or BRAC2
if (B == "B1"){
  dclean <- sequence_relation(dclean, c("task", "ANSWER", "cuecolor"), max(d$trialNum, na.rm = T) + 1)
} else if (B == "B2"){
  dclean <- sequence_relation(dclean, c("task", "ANSWER", "framecolor"), max(d$trialNum, na.rm = T) + 1)
}

# sum(dclean[dclean$task_R == 99, "trialNum"]) == 0
# sum(dclean$ANSWER_R == 99) == length(unique(dclean$pp))*length(unique(dclean$blockNum))

# create a column to signal post-errors trials
dclean <- sequence_relation(dclean, "error", max(d$trialNum, na.rm = T) + 1, type = "error")

# Add demographic variables to the dataset
for (j in unique(demotemp1$pp)){ # some pps don't have demo (1 in B1)
  dclean[dclean$pp == j, "age"] <- demotemp1[demotemp1$pp == j, "age"]
  dclean[dclean$pp == j, "sex"] <- demotemp1[demotemp1$pp == j, "sex"]
  dclean[dclean$pp == j, "psyKnowledge"] <- demotemp1[demotemp1$pp == j, "psyKnowledge"]
  dclean[dclean$pp == j, "education"] <- demotemp1[demotemp1$pp == j, "education"]
  dclean[dclean$pp == j, "handedness"] <- demotemp1[demotemp1$pp == j, "handedness"]
  dclean[dclean$pp == j, "motherTongue"] <- demotemp1[demotemp1$pp == j, "motherTongue"]
  dclean[dclean$pp == j, "country"] <- demotemp1[demotemp1$pp == j, "country"]
}

dclean[dclean$age == "twenty one years old" & !is.na(dclean$age), "age"] <- 21

# fix Browser and OS
for (i in 1:dim(dclean)[1]){
  dclean$Participant.OS[i] <- gsub("^Mac OS(.)+", "MacOs", dclean$Participant.OS[i])
  dclean$Participant.OS[i] <- gsub("^Windows(.)+", "Windows", dclean$Participant.OS[i])
  dclean$Participant.Browser[i] <- gsub("^Safari(.)+", "Safari", dclean$Participant.Browser[i])
  dclean$Participant.Browser[i] <- gsub("^Chrome(.)+", "Chrome", dclean$Participant.Browser[i])
}

# save the dataset ready for the analyses
# - _Pro.csv
# - _RWTH.csv
write.csv2(dclean, paste0(dataDir, B, "_Pro.csv"), row.names=FALSE)

# Counterbalance overview ---------------------------------------

# Count mappings
maps <- unique(logbook_toExp$mapping)
mapps <- data.frame("map" = maps, "count" = rep(0, length(maps)))
mapps$incl_remove <- 0

for (i in 1:length(maps)){
  mapps[i,2] <- sum(logbook_toExp$mapping == mapps[i,1] & logbook_toExp$remove == 0)
  mapps[i,3] <- sum(logbook_toExp$mapping == mapps[i,1])
}

sum(mapps$count)
sum(mapps$incl_remove)
write.csv(mapps[mapps$count < 4, "map"], paste0(tabDir, "counterbalcneToRun.csv"))
