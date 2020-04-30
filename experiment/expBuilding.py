# Building Gorilla version of BRAC01 and BRAC02
import sys
sys.path.append(r'C:\Users\Elena\Documents\AA_PhD\PsychoPy\MyFunctions')
import pandas as pd
import csv
from funx_10 import orderStimWithinTasks_str as pseudorandomize
from funx_10 import DfBooleanOrder
from Draw_tasks_4 import draw, drawStimuli, trainingTrials, instr_training, buildAndPasteBlocks

# project folders
sheetDir = "spreadsheets/"
imgDir = "img/"

#-------- Instructions + training Trials

# Create 8 spreadsheets and save them in spreadsheets folder
instr_training()

#-------- Stimuli and experimental trials

for experiment in ["BRAC1", "BRAC2"]:

    # Draw the stimuli and save them in img folder
    # Crate dataframes with all possible kind of trials
    df0_df300 = drawStimuli(experiment, imgDir)
    # save the df with cocoa = 0 and = 300 separately
    df0 = df0_df300[0]
    df300 = df0_df300[1]

    # Create experimental blocks and pseudo randomize them
    # create a sequence of 8 block starting with cocoa = 0 and one starting with
    # cocoa = 300
    for i in range(4):
        for startCocoa in [0, 300]:
            experimentDf = buildAndPasteBlocks(df0, df300, startCocoa)
            fileName = "experiment" + str(startCocoa) + "_num" + str(i)
            experimentDf.to_csv(sheetDir+fileName+".csv", sep = ";")
