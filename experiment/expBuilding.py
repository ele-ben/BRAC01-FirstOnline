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

# define the strings composing each instruction
magn =  "greater or less than 5."
par = "odd or even."
hori = "When the rectangle is horizontal, your task is to tell whether the number is "
vert = "When the rectangle is vertical, your task is to tell whether the number is "
greatM = "greater than 5."
lessM = "less than 5."
oddM = "odd."
evenM = "even."
A = "Press A to indicate "
L = "Press L to indicate "
#size = "## "
# Create 8 spreadsheets and save them in spreadsheets folder
figures = [[hori, vert], [vert, hori]]
fig_dict = {hori[22:26]: "blackblackhori.png", vert[22:26]: "blackblackvert.png"}
keys = [[A, L], [L, A]]
keys1 = [[A, L], [L, A]]
for fig in figures:
    for key in keys:
        for key1 in keys1:

instr_training(imgDir)

draw(cuecolor, framecolor, orientation, imgDf, imgDir)

#-------- Stimuli and experimental trials

for experiment in ["BRAC1", "BRAC2"]:
    print(experiment)
    # Draw the stimuli and save them in img folder
    # Crate dataframes with all possible kind of trials
    df0_df300 = drawStimuli(experiment, imgDir)
    # save the df with cocoa = 0 and = 300 separately
    df0 = df0_df300[0]
    df300 = df0_df300[1]
    #print(df0[:5])
    # Create experimental blocks and pseudo randomize them
    # create a sequence of 8 block starting with cocoa = 0 and one starting with
    # cocoa = 300
    for i in range(4):
        print(i)
        for startCocoa in [0, 300]:
            experimentDf = buildAndPasteBlocks(df0, df300, startCocoa)
            fileName = experiment + "_1st"+ str(startCocoa) + "_n" + str(i)
            experimentDf.to_csv(sheetDir+fileName+".csv", sep = ";")
