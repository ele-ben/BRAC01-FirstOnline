# Building Gorilla version of BRAC01 and BRAC02
import sys
sys.path.append(r'C:\Users\Elena\Documents\AA_PhD\PsychoPy\MyFunctions')
import pandas as pd
import csv
# from funx_10 import orderStimWithinTasks_str as pseudorandomize
# from funx_10 import DfBooleanOrder
from Draw_tasks_4 import draw, drawStimuli, trainingTrials, ANSWER, buildAndPasteBlocks

# project folders
sheetDir = "spreadsheets/"
imgDir = "img/"

# Do (almost) the same operations for th e2 experiments
for experiment in ["BRAC1", "BRAC2"]:

    #-------- Instructions + training Trials

    # -------- Define some variables --------

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
    size = "## "

    # fit the strings in convenient lists
    figures = [[hori, vert], [vert, hori]]
    fig_dict = {"hori": "blackblackhori.png", "vert": "blackblackvert.png"}
    keys = [[A, L], [L, A]]
    keys1 = [[A, L], [L, A]]

    # preallocate instructions df
    instructions = pd.DataFrame([], columns = ["display", "magnMap",
    "parMap", "greatMap", "lessMap", "oddMap", "evenMap", "firstFig",
     "secondFig"])

    # loop over the string list to build the different experiment versions having
    # different mappings
    for fig in figures:
        for key in keys:
            for key1 in keys1:

                # -------- Instructions + training Trials --------

                # build the instructions row with the current combination
                row = {
                    "display": "Instructions", "magnMap": size + fig[0] + magn,
                    "parMap": size + fig[1] + par,
                    "greatMap":size + key[0] + greatM,
                    "lessMap": size + key[1]+ lessM,
                    "oddMap": size + key1[0] + oddM,
                    "evenMap": size + key1[1] + evenM,
                    "firstFig": fig_dict[fig[0][22:26]],
                    "secondFig": fig_dict[fig[1][22:26]]
                    }
                # fill the 1-row instructions-df
                instructions = instructions.append(row, ignore_index = True)
                # generate a string to "name" the current mapping
                map = fig[0][22:26] + key[0][6:7] + key1[0][6:7]
                # generate traning trials
                trainingShuf = trainingTrials(imgDir)
                # fill the right answer to training trials  given the current combination
                ANSWER(trainingShuf, fig, key, key1)
                # add the start display as last row of training trials
                startDisplay = {"display": "start"}
                trainingShuf.append(startDisplay, ignore_index = True)

                # --------- Draw the "mappings Guide" ---------

                #for orientation in ["hori", "vert"]:
                #draw("black", "black", orientation, imgDf, imgDir)

                # --------- Experimental blocks ---------

                # Draw the stimuli and save them in img folder
                # Crate dataframes with all possible kind of trials
                df0_df300 = drawStimuli(experiment, imgDir)
                # save these basic dfs with cocoa = 0 and = 300 separately
                df0 = df0_df300[0]
                df300 = df0_df300[1]
                # Pseudo randomize  experimental blocks and pile 8 of them up:
                # Create a sequence of 8 block starting with cocoa = 0 and one starting with
                # cocoa = 300
                for startCocoa in [0, 300]:
                    # multiply the basic dfs to create a block, randomize, pile up
                    experimentDf = buildAndPasteBlocks(df0, df300, startCocoa)
                    # concatenate instructions, training block, experimental blocks
                    fullExperiment = pd.concat(
                        [instructions, trainingShuf, experimentDf],
                        sort = False, ignore_index = True
                        )
                    fileName = experiment + "_" + map + "_1st"+ str(startCocoa)
                    fullExperiment.to_csv(sheetDir + fileName +".csv", sep = ";")

# draw(cuecolor, framecolor, orientation, imgDf, imgDir)
#
# #-------- Stimuli and experimental trials
#
# for experiment in ["BRAC1", "BRAC2"]:
#     print(experiment)
#     # Draw the stimuli and save them in img folder
#     # Crate dataframes with all possible kind of trials
#     df0_df300 = drawStimuli(experiment, imgDir)
#     # save the df with cocoa = 0 and = 300 separately
#     df0 = df0_df300[0]
#     df300 = df0_df300[1]
#     #print(df0[:5])
#     # Create experimental blocks and pseudo randomize them
#     # create a sequence of 8 block starting with cocoa = 0 and one starting with
#     # cocoa = 300
#     for i in range(4):
#         print(i)
#         for startCocoa in [0, 300]:
#             experimentDf = buildAndPasteBlocks(df0, df300, startCocoa)
#             fileName = experiment + "_1st"+ str(startCocoa) + "_n" + str(i)
#             experimentDf.to_csv(sheetDir+fileName+".csv", sep = ";")
