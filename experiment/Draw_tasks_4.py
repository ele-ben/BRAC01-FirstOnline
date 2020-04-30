import math, csv
import pandas as pd
from PIL import Image, ImageDraw, ImageFont
from win32api import GetSystemMetrics
import sys
sys.path.append(r'C:\Users\Elena\Documents\AA_PhD\PsychoPy\MyFunctions')
#from BRACfun import no_StimRepetition, shuffle_rows
from funx_10 import shuffle_rows, noStimRepetition, DfBooleanOrder
from funx_10 import orderStimWithinTasks_str as pseudorandomize

def draw(cuecolor, framecolor, orientation, imgDf, imgDir, training = ""):
    #myDir = "C:/Users/Elena/Documents/AA_PhD/Projects/BRAC01-FirstOnline/experiment/"
    #imgDir = "img/"
    background = (255,255,255)
    # General
    w, h = 800, 600
    orientations = ["hori", "vert"]
    # Frame
    x1, x2 = w/2 - 250, w/2 + 250
    y1, y2 =  h/2 - 250,  h/2 + 250
    # Text
    (x, y) = w/2 - 13.8666666666, h*0.5 - 26.6666666667
    color = 'rgb(0, 0, 0)' # black color
    # Stimuli
    stimuli = [1, 2, 3, 4, 6, 7, 8, 9]
    # define a function that draws my experimental scree: an rectangle (the cue) either
    # vertical or horizontal and below, in the centre, a number (the stimulus)
    # Both are surronded by an empty square
    # define name of the image file based on the parameters
    cueFileName = cuecolor + framecolor + orientation + ".png"
    # define position of the cue given the orientation
    if orientation=="hori":
        x3, x4 = w/2 - 50, w/2 + 50
        y3, y4 = ((y1 + y2)/2 + y1)/2 - 20, ((y1 + y2)/2 + y1)/2 + 20
    if orientation=="vert":
        x3, x4 = w/2 - 20, w/2 + 20
        y3, y4 = ((y1 + y2)/2 + y1)/2 - 50, ((y1 + y2)/2 + y1)/2 + 50
    # save coordinates of the cue and of the frame
    shape = [(x1, y1), (x2,y2)]
    shape2 = [(x3, y3), (x4, y4)]
    # creating new Image object - white background
    img = Image.new("RGB", (w, h), color=background)
    # draw the cue and the frame
    img1 = ImageDraw.Draw(img)
    font = ImageFont.truetype('arial.ttf', size=40)
    img1.rectangle(shape, fill = background, outline =framecolor, width=2)
    img1.rectangle(shape2, fill= cuecolor, outline=cuecolor)
    # save the image, on purpose without number
    img.save(imgDir + cueFileName)
    # because of the experimental design, I don't want any number when both are
    # black
    if cuecolor == "black" and framecolor == "black" and training == "":
        pass
    else:
    # Adding stimuli
        for num in stimuli:
            stimulus = str(num)
            img1 = ImageDraw.Draw(img)
            font = ImageFont.truetype('arial.ttf', size=40)
            img1.rectangle(shape, fill =background, outline =framecolor, width=2)
            img1.rectangle(shape2, fill=cuecolor, outline=cuecolor)
            img1.text((x, y), stimulus, fill=color, font=font, align="center")
            stimFileName = cuecolor + framecolor + orientation + stimulus + ".png"
            # save the image with each number
            img.save(imgDir + stimFileName)
            # save image info in a dictionary to append to a dataframe
            row = {"cuecolor": cuecolor, "framecolor": framecolor, "orientation": orientation,
                "stimulus": int(stimulus), "cueFileName": cueFileName, "stimFileName": stimFileName
            }
            # append the dictionary
            imgDf = imgDf.append(row, ignore_index = True)
    return imgDf

# call the draw function to draw the images and save their details in a df
# info in the df are further organized s.t it represents the minimum info needed
# to build each experimental block
def drawStimuli(experiment, imgDir):
    # prepare empty df
    colNames = ["cuecolor", "framecolor", "orientation", "stimulus", "cueFileName",
        "stimFileName"]
    blackFrDf = pd.DataFrame([], columns = colNames)
    # the images to be drawn are different in BRAC1 and BRAC2
    if experiment == "BRAC1":
        for cuecolor in ["blue", "red", "black"]: # cue varies and frame stays black
            framecolor = "black"
            for orientation in ["hori", "vert"]:
                blackFrDf = draw(cuecolor, framecolor, orientation, blackFrDf, imgDir)
    elif experiment == "BRAC2":
        for framecolor in ["blue", "red", "black"]: # frame varies and cue stays black
            cuecolor = "black"
            for orientation in ["hori", "vert"]:
                blackFrDf = draw(cuecolor, framecolor, orientation, blackFrDf, imgDir)
    else:
        print("function first input must be either string BRAC1 or BRAC2")
    # add other experimental or Gorilla variables
    blackFrDf["display"] = "trial"
    blackFrDf["cocoa"] = 0 # the delay is set to 0
    # we create a column to define what happens the first 300 ms of cue presentation
    # When delay - cocoa - is 0, nothin changes between the first and the second
    # 300 ms, thus the column values are set identical to cueFileName
    blackFrDf["cue0FileName"] = blackFrDf["cueFileName"]
    # create a copy of the df in which cocoa will be 300
    blackFrDf300 = blackFrDf.copy()
    blackFrDf300["cocoa"] = 300
    # when cocoa=300, in the first 300 ms both shapes are black
    # in order to assign the correct image, we need to know whether the cue is
    # hori or vert in that trial
    for i in range(len(blackFrDf300)):
        # if cue is hori then assign the blackblack img with hori cue
        if blackFrDf300["orientation"].iloc[i] == "hori":
            blackFrDf300["cue0FileName"].iloc[i] = "blackblackhori.png"
        # if cue is vert then assign the blackblack img with vert cue
        elif blackFrDf300["orientation"].iloc[i] == "vert":
            blackFrDf300["cue0FileName"].iloc[i] = "blackblackvert.png"
    return [blackFrDf, blackFrDf300]

## - Defining training trials
def trainingTrials(imgDir):
    # prepare empty df
    colNames = ["cuecolor", "framecolor", "orientation", "stimulus", "cueFileName",
        "stimFileName", "task", "ANSWER"]
    trainDf = pd.DataFrame([], columns = colNames)
    for orientation in ["hori", "vert"]:
        trainDf = draw("black", "black", orientation, trainDf, imgDir, training = 1)
    # use self-made no_stimRepetition function to avoid n-1 repetitions of stimuli
    #training.merge(trainDf, on = ["orientation", "stimuli"])
    stimuli = [1,2,3,4,6,7,8,9]
    training_stimuli = noStimRepetition(len(trainDf), stimElmns = stimuli)
    trainingShuf = shuffle_rows(training_stimuli, trainDf, "stimulus")
    trainingShuf["display"] = "training"
    return trainingShuf
    #trainingShuf.to_csv("spreadsheets/"+"trainingTrials" + ".csv", sep = ";")

##- Define the spreadsheets driving instructions presentations and trainingShuf
# create the 8 different instructions + training spreadsheets
def instr_training(imgDir):
#INSTRUCTIONS
# # define the strings composing each instruction
#     magn =  "greater or less than 5."
#     par = "odd or even."
#     hori = "When the rectangle is horizontal, your task is to tell whether the number is "
#     vert = "When the rectangle is vertical, your task is to tell whether the number is "
#     greatM = "greater than 5."
#     lessM = "less than 5."
#     oddM = "odd."
#     evenM = "even."
#     A = "Press A to indicate "
#     L = "Press L to indicate "
#     size = "## "
    # # compose sentences in loop to get the possible mappings
    # # define lists to loop in
    # figures = [[hori, vert], [vert, hori]]
    # fig_dict = {hori[22:26]: "blackblackhori.png", vert[22:26]: "blackblackvert.png"}
    # keys = [[A, L], [L, A]]
    # keys1 = [[A, L], [L, A]]
    # for fig in figures:
    #     for key in keys:
    #         for key1 in keys1:
                # prepare the dataframe to host instructions info
                instr = pd.DataFrame([], columns = ["display", "magnMap",
                "parMap", "greatMap", "lessMap", "oddMap", "evenMap", "firstFig",
                 "secondFig"])
                # build the instructions row with the possible combinations
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
                instr = instr.append(row, ignore_index = True)
                # create a self-speaking name: orientation of cue for magnitude
                # key for great and key for odd
                instFile = fig[0][22:26] + key[0][6:7] + key1[0][6:7]
                # TRAINING TRIALS
                # generate the sequence of trials
                trainingShuf = trainingTrials(imgDir)
                # assign the actual task to each row, given the cue
                trainingShuf.loc[trainingShuf["orientation"] == fig[0][22:26], "task"] = "magnit"
                trainingShuf.loc[trainingShuf["orientation"] == fig[1][22:26], "task"] = "parity"
                # fill in answer column for Gorilla to give feedvback
                # extract the correct key from the mapping of the current loop
                oddKey = key1[0][6:7]
                evenKey = key1[1][6:7]
                greatKey = key[0][6:7]
                lessKey = key[1][6:7]
                trainingShuf.loc[(trainingShuf["task"] == "magnit") & (trainingShuf["stimulus"] > 5), "ANSWER"] = greatKey
                trainingShuf.loc[(trainingShuf["task"] == "magnit") & (trainingShuf["stimulus"] < 5), "ANSWER"] = lessKey
                trainingShuf.loc[(trainingShuf["task"] == "parity") & (trainingShuf["stimulus"] % 2 == 0), "ANSWER"] = evenKey
                trainingShuf.loc[(trainingShuf["task"] == "parity") & (trainingShuf["stimulus"] % 2 != 0), "ANSWER"] = oddKey
                #paste the training trials below the instructions row
                instrPlusTraining = pd.concat([instr, trainingShuf], ignore_index=True, sort=False)
                # add another row for the last display before the experiment starts
                startDisplay = {"display": "start", "InstrRow": "-" + str(len(instrPlusTraining))}
                instrPlusTraining = instrPlusTraining.append(startDisplay, ignore_index = True)
                # # export the spreadsheets
                # instrPlusTraining.to_csv("spreadsheets/"+ instFile + ".csv", sep = ";", index= False)

# build blocks, pseudorandomize them and paste them together
def buildAndPasteBlocks(df0, df300, startCocoa):
    # We want 3 exemplars for each kind of trial
    trialsPercondition = 3
    block0 = pd.concat([df0]*trialsPercondition, ignore_index = True)
    block300 = pd.concat([df300]*trialsPercondition, ignore_index = True)
    # define whether they have blocks 2,4,6,8 with cocoa = 0 and 1,3,5,7 with
    # cocoa 300 or viceersa
    if startCocoa == 0:
        blocks = [block0, block300]*4
    elif startCocoa == 300:
        blocks = [block300, block0]*4
    else:
        print("startCocoa must be either int 0 or 300")
    experiment = pd.DataFrame([], columns = block0.columns)
    for i in range(len(blocks)):
        block = blocks[i]
        stimElmns = list(set(block.stimulus))
        cues = list(set(block.orientation))
        #set block number
        block.blockN = i
        #help(pseudorandomize)
        Stim_Cues = pseudorandomize(len(block), stimElmns, 1, *cues)
        #help(DfBooleanOrder)
        block_shuf = DfBooleanOrder(
            block, "stimulus", Stim_Cues.stim, "orientation", Stim_Cues.task
        )
        # append a interblock-break screen
        breakScreen = {
            "display": "break",
            "breakMessage": "You have completed block " +str(i+1)+ " out of 8."
            }
        block_shuf = block_shuf.append(breakScreen, ignore_index=True, sort = False)
        experiment = experiment.append(block_shuf, sort = False)
    return experiment
