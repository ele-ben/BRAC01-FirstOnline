import math, csv
import pandas as pd
from PIL import Image, ImageDraw, ImageFont
from win32api import GetSystemMetrics
from BRACfun import no_StimRepetition, shuffle_rows

def draw(cuecolor, framecolor, orientation, imgDf, training = ""):
    myDir = "C:/Users/Elena/Documents/AA_PhD/Projects/BRAC01-FirstOnline/experiment/"
    imgDir = "img/"
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
    img.save(myDir + imgDir + cueFileName)
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
            img.save(myDir + imgDir + stimFileName)
            # save image info in a dictionary to append to a dataframe
            row = {"cuecolor": cuecolor, "framecolor": framecolor, "orientation": orientation,
                "stimulus": int(stimulus), "cueFileName": cueFileName, "stimFileName": stimFileName
            }
            # append the dictionary
            imgDf = imgDf.append(row, ignore_index = True)
    return imgDf

# run the function in a loop to generate all the requred colours and numbers
# generate the images and save the df with the details in a csv
def drawStimuli(BRAC):
    if BRAC == 1:
        colNames = ["cuecolor", "framecolor", "orientation", "stimulus", "cueFileName",
            "stimFileName"]
        # prepare empty df
        blackFrDf = pd.DataFrame([], columns = colNames)
        # first BRAC01 file, where the frame stays black and the cue varies
        imgSheetName = "blackFrame"
        # run the function for all the possible combinations of BRAC01
        for cuecolor in ["blue", "red", "black"]:
            framecolor = "black"
            for orientation in ["hori", "vert"]:
                blackFrDf = draw(cuecolor, framecolor, orientation, blackFrDf)
        # add other variables relevant for Gorilla
        blackFrDf["display"] = "trial"
        blackFrDf["randomise_trials"] = 1
        blackFrDf["cue0FileName"] = blackFrDf["cueFileName"]
        #define cue0FileName column for cocoa = 300, when the cue-frame config is
        # black black before getting the colours
        blackFrDf1 = blackFrDf.copy()
        blackFrDf1["randomise_trials"] = 2
        for i in range(len(blackFrDf1)):
            if blackFrDf1["orientation"].loc[i] == "hori":
                blackFrDf1["cue0FileName"].loc[i] = "blackblackhori.png"
            elif blackFrDf1["orientation"].loc[i] == "vert":
                blackFrDf1["cue0FileName"].loc[i] = "blackblackvert.png"

        blackFrDf = blackFrDf.append(blackFrDf1, ignore_index = True)
        # ....
        # save the df in a csv to be uploaded in Gorilla
        blackFrDf.to_csv("spreadsheets/"+imgSheetName + ".csv", sep = ";")
    elif BRAC == 2:
        # second BRAC02 file, where the cue stays black and the frame varies
        blackCuDf = pd.DataFrame([], columns = colNames)
        # declare the name of the csv
        imgSheetName = "blackCue"
        for framecolor in ["blue", "red"]: # I don't need the black here
            cuecolor = "black"
            for orientation in ["hori", "vert"]:
                blackCuDf = draw(cuecolor, framecolor, orientation, blackCuDf)
        # add other variables relevant for Gorilla
        # ....
        # save the df in a csv to be uploaded in Gorilla
        blackCuDf.to_csv("spreadsheets/"+imgSheetName + ".csv", sep = ";")
    else:
        print("function input must be either integer 1 or int 2")

## - Defining training trials
def trainingTrials():
    # prepare empty df
    colNames = ["cuecolor", "framecolor", "orientation", "stimulus", "cueFileName",
        "stimFileName", "task", "ANSWER"]
    trainDf = pd.DataFrame([], columns = colNames)
    for orientation in ["hori", "vert"]:
        trainDf = draw("black", "black", orientation, trainDf, training = 1)
    # use self-made no_stimRepetition function to avoid n-1 repetitions of stimuli
    #training.merge(trainDf, on = ["orientation", "stimuli"])
    stimuli = [1,2,3,4,6,7,8,9]
    training_stimuli = no_StimRepetition(len(trainDf), stimuli)
    trainingShuf = shuffle_rows(training_stimuli, trainDf, "stimulus")
    trainingShuf["display"] = "training"
    return trainingShuf
    #trainingShuf.to_csv("spreadsheets/"+"trainingTrials" + ".csv", sep = ";")

##- Define the spreadsheets driving instructions presentations and trainingShuf
# create the 8 different instructions + training spreadsheets
def instr_training():
#INSTRUCTIONS
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
    # compose sentences in loop to get the possible mappings
    # define lists to loop in
    figures = [[hori, vert], [vert, hori]]
    fig_dict = {hori[22:26]: "blackblackhori.png", vert[22:26]: "blackblackvert.png"}
    keys = [[A, L], [L, A]]
    keys1 = [[A, L], [L, A]]
    fig = figures[0]
    for fig in figures:
        for key in keys:
            for key1 in keys1:
                # prepare the dataframe to host instructions info
                instr = pd.DataFrame([], columns = ["display", "magnMap",
                "parMap", "greatMap", "lessMap", "oddMap", "evenMap", "firstFig",
                 "secondFig"])
                # build the instructions row with the possible combinations
                row = {"display": "Instructions", "magnMap": fig[0] + magn,
                    "parMap": fig[1] + par, "greatMap": key[0] + greatM,
                    "lessMap": key[1]+ lessM, "oddMap": key1[0] + oddM,
                    "evenMap": key1[1] + evenM, "firstFig": fig_dict[fig[0][22:26]],
                    "secondFig": fig_dict[fig[1][22:26]]}
                instr = instr.append(row, ignore_index = True)
                # create a self-speaking name: orientation of cue for magnitude
                # key for great and key for odd
                instFile = fig[0][22:26] + key[0][6:7] + key1[0][6:7]
                # TRAINING TRIALS
                # generate the sequence of trials
                trainingShuf = trainingTrials()
                # assign the actual task to each row, given the cue
                trainingShuf.loc[trainingShuf["orientation"] == fig_dict[fig[0][22:26]], "task"] = "magnit"
                trainingShuf.loc[trainingShuf["orientation"] == fig_dict[fig[1][22:26]], "task"] = "parity"
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
                # export the spreadsheets
                instrPlusTraining.to_csv("spreadsheets/"+ instFile + ".csv", sep = ";", index= False)
