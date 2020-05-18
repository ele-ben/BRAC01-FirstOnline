import math, csv
import pandas as pd
from PIL import Image, ImageDraw, ImageFont
from win32api import GetSystemMetrics
import sys
# indicate where the funx10 module is stored...
#sys.path.append(r'C:\Users\Elena\Documents\AA_PhD\PsychoPy\MyFunctions')
# ... or put the funx_10 in the same folder and comment the line above
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

def ANSWER(trainingShuf, fig, key, key1):
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
    #print(trainingShuf[:4])


# magn =  "greater or less than 5."
# par = "odd or even."
# hori = "When the rectangle is horizontal, your task is to tell whether the number is "
# vert = "When the rectangle is vertical, your task is to tell whether the number is "
# greatM = "greater than 5."
# lessM = "less than 5."
# oddM = "odd."
# evenM = "even."
# A = "Press A to indicate "
# L = "Press L to indicate "
# size = "## "
#
# # fit the strings in convenient lists
# figures = [[hori, vert], [vert, hori]]
# fig_dict = {"hori": "blackblackhori.png", "vert": "blackblackvert.png"}
# keys = [[A, L], [L, A]]
# keys1 = [[A, L], [L, A]]
# fig = figures[1]
# key = keys[1]
# key1 = keys1[1]

def mappingsGuide(fig,  key, key1, imgDir):
    """Draw a mapping guide that visually describes cue-task and response-key
    mappings.

    Based on the current mappings draw a figure with 2 frame + cue images and
    the 4 words of the responses below them, such that their position suggests
    whther each word is associated to the left- or the righmost key.

    Parameters
    ----------
    task_cue_map: current task cue mapping
    key_resp_map: current response-key mapping
    cueMap: string indictaing current task_cue_map
    keyMap: string indictaing current key_resp_map
    cues: output of drawCues function
    cueDir: drectory where to save the output figures

    Returns
    ----------
    string
        the name of the figure generated.
    """
    # define white background
    background = (255, 255, 255)
    # define dimension of the outer frame in pixels
    lato = 120
    imageSize = [lato, lato]
    spaceBetw = lato
    # define dimension of the image in pixels
    w, h = imageSize[0]*2 + spaceBetw*3, 170
    # define coordinates for right frame
    topLx_FrameRx = (w - spaceBetw - lato, h/2 - lato/2)
    botRx_FrameRx = (topLx_FrameRx[0] + lato, h/2 + lato/2)
    # ... and for left frame
    topLx_FrameLx = (topLx_FrameRx[0] - spaceBetw - lato, h/2 - lato/2)
    botRx_FrameLx = (topLx_FrameLx[0] + lato, h/2 + lato/2)
    # save the frame coordinates
    frame_lx = [topLx_FrameLx, botRx_FrameLx]
    frame_rx = [topLx_FrameRx, botRx_FrameRx]
    # define size of cue
    lato_corto = 50
    lato_lungo = 20
    # define cues coordinate in the right frame
    topLx_CueRx_vert = (topLx_FrameRx[0] + (lato/2 - lato_lungo/2), topLx_FrameRx[1] + 10)
    botRx_CueRx_vert = (topLx_CueRx_vert[0] + lato_lungo, topLx_CueRx_vert[1] + lato_corto)
    # define cues coordinate in the left frame
    topLx_CueLx_hori = (topLx_FrameLx[0] + (lato/2 - lato_corto/2), topLx_FrameLx[1] + 10)
    botRx_CueLx_hori = (topLx_CueLx_hori[0] + lato_corto, topLx_CueLx_hori[1] + lato_lungo)
    # ... save each into a "cue" object
    cue_lx = [topLx_CueRx_vert, botRx_CueRx_vert]
    cue_rx = [topLx_CueLx_hori, botRx_CueLx_hori]
    # save the cues in a list
    shapes = [frame_lx, cue_lx, frame_rx, cue_rx]
    # define the text info
    font = ImageFont.truetype('arial.ttf', size=20)
    # define text x-coordinates for words indicating the 4 possible responses
    x_coord = [
        topLx_FrameLx[0], topLx_FrameLx[0] + lato,
        topLx_FrameRx[0], topLx_FrameRx[0] + lato
        ]
    # define where we should write what, based on the mapping
    if fig[0][22:26] == "hori": # this means magnitude is horizontal
    # and so must be drawn on the leftward rectangle
        tasks = ["magnit", "parity"] # we first write the magnit responses
    else: # this means magnitude is hori and parity is vertical
        tasks = ["parity", "magnit"] # we first write the parity responses
    if key[0][6:7] == "A": # this means greater is A
        ans = ["greater", "less"]
    else: # this means greater is A
        ans = ["less", "greater"]
    if key1[0][6:7] == "A": # this means odd is A
        ans1 = ["odd", "even"]
    else: # this means greater is A
        ans1 = ["even", "odd"]
    # a dictonary to map tasks to responses
    taskToResp = {"magnit": ans, "parity": ans1}
    # list of the words:
    words = taskToResp[tasks[0]] + taskToResp[tasks[1]]
    # draw the canvas upon which the figure is built
    img = Image.new("RGB", (w, h), color=background)
    img1 = ImageDraw.Draw(img)
    # draw the 4 shapes
    img1.rectangle(frame_lx, fill= background, outline= "black")
    img1.rectangle(frame_rx, fill= background, outline= "black")
    img1.rectangle(cue_lx, fill= "black", outline= "black")
    img1.rectangle(cue_rx, fill= "black", outline= "black")
    # loop over the words:
    for i in range(len(words)):
        img1.text((x_coord[i] - 20, botRx_FrameRx[1] + 3), words[i],
            fill="black", font=font, align="center"
            )
    #img.show()
    figName = fig[0][22:26] + key[0][6:7] + key1[0][6:7] + ".png"
    img.save(imgDir + figName)
    return figName

# build blocks, pseudorandomize them and paste them together
def buildAndPasteBlocks(df0, df300, startCocoa, fig, key, key1):
    # We want 3 exemplars for each kind of trial
    trialsPercondition = 3
    block0 = pd.concat([df0]*trialsPercondition, ignore_index = True)
    block300 = pd.concat([df300]*trialsPercondition, ignore_index = True)
    # define whether they have blocks 2,4,6,8 with cocoa = 0 and 1,3,5,7 with
    # cocoa 300 or viceersa
    if startCocoa == 0:
        blocks = [block0, block300]*2
        #blocks = [block0, block300]*4
    elif startCocoa == 300:
        blocks = [block300, block0]*2
        #blocks = [block300, block0]*4
    else:
        print("startCocoa must be either int 0 or 300")
    # prepare the experiment dataframe
    experiment = pd.DataFrame([], columns = block0.columns)
    # loop over the list of blocks
    for i in range(len(blocks)):
        block = blocks[i]
        stimElmns = list(set(block.stimulus))
        cues = list(set(block.orientation))
        #help(pseudorandomize)
        Stim_Cues = pseudorandomize(len(block), stimElmns, 1, *cues)
        #help(DfBooleanOrder)
        block_shuf = DfBooleanOrder(
            block, "stimulus", Stim_Cues.stim, "orientation", Stim_Cues.task
        )
        #set block and trial number
        block_shuf["blockN"] = i
        block_shuf["trialN"] = list(range(len(block)))
        # define the correct ANSWER
        ANSWER(block_shuf, fig, key, key1)
        # append a interblock-break screen
        breakScreen = {
            "display": "break",
            "breakMessage": "## You have completed block " +str(i+1)+ " out of " + str(len(blocks)) + "."
            }
        block_shuf = block_shuf.append(breakScreen, ignore_index=True, sort = False)
        experiment = experiment.append(block_shuf, sort = False)
    return experiment
