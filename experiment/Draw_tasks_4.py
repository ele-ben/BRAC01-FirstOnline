import math, csv
import pandas as pd
from PIL import Image, ImageDraw, ImageFont
from win32api import GetSystemMetrics

# PARAMETERS #
myDir = "C:/Users/Elena/Documents/AA_PhD/Projects/BRAC01-FirstOnline/experiment/"
# General
w, h = 800, 600
orientations = ["horiz", "vert"]
# Frame
x1, x2 = w/2 - 250, w/2 + 250
y1, y2 =  h/2 - 250,  h/2 + 250

# Text
(x, y) = w/2 - 13.8666666666, h*0.5 - 26.6666666667
color = 'rgb(0, 0, 0)' # black color

# Stimuli
stimuli = [1, 2, 3, 4, 6, 7, 8, 9]

# ( ͡❛ ͜ʖ ͡❛)
# ( ͡❛ ͜ʖ ͡❛)
# ( ͡❛ ͜ʖ ͡❛)
# define a function that draws my experimental scree: an rectangle (the cue) either
# vertical or horizontal and below, in the centre, a number (the stimulus)
# Both are surronded by an empty square
def draw(cuecolor, framecolor, orientation, imgDf):
    # define name of the image file based on the parameters
    cueFileName = cuecolor + framecolor + orientation + ".png"
    # define position of the cue given the orientation
    if orientation=="horiz":
        x3, x4 = w/2 - 50, w/2 + 50
        y3, y4 = ((y1 + y2)/2 + y1)/2 - 20, ((y1 + y2)/2 + y1)/2 + 20
    if orientation=="vert":
        x3, x4 = w/2 - 20, w/2 + 20
        y3, y4 = ((y1 + y2)/2 + y1)/2 - 50, ((y1 + y2)/2 + y1)/2 + 50
    # save coordinates of the cue and of the frame
    shape = [(x1, y1), (x2,y2)]
    shape2 = [(x3, y3), (x4, y4)]

    # creating new Image object - grey background
    img = Image.new("RGB", (w, h), color=(224, 224, 224))
    # draw the cue and the frame
    img1 = ImageDraw.Draw(img)
    font = ImageFont.truetype('arial.ttf', size=40)
    img1.rectangle(shape, fill ="#E0E0E0", outline =framecolor, width=2)
    img1.rectangle(shape2, fill=cuecolor, outline=cuecolor)
    # save the image, on purpose without number
    img.save(myDir + "img/" + cueFileName)
    # because of the experimental design, I don't want any number when both are
    # black
    if cuecolor == "black" and framecolor == "black":
        pass
    else:
    # Adding stimuli
        for num in stimuli:
            stimulus = str(num)
            img1 = ImageDraw.Draw(img)
            font = ImageFont.truetype('arial.ttf', size=40)
            img1.rectangle(shape, fill ="#E0E0E0", outline =framecolor, width=2)
            img1.rectangle(shape2, fill=cuecolor, outline=cuecolor)
            img1.text((x, y), stimulus, fill=color, font=font, align="center")
            stimFileName = cuecolor + framecolor + orientation + stimulus + ".png"
            # save the image with each number
            img.save(myDir + "img/" + stimFileName)
            # save image info in a dictionary to append to a dataframe
            row = {"cuecolor": cuecolor, "framecolor": framecolor, "orientation": orientation,
                "stimulus": stimulus, "cueFileName": cueFileName, "stimFileName": stimFileName
            }
            # append the dictionary
            imgDf = imgDf.append(row, ignore_index = True)
    return imgDf

# run the function in a loop to generate all the requred colours and numbers
# generate the images and save the df with the details in a csv
colNames = ["cuecolor", "framecolor", "orientation", "stimulus", "cueFileName",
    "stimFileName"]
# prepare empty df
blackFrDf = pd.DataFrame([], columns = colNames)
# first BRAC01 file, where the frame stays black and the cue varies
imgSheetName = "blackFrame"
# run the function for all the possible combinations of BRAC01
for cuecolor in ["blue", "red", "black"]:
    framecolor = "black"
    for orientation in orientations:
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
    if blackFrDf1["orientation"].loc[i] == "horiz":
        blackFrDf1["cue0FileName"].loc[i] = "blackblackhoriz.png"
    elif blackFrDf1["orientation"].loc[i] == "vert":
        blackFrDf1["cue0FileName"].loc[i] = "blackblackvert.png"

blackFrDf = blackFrDf.append(blackFrDf1, ignore_index = True)

# ....
# save the df in a csv to be uploaded in Gorilla
blackFrDf.to_csv(imgSheetName + ".csv", sep = ";")

# second BRAC02 file, where the cue stays black and the frame varies
blackCuDf = pd.DataFrame([], columns = colNames)
# declare the name of the csv
imgSheetName = "blackCue"
for framecolor in ["blue", "red"]: # I don't need the black here
    cuecolor = "black"
    for orientation in orientations:
        blackCuDf = draw(cuecolor, framecolor, orientation, blackCuDf)
# add other variables relevant for Gorilla
# ....
# save the df in a csv to be uploaded in Gorilla
blackCuDf.to_csv(imgSheetName + ".csv", sep = ";")
