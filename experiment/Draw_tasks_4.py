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

def draw(cuecolor, framecolor, orientation, imgSheetName, imgDf):
    # define position of the cue given the orientation
    cueFileName = cuecolor + framecolor + orientation + ".png"

    if orientation=="horiz":
        x3, x4 = w/2 - 50, w/2 + 50
        y3, y4 = ((y1 + y2)/2 + y1)/2 - 20, ((y1 + y2)/2 + y1)/2 + 20
    if orientation=="vert":
        x3, x4 = w/2 - 20, w/2 + 20
        y3, y4 = ((y1 + y2)/2 + y1)/2 - 50, ((y1 + y2)/2 + y1)/2 + 50

    shape = [(x1, y1), (x2,y2)]
    shape2 = [(x3, y3), (x4, y4)]

    # creating new Image object
    img = Image.new("RGB", (w, h), color=(224, 224, 224))

    img1 = ImageDraw.Draw(img)
    font = ImageFont.truetype('arial.ttf', size=40)
    img1.rectangle(shape, fill ="#E0E0E0", outline =framecolor, width=2)
    img1.rectangle(shape2, fill=cuecolor, outline=cuecolor)
    img.save(myDir + "img/" + cueFileName)

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
            img.save(myDir + "img/" + stimFileName)
            # save info in the dataframe
            row = {"cuecolor": cuecolor, "framecolor": framecolor, "orientation": orientation,
                "stimulus": stimulus, "cueFileName": cueFileName, "stimFileName": stimFileName
            }
            imgDf = imgDf.append(row, ignore_index = True)
            print(imgDf)
            #print(imgDf)
    return imgDf

# run the fun in a loop
# generate the images and save their details in a csv
colNames = ["cuecolor", "framecolor", "orientation", "stimulus", "cueFileName", "stimFileName"]
# prepare empty df
imgDf = pd.DataFrame([], columns = colNames)
completeDf = pd.DataFrame([], columns = colNames)
# first BRAC01 file, where the frame stays black
imgSheetName = "blackFrame"
# run the function for all the possible combinations of BRAC01
for cuecolor in ["blue", "red", "black"]:
    framecolor = "black"
    for orientation in orientations:
        imgDf = draw(cuecolor, framecolor, orientation, imgSheetName, imgDf)
        #completeDf = completeDf.append(imgDf, ignore_index = True)
completeDf["display"] = "cocoa0"
completeDf.to_csv(imgSheetName + ".csv", sep = ";")


# second BRAC02 file, where the cue stays black
imgSheetName = "blackCue"
# open the csv and write the first line - column names
with open(imgSheetName + ".csv", 'a') as file:
   fieldnames = colNames
   writer = csv.DictWriter(file, fieldnames = fieldnames, lineterminator = "\n")
   writer.writeheader()
for framecolor in ["blue", "red"]:
    cuecolor = "black"
    for orientation in orientations:
        draw(cuecolor, framecolor, orientation, imgSheetName, imgDf)
