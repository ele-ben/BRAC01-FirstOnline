
#
#                     BRAC02
#              Cue-UNrelated Contex 
# ciao cucciola
# aaaaa
# ===================== Which device? ==================
computer = "asus" 
#computer = "office"
#computer = "lab"

# ===================== Imports ========================
import os, sys

import random, pyglet, csv, numpy as np

import pandas as pd

from psychopy import sound, core, event, gui, visual, data

pyglet.options['shadow_window']=False

from datetime import datetime

from numpy.random import choice

from BRACfun import navigate_instructions, shuffle_StimTask_str, balanceTransitionsMinus1_str, no_StimRepetition, shuffle_rows, shuffle_rows2

# ===================== Set Directory ==================

if computer == "office":
    myDir = "C:\\Users\\Elena\\Documents\\PsychoPy\\"
elif computer == "asus":
    myDir = "C:\\Users\\Elena\\Documents\\AA_PhD\\PsychoPy\\"
elif computer == "lab":
    myDir = "\\home\\experiment\\psychopy\\Elena\\"

# ===================== Dialog ========================

#expinfo= {"pp" : "", "age" : "", "gender" : "", "handedness": "", "en or de": ""}
#
#dlg = gui.DlgFromDict(expinfo, title = "Judge the number!") #opens a dialog window
#
#if dlg.OK:
#    thisInfo = dlg.data
#    print(thisInfo)
#else: print('user cancelled')
#
# ==================== Prepare file ====================
#
#save variables for the output
#
#pp = int(expinfo['pp'])
#
#age = int(expinfo['age'])
#
#sex = expinfo['gender']
#
#hand = expinfo['handedness']
#
#language = expinfo['language']

# ----------- Total Huge Debug -----------
# open pps file
#
#instr_check_out = pd.DataFrame([], columns=["pp", "cue_task", "mag_key", "par_key", "cueMap", "respMag", "respPar"])
#
#with open("instr_check.csv", 'a') as file:
#    fieldnames = instr_check_out.columns
#    writer = csv.DictWriter(file, fieldnames = fieldnames, lineterminator = "\n")
#    writer.writeheader()
#
#for ij in range(1,33):
# ====== debug ======

pp = 1

age = 17

sex = 17

hand = 17

language = "en"
# ===================

hour= datetime.now()

hour_str = hour.strftime("%m%d%H%M%S")

# open file
file_name = str(pp) + "_" + hour_str + '_Brac02.csv'

outData = pd.DataFrame([], columns=["pp", "age", "sex", "hand", "trialNum", 
    "blockNum", "cocoa", "response", "correctResp", "rt", "accuracy", "task",
    "stim", "colour", "shape", "fullOri", "condID", "sequence"])
    
if computer == "lab":
    output_name = file_name
    #print output_name
elif computer == "office" or "asus":
    output_name = myDir + file_name
    #print(output_name)

#    with open(output_name, 'a') as file:
#        fieldnames = outData.columns
#        writer = csv.DictWriter(file, fieldnames = fieldnames, lineterminator = "\n")
#        writer.writeheader()

# ==================== Variables =======================

pyClock = core.Clock()

grey = [0,0,0] #background

fixationWait = 1.4

cueWait = 0.3

targetWait = 2.5

slowResp = 2500

keyList = ["a", "l", "escape"]

quit_go_keys = ["escape","space"]

lx_rx_scroll = ["left", "right"]

# ================== Combine Conditions ====================

# within-subjects (and block) variables and list of possible combinations

taskLst = ["magnit", "parity"]

stimuli = [s for s in range(1,10) if s !=5]

colour = ["red", "blue"]

shape = ["empty"]

# cartesian product of the above-listed variables

feat_comb = pd.DataFrame([(x,y,z) for x in taskLst for y in stimuli for z in colour], columns=['task', 'stim', 'colour'])

feat_comb["condID"] = range(len(feat_comb))

#add a temporary cocoa value

feat_comb["cocoa"] = np.ones(len(feat_comb)) 

# between-subjects variables and list of combinations
# two possible task-keys mappings for each task

resp_parity = [{"even": ["a"], "odd": ["l"]}, {"even": ["l"], "odd": ["a"]}]

resp_magnit = [{"less": ["a"], "more": ["l"]}, {"less": ["l"], "more": ["a"]}] 

cue_task = [{"magnit": 0, "parity": 90}, {"magnit": 90, "parity": 0}]

# two possible cocoa in odd numbered-blocks (cocoa of the even-numbered follows suit)

odd_cocoa = [0, 300]

#generate cartesian products of the levels of the variables above and make into a df

pps_combinations = [(x,y,z,cocoa) for x in [0,1] for y in [0,1] for z in [0,1] for cocoa in [0,300]]

pps_comb = pd.DataFrame(pps_combinations, columns=['corrResp_parity', 'corrResp_magnit', 'cue_task', 'odd_cocoa'])

#repeat the bewteen-pps conditions a sufficient number of times (2 would have been enough for our N=32)

pps_comb = pd.concat([pps_comb]*4, ignore_index=True)

# participant number used as row names --> row index goes from 1 to len(pps_comb)

pps_comb["pps"] = range(1, len(pps_comb)+1)

pps_comb.set_index("pps", inplace= True)


# =============== Trial sequence ===============

trialsPercondition= 3 # established a priori by us

#blocks = range(1,9)

blocks = [1,2] # debug

# trials and blocks counters

trN = 0 

blN = 1

# repeat the df with the possible conditions for the number of times we want each condition to be repeated

trialSeq =  pd.concat([feat_comb]*trialsPercondition, ignore_index = True)

trialSeq["trID"] = np.arange(len(trialSeq)) #create a unique trial identifier

# ------------ Training trials sequence -----------------

# define and shuffle training trials, pick every second row of feat_comb

training = feat_comb.iloc[::2]

training.reset_index(inplace = True, drop= True)

# use self-made no_stimRepetition function to avoid n-1 repetitions of stimuli

training_stimuli = no_StimRepetition(len(training), stimuli)

trainingShuf = shuffle_rows(training_stimuli, training, "stim")

# ------------ Debug ------------

# check no same stim in a row

for jj in range(1, len(training)):
        if trainingShuf["stim"].loc[jj] == trainingShuf["stim"].loc[jj-1]:
            raise Warning("2 equal stimuli are found in subsequent positions")

# check each stim same number of times

vec = [0]*10

for i in stimuli:
    vec[i] = sum(trainingShuf["stim"] == i)
    
vec1 = [vec[i] for i in stimuli]

equalTimes = all(x == vec1[0] for x in vec1)

if (not equalTimes):
    raise Warning("the training sequence is wrong: stimuli are not equally represented")

# ------------------------

# =============== Window and Shapes =====================

# According to the machine set pixels, screen and fullscreen:

if computer == "office":
    win = visual.Window([1700,1000], fullscr = False, monitor="Fujitsu_B24W-7", units="cm", color = grey)
    fWidth = 3.5
    fHeig = 1.47
    eSide = 24
    fpos = [0,7.5]
    charHei = 1
    instrPos = 9

elif computer == "asus":
    win = visual.Window([1200,700], fullscr = False, monitor="asusPC", units="cm", color = grey)
    fWidth = 1.5
    fHeig = 0.47
    eSide = 12
    fpos = [0,3.5]
    charHei = 0.5
    instrPos = 4.5

elif computer == "lab":
    win = visual.Window([1024,768], fullscr = True, monitor="testMonitor", units="cm", color = grey)
    fWidth = 3.5
    fHeig = 1.47
    eSide = 24
    fpos = [0,7.5]
    charHei = 1
    instrPos = 9

fixation = visual.TextStim(win=win, ori=0, text = "+", font='Arial', alignHoriz='center', alignVert='center', height=1, color='black') 

#- draw the cue and the frame with variable features
# --> Full stands for cue, empty for frame
# the full shape has area = 7141 pixel; the lines of the empty shape have total area= 7129; 1 cm = 37.13 pixel on this screen

class cueAndFrame:
    def __init__(self, colorEmpty, fullOri, scale): #object properties
        self.full = visual.Rect(win=win, width = fWidth, height = fHeig, ori=fullOri, lineWidth = 0, fillColor = "black", pos= fpos) #actually create the object cue + square
        self.empty = visual.Rect(win=win, width = eSide, height = eSide, ori=0, lineWidth = 2, lineColor = colorEmpty, fillColor = None, pos= [0, 0])
        self.full_scaled = self.full # create a duplicate
        self.empty_scaled = self.empty
        self.full_scaled.size = self.full.size/scale # define smaller dim of the duplicate
        self.full_scaled.pos = self.full.pos/scale
        self.empty_scaled.size = self.empty.size/scale
    def draw(self): #its method
        self.full.draw()
        self.empty.draw()        
    def draw_scaled(self): #its method for a smaller version
        self.full_scaled.draw()
        self.empty_scaled.draw()

# define stimulus class

class target:
    def __init__(self, stim_number): #object properties
        self.target = visual.TextStim(win=win, ori=0, text = int(stim_number), font='Arial', alignHoriz='center', alignVert='center', height=1, color='black')
    def draw(self): 
        self.target.draw()
        
#define a screen with instructions on top and bottom and cue+frame instance

class build_instructions:
    def __init__(self, instructAbove, fullOri, instructBelow):
        self.up = instructAbove
        self.example = cueAndFrame("black", fullOri,2)
        self.down = instructBelow
    def draw(self): 
        self.up.draw()
        self.example.draw_scaled() #how many times smaller
        self.down.draw()

# =============== define instructions =============== CAMBIA SPIEGAZIONE PER FRAME!!

if language == "de":
    instructions0 = visual.TextStim(win=win, ori=0, text = "Herzlich willkommen!\nDanke, dass du an diesem Experiment teilnimmst.\nDu kannst die Pfeiltasten nutzen, um die Instruktionen vor und zurueck zu klicken.", font='Arial', height=1, wrapWidth=30, color='white')
    instructions1 = visual.TextStim(win=win, ori=0, text ="Wir werden dir Zahlen von 1 bis 9 (ohne 5) praesentieren. \nBei jeder Zahl hast du die Aufgabe, zu entscheiden, ob die Zahl gerade bzw. ungerade oder groesser bzw. kleiner als 5 ist. \nBevor die Zahlen erscheinen, wird ein Rechteck im oberen Bereich des Bildschirms dargeboten. Dabei indiziert die Orientierung des Rechtecks, welche Aufgabe du ausfuehren sollst. Ausserdem wird das Rechteck in unterschiedlichen Farben dargeboten, wobei die Farben keine Informationen fuer deine Aufgabe liefern. \n\nDu kannst die Pfeiltasten nutzen, um die Instruktionen vor und zurueck zu klicken.", font='Arial', height=1, wrapWidth=30, color='white')
    instructions2_magn0 = visual.TextStim(win=win, ori=0, text = "Wenn das Rechteck horizontal dargeboten wird, dann sollst du entscheiden, ob die dargebotene Zahl groesser oder kleiner als 5 ist.", font='Arial', pos = [0,9], height=1, wrapWidth=30, color='white')
    instructions2_magn90 = visual.TextStim(win=win, ori=0, text = "Wenn das Rechteck vertikal dargeboten wird, dann sollst du entscheiden, ob die dargebotene Zahl groesser oder kleiner als 5 ist.", font='Arial', pos = [0,9], height=1, wrapWidth=30, color='white')
    instructions2_par90 = visual.TextStim(win=win, ori=0, text = "Wenn das Rechteck vertikal dargeboten wird, dann sollst du entscheiden, ob die dargebotene Zahl gerade oder ungerade ist.", font='Arial', pos = [0,9], height=1, wrapWidth=30, color='white')
    instructions2_par0 = visual.TextStim(win=win, ori=0, text = "Wenn das Rechteck horizontal dargeboten wird, dann sollst du entscheiden, ob die dargebotene Zahl gerade oder ungerade ist.", font='Arial', pos = [0,9], height=1, wrapWidth=30, color='white')
    instructions3_aLess = visual.TextStim(win=win, ori=0, text = "Wenn die Zahl kleiner als 5 ist, druecke die Taste A \n und wenn die Zahl groesser als 5 ist, druecke die Taste L", font='Arial', pos = [0,-9], height=1, wrapWidth=30, color='white')
    instructions3_aGreat = visual.TextStim(win=win, ori=0, text = " Wenn die Zahl kleiner als 5 ist, druecke die Taste L \n und wenn die Zahl groesser als 5 ist, druecke die Taste A ", font='Arial',pos = [0,-9], height=1, wrapWidth=30, color='white')
    instructions4_aOdd = visual.TextStim(win=win, ori=0, text = "Wenn die Zahl ungerade ist, druecke die Taste A \nund wenn die Zahl gerade ist, druecke die Taste L ", font='Arial', pos = [0,-9], height=1, wrapWidth=30, color='white')
    instructions4_aEven = visual.TextStim(win=win, ori=0, text = "Wenn die Zahl ungerade ist, druecke die Taste L \nund wenn die Zahl gerade ist, druecke die Taste A ", font='Arial', pos = [0,-9], height=1, wrapWidth=30, color='white')
    instructions5 = visual.TextStim(win=win, ori=0, text = "Bitte reagiere so schnell und so genau wie moeglich. Um deine Geschwindigkeit zu erhoehen, halte deine Zeigefinger waehrend des gesamten Experiments auf den Tasten A und L. Wenn du Fragen hast, kannst du diese der Versuchsleiterin stellen. \nDu wirst mit einem Ubungsblock starten, um dich an die Aufgabe zu gewoehnen. \nWenn du bereit bist, druecke die Leertaste, um mit dem Ubungsblock zu starten.", font='Arial', height=1, wrapWidth=30, color='white')
    beginExp = visual.TextStim(win=win, ori=0, text = "Damit endet der Uebungsblock. Bei jeglichen Fragen kannst du gerne die Versuchsleiterin fragen. \nDruecke die Leertaste, um das Experiment zu starten. ", font='Arial', height=1, wrapWidth=20, color='white')
    thanks = visual.TextStim(win=win, ori=0, text = "Damit endet das Experiment. Danke fuer deine Teilnahme. Druecke escape um das Experiment zu schliessen. ", font='Arial', height=1, wrapWidth=20, color='white')
    warnAcc = visual.TextStim(win=win, ori=0, text = "Diese Antwort ist falsch! Bitte versuche sorgfaeltiger zu antworten.", font='Arial', height=1, wrapWidth=20, color='white')
    warnRT = visual.TextStim(win=win, ori=0, text = "Bitte versuche schneller zu reagieren.", font='Arial', height=1, wrapWidth=20, color='white')
    pause_text = "Du hast den Block %d abgeschlossen. Du kannst gerne eine kurze Pause machen.\nDruecke die Leertaste, um mit dem naechsten Block zu starten."

elif language == "en":
    instructions0 = visual.TextStim(win=win, text = "Welcome!\nThank you for taking part in this experiment.\nUse the arrows to move back and forth in these instructions.", height=charHei,wrapWidth=30)
    instructions1 = visual.TextStim(win=win, ori=0, text ="We will present you numbers from 1 to 9, 5 excluded.\nFor each number, your task is to tell whether the number is odd or even or whether it is greater than 5 or less than 5.\nBefore each number appears, it will appear a rectangle at the top of the screen. The orientation of the rectangle will indicate which task you are to perform. The rectangle and the number will be surrounded by a frame. You will notice that this frame will appear in different colours. Yet the frame and its colour are not informative for your task.\n\nUse the arrows to move back and forth through these instructions.", height=charHei, wrapWidth=30)
    instructions2_magn0 = visual.TextStim(win=win, ori=0, text = "When the rectangle is horizontal, your task is to tell whether the number is greater then 5 or less than 5.",  pos = [0,instrPos], height=charHei, wrapWidth=30)
    instructions2_magn90 = visual.TextStim(win=win, ori=0, text = "When the rectangle is vertical, your task is to tell whether the number is greater then 5 or less than 5.",  pos = [0,instrPos], height=charHei, wrapWidth=30)
    instructions2_par90 = visual.TextStim(win=win, ori=0, text = "When the rectangle is vertical, your task is to tell whether the number is odd or even.",  pos = [0,instrPos], height=charHei, wrapWidth=30)
    instructions2_par0 = visual.TextStim(win=win, ori=0, text = "When the rectangle is horizontal, your task is to tell whether the number is odd or even.",  pos = [0,instrPos], height=charHei, wrapWidth=30)
    instructions3_aLess = visual.TextStim(win=win, ori=0, text = "Please press A key to indicate LESS than 5 \nand L key to indicate GREATER than 5",  pos = [0,-instrPos], height=charHei, wrapWidth=30)
    instructions3_aGreat = visual.TextStim(win=win, ori=0, text = "Please press L key to indicate LESS than 5 \nand A key to indicate GREATER than 5", pos = [0,-instrPos], height=charHei, wrapWidth=30)
    instructions4_aOdd = visual.TextStim(win=win, ori=0, text = "Please press A key to indicate ODD \nand L key to indicate EVEN",  pos = [0,-instrPos], height=charHei, wrapWidth=30)
    instructions4_aEven = visual.TextStim(win=win, ori=0, text = "Please press L key to indicate ODD \nand A key to indicate EVEN",  pos = [0,-instrPos], height=charHei, wrapWidth=30)
    instructions5 = visual.TextStim(win=win, ori=0, text = "Please try to answer as fast as possible, but also to be as accurate as possible. In order to increase your speed, please keep your index fingers on the A and L keys through all the duration of the experiment. If you have any questions, please ask the experimenter.\nYou will first start with a short training, to familiarize with the task.\nWhen you are ready, press the space bar to begin the training session.",  alignHoriz='center', alignVert='center', height=charHei, wrapWidth=30)
    beginExp = visual.TextStim(win=win, ori=0, text = "The training has ended. If you have doubts do not hesitate to ask the experimenter.\nPress the space bar when you are ready to begin the experiment.",  height=charHei, wrapWidth=20)
    thanks = visual.TextStim(win=win, ori=0, text = "The experiment has ended. Thank you very much! Press escape to quit.",  height=charHei, wrapWidth=20)
    warnAcc = visual.TextStim(win=win, ori=0, text = "The answer is incorrect! Please try to be more careful!",  height=charHei, wrapWidth=20)
    warnRT = visual.TextStim(win=win, ori=0, text = "Please try to be faster!",  height=charHei, wrapWidth=20)
    pause_text = "You have concluded block %d . You may take a break now. Press the space bar to start the following block."

# define pause bewteen blocks class

class pause:
    def __init__(self, blN):
        self.pause = visual.TextStim(win=win, ori=0, text = pause_text %(blN), font='Arial', alignHoriz='center', alignVert='center', height=1, wrapWidth=20, color='white')
    def draw(self):
        self.pause.draw()

# ============ Set participant-specific parameters ===============

# find the row of this participant where all the mappings are stored and draw them out

currPp = pps_comb.loc[pp]

corrResp_parity = resp_parity[currPp["corrResp_parity"]]

corrResp_magnit = resp_magnit[currPp["corrResp_magnit"]]

# build participant's correct response dictionary

corrResp_dict = {"magnit": corrResp_magnit, "parity": corrResp_parity}

# draw out her cue task mapping from the cue_task entry of currPp column

cue_task = cue_task[currPp["cue_task"]]

# establish cocoa for even blocks as the one not in odd blocks

if currPp["odd_cocoa"] == 300:
    even_cocoa = 0
else:
    even_cocoa = 300

# define her instructions

if cue_task["magnit"] == 0:
    instructions2_magn = instructions2_magn0
    instructions2_par = instructions2_par90
elif cue_task["magnit"] == 90:
    instructions2_magn = instructions2_magn90
    instructions2_par = instructions2_par0
    
if corrResp_dict["magnit"]["less"] == ["a"]:
    instructions3 = instructions3_aLess
elif corrResp_dict["magnit"]["less"] == ["l"]:
    instructions3 = instructions3_aGreat

if corrResp_dict["parity"]["odd"] == ["a"]:
    instructions4 = instructions4_aOdd
elif corrResp_dict["parity"]["odd"] == ["l"]:
    instructions4 = instructions4_aEven
    
# -------- debug --------
    
if instructions2_magn == instructions2_magn0 and instructions2_par == instructions2_par90:
    cueMap = "magn0"
elif instructions2_magn == instructions2_magn90 and instructions2_par == instructions2_par0:
    cueMap = "par0"
else:
    cueMap = " "

if instructions3 == instructions3_aLess:
    respMag = "aLess"
elif instructions3 == instructions3_aGreat:
    respMag = "aGreat"
    
if instructions4 == instructions4_aOdd:
    respPar = "aOdd"
elif instructions4 == instructions4_aEven:
    respPar = "aEven"

#--------------------------

# build the screen with instructions about task-cue mapping on the top, cue example in the middle, task-resp mapping on the bottom

instructions2_3 = build_instructions(instructions2_magn, cue_task["magnit"], instructions3)

instructions2_4 = build_instructions(instructions2_par, cue_task["parity"], instructions4)

# build instructions list

instructions_list= [instructions0, instructions1, instructions2_3, instructions2_4, instructions5]

lastPage = 4

# ============ Experiment starts ============
# instructions

navigate_instructions(win, instructions_list, lx_rx_scroll[0], lx_rx_scroll[1], quit_go_keys[0], quit_go_keys[1])

#--------------- Training block ---------------------

for i in range(3):
#for i in range(len(trainingShuf)):
    # fixation
    fixation.draw()     
    win.flip()    
    core.wait(fixationWait)
    
    #identify current trial 
    currTrial = trainingShuf.iloc[i]    
    stim = currTrial["stim"]    
    task = currTrial["task"]    
    fullOri = cue_task[task]
    #no context thus no colour in training trials
    cueAndFrame("black", fullOri, 1).draw()     
    win.flip()    
    core.wait(cueWait*2)    
    cueAndFrame("black", fullOri, 1).draw()    
    target(currTrial["stim"]).draw()
    win.flip()    
    pyClock.reset()    
    response= event.waitKeys(maxWait = targetWait, keyList = keyList)    
    rt = pyClock.getTime()*1000 #----------------------------------------- collect data or not? Nope. Give feedback or not?  Yes
    
    if response == ["escape"]:
        core.quit()    
    if not response:
        response = " "
        rt = " "    
    # find correct answer
    if stim > 5:
        stimIS = "more"
    else:
        stimIS = "less"    
    try:
        corrResp = corrResp_dict[task][stimIS]
        print(corrResp_dict[task])
    except KeyError:
        if stim%2 == 0:
            stimIS = "even"
        else:
            stimIS = "odd"
        corrResp = corrResp_dict[task][stimIS] 
    if response != corrResp:
        warnAcc.draw()
        win.flip()
        core.wait(fixationWait)        
    if rt> slowResp:
        warnRT.draw()
        win.flip()
        core.wait(fixationWait)

beginExp.draw()

win.flip()

press = event.waitKeys(keyList = quit_go_keys)

if press == ["escape"]:
    core.quit()

# --------------  Blocks start ----------------------

for j in blocks:
    
    # pseudorandomize stimuli and task and merge the column into the generic and ordered trials df
    stimAndTask = shuffle_StimTask_str(len(trialSeq), stimuli, 1, taskLst[0], taskLst[1])
    trialShuf = shuffle_rows2(trialSeq, "stim", stimAndTask["stim"], "task", stimAndTask["task"])
    sequence = "pseudorand"
    
    # ------------ Debug ------------

    # check no same stim in a row
    
    for jj in range(1, len(trialShuf)):
        if trialShuf["stim"].loc[jj] == trialShuf["stim"].loc[jj-1]:
            raise Warning("2 equal stimuli are found in subsequent positions")

    # check each stim same number of times

    vec = [0]*10
    for i in stimuli:
        vec[i] = sum(trialShuf["stim"] == i)
    vec1 = [vec[i] for i in stimuli]
    equalTimes = all(x == vec1[0] for x in vec1)
    if (not equalTimes):
        raise Warning("the training sequence is wrong: stimuli are not equally represented")
        
    # each task same number of times
    
    vec = [0]*2
    vec = [sum(trialShuf["task"] == t) for t in taskLst]
    if vec[0] != vec[1]:
        raise Warning("the training sequence is wrong: tasks are not equally represented")
    # ------------------------    
    
    # assign cocoa to this block: if odd block then odd_cocoa, else even_cocoa
    if blN%2 != 0:
        trialShuf["cocoa"] = currPp["odd_cocoa"]
    elif blN%2 == 0:
        trialShuf["cocoa"]= even_cocoa
    else:
        core.quit()
        print("block number is not odd nor even")
    # print "this is trialShuf"
    # print(trialShuf)   
    
#        # start loop for trial i in block j
    for i in range(2):
    
    # for i in range(len(trialSeq)):
        # fixation
        fixation.draw()        
        win.flip()        
        core.wait(fixationWait)
        #identify current trial and draw the task, stim, colour information out
        currTrial = trialShuf.iloc[i]
        print(currTrial)
        task = currTrial["task"]
        print(task)
        fullOri = cue_task[task]        
        stim = currTrial["stim"]
        # when delay --> draw black cue (always black) and black frame
        if currTrial["cocoa"]== 300:
            cueAndFrame("black", fullOri, 1).draw() 
            win.flip()
            core.wait(cueWait)
        # otherwise --> coloured frame from the beginning
        elif currTrial["cocoa"]== 0:
            cueAndFrame(currTrial["colour"], fullOri, 1).draw()
            win.flip()
            core.wait(cueWait)
        else:
            core.quit()
            print("cocoa column in trial shuff doesn't report correct value")
        
        # draw the compound of cue and frame
        cueAndFrame(currTrial["colour"], fullOri, 1).draw()
        win.flip()
        core.wait(cueWait)
        target(currTrial["stim"]).draw()
        cueAndFrame(currTrial["colour"], fullOri, 1).draw()
        win.flip()
        pyClock.reset()
        
        # record response and rt, fill it with empty when none is given
        response= event.waitKeys(maxWait = targetWait, keyList = keyList)        
        rt = pyClock.getTime()*1000        
        if not response:
            response = " "
            rt = " "        
        
        # find correct answer
        if stim > 5:
            stimIS = "more"
        else:
            stimIS = "less"        
        try:
            corrResp = corrResp_dict[task][stimIS]
        except KeyError:
            if stim%2 == 0:
                stimIS = "even"
            else:
                stimIS = "odd"
            corrResp = corrResp_dict[task][stimIS]
        # assign accuracy accordingly
        if response == corrResp:
            accuracy = 0
        else:
            accuracy = 1 #-------------------------------------------------------------- what to do with accuracy in unanswered trials?
        
        # you can write whatever order to give row as an input for open(output_name...)
        row = {"pp": pp, "age": age, "sex": sex, "hand": hand,
            "trialNum": trN, "blockNum" : blN, "cocoa": currTrial["cocoa"], "rt": rt,
            "accuracy": accuracy, "response": response, "correctResp": corrResp,
            "task": task,
            "stim": currTrial["stim"], "colour": currTrial["colour"], "fullOri": fullOri, "condID": currTrial["condID"], "shape": shape, "sequence": sequence}        
        # Save the current row into the csv file
        with open(output_name, 'a') as file:
            fieldnames = outData.columns
            writer = csv.DictWriter(file, fieldnames=fieldnames, lineterminator = "\n")
            writer.writerow(row)        
        
        #increase trial count and allow escape
        trN = trN+1        
        if response == ["escape"]:
            core.quit()
            
    # increase block count and show between-blocks pause screen and allow escape
    pause(blN).draw()    
    win.flip()    
    press = event.waitKeys(keyList = quit_go_keys)    
    if press == ["escape"]:
        core.quit()    
    blN = blN+1

#thank participant and quit

thanks.draw()

win.flip()

press = event.waitKeys(keyList = quit_go_keys)

#    # ------ Total Huge Debug ----------
#
#    row = {"pp": pp, "cue_task": cue_task, "mag_key": corrResp_dict["magnit"], "par_key":corrResp_dict["parity"], "cueMap": cueMap, 
#    "respMag": respMag, "respPar": respPar}
#
#    # Save the current row into the csv file
#    with open("instr_check.csv", 'a') as file:
#        fieldnames = instr_check_out.columns
#        writer = csv.DictWriter(file, fieldnames=fieldnames, lineterminator = "\n")
#        writer.writerow(row)
#    #-----------------------------------