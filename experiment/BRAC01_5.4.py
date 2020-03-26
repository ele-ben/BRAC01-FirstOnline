################################################
#             ....................             #
#                     BRAC01                   # # BRAC01_5.1 ha istruzioni corrette funge
#              Cue-related Contex              # # BRAC01_5.2.1 ha weighted pseudorandom e non randcarefully - testato per pp=1,2,3
#                                              # # BRAC01_5.2.2 ha instructions ascii compatible, works in the lab, still need to test what happens with new output_name in the lab
################################################ # BRAC01_5.3 come sopra ma non salva due votle i file

# ===================== Which Language? ==================
language = "de"
#language = "en"

# ===================== Which device? ==================
#computer = "asus" 
#computer = "office"
computer = "lab"

# ===================== Imports ========================
import os, sys

import random, pyglet, csv, numpy as np

import pandas as pd

pyglet.options['shadow_window']=False

from psychopy import sound, core, event, gui, visual, data

from datetime import datetime

from numpy.random import choice

# ===================== Useful Functions ================

# present instructions, keys are defined as arguments (strings!)
def navigate_instructions(instructions_list, lastPage, left, right, escape, begin): 
    page = 0    
    no_page = lastPage+1    
    backForward = [left, right, escape, begin]    
    while page < no_page:    
        instruction = instructions_list[page]
        instruction.draw()
        win.flip()
        press = event.waitKeys(keyList = backForward)        
        if press == [left]:
            if page != 0:
               page = page - 1
        elif press == [right]:
                if page != lastPage:
                   page = page + 1
        elif press == [escape]:
            core.quit()
        elif press == [begin] and page == lastPage:
            page = no_page
            
#function to shuffle df rows according a pre-shuffled column
def shuffle_rows(res, to_be_shuffled, target_columnStr):
    output_df = pd.DataFrame(columns = to_be_shuffled.columns)
    for i in range(len(res)):
        choices = to_be_shuffled[to_be_shuffled[target_columnStr]== res[i]].index
        ch = random.choice(choices)
        row = to_be_shuffled.iloc[ch]
        output_df = output_df.append(row, ignore_index = True)
        to_be_shuffled = to_be_shuffled.drop([ch], axis =0)
        to_be_shuffled.reset_index(inplace = True, drop= True)    
    return output_df

# shuffle numbers with weights, calls shuffle_rows to return a df
def weighted_pseudorand(to_be_shuffled, target_columnStr):
    numbers = np.array(to_be_shuffled["stim"])
    wei = np.ones(len(numbers))/len(numbers)
    res = np.zeros(len(numbers))
    for i in range(len(numbers)):
        num = res[i-1]
        counter = 0
        for counter, value in enumerate(numbers):
            if value == num:
                ind = counter
        instances = numbers==num #find the instances of the n-1 number...
        if any(instances):  #if there are instances left  
            wei[instances] = wei[instances]/2 #...these get half weight
            numbers = np.delete(numbers, ind) #rem ONE instance of elmn from global list
            wei = np.delete(wei, ind) #rem ONE instance of elmn from global weights list
            # redistribute probabilities
            remain = (1- sum(wei))/len(wei) #calculate the complementar to 1 and divide into all
            wei = wei+remain
            no_instances = numbers!=num #find all the other numbers
            temp_wei = wei[no_instances] # rem ALL of their weights
            temp_numbers = numbers[no_instances] #rem ALL instances from temporary list
            # redistribute probabilities
            remain_temp = (1- sum(temp_wei))/len(temp_wei) 
            temp_wei = temp_wei + remain_temp
            elmn = choice(temp_numbers, 1, p = temp_wei) # draw from list w/o instances
            res[i] = elmn #append the chosen element
        else: #if there are not instances left, just
            elmn = choice(numbers, 1, p = wei)
            #print type(elmn)
            res[i] = elmn
    #return res -- Nope, because I attache here the function than drwas the df rows accordingly
    trialsPermuted = shuffle_rows(res, to_be_shuffled, target_columnStr)
    return trialsPermuted

#randomize carefully and in a fixed n of cycles. Not very nice, just used for training
def carefully_Nminus1(to_be_shuffled, target_columnStr):    
    output_df = pd.DataFrame(columns = to_be_shuffled.columns)    
    totRows= len(to_be_shuffled)    
    s = set(to_be_shuffled[target_columnStr])    
    elems = list(s)    
    n_repeat = totRows/len(elems)    
    res = []    
    #algorithm to avoid n-1 repetitions
    for n in range(n_repeat):
        if res: #if res is full 
            lst = list(s.difference({res[-1]})) #all elements without the last.
            random.shuffle(lst) #randomizzi la lista mancante di quell'elemento...
            lst.append(res[-1]) #appendi alla lista quell'elemento
            lst[1:] = random.sample(lst[1:], len(lst)-1) #randomizzi tutto tranne il primo elemento. 
        else: # se res + ancora vuota
            lst = elems[:] #prendi tutti elementi
            random.shuffle(lst) #mischiali
        res.extend(lst)  
    for i in range(len(res)):
        choices = to_be_shuffled[to_be_shuffled[target_columnStr]== res[i]].index
        ch = random.choice(choices)
        row = to_be_shuffled.iloc[ch]
        output_df = output_df.append(row, ignore_index = True)
        to_be_shuffled = to_be_shuffled.drop([ch], axis =0)
        to_be_shuffled.reset_index(inplace = True, drop= True)    
    return output_df
# ===================== Set Directory ==================

if computer == "office":
    myDir = "C:\\Users\\Elena\\Documents\\PsychoPy\\"
elif computer == "asus":
    myDir = "C:\\Users\\Elena\\Documents\\AA_PhD\\PsychoPy\\"
elif computer == "lab":
    myDir = "\\home\\experiment\\psychopy\\Elena\\"

# ===================== Dialog ========================
expinfo= {"pp" : "", "age" : "", "gender" : "", "handedness": ""}

dlg = gui.DlgFromDict(expinfo, title = "Judge the number!") #opens a dialog window

if dlg.OK:
    thisInfo = dlg.data
    print thisInfo
else: print 'user cancelled'

# ==================== Prepare file ====================
#save variables for the output

pp = int(expinfo['pp'])

age = int(expinfo['age'])

sex = expinfo['gender']

hand = expinfo['handedness']

# ====== debug ======
#pp = 1
#
#age = 17
#
#sex = 17
#
#hand = 17
#
# ===================

hour= datetime.now()

hour_str = hour.strftime("%m%d%H%M%S")

# open file
file_name = str(pp) + "_" + hour_str + '_Brac01.csv'

outData = pd.DataFrame([], columns=["pp", "age", "sex", "hand", "trialNum", 
    "blockNum", "cocoa", "response", "correctResp", "rt", "accuracy", "task",
    "stim", "colour", "shape", "fullOri", "condID", "sequence"])
    
if computer == "lab":
    output_name = file_name
    #print output_name
elif computer == "office" or "asus":
    output_name = myDir + file_name
    #print output_name

with open(output_name, 'a') as file:
    fieldnames = outData.columns
    writer = csv.DictWriter(file, fieldnames=fieldnames, lineterminator = "\n")
    writer.writeheader()

# ==================== Variables =======================
pyClock = core.Clock()

grey = [0,0,0] #background

fixationWait = 1.4

cueWait = 0.3

targetWait = 2.5

slowResp = 2500

keyList = ["a", "l", "escape"]

# ================== Conditions ====================
# - WITHIN block variables and list of possible combinations
task = ["magnit", "parity"]

stim = range(1,5) + range(6,10)

colour = ["red", "blue"]

shape = ["full"]

feat_comb = pd.DataFrame([(x,y,z) for x in task for y in stim for z in colour], columns=['task', 'stim', 'colour'])

feat_comb["condID"] = range(len(feat_comb))

feat_comb["cocoa"] = np.ones(len(feat_comb)) #add a temp cocoa value

# - BEWTEEN-subjECTS variables and list of combinations
# two possible task-keys mappings for each task
resp_parity = [{"even": ["a"], "odd": ["l"]}, {"even": ["l"], "odd": ["a"]}]

resp_magnit = [{"less": ["a"], "more": ["l"]}, {"less": ["l"], "more": ["a"]}] 

cue_task = [{"magnit": 0, "parity": 90}, {"magnit": 90, "parity": 0}] #two possible cue-task mappings

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
trialsPercondition= 3

blocks = range(1,9)
#blocks = [1,2]

trN = 0 #trial counter

blN = 1 #block counter

# repeat the df with the possible conditions for the number of times we want each repetition
trialSeq =  pd.concat([feat_comb]*trialsPercondition, ignore_index=True)

trialSeq["trID"] = np.arange(len(trialSeq)) #create a unique trial identifier

# ------------ Training trials -----------------

# define and shuffle training trials
training = feat_comb.iloc[::2]

training.reset_index(inplace = True, drop= True)

for attempt in range(8): # the probability that this fails 8 times in a row should be very low. It takes roughly 700 milliseconds to apply the function 8 times (worst case scen)
    try:
        trainingShuf = weighted_pseudorand(training, "stim")
        break 
    except:
        seq = pd.DataFrame()
        print "training pseudorandomization failed " + str(attempt+1) + " time(s)"
        if attempt == 4:
            trainingShuf = carefully_Nminus1(training, "stim")

# =============== Window and Shapes =====================

# After debug set pixels, screen and fullscreen:
if computer == "office":
    win = visual.Window([1700,1000], fullscr = False, monitor="Fujitsu_B24W-7", units="cm", color = grey)
elif computer == "asus":
    win = visual.Window([1200,700], fullscr = False, monitor="asusPC", units="cm", color = grey)
elif computer == "lab":
    win = visual.Window([1024,768], fullscr = True, monitor="testMonitor", units="cm", color = grey)

fixation = visual.TextStim(win=win, ori=0, text = "+", font='Arial', alignHoriz='center', alignVert='center', height=1, color='black') 

#- draw the cue and the frame with variable features
# --> Full stands for cue, empty for frame
# the full shape has area = 7141 pixel; the lines of the empty shape have total area= 7129; 1 cm = 37.13 pixel on this screen

class cueAndFrame: #name the class
    def __init__(self, colorFull, fullOri, scale): #object properties
        self.full = visual.Rect(win=win, width = 3.5, height = 1.47, ori=fullOri, lineWidth = 0, fillColor = colorFull, pos= [0, 7.5]) #actually create the object cue + square
        self.empty = visual.Rect(win=win, width = 24, height = 24, ori=0, lineWidth = 2, lineColor = "black", fillColor = None, pos= [0, 0])
        self.full_scaled = self.full
        self.empty_scaled = self.empty
        self.full_scaled.size = self.full.size/scale
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

# =============== define instructions ===============
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
    beginExp = visual.TextStim(win=win, ori=0, text = "Damit endet der Ubungsblock. Bei jeglichen Fragen kannst du gerne die Versuchsleiterin fragen. \nDruecke die Leertaste, um das Experiment zu starten. ", font='Arial', height=1, wrapWidth=20, color='white')
    thanks = visual.TextStim(win=win, ori=0, text = "Damit endet das Experiment. Danke fuer deine Teilnahme. Druecke escape um das Experiment zu schliessen. ", font='Arial', height=1, wrapWidth=20, color='white')
    warnAcc = visual.TextStim(win=win, ori=0, text = "Diese Antwort ist falsch! Bitte versuche sorgfaeltiger zu antworten.", font='Arial', height=1, wrapWidth=20, color='white')
    warnRT = visual.TextStim(win=win, ori=0, text = "Bitte versuche schneller zu reagieren.", font='Arial', height=1, wrapWidth=20, color='white')
    quitOrGoKeys = ["escape","space"]
    pause_text = "Du hast den Block %d abgeschlossen. Du kannst gerne eine kurze Pause machen.\nDruecke die Leertaste, um mit dem naechsten Block zu starten."
elif language == "en":
    instructions0 = visual.TextStim(win=win, ori=0, text = "Welcome!\nThank you for taking part in this experiment.\nUse the arrows to move back and forth in these instructions.", font='Arial', height=1, wrapWidth=30, color='white')
    instructions1 = visual.TextStim(win=win, ori=0, text ="We will present you numbers from 1 to 9, 5 excluded.\nFor each number, your task is to tell whether the number is odd or even or whether it is greater than 5 or less than 5.\nBefore each number appears, it will appear a rectangle at the top of the screen. The orientation of the rectangle will indicate which task you are to perform. The rectangle will appear in different colours, yet colours are not informative for your task.\n\nUse the arrows to move back and forth through these instructions.", font='Arial', height=1, wrapWidth=30, color='white')
    instructions2_magn0 = visual.TextStim(win=win, ori=0, text = "When the rectangle is horizontal, your task is to tell whether the number is greater then 5 or less than 5.", font='Arial', pos = [0,9], height=1, wrapWidth=30, color='white')
    instructions2_magn90 = visual.TextStim(win=win, ori=0, text = "When the rectangle is vertical, your task is to tell whether the number is greater then 5 or less than 5.", font='Arial', pos = [0,9], height=1, wrapWidth=30, color='white')
    instructions2_par90 = visual.TextStim(win=win, ori=0, text = "When the rectangle is vertical, your task is to tell whether the number is odd or even.", font='Arial', pos = [0,9], height=1, wrapWidth=30, color='white')
    instructions2_par0 = visual.TextStim(win=win, ori=0, text = "When the rectangle is horizontal, your task is to tell whether the number is odd or even.", font='Arial', pos = [0,9], height=1, wrapWidth=30, color='white')
    instructions3_aLess = visual.TextStim(win=win, ori=0, text = "Please press A key to indicate LESS than 5 \nand L key to indicate GREATER than 5", font='Arial', pos = [0,-9], height=1, wrapWidth=30, color='white')
    instructions3_aGreat = visual.TextStim(win=win, ori=0, text = "Please press L key to indicate LESS than 5 \nand A key to indicate GREATER than 5", font='Arial',pos = [0,-9], height=1, wrapWidth=30, color='white')
    instructions4_aOdd = visual.TextStim(win=win, ori=0, text = "Please press A key to indicate ODD \nand L key to indicate EVEN", font='Arial', pos = [0,-9], height=1, wrapWidth=30, color='white')
    instructions4_aEven = visual.TextStim(win=win, ori=0, text = "Please press L key to indicate ODD \nand A key to indicate EVEN", font='Arial', pos = [0,-9], height=1, wrapWidth=30, color='white')
    instructions5 = visual.TextStim(win=win, ori=0, text = "Please try to answer as fast as possible, but also to be as accurate as possible. In order to increase your speed, please keep your index fingers on the A and L keys through all the duration of the experiment. If you have any questions, please ask the experimenter.\nYou will first start with a short training, to familiarize with the task.\nWhen you are ready, press the space bar to begin the training session.", font='Arial', alignHoriz='center', alignVert='center', height=1, wrapWidth=30, color='white')
    beginExp = visual.TextStim(win=win, ori=0, text = "The training has ended. If you have doubts do not hesitate to ask the experimenter.\nPress the space bar when you are ready to begin the experiment.", font='Arial', height=1, wrapWidth=20, color='white')
    thanks = visual.TextStim(win=win, ori=0, text = "The experiment has ended. Thank you very much! Press escape to quit.", font='Arial', height=1, wrapWidth=20, color='white')
    warnAcc = visual.TextStim(win=win, ori=0, text = "The answer is incorrect! Please try to be more careful!", font='Arial', height=1, wrapWidth=20, color='white')
    warnRT = visual.TextStim(win=win, ori=0, text = "Please try to be faster!", font='Arial', height=1, wrapWidth=20, color='white')
    pause_text = "You have concluded block %d. You may take a break now. Press the space bar to start the following block."
	quitOrGoKeys = ["escape","space"]
# define pause bewteen blocks class

class pause:
    def __init__(self, blN):
        self.pause = visual.TextStim(win=win, ori=0, text = pause_text %(blN), font='Arial', alignHoriz='center', alignVert='center', height=1, wrapWidth=20, color='white')
    def draw(self):
        self.pause.draw()

# ============ Set participant-specific parameters ===============

# find the line of this participant
currPp = pps_comb.loc[pp]

corrResp_parity = resp_parity[currPp["corrResp_parity"]]

corrResp_magnit = resp_magnit[currPp["corrResp_magnit"]]

corrResp_dict = {"magnit": corrResp_magnit, "parity": corrResp_parity}

cue_task = cue_task[currPp["cue_task"]] #cue task association

# establish cocoa for even blocks as the one not in odd blocks
if currPp["odd_cocoa"] == 300:
    even_cocoa = 0
else:
    even_cocoa = 300

#define her instructions
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

#build the screen with instructions about task-cue mapping on the top, cue example in the middle, task-resp mapping on the bottom
instructions2_3 = build_instructions(instructions2_magn, cue_task["magnit"], instructions3)

instructions2_4 = build_instructions(instructions2_par, cue_task["parity"], instructions4)

#build isntructions list
instructions_list= [instructions0, instructions1, instructions2_3, instructions2_4, instructions5]

lastPage = 4

# ============ Experiment starts ============
# instructions
navigate_instructions(instructions_list, lastPage, "left", "right", "escape", "space")

#--------------- Training block ---------------------

for i in range(len(trainingShuf)):
    
    fixation.draw() # fixation
    
    win.flip()
    
    core.wait(fixationWait)
    
    currTrial = trainingShuf.iloc[i] #identify current trial
    
    stim = currTrial["stim"]
    
    task = currTrial["task"]
    
    fullOri = cue_task[task]
    
    cueAndFrame("black", fullOri, 1).draw() #no context thus no colour in training trials
    
    win.flip()
    
    core.wait(cueWait*2)
    
    cueAndFrame("black", fullOri, 1).draw()
    
    target(currTrial["stim"]).draw()

    win.flip()
    
    pyClock.reset()
    
    response= event.waitKeys(maxWait = targetWait, keyList = keyList)
    
    rt = pyClock.getTime()*1000 #----------------------------------------- collect data or not? Give feedback or not?
    
    if response == ["escape"]:
        core.quit()
    
    if not response:
        response = " "
        rt = " "
    
    #find correct answer
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

press = event.waitKeys(keyList = quitOrGoKeys)

if press == ["escape"]:
    core.quit()

# --------------  Blocks start ----------------------
for j in blocks:    
    
    for attempt in range(4): # the probability that this fails 4 times in a row is 0.0000041%. It takes roughly 2 seconds to apply the function 4 times (worst case scen)
        try:
            trialShuf = weighted_pseudorand(trialSeq, "stim")
            sequence = "pseudorand"
            break 
        except:
            seq = pd.DataFrame()
            print "trials pseudorandomization failed " + str(attempt+1) + " time(s)"
            if attempt == 4:
                trialShuf = carefully_Nminus1(trialSeq, "stim")
                sequence = "carefully"

    if blN%2 != 0: #if odd block then odd cocoa
        trialShuf["cocoa"] = currPp["odd_cocoa"]
    elif blN%2 == 0:
        trialShuf["cocoa"]= even_cocoa
    else:
        core.quit()
        print "block number is not odd nor even"
    
    for i in range(len(trialShuf)):
    #for i in range(2):
        
        fixation.draw() # fixation
        
        win.flip()
        
        core.wait(fixationWait)
        
        currTrial = trialShuf.iloc[i] #identify current trial
        
        task = currTrial["task"]
        
        fullOri = cue_task[task]
        
        stim = currTrial["stim"]
        
        if currTrial["cocoa"]== 300:
            cueAndFrame("black", fullOri,1).draw() #draw black cue and frame (always black)
            win.flip()
            core.wait(cueWait)
        elif currTrial["cocoa"]== 0:
            cueAndFrame(currTrial["colour"], fullOri,1).draw()#draw already coloured cue and frame
            win.flip()
            core.wait(cueWait)
        else:
            core.quit()
            print "cocoa column in trial shuff doesn't report correct value"
        
        cueAndFrame(currTrial["colour"], fullOri,1).draw() #draw coloured cue and black frame
        
        win.flip()
        
        core.wait(cueWait)
        
        target(currTrial["stim"]).draw()
        
        cueAndFrame(currTrial["colour"], fullOri,1).draw()
        
        win.flip()
        
        pyClock.reset()
        
        response= event.waitKeys(maxWait = targetWait, keyList = keyList)
        
        rt = pyClock.getTime()*1000
        
        if not response:
            response = " "
            rt = " "
        
        #find correct answer
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

        if response == corrResp:
            accuracy = 0
        else:
            accuracy = 1 #-------------------------------------------- what to do with accuracy in unanswered trials?
        
        #you can write whatever order to give row as an input for open(output_name...)
        row = {"pp": pp, "age": age, "sex": sex, "hand": hand,
            "trialNum": trN, "blockNum" : blN, "cocoa": currTrial["cocoa"], "rt": rt,
            "accuracy": accuracy, "response": response, "correctResp": corrResp,
            "task": task,
            "stim": currTrial["stim"], "colour": currTrial["colour"], "fullOri": fullOri, "condID": currTrial["condID"], "shape": shape, "sequence": sequence}
        
        #Save the current row into the csv file
        #whatever order row dictonary has, data will be saved correctly
        with open(output_name, 'a') as file:
            fieldnames = outData.columns
            writer = csv.DictWriter(file, fieldnames=fieldnames, lineterminator = "\n")
            writer.writerow(row)
        
        #increase trial count
        trN = trN+1
        
        if response == ["escape"]:
            core.quit()
    
    # show between-blocks pause screen and increase block count
    pause(blN).draw()
    
    win.flip()
    
    press = event.waitKeys(keyList = quitOrGoKeys)
    
    if press == ["escape"]:
        core.quit()
    
    blN = blN+1

#thank participant and quit

thanks.draw()

win.flip()

press = event.waitKeys(keyList = quitOrGoKeys)
