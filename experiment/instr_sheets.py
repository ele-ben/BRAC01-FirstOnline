import pandas as pd
import csv
from BRACfun import no_StimRepetition, shuffle_rows
# create the 8 different instructions spreadsheets
def writeMyInstructions():
    # define the strings composing each instruction
    magn =  "greater or less than 5."
    par = "odd or even."
    horiMap = "When the rectangle is horizontal, your task is to tell whether the number is "
    verMap = "When the rectangle is vertical, your task is to tell whether the number is "
    greatM = "greater than 5."
    lessM = "less than 5."
    oddM = "odd."
    evenM = "even."
    A = "Press A to indicate "
    L = "Press L to indicate "

    tasks = [[magn, par], [par, magn]]
    keys = [[A, L], [L, A]]
    keys1 = [[A, L], [L, A]]
    for task in tasks:
        for key in keys:
            for key1 in keys1:
                instr = pd.DataFrame([], columns = ["display", "horiMap", "verMap", "greatMap", "lessMap", "oddMap", "evenMap"])
                row = {"display": "Instructions", "horiMap": horiMap + task[0],
                    "verMap": verMap + task[1], "greatMap": key[0] + greatM, "lessMap": key[1]+ lessM, "oddMap": key1[0] + oddM, "evenMap": key1[1] + evenM}
                instr = instr.append(row, ignore_index = True)
                instFile = task[0][:3] + key[0][6:7] + key1[0][6:7]
                instr.to_csv("spreadsheets/"+ instFile + ".csv", sep = ";", index= False)
