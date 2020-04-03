# Building Gorilla version of BRAC01 and BRAC02
# import sys
# import os
import pandas as pd
import csv
from instr_sheets import writeMyInstructions
from Draw_tasks_4 import draw, drawStimuli

myDir = "C:/Users/Elena/Documents/AA_PhD/Projects/BRAC01-FirstOnline/experiment/"

#create the stimuli
drawStimuli(1)
# create the instructions file
writeMyInstructions()
