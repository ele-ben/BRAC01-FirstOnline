#imports

import random, numpy as np

import pandas as pd

from random import randint

outside_Psychopy = 0

try:
   from psychopy import core, event, gui, visual
except:
   outside_Psychopy = 1

if outside_Psychopy == 0:
    # define instructions presentation function
    # present instructions, keys are defined as arguments (strings!)
    def navigate_instructions(win, instructions_list, left, right, escape, begin):
        page = 0
        lastPage = len(instructions_list)-1
        no_page = lastPage+1
        backForward = [left, right, escape, begin]
        while page < no_page:
            instruction = instructions_list[page]
            instruction.draw()
            win.flip()
            press = event.waitKeys(keyList = backForward)
            #print press
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


# ---- balance task rep and switch ----
# task0 and 1 can be strings

def balanceTransitionsMinus1_str(trials, task0, task1):
    maxCounter = 4
    if type(trials) != int:
        raise ValueError("trials must be of type integer.")
    if trials%2 != 0:
        raise ValueError("trials argument must be an even integer.")
    if trials <= 0:
        raise ValueError("trials must both be greater than 0.")
    seq_Completed = 0
    counter = 0
    while seq_Completed == 0 and counter <= maxCounter:
        seq = [task0, task1]* int(trials/2)
        random.shuffle(seq)
        rep=0
        sw=0
        zeross = 0 # task0
        oness = 0 # task1
        #count task0 and task1
        for i in range(trials):
            if seq[i] == "magnit":
                zeross +=1
            else:
                oness +=1
        #print("there are " + str(zeross) + " zeros and " + str(oness) + " ones")
        #count sw and repetition
        for i in range(trials-1):
            if seq[i] == seq[i+1]:
                rep +=1
            else:
                sw +=1
        diff = rep-sw
        #print("repetition " + str(rep) + " switch " + str(sw))
        if rep == 0: # when the sequence is 010101... or 101010 I switch the first two elements
            seq[0] = seq[1]
            seq[1] = seq[2]
        if diff > 1: # if more rep than sw and delta greater than 1
            Rounds = ((diff - 2) // 4) + 1 # how many rounds I need to fix the imbalance
            #print(str(Rounds))
            for j in range(1, Rounds+1):
                #print("we're in the exceeding rep case")
                strt = randint(0, trials-1) # I start from a random position in the sequence
                success = 0
                for i in range(trials-3): #I loop over the sequence
                    if success == 1:
                        break
                    else:
                        ind = (strt+i)%(trials-2) # increment position
                        if (seq[ind] == seq[ind+1] and seq[ind+1] == seq[ind+2]): #111 o 000
                            first = ind # save first position of the triplet
                            change = ind+1 # save the position to be changed
                            strt1 = randint(0, trials-1)
                            #print("first triplet " + str(seq[first]) + str(seq[change]) + str(seq[first+2]))
                            for k in range(trials-3): #I loop over the sequence
                                ind = (strt1+k)%(trials-2) # increment position
                                # look for 100 o 001 o ( 011 o 110)
                                if (seq[ind] == seq[first] and seq[ind+1] != seq[first] and seq[ind+2] != seq[first]) or (seq[ind] != seq[first] and seq[ind+1] != seq[first] and seq[ind+2] == seq[first]): # look for 100 o 001 o ( 011 o 110)
                                    #print("second triplet " + str(seq[ind]) + str(seq[ind+1]) + str(seq[ind+2]))
                                    temp = seq[change]
                                    seq[change] = seq[ind+1] #swap middle elements of the 2 triplets
                                    seq[ind+1] = temp
                                    success = 1
                                    break
        if diff < -1: # if more switches and again delta greater than 1
            Rounds = ((abs(diff) - 2) // 4) + 1 # how many rounds I need to fix the imbalance
            #print(str(Rounds))
            for j in range(1, Rounds+1):
                #print("we're in the exceeding sw case")
                strt = randint(0, trials-1) # I start from a random position in the sequence
                success = 0
                for i in range(trials-3): #I loop over the sequence
                    if success == 1:
                        break
                    else:
                        ind = (strt+i)%(trials-2) # increment position
                        if (seq[ind] != seq[ind+1] and seq[ind] == seq[ind+2]): # look for 101 or 010
                            first = ind # save first position of the triplet
                            change = ind+1 # save the position to be changed
                            #print("first triplet " + str(seq[first]) + str(seq[change]) + str(seq[first+2]))
                            strt1 = randint(0, trials-1)
                            for k in range(trials-3): #I loop over the sequence
                                ind = (strt1+k)%(trials-2) # increment position
                                if ind != change and ind != first -1: # ensure it doesn't step on the first triplet. There cannot be overlap in the searched triplet in the "more rep" case
                                    #cerca sequenza 110 o 011 o ( 001 o 100)
                                    if (seq[ind] == seq[first] and seq[ind+1] == seq[first] and seq[ind+2] != seq[first]) or (seq[ind] != seq[first] and seq[ind+1] == seq[first] and seq[ind+2] == seq[first]): # (001 o 100) o (110 o 011)
                                        #print("second triplet " + str(seq[ind]) + str(seq[ind+1]) + str(seq[ind+2]))
                                        temp = seq[change]
                                        seq[change] = seq[ind+1] #swap middle elements of the 2 triplets
                                        seq[ind+1] = temp
                                        success = 1
                                        break
        #these redudnant tests check integrity of the sequence after the manipulations
        counter += 1
        rep=0
        sw=0
        zeross = 0
        oness = 0
        for i in range(trials):
            if seq[i] == task0:
                zeross +=1
            else:
                oness +=1
        if zeross != oness:
            raise Warning("number of " + str(task0) + " is different from number of " + str(task1))
        for i in range(1, trials):
            if seq[i] == seq[i-1]:
                rep +=1
            else:
                sw +=1
        if abs(rep-sw) == 1  and zeross == oness:
            seq_Completed=1
        elif abs(rep-sw) != 1 and counter >= maxCounter+1:
            raise Warning("N - 1 transitions couldn't be balanced by balanceTransitionsMinus1_str function")
    # if everything ends
    seqAndDiff = [seq, rep, sw]
    return seqAndDiff

# --------------
# stim and task
# balance stimuli across STRING tasks
def shuffle_StimTask_str(trials, stimElmns, minusWhat, task0, task1):
    if (trials/2)%len(stimElmns) != 0:
            raise ValueError("stimElmns list length must be a divisor of trials/2, otherwise balancing is not possible by construction. Also, trials must even integer")
    maxCounter = 10
    seqCompleted = 0
    counter = 0
    while seqCompleted == 0 and counter <= maxCounter:
        if minusWhat == 1:
            taskSeq = np.array(balanceTransitionsMinus1_str(trials, 0, 1)[0])
        elif minusWhat == 2:
            taskSeq = balanceTransitionsMinus2(trials)
        else:
            raise ValueError("minusWhat must be either 1, if you want to balance n-1 rep and sw, or 2, if you want to balance n-2 rep and sw")
        stimAndTask = np.c_[taskSeq, np.zeros(trials)] # prepare an array trials*2 where the first column is trialSeq
        timesXtrial = trials/len(stimElmns)/2 # calculate how many times each stim stands with each of the 2 tasks
        stimLst = np.repeat(stimElmns,timesXtrial) # replicate the list with unique stimuli this number of times
        for task in range(2): # for task 0 and 1, create a vector of randomized stimuli
            stimSeq = np.random.permutation(stimLst)
            currTask = np.where(taskSeq == task)[0]
            currPos = 0
            for g in currTask: # paste one randomize vector aside tasks 0s and the other aside 1s
                stimAndTask[g, 1] = stimSeq[currPos]
                currPos += 1
        for j in range(1, trials): # now the stim sequence is checked for stimuli (n-1) repetitions
            if stimAndTask[j, 1] == stimAndTask[j-1, 1]: # if it's found, the first of the j-1,j pair is saved:
                change = stimAndTask[j-1, 1] # which number is that repeats
                itsTask = stimAndTask[j-1, 0] # to which task is it associated
                # look for an adapt position, starts from 2 numbers after the j of the j-1,j pair of numbers:
                # "i" goes from 2 to up to trials - 1, since you want to stop 2 positions before the starting point
                for i in range(2, trials-1):
                    found = [] # found is initially false
                    # ind runs in a circle over the sequence
                    ind = (j+i)%(trials-1) # I want ind to get to 94th position and to restart from 0 at the 95th
                    cond1 = stimAndTask[ind-1, 1] != change # the number before should not be = change
                    cond2 = stimAndTask[ind, 1] != change # the candidate number should not be = change
                    cond3 = stimAndTask[ind+1, 1] != change # the following before should not be = change
                    cond4 = stimAndTask[j-2, 1] != stimAndTask[ind, 1] # the found number should be different from the one before j-1
                    condTask = stimAndTask[ind, 0] == itsTask # the task should be the same assigned to change
                    if cond1 and cond2 and cond3 and cond4 and condTask:
                        found = stimAndTask[ind, 1] # a suitable position has been found, swap change and found
                        stimAndTask[ind, 1] = change # the position in the middle of the triplet takes value change
                        stimAndTask[j-1, 1] = found # the position of the first element of the pair of equal numbers gets the value found
                        break
                if not found and counter == maxCounter:
                    raise Warning("the numbers cannot be correctly assigned to the 0s and the 1s. There are 1 (or more) pairs of 2 equal numbers in subsequent positions")
        # test for numbers assignment
        counter += 1
        vec = [[0]*int(trials/2), [0]*int(trials/2)] # preallocate an array: trials/2 is the max lenght allowed for stimElmn list
        for task in range(2):
            for i in stimElmns: # fill in the array with the times each element is found aside 0 and 1
                vec[task][i] = sum(np.logical_and(stimAndTask[:,1] == i,stimAndTask[:,0] == task))
            vec1 = [vec[task][i] for i in stimElmns] # take only the relevant positions of vec (some will be 0)
            equalTimes = all(x== vec1[0] for x in vec1) # are number of time all the same within a task?
            if (not equalTimes) and counter > maxCounter:
                raise Warning("the function is wrong: stimuli are not equally represented in task " + str(task))
        # test for effectiveness of the removal of numbers repetitions
        bool_2inARow = [stimAndTask[i, 1] == stimAndTask[i-1, 1] for i in range(1,trials)]
        if (any(bool_2inARow)) and counter > maxCounter:
            raise Warning("2 equal stimuli are found in subsequent positions")
        # test for integrity of trialSeq after manipulations
        if not all(taskSeq == stimAndTask[:,0]):
            raise Warning("the final sequence of tasks (0 and 1) is not identical to the starting one. The function should not cause the sequence to change")
        # assign 1 to seqCompleted variable to exit the while loop
        if (not any(bool_2inARow)) and equalTimes:
            seqCompleted = 1
            stimAndTask_df = pd.DataFrame(stimAndTask, columns = ['task', 'stim'])
            task0_indx = stimAndTask_df[stimAndTask_df['task'] == 0].index
            stimAndTask_df.loc[task0_indx, 'task'] = task0
            task1_indx = stimAndTask_df[stimAndTask_df['task'] == 1].index
            stimAndTask_df.loc[task1_indx, 'task'] = task1
    #return [stimAndTask_df, taskSeq, counter]
    return stimAndTask_df

# --------------
# order Stim only, aim is avoiding n-1 repetitions

def no_StimRepetition(trials, stimElmns):
    if (trials)%len(stimElmns) != 0:
            raise ValueError("stimElmns list length must be a divisor of trials, otherwise balancing is not possible by construction. Also, trials must even integer")
    maxCounter = 10
    seqCompleted = 0
    counter = 0
    while seqCompleted == 0 and counter <= maxCounter:
        #stimAndTask = np.zeros(trials) # prepare a vec
        timesXstim = trials/len(stimElmns) # calculate how many times each stim appears
        stimLst = np.repeat(stimElmns,timesXstim) # replicate the list with unique stimuli this number of times
        stimSeq = np.random.permutation(stimLst)
        for j in range(1, trials): # now the stim sequence is checked for stimuli (n-1) repetitions
            if stimSeq[j] == stimSeq[j-1]: # if it's found, the first of the j-1,j pair is saved:
                change = stimSeq[j-1] # which number is that repeats
                # look for an adapt position, starts from 2 numbers after the j of the j-1,j pair of numbers:
                # "i" goes from 2 to up to trials - 1, since you want to stop 2 positions before the starting point
                for i in range(2, trials-1):
                    found = [] # found is initially false
                    # ind runs in a circle over the sequence
                    ind = (j+i)%(trials-1) # I want ind to get to 94th position and to restart from 0 at the 95th (assuming 96 trials)
                    cond1 = stimSeq[ind-1] != change # the number before should not be = change
                    cond2 = stimSeq[ind] != change # the candidate number should not be = change
                    cond3 = stimSeq[ind+1] != change # the following before should not be = change
                    cond4 = stimSeq[ind] != stimSeq[j-2]# the candidate number should not be = the one before change
                    if cond1 and cond2 and cond3 and cond4:
                        found = stimSeq[ind] # a suitable position has been found, swap change and found
                        stimSeq[ind] = change # the position in the middle of the triplet takes value change
                        stimSeq[j-1] = found # the position of the first element of the pair of equal numbers gets the value found
                        break
                if not found and counter == maxCounter:
                    raise Warning("the numbers cannot be correctly ordered. There are 1 (or more) pairs of 2 equal numbers in subsequent positions")
        # test for numbers assignment
        counter += 1
        vec = [0]*trials # preallocate an array: trials is the max lenght allowed for stimElmn list
        for i in stimElmns: # fill in the array with the times each element is found
            vec[i] = sum(stimSeq == i)
        vec1 = [vec[i] for i in stimElmns] # take only the relevant positions of vec (some will be 0)
        equalTimes = all(x == vec1[0] for x in vec1) # are number of times all the same?
        if (not equalTimes) and counter > maxCounter:
            raise Warning("the function is wrong: stimuli are not equally represented")
        # test for effectiveness of the removal of numbers repetitions
        bool_2inARow = [stimSeq[i] == stimSeq[i-1] for i in range(1,trials)]
        if (any(bool_2inARow)) and counter > maxCounter:
            raise Warning("2 equal stimuli are found in subsequent positions")
        if (not any(bool_2inARow)) and equalTimes:
            seqCompleted = 1
    #return [stimAndTask, taskSeq, counter]
    return stimSeq

# --------------
# df order according to stim only, targetCol is string!
def shuffle_rows(res, df2shuf, targetCol):
    df_output = pd.DataFrame(columns = df2shuf.columns)
    for i in range(len(res)):
        choices = df2shuf[df2shuf[targetCol]== res[i]].index
        ch = random.choice(choices)
        row = df2shuf.iloc[ch]
        df_output = df_output.append(row, ignore_index = True)
        df2shuf = df2shuf.drop([ch], axis =0)
        df2shuf.reset_index(inplace = True, drop= True)
    return df_output

# --------------
# df order according to stim AND task
def shuffle_rows2(df2order, targetCol, stimSeq, taskCol, taskSeq):
    df_output = pd.DataFrame(columns = df2order.columns)
    for i in range(len(stimSeq)):
        boolean_condition = (df2order[taskCol] == taskSeq[i]) & (df2order[targetCol] == stimSeq[i])
        choices = df2order.loc[boolean_condition].index
        ch = random.choice(choices)
        row = df2order.iloc[ch]
        df_output = df_output.append(row, ignore_index = True)
        df2order = df2order.drop([ch], axis =0)
        df2order.reset_index(inplace = True, drop= True)
    return df_output
