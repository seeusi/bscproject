import random
import csv

#randnum = random.uniform(1, 10) #random floating point value between 1 and 10
#myint = random.randint(1,6) #random integer between 1 and 6
#random.choice(list)
#random.choices(list, weights = [], k=n) n is the number of random picks
    #randomly selects same item in list multiple times
#range(1,53) 53 is non-inclusive
#random.shuffle(list(range(1,53)))
#sample selects unique items, random.sample(list, k=n)

eachline = []

for num in range (500):
    parameter1 = round(random.uniform(10,20), 3)
    parameter2 = round(random.uniform(20,200), 3)
    parameter3 = round((parameter1 * parameter1) + (2 * parameter2), 1)
    parameter4 = round(random.uniform(0,5), 3)
    parameter5 = round(random.uniform(50, 500), 3)
    parameter6 = round(random.uniform(30, 50), 3)
    parameter7 = round((parameter5 - (parameter4 * parameter6)), 1)
    parameter8 = round(random.uniform(100,200), 3)
    parameter9 = round(random.uniform(2,20), 3)
    parameter10 = round((parameter8 / parameter9 / 10), 1)
    eachitem = [parameter1, parameter2, parameter3, parameter4, parameter5,
                parameter6, parameter7, parameter8, parameter9, parameter10]
    eachline.append(eachitem)

with open("sampledata.csv", "w") as datafile:
    mywriter = csv.writer(datafile)
    mywriter.writerow(["Variable1", "Variable2", "Variable 3", "Variable 4",
                        "Variable 5", "Variable 6", "Variable 7", "Variable 8",
                        "Variable 9", "Variable 10"])
    for line in eachline:
        mywriter.writerow(line)
