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

for num in range (50):
    parameter1 = round(random.uniform(0,20), 1)
    parameter2 = round(random.uniform(0,30), 1)
    output = (parameter1 * parameter1) + (2 * parameter2)
    eachitem = [parameter1, parameter2, round(output, 1)]
    eachline.append(eachitem)

with open("mydata.csv", "w") as datafile:
    mywriter = csv.writer(datafile)
    mywriter.writerow(["parameter1", "parameter2", "output"])
    for line in eachline:
        mywriter.writerow(line)
