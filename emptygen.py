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

with open("emptydata.csv", "w") as emptyfile:
    mywriter = csv.writer(emptyfile)
    for line in eachline:
        mywriter.writerow(line)
