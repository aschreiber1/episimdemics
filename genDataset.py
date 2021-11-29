from random import randint
import random
import csv

NUM_PEOPLE = 100
NUM_LOCATIONS = 10
NUM_DAYS = 60
MAX_LOCATIONS_PER_DAY = 5
MAX_LENGTH = 120
START_TIME = 360
END_TIME = 1320

def get_non_trivial_wait_time():
    return fast_rand_int(0,MAX_LENGTH)

#random.randint() is slow, this is faster
def fast_rand_int(minimum, maximum):
    maximum+=1
    return int((maximum-minimum)*random.random() + minimum)

def generate_daily_activities():
    current_time = START_TIME + get_non_trivial_wait_time() #dont make everyone start at the same time
    num_locations_visted = fast_rand_int(0, MAX_LOCATIONS_PER_DAY)
    out = []
    for visit in range(num_locations_visted):
        location = fast_rand_int(1,NUM_LOCATIONS) # random location
        duration = fast_rand_int(1,MAX_LENGTH) # random, non trivial, duratoin
        finish_time = current_time + duration
        if finish_time > END_TIME:
            break
        out.append((location, current_time, finish_time))
        current_time = finish_time + get_non_trivial_wait_time()
    return out

def generate_dataset():
    out = []
    for day in range(NUM_DAYS):
        for person in range(NUM_PEOPLE):
            for location,start_time,finish_time in generate_daily_activities():
                out.append((day, person, location, start_time, finish_time))
    return out

def write_output_to_csv(dataset):
    header = ('Day', 'Person', 'Location', 'StartTime', 'FinishTime')
    with open('dataset.csv', 'w') as f:
        writer = csv.writer(f)
        writer.writerow(header)
        writer.writerows(dataset)

dataset = generate_dataset()
write_output_to_csv(dataset)