import datetime
import sys
import pprint

date_start = datetime.date(2015, 10, 5)
date_end = datetime.date(2016, 2, 8)
date_delta = datetime.timedelta(days=1)

whatsapp_file = sys.argv[1]

with open(whatsapp_file, 'r') as fp:
    lines = fp.readlines()

day_count = {}

while date_start <= date_end:
    date_start += date_delta
    day_count[date_start.strftime("%d/%m/%Y")] = 0


line_count = 0
for line in lines:
    line_count += 1
    try:
        day_count[line[0:10]] += 1
    except:
        pass

max_count = max(day_count.values())
max_key = max(day_count, key=lambda k: day_count[k])

print("Maximum messages: ", max_count, " on date: ", max_key)
print("Total line count = ", line_count)
print("Total days = ", len(day_count))
