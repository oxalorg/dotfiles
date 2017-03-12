"""
A script which helped me extract some data from
a sqlite3 database. Might be useful as a template
for something similar in the future.
"""
import sqlite3
import datetime
import html

conn = sqlite3.connect('oxal.org.db-2017-01-24', detect_types=sqlite3.PARSE_DECLTYPES|sqlite3.PARSE_COLNAMES)
c = conn.cursor()

posts = []

for row in c.execute('SELECT id, body, creation_time as "ts [timestamp]" from post'):
    id = row[0]
    body = row[1]
    date = row[2]
    body = html.unescape(body.replace(r'<br />', '\n'))
    fname = date.isoformat()[:-7] + ".md"
    print('Extracting to: ', fname)
    with open(fname, 'w') as fp:
        fp.write("{}".format(body))


