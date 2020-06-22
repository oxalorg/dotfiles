import json
import sys
import re
import requests
import shutil

def download_file(url):
    local_filename = url.split('/')[-1]
    r = requests.get(url, stream=True)
    with open(local_filename, 'wb') as f:
        shutil.copyfileobj(r.raw, f)
    return local_filename

with open('philosophize-this.json', 'r') as fp:
    pt = json.load(fp)

items = pt['rss']['channel']['item']

result_set = []

def parse(item):
    m = re.match(r'Episode #(\d+) \.\.\. (.*)', item['title'])
    id, title = m.groups()
    mp3 = item['enclosure']['_url']
    summary = item['subtitle']['__cdata']
    return dict(id=int(id), title=title, mp3=mp3, summary=summary)


for item in items:
    result = parse(item)
    result_set.append(result)


def process():
    with open('processed-philosophize-this.json', 'w') as fp:
        json.dump(result_set, fp)


def download(start, end):
    item_range = [x for x in result_set if start <= x['id'] <= end]
    for i, item in enumerate(reversed(item_range)):
        print('{} files remaining'.format(end-start-i))
        fname = download_file(item['mp3'])
        print('{} download completed'.format(fname))


if __name__ == '__main__':
    if sys.argv[1] == '-p':
        process()
    elif sys.argv[1] == '-d':
        start = int(sys.argv[2])
        end = int(sys.argv[3])
        download(start, end)
    else:
        print(result_set)
