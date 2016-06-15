#!/usr/bin/env python3
"""Script to fetch lyrics"""
import urllib.request
import urllib.parse

def get_lyrics(title, artist):
    lyr_endpt = 'http://makeitpersonal.co/lyrics'
    query = urllib.parse.urlencode([('artist', artist), ('title', title)])
    lyr_url = lyr_endpt + '?' + query
    print(lyr_url)
    f = urllib.request.urlopen(lyr_url)
    print(f.read().decode('utf-8'))

if __name__ == '__main__':
    title = input('Song: ')
    artist = input('Creator: ')
    get_lyrics(title, artist)
