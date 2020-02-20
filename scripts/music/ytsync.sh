#!/bin/bash
youtube-dl --download-archive downloaded.txt --no-post-overwrites -ciwx --audio-format mp3 -o "%(title)s.%(ext)s" "https://www.youtube.com/playlist?list=PL3_NLXp9puXWs19A9mdPXkoQ_WZ4IEBvp"
