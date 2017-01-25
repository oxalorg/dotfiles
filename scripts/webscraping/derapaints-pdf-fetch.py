#!python3
# To grab and download bulk .pdf from a site.
#
# Author: @MiteshNinja
# Date: 2015, October 24

import requests
import bs4
import re
import sys
import os

# GET the following URL
target_page = "http://derapaints.net/investors.html"
res = requests.get(target_page)
res.raise_for_status()

# Pass the response object res to BeautifulSoup
soup = bs4.BeautifulSoup(res.text)

# Regex for extraction
result = soup.find_all('a', href=re.compile("^pdf/.+\.pdf$"))

for link in result:
    rel_url = link.get('href')
    print("Downloading: " + rel_url)
    pdf_url = target_page[:-14] + rel_url
    try:
        # Create a response object and grab the PDFs
        pdf_res = requests.get(pdf_url)
        pdf_res.raise_for_status()
        # Open a file in ./pdf/ directory
        pdf_file = open("./pdf/" + os.path.basename(rel_url), 'wb')
        # Write chunks of data from the response object
        for chunk in pdf_res.iter_content(100000):
            pdf_file.write(chunk)
        print("File downloaded to: ./pdf/" + os.path.basename(rel_url))
    except:
        print("Exception Catched in writing to file: ", sys.exc_info()[0])
