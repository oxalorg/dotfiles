#!python3
# To grab previous year question papers from a website
# automatically and downloading the .pdfs
#
# Website used: http://library.spit.ac.in/qpte1.html
# Author: @MiteshNinja
# Date: 2015, October 24

import requests
import bs4
import re
import sys
import os

# GET the following URL
target_page = "http://library.spit.ac.in/qpte1.html"
res = requests.get(target_page)
res.raise_for_status()

# Store the @target_page in a temporary location
# This is not useful, since we can directly extract
# the tags from response object created earlier
temp_file = open('/tmp/target_page.txt', 'wb')
for chunk in res.iter_content(100000):
	temp_file.write(chunk)
    
temp_file.close()

# Pass the response object @res to BeautifulSoup
soup = bs4.BeautifulSoup(res.text)

# Find all <a> tags where href contains "COM" for computers
result = soup.find_all('a', href=re.compile("COM"))

# Iterate through every tag found using the above result
for link in result:
    # Get the 'href' attribute
    rel_url = link.get('href')
    print("Downloading: " + rel_url)
    # Some string magic because rel_url is a relative link from root domain
    pdf_url = target_page[:-10] + rel_url
    # Create a response object and grab the PDFs
    pdf_res = requests.get(pdf_url)
    pdf_res.raise_for_status()
    try:
        # Open a file in ./pdf/ directory
        pdf_file = open("./pdf/" + os.path.basename(rel_url), 'wb')
        # Write chunks of data from the response object 
        for chunk in pdf_res.iter_content(100000):
            pdf_file.write(chunk)
        print("File downloaded to: ./pdf/" + os.path.basename(rel_url))
    except:
        print("Exception Catched in writing to file: ", sys.exc_info()[0])