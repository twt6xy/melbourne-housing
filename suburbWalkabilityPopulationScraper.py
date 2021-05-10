#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon May 10 19:23:50 2021

@author: willtyree
"""

import requests
from bs4 import BeautifulSoup
import pandas as pd
import collections

def getWalkIdxAndPopulation(url):
    page = requests.get(url)                          # Download the web page containing the data.
    soup = BeautifulSoup(page.content, 'html.parser') # Create a BeautifulSoup class to parse the page.
    table = soup.table                                # obtain the table containing the data
    cols = []                                         # create a list that will hold the column names

    for row in table.find_all('th'):                  # loop through every row in the table denoted by the 'th' header
        cols.append(str(row.find(text=True)))         # add the column name to the cols list

    data = collections.OrderedDict()                  # create an ordered dictionary with the collections module that will hold the data

    for col in cols:                                  # loop through the column names of the data and set them as the keys for the dictionary
        data[col] = []

    for row in table.findAll("tr"):                                              # loop through the rows
        cells = row.findAll("td")                                                # find all the cells in each row
        if len(cells) > 1:                                                       # if there is more than one cell in each row
            for index in range(len(cells)):                                      # loop through each cell of that row
                if cells[index].find(text=True)=='\n':                           # if there is data in that cell find the text and move to the next cell
                    data[cols[index]].append(row.findAll('a')[1].find(text=True))
                else:                                                            # else populate cell with a null value
                    data[cols[index]].append(cells[index].find(text=True))

    df = pd.DataFrame(data)
    return df

suburbData = getWalkIdxAndPopulation('https://www.walkscore.com/AU-VIC/Melbourne')
suburbData = suburbData.drop(['Transit Score', 'Bike Score', 'Rank'], axis=1)
suburbData = suburbData.rename(columns={'Name': 'Suburb'})
suburbData.to_csv('suburbWalkabilityPopulation.csv')
