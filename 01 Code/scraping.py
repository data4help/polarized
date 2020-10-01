#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Sep 16 21:35:41 2020

@author: paulmora
"""

# %% Preliminaries

import numpy as np
import pandas as pd
import requests
from bs4 import BeautifulSoup
from tqdm import tqdm
import re
from collections import Counter

# Paths
main_path = r"/Users/paulmora/Documents/projects/polarized"
raw_path = r"{}/00 Raw".format(main_path)
code_path = r"{}/01 Code".format(main_path)
data_path = r"{}/02 Data".format(main_path)
output_path = r"{}/03 Output".format(main_path)

# %% Scraping

"""
We start by scraping all the links of the senate and the house over the years
for that we look for certain gap fillers in our links.
"""

base_url = "https://www.congress.gov/roll-call-votes"
page = requests.get(base_url)
soup = BeautifulSoup(page.content, "html.parser")
str_soup = str(soup)
parsed_soup = str_soup.split("href")

dict_pattern = {
    "house": r'http://clerk.house.gov/evs/(.*)/index.asp',
    "senate": (r'http://www.senate.gov/legislative/LIS/'
               r'roll_call_lists/vote_menu_(.*).htm')
    }
links_additions = {"house": [], "senate": []}
for key in links_additions:
    pattern = dict_pattern[key]
    for snippet in parsed_soup:
        try:
            link_string = re.search(pattern, snippet).group(1)
            links_additions[key].append(link_string)
        except AttributeError:
            pass

# %% House

"""
Now we extract the links which contains the result of the vote as well
as the links which contain the information of who initiated the bill
"""

house_raw_link = r'http://clerk.house.gov/evs/{}/ROLL_{}.asp'
df_list = []
column_names = ["Roll", "Date", "Issue",
                "Question", "Result", "Title/Description"]
for site in tqdm(links_additions["house"]):

    start_num = 0
    valid_page = True
    while valid_page:

        num_string = str(start_num) + "00"
        url = house_raw_link.format(site, num_string)
        page = requests.get(url)
        soup = BeautifulSoup(page.content, "html.parser")

        if "Resource Unavailable" in str(soup):
            valid_page = False
        else:
            table_rows = soup.find_all("tr")
            row_content = []
            for tr in table_rows:
                td = tr.find_all("td")
                row = [tr.text for tr in td]
                row_content.append(row)
            df_table = pd.DataFrame(row_content, columns=column_names)

            pattern = r'\n (.*)</sup> Congress'
            title_soup = str(soup.find_all("h2"))
            raw_congress_num = re.search(pattern, title_soup).group(1)
            congress_num = raw_congress_num.replace("<sup>", "")
            df_table.loc[:, "congress"] = congress_num
            df_table.loc[:, "year"] = site
            df_list.append(df_table)
            start_num += 1

total_df = pd.concat(df_list, axis=0)

"""
Now we clean the concatenated dataframe which contains many unusable rows.
A usable is defined as follows:
    - does not have a nan in a critical column
    - does not have a None anywhere
    - has a number in the end of the issue column
"""


total_df.loc[:, "Issue"] = total_df.loc[:, "Issue"].replace("", np.nan)
bool_non_nan = ~total_df.loc[:, "Issue"].isna()
non_nan_df = total_df.loc[bool_non_nan, :]

bool_ends_with_number = non_nan_df.loc[:, "Issue"].str[-1].str.isnumeric()
cleaned_df = non_nan_df.loc[bool_ends_with_number, :]
cleaned_df.reset_index(inplace=True, drop=True)

"""
Now we scrape the results of these bills
"""

base_url = 'https://clerk.house.gov/evs/{}/roll{}.xml'
columns = ["republican_{}", "democratic_{}", "indepedent_{}", "total_{}"]
for i, (roll, year) in tqdm(enumerate(zip(cleaned_df.loc[:, "Roll"],
                                          cleaned_df.loc[:, "year"]))):
    try:
        if len(str(roll)) < 3:
            roll = "0" + str(roll)
        url = base_url.format(year, roll)
        page = requests.get(url)
        soup = BeautifulSoup(page.content, "xml")
        for yea, nay, present, not_voting, col in zip(
                soup.find_all("yea-total"),
                soup.find_all("nay-total"),
                soup.find_all("present-total"),
                soup.find_all("not-voting-total"),
                columns):
            cleaned_df.loc[i, col.format("yea")] = yea.text
            cleaned_df.loc[i, col.format("nay")] = nay.text
            cleaned_df.loc[i, col.format("present")] = present.text
            cleaned_df.loc[i, col.format("not_voting")] = not_voting.text
    except:
        pass

"""
Now we scrape who initiated the bill
"""

base_url = 'https://www.congress.gov/bill/{}-congress/house-bill/{}'
pattern = "\[(.*)\]"
for i, (congress, issue) in tqdm(enumerate(zip(cleaned_df.loc[:, "congress"],
                                               cleaned_df.loc[:, "Issue"]))):
    try:
        issue_number = re.findall('\d+', issue)[0]
        url = base_url.format(congress, issue_number)
        page = requests.get(url)
        soup = BeautifulSoup(page.content, "html.parser")
        table_rows = soup.find_all("tr")
        relevant_substring = str(table_rows[0].find_all("td")[0])
        party = re.search(pattern, relevant_substring).group(1).split("-")[0]
        cleaned_df.loc[i, "proposing_party"] = party
        cleaned_df.loc[i, "url"] = url
    except:
        pass

"""
Finally we save the dataframe into an excel sheet
"""

cleaned_df.loc[:, "proposing_party"].value_counts()
cleaned_df.to_excel(r"{}/house_data.xlsx".format(data_path))

# %% Senate

headers = {'User-Agent': 'Mozilla/5.0 (Windows NT 6.1; WOW64) ' +
           'AppleWebKit/537.36 (KHTML, like Gecko) ' +
           'Chrome/56.0.2924.76 Safari/537.36',
           'Accept-Language': 'en-US,en;q=0.8'
           }
voting_url = (r"https://www.senate.gov/legislative/LIS/roll_call_lists/"
              r"roll_call_vote_cfm.cfm?congress={}&session={}&vote={}")
proposing_pattern = " \[(.*)\]</a>"
num_url = ("https://www.senate.gov/legislative/LIS/"
           "roll_call_lists/vote_menu_{}.htm")
num_pattern = "\((.*)\) "
senate_df = pd.DataFrame()
for congress in tqdm(links_additions["senate"]):
   
    try:
        url = num_url.format(congress)
        page = requests.get(url, headers=headers)
        soup = BeautifulSoup(page.content, "html.parser")
        table_rows = soup.find_all("tr")
        row_content = []
        for tr in table_rows:
            td = tr.find_all("td")
            row = [tr.text for tr in td]
            row_content.append(row)
        num_decisions = int(re.search(num_pattern, row_content[1][0]).group(1))

        for num in tqdm(range(1, num_decisions+1)):
            try:
                string_num = ("0" * (5-len(str(num))) + str(num))
                congress_num = congress.split("_")[0]
                session_num = congress.split("_")[1]
                url = voting_url.format(congress_num, session_num, string_num)
                page = requests.get(url, headers=headers)
                soup = BeautifulSoup(page.content, "html.parser")
                str_soup = str(soup)

                table_part = str_soup\
                    .split("Alphabetical by Senator Name")[1]\
                    .split("Grouped By Vote Position")[0]\
                    .split('class="contenttext">')[1]\
                    .split("</span>")[0]

                party_pattern = "\((.*)-"
                party_list = re.findall(party_pattern, table_part)

                vote_pattern = "<b>(.*)</b>"
                vote_list = re.findall(vote_pattern, table_part)

                combi_list = ["{}_{}".format(x, y) for x, y
                              in zip(party_list, vote_list)]

                question = str_soup.split("<question>")[1].split("</div>")[0]
                dict_results = dict(Counter(combi_list))
                prop_url = str_soup.split("Number: </b><a href=")[1]\
                    .split('">')[0].split('"')[1]

                prop_page = requests.get(prop_url, headers=headers)
                prop_soup = BeautifulSoup(prop_page.content, "html.parser")
                part_soup = str(prop_soup.find_all("table",
                                                   class_="standard01"))
                party = re.search(proposing_pattern, part_soup).group(1)\
                    .split("-")[0]
                dict_results.update({"proposing_party": party,
                                     "congress": congress,
                                     "question": question})
                senate_df = senate_df.append(dict_results, ignore_index=True)
            except:
                True
    except:
        True

senate_df.loc[:, "proposing_party"].value_counts()
senate_df.to_excel(r"{}/senate_data.xlsx".format(data_path))
