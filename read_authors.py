import json
import os
from pathlib import Path
import datetime
import calendar
import pandas as pd
import seaborn
# import scipy
from pprint import pprint
from pmaw import PushshiftAPI
import matplotlib.pyplot as plt

api = PushshiftAPI()

# assign directory
author_directory = '/Users/yunchaewon/gardeners/authors'

files = Path(author_directory).glob('*')
sr_dict = dict()
subreddits = pd.DataFrame()#(columns='author')

for file in files:
    author = str(file).split('/')[-1][:-5]
    #print(author)
    df = pd.read_json(file)
    # subreddits = pd.Series(df.subreddit.unique())
    sr_dict[author] = list(df.subreddit.unique())
#     subreddits = set(list(subreddits) + sr_dict[author])
#     print(subreddits)
# new = pd.DataFrame(columns=sr_dict.keys(), index=list(subreddits))

    new = pd.DataFrame({'author': author,
                       'srd': sr_dict[author]})
    subreddits = pd.concat([subreddits, new], axis=0, ignore_index=True)
print(subreddits)
print(subreddits.groupby('srd'))#['The_Donald'])
#subreddits.srd.value_counts().plot(kind='bar')
subreddits.srd.value_counts().plot(kind='line')

plt.show()
