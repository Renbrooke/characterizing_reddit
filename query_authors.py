import json
import os
from pathlib import Path
import datetime
import calendar
import pandas as pd
from pprint import pprint
from pmaw import PushshiftAPI
api = PushshiftAPI()

samples = pd.read_json('/Users/yunchaewon/gardeners/TheDonald1000.json')

end_date = datetime.date(2021, 1, 6)
start_date = datetime.date(2016, 11, 8)

work_dir = '/Users/yunchaewon/gardeners/'


folder_path = os.path.join(work_dir, 'authors/')

try:
    os.makedirs(folder_path, exist_ok = False)
except:
    print(f'{folder_path} directory already exists')

for author in samples.author.unique():
    path = os.path.join(folder_path, author)
    if os.path.exists(path):
        print(f'{author} was queried before')
    else:
        authored = list(api.search_submissions(author=author,
                                                before=calendar.timegm(end_date.timetuple()),
                                                after=calendar.timegm(start_date.timetuple()),
                                                safe_exit=True))

        submissions = pd.DataFrame([submission for submission in authored])
        if len(submissions):
            submissions['date'] = pd.to_datetime(submissions['created_utc'], utc=True, unit='s')

            #subreddit_submissions.to_csv('python.csv', index = True)
            submissions.to_json(f'{folder_path}/{author}.json', index = True)
