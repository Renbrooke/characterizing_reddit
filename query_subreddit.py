from pathlib import Path
import os
import time
from dateutil.relativedelta import *
import calendar
import datetime

import seaborn as sns
import pandas as pd
from pmaw import PushshiftAPI

# sns.set_theme(style="whitegrid")
# pd.set_option('max_colwidth', 500)
# pd.set_option('max_columns', 50)

api = PushshiftAPI()

def get_daily_response(since_date, until_date):
    print(f'Submissions from :{since_date}')
    print(f'Submissions until :{until_date}')

    api_request_generator = list(api.search_submissions(subreddit='The_Donald',
                                                        before=calendar.timegm(until_date.timetuple()),
                                                        after=calendar.timegm(since_date.timetuple()),
                                                        safe_exit=True))
    print(api.metadata_.get('shards'))
    if len(api_request_generator):
        submissions = pd.DataFrame([submission for submission in api_request_generator])
        submissions['date'] = pd.to_datetime(submissions['created_utc'], utc=True, unit='s')

        #subreddit_submissions.to_csv('python.csv', index = True)
        submissions.to_json(f'submissions/{since_date +"_"+ until_date}.json', index = True)
        print(f'Finished: submissions/{since_date +"_"+ until_date}.json')
    else:
        return


if __name__ == '__main__':
    work_dir = '/Users/yunchaewon/gardeners/'

    try:
        sub_path = os.path.join(work_dir, 'submissions/')
        os.makedirs(sub_path, exist_ok=False)
        print(f'Created submissions/ directory!')
    except:
        print('submission folder already exists')

    end_date = datetime.date(2021, 1, 6)  # November 8, 2016 = donald trump elected
    start_date = datetime.date(2016, 11, 8)  # November 8, 2016 = donald trump elected
    next_date = start_date
    while next_date <= end_date:
        next_date = start_date + relativedelta(days=+ 1)
        start = time.process_time()
        get_daily_response(start_date, next_date)
        print(time.process_time() - start)
        start_date = next_date
