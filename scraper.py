import requests
from bs4 import BeautifulSoup
import re
import pandas as pd


# Functions
def get_soup(url):
    resp = requests.get(url).text
    return BeautifulSoup(resp, parser='html_parser')


all_players = pd.DataFrame()
for year in range(1980, 2021):
    print(f'Pulling data from {year}...')
    page_url = f'https://www.basketball-reference.com/leagues/NBA_{year}_totals.html'
    soup = get_soup(page_url)
    table = soup.find('tbody')
    rows = table.find_all('tr', attrs={'class': 'full_table'})
    player_stats = [i.find_all('td') for i in rows]

    # Get totals
    print('Getting stat totals')
    totals = pd.DataFrame()
    for k, p in enumerate(player_stats):
        # Get the column names for the table
        cols = ['year'] + [re.findall('(?<=data-stat=")[\\d\\w\\.]+', str(i))[0]
                for i in player_stats[k]]
        # Get the stat values for each column
        vals = [year] + \
            re.findall('(?<=csk=")[\\-\\s\\d\\w\\.,\']+',
                       str(player_stats[k][0])) + \
            [['0.0'][0] if re.findall('(?<=>)[-\\d\\w\\.]+(?=<)', str(i)) == []
             else re.findall('(?<=>)[-\\d\\w\\.]+(?=<)', str(i))[0]
             for i in player_stats[k][1:len(player_stats[k])]]
        # Make it into a dataframe
        df_row = pd.DataFrame(vals, index=cols).transpose()
        totals = pd.concat([totals, df_row])

    # Get per 100 poss
    print('Getting stats per 100 possessions')
    page_url = f'https://www.basketball-reference.com/leagues/NBA_{year}_per_poss.html'
    soup = get_soup(page_url)
    table = soup.find('tbody')
    rows = table.find_all('tr', attrs={'class': 'full_table'})
    player_stats = [i.find_all('td') for i in rows]

    per_poss = pd.DataFrame()
    for k, p in enumerate(player_stats):
        # Get the column names for the table
        cols = ['year'] + [re.findall('(?<=data-stat=")[\\d\\w\\._\\-]+', str(i))[0]
                           if len(re.findall('data-stat=""', str(i))) == 0
                           else 'toss_col' for i in player_stats[k]]
        # Get the stat values for each column
        vals = [year] + \
            re.findall('(?<=csk=")[\\-\\s\\d\\w\\.,\']+',
                       str(player_stats[k][0])) + \
            [['0.0'][0] if re.findall('(?<=>)[-\\d\\w\\.]+(?=<)', str(i)) == []
             else re.findall('(?<=>)[-\\d\\w\\.]+(?=<)', str(i))[0]
             for i in player_stats[k][1:len(player_stats[k])]]
        # Make it into a dataframe
        df_row = pd.DataFrame(vals, index=cols).transpose()
        per_poss = pd.concat([per_poss, df_row])

    # Get advanced
    print('Getting advanced stats')
    page_url = f'https://www.basketball-reference.com/leagues/NBA_{year}_advanced.html'
    soup = get_soup(page_url)
    table = soup.find('tbody')
    rows = table.find_all('tr', attrs={'class': 'full_table'})
    player_stats = [i.find_all('td') for i in rows]

    advanced = pd.DataFrame()
    for k, p in enumerate(player_stats):
        # Get the column names for the table
        cols = ['year'] + [re.findall('(?<=data-stat=")[\\d\\w\\._\\-]+', str(i))[0]
                for i in player_stats[k]]
        # Get the stat values for each column
        vals = [year] + \
            re.findall('(?<=csk=")[\\-\\s\\d\\w\\.,\']+',
                       str(player_stats[k][0])) + \
            [['0.0'][0] if re.findall('(?<=>)[-\\d\\w\\.]+(?=<)', str(i)) == []
             else re.findall('(?<=>)[-\\d\\w\\.]+(?=<)', str(i))[0]
             for i in player_stats[k][1:len(player_stats[k])]]
        # Make it into a dataframe
        df_row = pd.DataFrame(vals, index=cols).transpose()
        advanced = pd.concat([advanced, df_row])

    full = totals.merge(per_poss).merge(advanced)
    all_players = pd.concat([all_players, full])

all_players.to_csv('./data/player_stats.csv', index=False)
