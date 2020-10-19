import requests
import bs4
import re
import pandas as pd

all_players = pd.DataFrame()
for year in range(1980, 2021):
    print(f'Pulling data from {year}...')
    page_url = f'https://www.basketball-reference.com/leagues/NBA_{year}_per_game.html'
    text = requests.get(page_url).text
    table = bs4.BeautifulSoup(text, 'html.parser').find('tbody')
    rows = table.find_all('tr', attrs={'class': 'full_table'})
    player_stats = [i.find_all('td') for i in rows]

    for k, p in enumerate(player_stats):
        # Get the column names for the table
        cols = ['year'] + [re.findall('(?<=data-stat=")[\\d\\w\\.]+', str(i))[0]
                for i in player_stats[k]]
        # Get the stat values for each column
        vals = [year] + \
            re.findall('(?<=append-csv=")[-\\s\\d\\w\\.]+',
                       str(player_stats[k][0])) + \
            [['0.0'][0] if re.findall('(?<=>)[-\\d\\w\\.]+(?=<)', str(i)) == []
             else re.findall('(?<=>)[-\\d\\w\\.]+(?=<)', str(i))[0]
             for i in player_stats[k][1:len(player_stats[k])]]
        # Make it into a dataframe
        df_row = pd.DataFrame(vals, index=cols).transpose()
        all_players = pd.concat([all_players, df_row])

all_players.to_csv('~/Desktop/player_stats.csv')

# College stats
player_ids = []
for year in range(1980, 2021):
    print(f'Pulling data from {year}...')
    page_url = f'https://www.basketball-reference.com/leagues/NBA_{year}_per_game.html'
    text = requests.get(page_url).text
    table = bs4.BeautifulSoup(text, 'html.parser').find('tbody')
    rows = table.find_all('tr', attrs={'class': 'full_table'})
    new_ids = [
     re.findall('(?<=data-append-csv=")[\\d\\w]+(?=")', str(i))[0] for
     i in rows]
    player_ids.extend(new_ids)
player_ids = list(set(player_ids))
player_ids.sort()
base_url = 'https://www.basketball-reference.com/'
college_stats = pd.DataFrame()
for num, id in enumerate(player_ids):
    if num % 1000 == 0:
        print(f'Getting stats for {id}...this is player {num}')
    url = f'https://www.basketball-reference.com/players/{id[0]}/{id}.html'
    text = requests.get(url).text
    comments = bs4.BeautifulSoup(text, 'html.parser') \
        .find_all(string=lambda text:isinstance(text, bs4.Comment))
    college_div = [i for i in comments if 'college' in i]
    if len(college_div) == 0:
        print(f'No college stats available for {id}')
    else:
        formatted = bs4.BeautifulSoup(str(college_div), 'lxml')
        table = formatted.find('tbody')
        rows = table.find_all('tr')
        for k, p in enumerate(rows):
            # Get the column names for the table
            cols = ['player_id'] + [re.findall('(?<=data-stat=")[\\d\\w\\.]+', str(p))][0]
            # Get the stat values for each column
            vals = [str(id)] + re.findall(
                '(?<=>)(?<!/a>)[-\\d\\w\\.]*?(?=</)(?!</tr)', str(p))
            df_row = pd.DataFrame(vals, index=cols).transpose()
            college_stats = pd.concat([college_stats, df_row])

college_stats.to_csv('~/Desktop/college_stats.csv')
