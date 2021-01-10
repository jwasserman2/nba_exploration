import requests
import bs4
import re
import pandas as pd
import numpy as np

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

# Get missing minutes (CSV from R script)
missing_mp = pd.read_csv('~/Desktop/missing_mp_players.csv')
ids = missing_mp.player_id.tolist()
new_mp_df = pd.DataFrame()
for num, id in enumerate(ids):
    print(f'''
    Getting stats for {id}...this is player {num}.
    The new missing minutes df has {new_mp_df.shape[0]} rows.''')
    nba_url = f'https://www.basketball-reference.com/players/{id[0]}/{id}.html'
    text = requests.get(nba_url).text
    title = bs4.BeautifulSoup(text, 'html.parser').find('title')
    name = re.findall("(?<=<title>)[\\w\\-\\s\\.']+(?= Stats)", str(title))[0] \
        .replace(' ', '-').replace(".", '').replace("'", "").lower()
    ncaa_url = f'https://www.sports-reference.com/cbb/players/{name}-1.html'
    ncaa_text = requests.get(ncaa_url).text
    ncaa_title = bs4.BeautifulSoup(ncaa_text, 'html.parser').find('title')
    if len(re.findall('404', str(ncaa_title))) > 0:
        print(f'{name} resulted in a 404. Moving on...')
    else:
        rows = bs4.BeautifulSoup(ncaa_text, 'html.parser') \
            .find('table') \
            .find_all('tr')
        keep_rows = [i for i in rows if re.findall('^<tr(?!>)', str(i))]
        mps = [re.findall('(?<=data-stat="mp_per_g">)[0-9\\.]+', str(i))[0] if
               len(re.findall('(?<=data-stat="mp_per_g">)[0-9\\.]+', str(i))) > 0
               else np.nan for i in keep_rows]
        bad_seasons_read = [re.findall('(?<=.html">)[\\d\\-]+(?=</a)', str(i)) for i in keep_rows]
        seasons = [i[0] for i in bad_seasons_read if len(i) > 0]
        df = pd.DataFrame({
            'player_id': list(np.repeat(id, len(mps))),
            'season': seasons,
            'mp': mps
            })
        new_mp_df = pd.concat([new_mp_df, df], axis=0)

new_mp_df.to_csv('~/Desktop/scraped_missing_mps.csv')
