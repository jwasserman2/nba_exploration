# BeautifulSoup Scraper for NCAA and NBA Reference Websites
# Author: Josh Wasserman
import logging

from bs4 import BeautifulSoup, Comment
import pandas as pd
import re
import requests

logger = logging.getLogger()
logger.setLevel('INFO')

# Constant URL's
base_ncaa_url = 'https://www.sports-reference.com'

# Constant regex expressions
PLYR_NAME_REGEX = '(?<=csk=")[\\\'A-Za-z0-9\\s\\-,.\\\\_()é]+(?=")'
STAT_REGEX = '(?<=data-stat=")[A-Za-z0-9_\\-]+(?=")'
ID_REGEX = '(?<=data-append-csv=")[A-Za-z0-9\\-_]+(?=")'


# Functions
def get_soup(url):
    resp = requests.get(url).text
    return BeautifulSoup(resp, parser='html_parser')


def regex_fail_return_zero(pattern, text):
    results = re.findall(pattern, text)

    try:
        return float(results[0])
    except ValueError:
        return results[0]
    except IndexError:
        return 0


def make_stats_df(hidden_tables, type: str):
    div = [i for i in hidden_tables if '"div_' + type + '"' in i]
    soup = BeautifulSoup(str(div), 'lxml').find('tbody').find_all('tr')
    s_dict = {
        re.findall(PLYR_NAME_REGEX, str(y))[1]: {
            re.findall(STAT_REGEX, str(x))[0]:
                regex_fail_return_zero("(?<=>)[0-9,.]+(?=<)", str(x))
            for x in y
        } for y in soup
    }
    for x in soup:
        pid = re.findall(ID_REGEX, str(x))[0]
        name = re.findall(PLYR_NAME_REGEX, str(x))[1]
        s_dict[name].update({'pid': pid})

    df = pd.DataFrame(s_dict).transpose().reset_index().drop(
        columns=['ranker', 'player'])

    return df


# Load schools
logging.info('Grabbing schools from NCAA Reference...')
school_url = 'https://www.sports-reference.com/cbb/schools'
school_soup = get_soup(school_url)
schools = school_soup.find('tbody') \
    .find_all('td', attrs={'data-stat': 'school_name'})
regex = '(?<=href=")[A-Za-z/\-]+(?=")'
schools_links = [re.findall(regex, str(x))[0] for x in schools]
logging.info('Finished pulling schools')

# Get seasons for each school
logging.info('Grabbing seasons for each school from NCAA Reference...')
season_links = []
for num, suffix in enumerate(schools_links):
    school_soup = get_soup(base_ncaa_url + suffix)
    seasons = school_soup.find('tbody') \
        .find_all('td', attrs={'data-stat': 'season'})
    seasons = [x for x in seasons if 'href' in str(x)]
    regex = '(?<=href=")[A-Za-z0-9/.\-]+(?=")'
    season_links.extend([re.findall(regex, str(x))[0] for x in seasons])

    if num % 50 == 0:
        logging.info('Grabbed seasons for school {} out of {}'.format(
            num, len(schools_links) - 1))
logging.info('Finished pulling seasons')

# Make individual-level NCAA stats df
all_ncaa = pd.DataFrame()
all_school_stats = pd.DataFrame()
for num, suffix in enumerate(season_links[10768:len(season_links)]):
    year = suffix.split('/')[len(suffix.split('/')) - 1].replace(
        '.html', '')
    if int(year) < 1995:
        continue

    school = suffix.split('/')[len(suffix.split('/')) - 2]
    link = base_ncaa_url + suffix
    season_soup = get_soup(link)

    # Grab SOS
    team_stats = season_soup.find_all('div', attrs={'id': 'meta'})[0].find_all(
        'p')
    regex = '(?<=SOS</a>:</strong> )[\\d.-]+'
    try:
        sos = [regex_fail_return_zero(regex, str(x)) for x in team_stats if 'SOS'
               in str(x)][0]
    except IndexError:
        logging.warn('No SOS for {} in {}'.format(school, year))
        continue

    # Calculate pace
    team_soup = season_soup.find_all(
        'div', attrs={'id': 'all_schools-totals'})[0].find_all('tr')
    regex = '(?<=data-stat=")[A-Za-z0-9_]+(?=")'
    team_stats = [y for z in [x.find_all('td') for x in team_soup
                  if 'Opponent' in str(x) or 'Team' in str(x)] for y in z]
    stats_dict = {
        re.findall(regex, str(x))[0]: regex_fail_return_zero(
            "(?<=>)[0-9,.]+(?=<)", str(x)) for x in team_stats
    }
    team_poss = (0.5 * (stats_dict['fga'] + 0.475 * stats_dict['fta'] -
                        stats_dict['orb'] + stats_dict['tov']) +
                 0.5 * (stats_dict['opp_fga'] + 0.475 * stats_dict['opp_fta'] -
                        stats_dict['opp_orb'] + stats_dict['opp_tov']))
    team_mp = stats_dict['g'] * 40 * 5 if stats_dict['mp'] == 0 else stats_dict['mp']
    pace = 40 * (team_poss / (0.2 * team_mp))

    stats_dict.update({
        'school': school,
        'season': year,
        'team_poss': team_poss,
        'team_mp': team_mp,
        'pace': pace,
        'sos': sos})
    school_df = pd.DataFrame({k: [v] for k, v in stats_dict.items()})

    all_school_stats = pd.concat([all_school_stats, school_df], axis=0)
    if num % 50 == 0:
        logging.info('Grabbed team stats for {}, number {} out of {}'.format(
            suffix, num, len(season_links)))

    # Get player stats
    comments = season_soup.find_all(string=lambda text:isinstance(text, Comment))
    totals = make_stats_df(comments, 'totals')
    try:
        adv = make_stats_df(comments, 'advanced')
    except AttributeError:
        logging.warn('No advanced stats for {} in {}'.format(school, year))
        continue

    joined = totals.merge(adv)
    joined['index'] = joined['index'].str.replace("\\", "")
    joined['sos'] = sos
    joined['pace'] = pace

    # Get player bio info
    regex = '(?<=data-stat=")[A-Za-z0-9_.\\-]+(?=")'
    player_bios = season_soup.find('tbody').find_all('tr')
    bios_dict = {
        re.findall(PLYR_NAME_REGEX, str(y))[0]:
            {
                re.findall(STAT_REGEX, str(x))[0]:
                    regex_fail_return_zero("(?<=>)[A-Za-z0-9,.\\-]+", str(x))
                    for x in y
            }
        for y in player_bios
    }

    bios = pd.DataFrame(bios_dict).transpose().reset_index()
    try:
        bios = bios.drop(columns=['player', 'summary', 'hometown'])
    except KeyError:
        bios = bios.drop(columns=['player', 'summary'])

    joined = joined.merge(bios)
    joined['season'] = year
    joined['school'] = school

    # Add to full df
    all_ncaa = pd.concat([all_ncaa, joined], axis=0)

    if num % 50 == 0:
        logging.info('Grabbed stats for team/season {}, number {} out of {}'.format(
            suffix, num, len(season_links)))

logging.info('Finished pulling player stats')
all_ncaa.to_csv('~/Desktop/all_ncaa_stats.csv', index=False)
all_school_stats.to_csv('~/Desktop/all_school_stats.csv', index=False)
