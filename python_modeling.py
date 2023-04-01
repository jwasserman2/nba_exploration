import os
import re

from joblib.parallel import delayed, Parallel, parallel_backend
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.cluster import KMeans

def _read_csv(args, discard_cols=[]):
    path = os.path.join(*args)
    return pd.read_csv(path)

def name_fix(name):
    return (name
            .lower()
            .replace(' ', '-')
            .replace("'", '')
            .replace('.', '')
            .replace('á', 'a')
            .replace('è', 'e'))


ncaa_player = _read_csv(['~', 'Desktop', 'all_ncaa_stats.csv'])
ncaa_team = _read_csv(['~', 'Desktop', 'all_school_stats.csv'])
nba_player = _read_csv(['~', 'Desktop', 'player_stats.csv'])

ncaa_player_remove_cols = ["bpm-dum", "high_school", "number", "ws-dum", "sos"]
nba_player_remove_cols = ["toss_col", "ws-dum", "bpm-dum"]
ncaa_player = ncaa_player[[c for c in ncaa_player if c not in ncaa_player_remove_cols]]
nba_player = nba_player[[c for c in nba_player if c not in nba_player_remove_cols]]

# Convert height to inches
ncaa_player['height'] = ncaa_player['height'].apply(
    lambda x: None if x == '0' else int(x.split('-')[0]) * 12 + int(x.split('-')[1]))
ncaa_player.columns
# Convert RSCI to factor by clustering WS and MP
plot_dvs = ['ws', 'mp']
plot_df = ncaa_player.groupby(['index', 'rsci'])[plot_dvs].agg('mean').groupby(
    ['rsci'])[plot_dvs].agg('mean').reset_index()
plot_df = plot_df[plot_dvs].apply(
    lambda x: (x - np.mean(x)) / np.std(x)).reset_index()

plt.scatter(x='index', y='ws', c="#191970", data=plot_df)
plt.scatter(x='index', y='mp', c="#DAA520", data=plot_df)
hs_rank_map = {'0': 'unranked'}
hs_rank_map.update(dict(zip([str(i) for i in range(1, 5)], ['1-4'] * 4)),
                   **dict(zip([str(i) for i in range(5, 11)], ['5-10'] * 6)),
                   **dict(zip([str(i) for i in range(11, 31)], ['11-30'] * 20)),
                   **dict(zip([str(i) for i in range(31, 51)], ['31-50'] * 20)),
                   **dict(zip([str(i) for i in range(51, 101)], ['51-100'] * 50)))

ncaa_player = ncaa_player[ncaa_player['rsci'].notnull()]
ncaa_player['rsci'] = ncaa_player['rsci'].astype(int).astype(str).map(hs_rank_map)

# Join NBA and NCAA datasets
pid_map = {
  "dewan-hernandez": "dewan-huell",
  "garrison-mathews": "garrison-matthews",
  "ja-morant": "temetrius-morant",
  "zach-norvell": "zach-norvelljr",
  "kz-okpala": "kezie-okpala",
  "marvin-bagley": "marvin-bagleyiii",
  "shake-milton": "malik-milton",
  "ray-spalding": "raymond-spalding",
  "bam-adebayo": "edrice-adebayo",
  "naz-mitrou-long": "nazareth-mitrou-long",
  "andrew-white": "andrew-whiteiii",
  "kay-felder": "kahlil-felder",
  "yogi-ferrell": "kevin-ferrell",
  "fred-vanvleet": "fred-van-vleet",
  "stephen-zimmerman": "stephen-zimmermanjr",
  "bryce-dejean-jones": "bryce-jones",
  "devyn-marble": "roy-devyn-marble",
  "ish-smith": "ishmael-smith",
  "jeff-ayres": "jeff-pendergraph",
  "henry-walker": "bill-walker",
  "lou-amundson": "louis-amundson",
  "jeff-sheppard": "jeffrey-sheppard",
  "michael-porter": "michael-porterjr",
  "john-lucas-iii": "john-lucas-2",
}

ncaa_player['nba_pid'] = [
    pid if pid in ["tony-mitchell-3", "tony-mitchell-4", "john-lucas-2"]
    else re.sub('-[\\d]+', '', re.sub('--[\\d]+', '', pid))
    for pid in ncaa_player['pid']]

nba_player['nba_pid'] = ['-'.join(name) for name in
    zip([name_fix(n[1]) for n in nba_player['player'].str.split(',')],
        [name_fix(n[0]) for n in nba_player['player'].str.split(',')])]
nba_player.loc[
    (nba_player['player'] == "Mitchell,Tony") & (nba_player['team_id'] == "MIL"),
    'nba_pid'] = 'tony-mitchell-3'
nba_player.loc[
    (nba_player['player'] == "Mitchell,Tony") & (nba_player['team_id'] == "DET"),
    'nba_pid'] = 'tony-mitchell-4'
nba_player.loc[nba_player['nba_pid'].isin(pid_map), 'nba_pid'] = nba_player.loc[
    nba_player['nba_pid'].isin(pid_map), 'nba_pid'].map(pid_map)

inclusion_df = ncaa_player.merge(
    nba_player, how='left', on='nba_pid', suffixes=['_ncaa', '_nba'])

inclusion_df['dv'] = 0
inclusion_df.loc[inclusion_df['pts_nba'].notnull(), 'dv'] = 1
inclusion_df.loc[inclusion_df['dv'] == 1, ['nba_pid', 'season', 'team_id']]
