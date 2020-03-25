'''
CBB_ATS_Scraper.py

This module retrieves game result data for the provided team (teamID)
during the provided season (season).
'''

import urllib
import urllib.request as request
from bs4 import BeautifulSoup
import requests
import pypyodbc

#Connect to SQLExpress DB
connection = pypyodbc.connect(driver='{SQL Server}',server='localhost\SQLEXPRESS',
                              database='7406Project', trusted_connection='yes')
cursor = connection.cursor()

#Create SQL Command string
#Insert Into statement for the CBB_CoversResults table
#Declare fields and leave VALUES list with ? to pass values later
SQLCommand = ('INSERT INTO dbo.ATS '+
              '(season, teamName, ats) '+
              'VALUES (?,?,?)')

#seasons = ['2015-2016','2016-2017','2017-2018','2018-2019','2019-2020']
seasons = ['2014-2015']

base = 'https://www.covers.com/sport/basketball/ncaab/statistics/team-betting/'

for season in seasons:
    count = 0
    url = base+season
    page = request.urlopen(url)
    soup = BeautifulSoup(page, 'html.parser')
    atsSoup = soup.find('tbody').find_all('tr')

    for row in atsSoup:
        team = row.find('a').find('span').text
        ats = float(row.find_all('td')[3].text)
        values = [season, team, ats]

        cursor.execute(SQLCommand, values)
        connection.commit()
        count += 1
        
    print('Imported '+str(count)+' rows for '+season)

