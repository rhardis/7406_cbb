import requests
import urllib.parse
from bs4 import BeautifulSoup
from urllib.error import URLError, HTTPError
import pypyodbc
from time import sleep

###SQL Setup
connection1 = pypyodbc.connect(driver='{SQL Server}',server='localhost\SQLEXPRESS',
                              database='7406Project', trusted_connection='yes')

cursorSelect = connection1.cursor()
SQLSelectCommand = ('SELECT DISTINCT a.TEAM '+
                    'FROM dbo.cbbteams a LEFT JOIN dbo.coaches_clean b '+
                    'ON a.TEAM = b.team WHERE b.team IS NULL')
cursorSelect.execute(SQLSelectCommand)

teamtups = list(cursorSelect.fetchall())
teams = [tup[0] for tup in teamtups]

connection1.close()

connection2 = pypyodbc.connect(driver='{SQL Server}',server='localhost\SQLEXPRESS',
                              database='7406Project', trusted_connection='yes')

cursorInsert = connection2.cursor()
SQLInsertCommand = ('INSERT INTO dbo.coaches (team,season,coach,W,L) '+
                    'VALUES (?,?,?,?,?)')

###Soup Setup
sessID = '65c91me5dfqkvqd613pk72saq4'

pwd=open("pwd.txt","r").readlines()[0]
data = {
'username':'sackfieldf16@gmail.com',
'password':pwd}

headers = {
'accept':'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3',
'accept-encoding': 'gzip, deflate, br',
'accept-language': 'en-US,en;q=0.9',
'cookie': '_ga=GA1.2.1833630083.1574351500; _gid=GA1.2.895723277.1585085099; _gat_gtag_UA_11558853_1=1; PHPSESSID='+sessID,
'referer': 'https://kenpom.com/handlers/login_handler.php',
'upgrade-insecure-requests': '1',
'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.131 Safari/537.36'}

login_url = 'https://kenpom.com/index.php'
login_data = urllib.parse.urlencode(data).encode("utf-8")
base_url = 'https://kenpom.com/history.php?t='

def create_soup(url):
    with requests.Session() as s:
        s.headers.update(headers)
        try:
            s.post(login_url,login_data)
            resp = s.get(url)
        except HTTPError as e:
            print('ERROR CODE: ',e.code)
            return None
        except URLError as e:
            print('ERROR REASON: ',e.reason)
            return None
    return BeautifulSoup(resp.text,'html.parser')


###Scrape data and import into DB
for team in teams:
    sleep(2)
    if '&' in team:
        teamstr = team.replace('&','%26')
    elif team == 'North Carolina St.':
        teamstr = 'N.C. State'
    else:
        teamstr = team
    soup = create_soup(base_url+teamstr)
    rows = soup.find('tbody').find_all('tr')
    for row in rows:
        season = row.find_all('td')[0].text.strip()
        coach = row.find_all('td')[2].text.strip()
        W = row.find_all('td')[4].contents[0].strip().split('-')[0]
        L = row.find_all('td')[4].contents[0].strip().split('-')[1]
        values = [team, season, coach, W, L]
        cursorInsert.execute(SQLInsertCommand, values)
        connection2.commit()

    print('Imported '+str(len(rows))+' years for '+team)

connection2.close()




