SELECT * FROM dbo.ATS
ORDER BY season, teamName

SELECT * FROM dbo.cbbteams

SELECT a.*, b.* FROM (
SELECT DISTINCT teamName FROM dbo.ATS
) a
FULL OUTER JOIN  (
SELECT DISTINCT TEAM FROM dbo.cbbteams
) b ON a.teamName = b.TEAM
WHERE a.teamName IS NULL OR b.TEAM IS NULL

SELECT season, COUNT(*) FROM dbo.ATS
GROUP BY season

SELECT [YEAR], COUNT(*) FROM dbo.cbbteams
GROUP BY [YEAR]

--Full dataset with ATS
SELECT a.*, b.ats
FROM dbo.cbbteams a
LEFT JOIN dbo.ATS b
ON a.TEAM = b.teamName AND a.[YEAR] = RIGHT(b.season,4)
ORDER BY b.ats, a.[YEAR]


UPDATE dbo.cbbteams SET TEAM = 'Little Rock' WHERE TEAM = 'Arkansas Little Rock'
UPDATE dbo.cbbteams SET TEAM = 'IPFW' WHERE TEAM = 'Fort Wayne'

/*
SELECT TeamName, TeamID, SUM(a.Cover) as totalcovers, COUNT(*) as totalgames,
CONVERT(DECIMAL(10,2),100*(CAST(SUM(a.Cover) as float)/CAST(COUNT(*) as float))) AS ats
INTO [7406Project].[dbo].[2015ATS]
FROM (
SELECT k.GameID, k.W_TeamName as TeamName, k.W_TeamID as TeamID,
CASE WHEN (k.W_TeamID = k.P_TeamID AND k.[P_Cover?] = 1) OR (k.W_TeamID <> k.P_TeamID) THEN 1
	 ELSE 0 END AS Cover
FROM dbo.CBB_KPGames k
JOIN dbo.CBB_Map_Game m ON k.GameID = m.KP_GameID
JOIN dbo.CBB_GameResults_Box g ON m.CBB_GameID = g.GameID
WHERE k.Season = '2015' AND g.GameType = 'Regular'
UNION
SELECT k.GameID, k.L_TeamName as TeamName, k.L_TeamID as TeamID,
CASE WHEN (k.L_TeamID <> k.P_TeamID AND k.[P_Cover?] = 0) THEN 1
	 ELSE 0 END AS Cover
FROM dbo.CBB_KPGames k
JOIN dbo.CBB_Map_Game m ON k.GameID = m.KP_GameID
JOIN dbo.CBB_GameResults_Box g ON m.CBB_GameID = g.GameID
WHERE k.Season = '2015' AND g.GameType = 'Regular') a
--WHERE TeamName = 'Florida St.'
GROUP BY TeamName, TeamID
ORDER BY ats
*/

DELETE FROM dbo.ATS WHERE season = '2014-2015'

INSERT INTO dbo.ATS (season, teamName, ats)
SELECT '2014-2015', TeamName, ats
FROM dbo.ATS2015



CREATE TABLE dbo.coaches
(team nvarchar(255),
 season nvarchar(50),
 coach nvarchar(255),
 W int,
 L int)


SELECT * FROM dbo.coaches
WHERE team = 'Louisville'


SELECT * FROM dbo.coaches 
WHERE coach LIKE '% [A-Z][0-9]'
OR coach LIKE '% [A-Z][0-9][0-9]'
OR coach LIKE '% CH'
OR coach LIKE '%2nd%'
ORDER BY team

SELECT * FROM dbo.coaches_clean WHERE Result <> '0'

UPDATE dbo.coaches_clean
SET TW = CASE WHEN Result IN ('R1','R0','0') THEN 0
			  WHEN Result = 'R2' THEN 1
			  WHEN Result = 'S16' THEN 2
			  WHEN Result = 'E8' THEN 3
			  WHEN Result = 'F4' THEN 4
			  WHEN Result = '2nd' THEN 5
			  ELSE 6 END
 
SELECT * FROM dbo.coaches_clean
ORDER BY TW DESC, season DESC


SELECT coach, SUM(CAST(TW as int)) as totalTW
FROM dbo.coaches_clean
GROUP BY coach
ORDER BY totalTW DESC

SELECT a.coach, a.season, a.TW, SUM(b.TW) as prevTW
FROM dbo.coaches_clean a
JOIN dbo.coaches_clean b ON a.coach = b.coach
WHERE a.season > b.season
GROUP BY a.coach, a.season, a.TW
ORDER BY a.coach, a.season

/*
CREATE VIEW dbo.VW_coach_TW AS
SELECT a.coach, a.season, a.team, a.TW, SUM(b.W) as prevW, SUM(b.TW) as prevTW
FROM dbo.coaches_clean a
JOIN dbo.coaches_clean b ON a.coach = b.coach
WHERE a.season > b.season
GROUP BY a.coach, a.season, a.team, a.TW
ORDER BY a.coach, a.season
*/

SELECT * FROM dbo.VW_coach_TW 
WHERE team = 'Texas A&M'

--Full dataset with tourney wins
SELECT a.*, 
CASE WHEN b.prevTW IS NOT NULL THEN b.prevTW ELSE 0 END as prev_TW,
CASE WHEN b.prevW IS NOT NULL THEN b.prevW ELSE 0 END as prev_W
FROM dbo.cbbteams a
LEFT JOIN dbo.VW_coach_TW b ON a.TEAM = b.team AND a.[YEAR] = b.season
ORDER BY b.prevTW

--Full dataset with ATS + tourney wins
SELECT a.*, b.ats, 
CASE WHEN c.prevTW IS NOT NULL THEN c.prevTW ELSE 0 END as prev_TW,
CASE WHEN c.prevW IS NOT NULL THEN c.prevW ELSE 0 END as prev_W
FROM dbo.cbbteams a
LEFT JOIN dbo.ATS b ON a.TEAM = b.teamName AND a.[YEAR] = RIGHT(b.season,4)
LEFT JOIN dbo.VW_coach_TW c ON a.TEAM = c.team AND a.[YEAR] = c.season
ORDER BY b.ats, a.[YEAR]




--Teams not in coaches_clean
SELECT DISTINCT a.TEAM
FROM dbo.cbbteams a LEFT JOIN dbo.coaches_clean b
ON a.TEAM = b.team WHERE b.team IS NULL


