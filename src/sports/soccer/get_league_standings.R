source('src/sports/soccer/api/get_standings_by_league_id.R')

convert_from_raw_standings <- function(rawStandings) {
	df <- data.frame(
		Rank = rawStandings$rank,
		TeamId = rawStandings$team_id,
		TeamName = rawStandings$teamName,
		TeamLogoUrl = rawStandings$logo,
		Group = rawStandings$group,
		Form = rawStandings$forme,
		Change = rawStandings$status,
		CurrentResult = rawStandings$description,
		Played = rawStandings$all$matchsPlayed,
		Wins = rawStandings$all$win,
		Draws = rawStandings$all$draw,
		Losses = rawStandings$all$lose,
		GoalsFor = rawStandings$all$goalsFor,
		GoalsAgainst = rawStandings$all$goalsAgainst,
		HomePlayed = rawStandings$home$matchsPlayed,
		HomeWins = rawStandings$home$win,
		HomeDraws = rawStandings$home$draw,
		HomeLosses = rawStandings$home$lose,
		HomeGoalsFor = rawStandings$home$goalsFor,
		HomeGoalsAgainst = rawStandings$home$goalsAgainst,
		AwayPlayed = rawStandings$away$matchsPlayed,
		AwayWins = rawStandings$away$win,
		AwayDraws = rawStandings$away$draw,
		AwayLosses = rawStandings$away$lose,
		AwayGoalsFor = rawStandings$away$goalsFor,
		AwayGoalsAgainst = rawStandings$away$goalsAgainst,
		GoalDifference = rawStandings$goalsDiff,
		Points = rawStandings$points,
		LastUpdate = rawStandings$lastUpdate
	)
	return(df)
}

get_league_standings <- function(leagueId){
	allRawStandings <- get_standings_by_league_id(leagueId)
	standingsGroupCount <- length(allRawStandings)

	standings <- convert_from_raw_standings(allRawStandings[1][[1]])
	if(standingsGroupCount > 1) {
		for(i in 2:standingsGroupCount){
			convertedStandings <- convert_from_raw_standings(allRawStandings[i][[1]])
			standings <- rbind(standings, convertedStandings)
		}
		convertedStandings <- NULL
	}
}
