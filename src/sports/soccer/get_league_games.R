source('src/sports/soccer/get_games_by_league_id.R')

get_league_games <- function(leagueId){
	if(is.null(leagueId)){
		return(NULL)
	}

	rawGames <- get_games_by_league_id(leagueId)
	gameTimes <- ymd_hms(rawGames$event_date) %>% with_tz('America/Detroit')

	leagueGames <- data.frame(
		GameId = rawGames$fixture_id,
		Round = rawGames$round,
		GameTime = format(gameTimes, '%Y-%m-%d %H:%M %Z'),
		HomeTeam = rawGames$homeTeam$team_name,
		AwayTeam = rawGames$awayTeam$team_name,
		HomeScore = rawGames$goalsHomeTeam,
		AwayScore = rawGames$goalsAwayTeam,
		stringsAsFactors = FALSE
	)

	return (leagueGames)
}