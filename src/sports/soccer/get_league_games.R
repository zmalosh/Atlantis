source('requirements.R')
source('src/sports/soccer/api/get_games_by_league_id.R')

get_league_games <- function(leagueId){
	if(is.null(leagueId)){
		return(NULL)
	}

	rawGames <- get_games_by_league_id(leagueId)

	predModel <- SportPredictR::ensemble_model(gameIds =  rawGames$fixture_id,
											   homeTeamIds = rawGames$homeTeam$team_name,
											   awayTeamIds = rawGames$awayTeam$team_name,
											   homeScores = ifelse(rawGames$status == 'Match Finished', rawGames$goalsHomeTeam, NA),
											   awayScores = ifelse(rawGames$status == 'Match Finished', rawGames$goalsAwayTeam, NA),
											   isNeutralSite = F)
	allPreds <- predModel$predictByIds(rawGames$homeTeam$team_name, rawGames$awayTeam$team_name)
	preds <- allPreds$pred

	gameTimes <- ymd_hms(rawGames$event_date) %>% with_tz('America/Detroit')

	pctDecimalPlaces <- 3
	leagueGames <- data.frame(
		GameId = rawGames$fixture_id,
		Round = rawGames$round,
		GameTime = format(gameTimes, '%Y-%m-%d %H:%M %Z'),
		HomeTeam = rawGames$homeTeam$team_name,
		AwayTeam = rawGames$awayTeam$team_name,
		HomePct = format(round(preds$HomeWinPct, pctDecimalPlaces), nsmall = pctDecimalPlaces),
		DrawPct = format(round(preds$DrawWinPct, pctDecimalPlaces), nsmall = pctDecimalPlaces),
		AwayPct = format(round(preds$AwayWinPct, pctDecimalPlaces), nsmall = pctDecimalPlaces),
		HomeScore = ifelse(rawGames$status == 'Match Finished', rawGames$goalsHomeTeam, NA),
		AwayScore = ifelse(rawGames$status == 'Match Finished', rawGames$goalsAwayTeam, NA),
		HomeTeamLogoUrl = rawGames$homeTeam$logo,
		AwayTeamLogoUrl = rawGames$awayTeam$logo,
		stringsAsFactors = FALSE
	)

	return (leagueGames)
}