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

	tempGames <- rawGames %>% inner_join(preds, by = c('fixture_id' = 'GameId')) %>% filter(rawGames$status != 'Match Postponed')
	gameTimes <- ymd_hms(tempGames$event_date) %>% with_tz('America/Detroit')

	pctDecimalPlaces <- 3
	leagueGames <- data.frame(
		GameId = tempGames$fixture_id,
		Round = tempGames$round,
		GameTime = format(gameTimes, '%Y-%m-%d %H:%M %Z'),
		HomeTeam = tempGames$homeTeam$team_name,
		AwayTeam = tempGames$awayTeam$team_name,
		HomePct = format(round(tempGames$HomeWinPct, pctDecimalPlaces), nsmall = pctDecimalPlaces),
		DrawPct = format(round(tempGames$DrawWinPct, pctDecimalPlaces), nsmall = pctDecimalPlaces),
		AwayPct = format(round(tempGames$AwayWinPct, pctDecimalPlaces), nsmall = pctDecimalPlaces),
		HomeScore = ifelse(tempGames$status == 'Match Finished', tempGames$goalsHomeTeam, NA),
		AwayScore = ifelse(tempGames$status == 'Match Finished', tempGames$goalsAwayTeam, NA),
		HomeTeamLogoUrl = tempGames$homeTeam$logo,
		AwayTeamLogoUrl = tempGames$awayTeam$logo,
		stringsAsFactors = FALSE
	)

	return (leagueGames)
}