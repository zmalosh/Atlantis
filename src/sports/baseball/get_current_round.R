get_current_round <- function(leagueId){
	source('src/sports/baseball/get_league_games.R')

	games <- get_league_games(leagueId)
	gameDates <- games$Round %>% stringr::str_replace_all('_', '-') %>% ymd() %>% unique()

	currentDate <- date(lubridate::now())

	maxDate <- max(gameDates)
	isAllPast <- maxDate < currentDate
	if(isAllPast){
		return(maxDate)
	}

	minDate <- min(gameDates)
	isAllFuture <- minDate > currentDate
	if(isAllFuture){
		return(minDate)
	}

	nextGame <- (games %>% filter(is.na(HomeScore) & is.na(AwayScore)) %>% arrange(GameTime) %>% slice(1))[1,]
	currentRound <- nextGame$Round
	return(currentRound)
}
