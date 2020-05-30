get_league_games <- function(leagueId){
	if(is.null(leagueId)){
		return(NULL)
	}

	source('src/sports/baseball/api/get_games_by_league_id.R')

	rawGames <- get_games_by_league_id(leagueId)

	games <- rawGames %>%
		mutate(GameTimeAsDate = ymd_hms(date),
			   GameTime = format(GameTimeAsDate, '%Y-%m-%d %H:%M %Z'),
			   IsInPast = GameTimeAsDate < lubridate::now(),
			   IsFinal = status$long == 'Finished',
			   IsLive = stringr::str_starts(status$long, 'Inning'),
			   IsNotStarted = status$long == 'Not Started',
			   IsValid = (IsNotStarted & !IsInPast) | IsLive | (IsFinal & IsInPast),
			   HomeScore = ifelse(IsFinal, scores$home$total, NA),
			   AwayScore = ifelse(IsFinal, scores$away$total, NA),
			   GameId = id,
			   Round = stringr::str_sub(date, 1, 10) %>% stringr::str_replace_all(pattern = '-', replacement = '_'),
			   HomeTeam = teams$home$name,
			   AwayTeam = teams$away$name,
			   HomeTeamLogoUrl = teams$home$logo,
			   AwayTeamLogoUrl = teams$away$logo) %>%
		filter(IsValid) %>%
		select(GameId, Round, GameTime, HomeTeam, AwayTeam, HomeScore, AwayScore, HomeTeamLogoUrl, AwayTeamLogoUrl)

	return (games)
}