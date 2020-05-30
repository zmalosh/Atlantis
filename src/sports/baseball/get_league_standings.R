library(tidyverse)
get_league_standings <- function(leagueId){

	# STANDINGS NEED TO BE REVAMPED TO BE EITHER GENERALIZED (NON-SOCCER IN VIEW) OR SPORT-SPECIFIC.
	# ABANDONING FOR NOW
	return(NULL)


	source('src/sports/baseball/get_league_games.R')

	games <- get_league_games(leagueId)
	games$Dummy <- 1
	finalGames <- games %>% filter(!is.na(HomeScore) & !is.na(AwayScore))

	teams <- data.frame(Team = games$HomeTeam %>% unique(), Dummy = 1)
	teamForm <- teams %>%
		inner_join(games, by = 'Dummy') %>%
		transform(GameTimeStamp = GameTime %>%
				  	stringr::str_replace('UTC','') %>%
				  	stringr::str_trim() %>%
				  	stringr::str_replace(' ', 'Z') %>%
				  	lubridate::ymd_hm() %>%
				  	as.integer()) %>%
		filter(!is.na(HomeScore) & (Team == HomeTeam | Team == AwayTeam)) %>%
		group_by(Team) %>%
		mutate(GameNumber = row_number(-GameTimeStamp)) %>%
		filter(GameNumber <= 5) %>%
		transform(GameResult = ifelse(HomeScore == AwayScore,
									  'D',
							   ifelse(Team == HomeTeam,
							   		  ifelse(HomeScore > AwayScore, 'W', 'L'),
							   		  ifelse(HomeScore > AwayScore, 'L', 'W')))) %>%
		ungroup() %>%
		group_by(Team) %>%
		arrange(GameNumber) %>%
		mutate(Form = paste0(GameResult, collapse = '')) %>%
		ungroup() %>%
		filter(GameNumber == 1) %>%
		select(Team, Form)


	homeStandings <- finalGames %>%
		transform(Team = HomeTeam, LogoUrl = HomeTeamLogoUrl) %>%
		group_by(Team, LogoUrl) %>%
		summarise(Games = n(),
				  Wins   = sum(ifelse(HomeScore  > AwayScore, 1, 0)),
				  Losses = sum(ifelse(HomeScore  < AwayScore, 1, 0)),
				  Ties   = sum(ifelse(HomeScore == AwayScore, 1, 0)),
				  RF = sum(HomeScore),
				  RA = sum(AwayScore),
				  RD = sum(HomeScore - AwayScore)) %>%
		ungroup() %>%
		mutate(IsHome = 1)

	awayStandings <- finalGames %>%
		transform(Team = AwayTeam, LogoUrl = AwayTeamLogoUrl) %>%
		group_by(Team, LogoUrl) %>%
		summarise(Games = n(),
				  Wins   = sum(ifelse(HomeScore  < AwayScore, 1, 0)),
				  Losses = sum(ifelse(HomeScore  > AwayScore, 1, 0)),
				  Ties   = sum(ifelse(HomeScore == AwayScore, 1, 0)),
				  RF = sum(AwayScore),
				  RA = sum(HomeScore),
				  RD = sum(AwayScore - HomeScore)) %>%
		ungroup() %>%
		mutate(IsHome = 0)

	standings <- rbind(homeStandings, awayStandings) %>%
		group_by(Team, LogoUrl)

	return(NULL)
}