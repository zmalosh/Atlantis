source('requirements.R')
source('src/sports/soccer/api/get_game_details_by_fixture_id.R')

get_game_details <- function(gameId){
	rawGameDetails <- get_game_details_by_fixture_id(gameId)

	result <- data.frame(
		GameId = rawGameDetails$fixture_id,
		LeagueName = rawGameDetails$league$name,
		CountryName = rawGameDetails$league$country,
		HomeTeam = rawGameDetails$homeTeam$team_name,
		AwayTeam = rawGameDetails$awayTeam$team_name,
		GameTime =  ymd_hms(rawGameDetails$event_date) %>% with_tz('America/Detroit'),
		GameStatus = rawGameDetails$status,
		HomeScore = rawGameDetails$goalsHomeTeam,
		AwayScore = rawGameDetails$goalsAwayTeam,
		LeagueLogoUrl = rawGameDetails$league$logo,
		CountryFlag = rawGameDetails$league$flag,
		HomeTeamLogoUrl = rawGameDetails$homeTeam$team_name,
		AwayTeamLogoUrl = rawGameDetails$awayTeam$logo,
		stringsAsFactors = FALSE
	)

	return(result)
}
