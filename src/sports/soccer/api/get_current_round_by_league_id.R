source('src/sports/soccer/api/get_api_football_json_from_url.R')

get_current_round_by_league_id <- function(leagueId){
	require(stringr)
	url <- paste0('https://api-football-v1.p.rapidapi.com/v2/fixtures/rounds/', leagueId, '/current')
	json <- get_api_football_json_from_url(url)
	currentRound <- stringr::str_replace_all(json$fixtures[1], '_', ' ')
	return (currentRound)
}