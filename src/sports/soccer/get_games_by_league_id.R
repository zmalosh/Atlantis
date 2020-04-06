source('requirements.R')
source('src/sports/soccer/get_api_football_json_from_url.R')

get_games_by_league_id <- function(leagueId){
	url <- paste0('https://api-football-v1.p.rapidapi.com/v2/fixtures/league/', leagueId)
	json <- get_api_football_json_from_url(url)
	games <- json$fixtures
	return (games)
}