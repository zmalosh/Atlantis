source('requirements.R')
source('src/sports/soccer/api/get_api_football_json_from_url.R')

get_standings_by_league_id <- function(leagueId){
	url <- paste0('https://api-football-v1.p.rapidapi.com/v2/leagueTable/', leagueId)
	json <- get_api_football_json_from_url(url)
	result <- json$standings
	return (result)
}