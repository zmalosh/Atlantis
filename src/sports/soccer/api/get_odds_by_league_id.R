source('requirements.R')
source('src/sports/soccer/api/get_api_football_json_from_url.R')

get_odds_by_league_id <- function(leagueId){
	url <- paste0('https://api-football-v1.p.rapidapi.com/v2/odds/league/', leagueId)
	json <- get_api_football_json_from_url(url)
	result <- as.data.frame(json$odds)
	return (result)
}