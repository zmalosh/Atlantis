get_all_competitions <- function(){
	source('src/sports/soccer/api/get_api_football_json_from_url.R')
	url <- paste0('https://api-football-v1.p.rapidapi.com/v2/leagues/')
	json <- get_api_football_json_from_url(url)
	leagues <- json$leagues
	return (json$leagues)
}