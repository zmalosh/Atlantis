source('requirements.R')
source('src/sports/soccer/api/get_api_football_json_from_url.R')

get_all_bookmakers <- function(){
	url <- 'https://api-football-v1.p.rapidapi.com/v2/odds/bookmakers'
	json <- get_api_football_json_from_url(url)
	result <- json$bookmakers
	return (result)
}
