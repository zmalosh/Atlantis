get_all_competitions <- function(){
	source('src/sports/baseball/api/get_api_baseball_json_from_url.R')
	url <- paste0('https://api-baseball.p.rapidapi.com/leagues')
	json <- get_api_baseball_json_from_url(url)
	return (json)
}