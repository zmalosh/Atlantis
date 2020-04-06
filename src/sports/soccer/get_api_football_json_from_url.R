source('requirements.R')

get_api_football_json_from_url <- function(url){
	key <- read_file('src/sports/soccer/api.key')
	headers <- c(key)
	names(headers) <- 'X-RapidAPI-Key'

	response <- httr::GET(url, add_headers(.headers = headers))
	rawJson <- httr::content(response, as = 'text')
	json <- jsonlite::fromJSON(rawJson)$api
	return (json)
}