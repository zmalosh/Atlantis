source('requirements.R')

get_api_baseball_json_from_url <- function(url){
	key <- read_file('src/sports/baseball/api/api.key')
	headers <- c(key, 'api-baseball.p.rapidapi.com', TRUE)
	names(headers) <- c('X-RapidAPI-Key', 'x-rapidapi-host', 'useQueryString')

	response <- httr::GET(url, add_headers(.headers = headers))
	rawJson <- httr::content(response, as = 'text')
	json <- jsonlite::fromJSON(rawJson)$api
	return (json)
}