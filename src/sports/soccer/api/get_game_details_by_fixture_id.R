source('requirements.R')
source('src/sports/soccer/api/get_api_football_json_from_url.R')

get_game_details_by_fixture_id <- function(fixtureId){
	url <- paste0('https://api-football-v1.p.rapidapi.com/v2/fixtures/id/', fixtureId)
	json <- get_api_football_json_from_url(url)
	result <- as.data.frame(json$fixtures)
	return (result)
}