source('requirements.R')
source('src/sports/baseball/api/get_api_baseball_json_from_url.R')

get_games_by_league_id <- function(leagueId){
	leagueTokens <- stringr::str_split(leagueId, '_')
	apiLeagueId <- leagueTokens[[1]][1]
	apiSeason <- leagueTokens[[1]][2]
	url <- paste0('https://api-baseball.p.rapidapi.com/games?league=', apiLeagueId, '&season=', apiSeason)
	json <- get_api_baseball_json_from_url(url)
	return (json)
}