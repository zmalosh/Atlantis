get_soccer_controller <- function(){
	source('src/sports/soccer/get_bookmakers.R')
	source('src/sports/soccer/get_country_groups.R')
	source('src/sports/soccer/get_game_details.R')
	source('src/sports/soccer/get_league_games.R')
	source('src/sports/soccer/get_league_odds.R')
	source('src/sports/soccer/get_league_standings.R')

	ctrl <- list(
		'get_sport_display_name' = function(){return('Soccer')},
		'get_bookmakers' = get_bookmakers,
		'get_country_groups' = get_country_groups,
		'get_game_details' = get_game_details,
		'get_league_games' = get_league_games,
		'get_league_odds' = get_league_odds,
		'get_league_standings' = get_league_standings,
		'get_current_period' = get_current_round_by_league_id
	)

	return(ctrl)
}
