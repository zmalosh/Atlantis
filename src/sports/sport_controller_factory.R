source('requirements.R')

sport_controller_factory_options <- list(
	'SOCCER' = 'SOCCER'
)

sport_controller_factory <- function(sport){
	ctrl <- NULL

	strSport <- stringr::str_to_upper(sport)

	if(strSport == 'SOCCER'){
		source('src/sports/soccer/get_soccer_controller.R')
		ctrl <- get_soccer_controller()
	}

	return (ctrl)
}
