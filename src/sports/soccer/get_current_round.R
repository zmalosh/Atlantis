get_current_round <- function(leagueId){
	if(is.null(leagueId)){
		return(NULL)
	}

	source('src/sports/soccer/api/get_current_round_by_league_id.R')
	currentRound <- get_current_round_by_league_id(leagueId)
	return(currentRound)
}