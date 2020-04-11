leagueGameListUI <- function(id){
	ns <- NS(id)

	return (tagList(
		dataTableOutput(ns('dtGames'))
	))
}

leagueGameList <- function(input, output, session, appState){
	observe({
		if(is.null(appState$LeagueGames)){
			output$dtGames <- NULL
		} else {
			output$dtGames <- DT::renderDataTable(appState$LeagueGames)
		}
	})
}