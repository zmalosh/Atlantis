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
			dtOptions <- list(lengthMenu = c(25, 50, 100, 500), pageLength = 500)
			output$dtGames <- DT::renderDataTable(appState$LeagueGames, options = dtOptions)
		}
	})
}