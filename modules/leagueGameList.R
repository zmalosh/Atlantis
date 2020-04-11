library(tidyverse)
source('src/sports/soccer/api/get_current_round_by_league_id.R')

leagueGameListUI <- function(id){
	ns <- NS(id)

	return (tagList(
		uiOutput(ns('ddlLeagueRounds')),
		dataTableOutput(ns('dtGames'))
	))
}

leagueGameList <- function(input, output, session, appState){
	ns <- session$ns

	allGamesVal <- 'All Games'

	appState$DisplayGames <- NULL

	observe({
		if(is.null(appState$LeagueGames)){
			output$dtGames <- NULL
			output$ddlRounds <- NULL
		} else {

			leagueRounds <- c(appState$LeagueGames %>% select(Round) %>% unique(), allGamesVal)
			if(!is.null(appState$CurrentLeagueRound)){
				appState$SelectedLeagueRound <- appState$CurrentLeagueRound
			} else {
				appState$SelectedLeagueRound <- leagueRounds[1]
			}

			output$ddlLeagueRounds <- renderUI({
				selectInput(ns('LeagueRound'), 'Rounds', as.list(leagueRounds), selected = isolate(appState$SelectedLeagueRound))
			})
		}
	})

	observeEvent(input$LeagueRound, {
		appState$SelectedLeagueRound <- input$LeagueRound
	})

	observe({
		if(!is.null(appState$LeagueGames) && !is.null(appState$SelectedLeagueRound)){
			if(appState$SelectedLeagueRound == allGamesVal){
				appState$DisplayGames <- appState$LeagueGames
			} else {
				appState$DisplayGames <- appState$LeagueGames %>% filter(Round == appState$SelectedLeagueRound)
			}

			dtOptions <- list(lengthMenu = c(25, 50, 100, 500), pageLength = 500)
			output$dtGames <- DT::renderDataTable(appState$DisplayGames, options = dtOptions)
		}
	})
}