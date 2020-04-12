library(tidyverse)

leagueStandingsUI <- function(id){
	ns <- NS(id)

	return (textOutput(ns('txtTestText')))
}

leagueStandings <- function(input, output, session, appState){
	ns <- session$ns

	output$txtTestText <- renderText('This is populated from the module')
}