#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#	http://shiny.rstudio.com/
#

library(shiny)
source('modules/competitionChooser.R')

gameViewerUI <- function(id){
	ns <- NS(id)

	return (tagList(
		dataTableOutput(ns('dtGames'))
	))
}

gameViewer <- function(input, output, session, appState){
	observe({
		if(is.null(appState$LeagueGames)){
			output$dtGames <- NULL
		} else {
			output$dtGames <- DT::renderDataTable(appState$LeagueGames)
		}
	})
}

ui <- fluidPage(
	titlePanel("Atlantis Intelligence"),
	br(),
	actionButton('reset_app', 'RESET APP'),
	br(),
	sidebarLayout(
		sidebarPanel(
			competitionChooserUI('competitionChooserElement')
		),
		mainPanel(
			gameViewerUI('gameViewerElement')
		)
	)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
	appState <- reactiveValues()
	appState$LeagueGames <- NULL

	callModule(module = competitionChooser, id = 'competitionChooserElement', appState = appState)
	callModule(module = gameViewer, id = 'gameViewerElement', appState = appState)
}

# Run the application
shinyApp(ui = ui, server = server)

