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
source('modules/leagueGameList.R')

ui <- fluidPage(
	titlePanel("Atlantis Intelligence"),
	br(),
	sidebarLayout(
		sidebarPanel(
			competitionChooserUI('competitionChooserElement'),
			width = 3
		),
		mainPanel(
			leagueGameListUI('leagueGameListElement')
		)
	)
)


server <- function(input, output, session) {
	appState <- reactiveValues()
	appState$LeagueGames <- NULL

	callModule(module = competitionChooser, id = 'competitionChooserElement', appState = appState)
	callModule(module = leagueGameList, id = 'leagueGameListElement', appState = appState)
}

# Run the application
shinyApp(ui = ui, server = server)

