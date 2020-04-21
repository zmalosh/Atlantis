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
source('modules/leagueStandings.R')
source('modules/gameDetailsPanel.R')

ui <- fluidPage(
	titlePanel("Atlantis Intelligence"),
	conditionalPanel(condition='output.ShowCompetitionChooser',
		competitionChooserUI('competitionChooserElement')
	),
	conditionalPanel(condition = 'output.ShowLeaguePanel',
		htmlOutput('titleLeaguePanel', escape = FALSE),
		tabsetPanel(id = 'tabsLeague',
			tabPanel('Games',
				leagueGameListUI('leagueGameListElement')
			),
			tabPanel('Standings',
				leagueStandingsUI('leagueStandingsElement')
			)
		)
	),
	conditionalPanel(condition='output.ShowGameDetailsPanel',
		gameDetailsPanelUI('gameDetailsPanelElement')
	)
)

server <- function(input, output, session) {

	appState <- reactiveValues()
	appState$LeagueGames <- NULL

	output$ShowCompetitionChooser <- reactive(!is.null(appState) && !is.null(appState$ShowCompetitionChooser) && appState$ShowCompetitionChooser)
	outputOptions(output, "ShowCompetitionChooser", suspendWhenHidden = FALSE)

	output$ShowLeaguePanel <- reactive(!is.null(appState) && !is.null(appState$ShowLeaguePanel) && appState$ShowLeaguePanel)
	outputOptions(output, "ShowLeaguePanel", suspendWhenHidden = FALSE)

	output$ShowGameDetailsPanel <- reactive(!is.null(appState) && !is.null(appState$ShowGameDetailsPanel) && appState$ShowGameDetailsPanel)
	outputOptions(output, "ShowGameDetailsPanel", suspendWhenHidden = FALSE)

	callModule(module = competitionChooser, id = 'competitionChooserElement', appState = appState)
	callModule(module = leagueGameList, id = 'leagueGameListElement', appState = appState)
	callModule(module = leagueStandings, id = 'leagueStandingsElement', appState = appState)
	callModule(module = gameDetailsPanel, id = 'gameDetailsPanelElement', appState = appState)

	observe({
		appState$ShowCompetitionChooser <- !is.null(appState) && (is.null(appState$SelectedLeagueId) || is.null(appState$LeagueGames))
		appState$ShowLeaguePanel <- !is.null(appState) && !is.null(appState$LeagueGames) && is.null(appState$SelectedGameId)
		appState$ShowGameDetailsPanel <- !is.null(appState) && !is.null(appState$SelectedGameId)

		# HTML TITLE
		if(!is.null(appState) && !is.null(appState$LeagueGames) && !is.null(appState$LeagueTitleDisplayHtml)){
			output$titleLeaguePanel <- renderUI(HTML(appState$LeagueTitleDisplayHtml))
		}
	})
}

# Run the application
shinyApp(ui = ui, server = server)

