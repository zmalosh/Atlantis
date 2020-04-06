#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#	http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
	titlePanel("Atlantis Intelligence"),
	actionButton('reset_app', 'RESET APP'),
	br(),
	tabsetPanel(
		tabPanel('League Selection',
			mainPanel(
				uiOutput('ddlCountry'),
				textOutput('txtSelectedCountry'),
				uiOutput('ddlLeague'),
				textOutput('txtSelectedLeagueId'),
				uiOutput('btnLoadGamesWrapper')
			)
		)
	)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

	notSelectedVal <- -1

	appState <- reactiveValues()
	appState$SelectedLeagueId <- NULL

	source('src/sports/soccer/get_country_groups.R')
	source('src/sports/soccer/get_all_competitions.R')

	countryGroups <- get_country_groups() %>% arrange(GroupPriority, CountryPriority, CountryName)
	countryDdlOptions <- c(notSelectedVal, as.list(countryGroups$CountryCode))
	names(countryDdlOptions) <- c('Select Country...', as.list(paste(countryGroups$GroupName, '-', countryGroups$CountryName)))
	appState$CountryOptions <- countryDdlOptions

	allLeagues <- get_all_competitions()

	output$ddlCountry <- renderUI({
		selectInput('CountryCode', 'Countries', appState$CountryOptions)
	})

	observeEvent(input$CountryCode, {
		appState$SelectedLeagueId <- NULL

		if(is.null(input$CountryCode) || input$CountryCode == notSelectedVal){
			appState$SelectedCountryCode <- NULL
			appState$LeagueOptions <- NULL
			output$ddlLeague <- NULL
			output$btnLoadGamesWrapper <- NULL
		} else {
			appState$SelectedCountryCode <- input$CountryCode

			countryLeagues <- allLeagues %>%
				filter(input$CountryCode == country_code & is_current == 1 & toupper(type) == 'LEAGUE') %>%
				arrange(season, name)
			leagueDdlOptions <- c(notSelectedVal, as.list(countryLeagues$league_id))
			leagueDdlOptionNames <- c('Select League...', as.list(paste(countryLeagues$season, '-', countryLeagues$name)))
			names(leagueDdlOptions) <- leagueDdlOptionNames
			appState$LeagueOptions <- leagueDdlOptions

			output$ddlLeague <- renderUI({
				selectInput('LeagueId', 'Leagues', appState$LeagueOptions)
			})
		}
	})

	observeEvent(input$LeagueId, {
		if(is.null(input$LeagueId) ||input$LeagueId == notSelectedVal){
			appState$SelectedLeagueId <- NULL
			output$btnLoadGamesWrapper <- NULL
		} else {
			appState$SelectedLeagueId <- input$LeagueId
			output$btnLoadGamesWrapper <- renderUI({
				actionButton('btnLoadGames', 'Load Games')
			})
		}
	})

	output$txtSelectedCountry <- renderText(appState$SelectedCountryCode)
	output$txtSelectedLeagueId <- renderText(appState$SelectedLeagueId)
}

# Run the application
shinyApp(ui = ui, server = server)

