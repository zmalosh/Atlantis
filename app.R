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
				uiOutput('ddlLeague')
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

	countryGroups <- get_country_groups() %>% arrange(GroupPriority, CountryPriority, CountryName)
	countryDdlOptions <- c(notSelectedVal, as.list(countryGroups$CountryCode))
	names(countryDdlOptions) <- c('Select Country...', as.list(paste(countryGroups$GroupName, '-', countryGroups$CountryName)))
	appState$CountryOptions <- countryDdlOptions

	output$ddlCountry <- renderUI({
		selectInput('CountryCode', 'Countries', appState$CountryOptions)
	})

	observeEvent(input$CountryCode, {
		appState$SelectedLeagueId <- NULL

		if(is.null(input$CountryCode) || input$CountryCode == notSelectedVal){
			appState$SelectedCountryCode <- NULL
			output$ddlLeague <- NULL
		}
		appState$SelectedCountryCode <- input$CountryCode


	})

	output$txtSelectedCountry <- renderText(appState$SelectedCountryCode)
}

# Run the application
shinyApp(ui = ui, server = server)

