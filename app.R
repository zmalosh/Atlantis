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

ui <- fluidPage(
	titlePanel("Atlantis Intelligence"),
	br(),
	actionButton('reset_app', 'RESET APP'),
	br(),
	tabsetPanel(
		tabPanel('League Selection',
			mainPanel(
				competitionChooserUI('competitionChooserElement')
			)
		)
	)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
	appState <- reactiveValues()
	callModule(module = competitionChooser, id = 'competitionChooserElement', appState = appState)
}

# Run the application
shinyApp(ui = ui, server = server)

