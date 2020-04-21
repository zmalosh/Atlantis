source('requirements.R')

gameDetailsPanelUI <- function(id){
	ns <- NS(id)

	return(tabsetPanel(id = ns('tabsGameDetails'),
					   tabPanel('Summary', 'Game Summary Goes Here'),
					   tabPanel('Details', 'Go in depth here'))
	)
}

gameDetailsPanel <- function(input, output, session, appState){
	ns <- session$ns

	a <- 1
}