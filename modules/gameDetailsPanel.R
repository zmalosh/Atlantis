source('requirements.R')

gameDetailsPanelUI <- function(id){
	ns <- NS(id)

	return(tagList(
		p(actionButton(inputId = ns('btnResetSelectedGameId'), label = 'RESET GAME')),
		tabsetPanel(id = ns('tabsGameDetails'),
			tabPanel('Summary', 'Game Summary Goes Here'),
			tabPanel('Details', 'Go in depth here')
		)
	))
}

gameDetailsPanel <- function(input, output, session, appState){
	ns <- session$ns

	observeEvent(input$btnResetSelectedGameId, {
		if(!is.null(appState)){
			appState$SelectedGameId <- NULL
		}
	})
}