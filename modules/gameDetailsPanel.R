source('requirements.R')
source('src/sports/soccer/get_game_details.R')

getGamePredictions <- function(predModel, homeTeamId, awayTeamId, isNeutralSite, homeSpread = 0){
	pctDecimalPlaces <- 3
	preds <- predModel$predictByIds(homeTeamId, awayTeamId, isNeutralSite = isNeutralSite, homeSpread = homeSpread)
	ensembleModelResult <- data.frame(
		ModelName = 'EnsembleModel',
		HomePct = format(round(preds$pred$HomeWinPct, pctDecimalPlaces), nsmall = pctDecimalPlaces),
		DrawPct = format(round(preds$pred$DrawWinPct, pctDecimalPlaces), nsmall = pctDecimalPlaces),
		AwayPct = format(round(preds$pred$AwayWinPct, pctDecimalPlaces), nsmall = pctDecimalPlaces),
		stringsAsFactors = FALSE
	)
	baseModelResults <- data.frame(
		ModelName = names(preds$base.preds),
		HomePct = sapply(preds$base.preds, function(x) format(round(x$HomeWinPct, pctDecimalPlaces), nsmall = pctDecimalPlaces)),
		DrawPct = sapply(preds$base.preds, function(x) format(round(x$DrawWinPct, pctDecimalPlaces), nsmall = pctDecimalPlaces)),
		AwayPct = sapply(preds$base.preds, function(x) format(round(x$AwayWinPct, pctDecimalPlaces), nsmall = pctDecimalPlaces)),
		stringsAsFactors = FALSE
	)
	result <- rbind(ensembleModelResult, baseModelResults)
	row.names(result) <- NULL
	return(result)
}

gameDetailsPanelUI <- function(id){
	ns <- NS(id)

	return(tagList(
		p(actionButton(inputId = ns('btnResetSelectedGameId'), label = 'RESET GAME')),
		div(textOutput(ns('txtSelectedGameId'))),
		tabsetPanel(id = ns('tabsGameDetails'),
			tabPanel('Predictions',
				uiOutput(ns('numHomeSpread')),
				dataTableOutput(ns('dtSpreadPredictions'))
			),
			tabPanel('Summary', 'Game Summary Goes Here'),
			tabPanel('Details', 'Go in depth here')
		)
	))
}

gameDetailsPanel <- function(input, output, session, appState){
	ns <- session$ns

	observe({
		if(!is.null(appState) && !is.null(appState$SelectedGameId)){
			output$txtSelectedGameId <- renderText(paste0('GameId: ', appState$SelectedGameId))

			rawGameDetails <- get_game_details(appState$SelectedGameId)

			appState$SelectedGameDetails <- rawGameDetails
			appState$SelectedHomeTeam <- rawGameDetails$HomeTeam
			appState$SelectedAwayTeam <- rawGameDetails$AwayTeam
		} else {
			output$txtSelectedGameId <- NULL

			appState$SelectedGameDetails <- NULL
			appState$SelectedHomeTeam <- NULL
			appState$SelectedAwayTeam <- NULL
		}
	})

	# RESET GAME BUTTON
	observeEvent(input$btnResetSelectedGameId, {
		if(!is.null(appState)){
			appState$SelectedGameId <- NULL
		}
	})

	# PREDICTIONS TAB
	observe({
		if(is.null(appState) || is.null(appState$LeaguePredModel) || is.null(appState$SelectedHomeTeam) || is.null(appState$SelectedAwayTeam)){
			hideTab(inputId = ns('tabsGameDetails'), target = 'Predictions')
			output$numHomeSpread <- NULL
		} else {
			showTab(inputId = ns('tabsGameDetails'), target = 'Predictions')
			output$numHomeSpread <- renderUI({
				sliderInput(ns('HomeSpread'), 'Home Spread', min = -5, max = 5, step = 0.5, value = 0)
			})
		}
	})
	observeEvent(input$HomeSpread, {
		if(is.null(input$HomeSpread) || is.null(appState$LeaguePredModel) ){
			output$dtSpreadPredictions <- NULL
		} else {
			spreadPreds <- getGamePredictions(appState$LeaguePredModel, appState$SelectedHomeTeam, appState$SelectedAwayTeam, F, input$HomeSpread)

			dtOptions <- list(lengthMenu = c(25, 50, 100, 500),
							  pageLength = 500,
							  ordering = F,
							  paging = F,
							  searching = F)
			dt <- datatable(spreadPreds,
							escape = FALSE,
							options = dtOptions,
							rownames = FALSE) %>%
				formatStyle(c('HomePct', 'DrawPct', 'AwayPct'),
							background = styleColorBar(range(c(0,1)), 'lightblue'),
							backgroundSize = '98% 88%',
							backgroundRepeat = 'no-repeat',
							backgroundPosition = 'center')
			output$dtSpreadPredictions <- DT::renderDataTable(dt)
		}
	})
}