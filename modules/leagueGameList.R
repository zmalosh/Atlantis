library(tidyverse)
source('src/sports/soccer/api/get_current_round_by_league_id.R')

leagueGameListUI <- function(id){
	ns <- NS(id)

	return (tagList(
		uiOutput(ns('ddlLeagueRounds')),
		dataTableOutput(ns('dtGames'))
	))
}

leagueGameList <- function(input, output, session, appState){
	ns <- session$ns

	allGamesVal <- 'All Games'

	appState$DisplayGames <- NULL

	observe({
		if(is.null(appState$LeagueGames)){
			output$dtGames <- NULL
			output$ddlRounds <- NULL
		} else {

			leagueRounds <- c(appState$LeagueGames %>% select(Round) %>% unique(), allGamesVal)
			if(!is.null(appState$CurrentLeagueRound)){
				appState$SelectedLeagueRound <- appState$CurrentLeagueRound
			} else {
				appState$SelectedLeagueRound <- leagueRounds[1]
			}

			output$ddlLeagueRounds <- renderUI({
				selectInput(ns('LeagueRound'), 'Rounds', as.list(leagueRounds), selected = isolate(appState$SelectedLeagueRound))
			})
		}
	})

	observeEvent(input$LeagueRound, {
		appState$SelectedLeagueRound <- input$LeagueRound
	})

	tableLogoHeight <- 20
	observe({
		if(!is.null(appState$LeagueGames) && !is.null(appState$SelectedLeagueRound)){
			if(appState$SelectedLeagueRound == allGamesVal){
				displayGames <- appState$LeagueGames
			} else {
				displayGames <- appState$LeagueGames %>% filter(Round == appState$SelectedLeagueRound)
			}

			displayGames$HomeTeam <- paste0('<span style="white-space:nowrap;"><img src="', displayGames$HomeTeamLogoUrl, '" height="', tableLogoHeight, '" />&nbsp;&nbsp;', displayGames$HomeTeam, '</span>')
			displayGames$AwayTeam <- paste0('<span style="white-space:nowrap;"><img src="', displayGames$AwayTeamLogoUrl, '" height="', tableLogoHeight, '" />&nbsp;&nbsp;', displayGames$AwayTeam, '</span>')


			shinyInput <- function(ids, labels, ...) {
				idCount <- length(ids)
				inputs <- character(idCount)
				for (i in 1:idCount) {
					btnId <- ns(paste0('scoreButton_', ids[i]))
					label <- labels[i]
					inputs[i] <- as.character(actionButton(btnId, label = label, ...))
				}
				inputs
			}

			scoreDisplay <- ifelse(is.na(displayGames$HomeScore), 'PREVIEW', paste0(displayGames$HomeScore,'-',displayGames$AwayScore))
			displayGames$HomeScore <- NULL
			displayGames$AwayScore <- NULL
			btnScore_onClick <- paste0('Shiny.onInputChange(\"', ns('select_button') ,'\",  this.id)')
			displayGames$Score <- shinyInput(displayGames$GameId, labels = scoreDisplay, onclick = btnScore_onClick)

			appState$DisplayGames <- displayGames %>% select(-c(GameId, Round, HomeTeamLogoUrl, AwayTeamLogoUrl))

			dtOptions <- list(lengthMenu = c(25, 50, 100, 500),
							  pageLength = 500,
							  ordering = appState$SelectedLeagueRound == allGamesVal,
							  paging = appState$SelectedLeagueRound == allGamesVal,
							  searching = appState$SelectedLeagueRound == allGamesVal)
			dt <- datatable(appState$DisplayGames,
							escape = FALSE,
							selection = 'none',
							options = dtOptions,
							rownames = FALSE) %>%
				formatStyle(c('HomePct', 'DrawPct', 'AwayPct'),
							background = styleColorBar(range(c(0,1)), 'lightblue'),
							backgroundSize = '98% 88%',
							backgroundRepeat = 'no-repeat',
							backgroundPosition = 'center')
			output$dtGames <- DT::renderDataTable(dt, server = FALSE)
		}
	})

	observeEvent(input$select_button, {
		gameId <- stringr::str_split(input$select_button, '_')[[1]][2]
	})
}