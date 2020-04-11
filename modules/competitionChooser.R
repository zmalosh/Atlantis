
competitionChooserUI <- function(id){
	ns <- NS(id)

	return(tagList(
		uiOutput(ns('ddlCountry')),
		uiOutput(ns('ddlLeague')),
		uiOutput(ns('btnLoadGamesWrapper'))
	))
}

competitionChooser <- function(input, output, session, appState){
	source('src/sports/soccer/get_country_groups.R')
	source('src/sports/soccer/get_all_competitions.R')
	source('src/sports/soccer/get_games_by_league_id.R')

	ns <- session$ns

	notSelectedVal <- -1

	appState$SelectedLeagueId <- NULL

	countryGroups <- get_country_groups() %>% arrange(GroupPriority, CountryPriority, CountryName)
	countryDdlOptions <- c(notSelectedVal, as.list(countryGroups$CountryCode))
	names(countryDdlOptions) <- c('Select Country...', as.list(paste(countryGroups$GroupName, '-', countryGroups$CountryName)))
	appState$CountryOptions <- countryDdlOptions

	allLeagues <- get_all_competitions()

	output$ddlCountry <- renderUI({
		selectInput(ns('CountryCode'), 'Countries', appState$CountryOptions)
	})

	observeEvent(input$CountryCode, {
		appState$SelectedLeagueId <- NULL
		appState$LeagueGames <- NULL
		output$btnLoadGamesWrapper <- NULL
		output$dtGames <- NULL

		if(is.null(input$CountryCode) || input$CountryCode == notSelectedVal){
			appState$SelectedCountryCode <- NULL
			appState$LeagueOptions <- NULL
			output$ddlLeague <- NULL
		} else {
			appState$SelectedCountryCode <- input$CountryCode

			countryLeagues <- allLeagues %>%
				filter(input$CountryCode == country_code & is_current == 1 & toupper(type) == 'LEAGUE') %>%
				arrange(season, name)
			leagueDdlOptions <- c(notSelectedVal, as.list(countryLeagues$league_id))
			leagueDdlOptionNames <- c('Select League...', as.list(paste0(countryLeagues$season, ' - ', countryLeagues$name, ' (', countryLeagues$league_id, ')')))
			names(leagueDdlOptions) <- leagueDdlOptionNames
			appState$LeagueOptions <- leagueDdlOptions

			output$ddlLeague <- renderUI({
				selectInput(ns('LeagueId'), 'Leagues', appState$LeagueOptions)
			})
		}
	})

	observeEvent(input$LeagueId, {
		output$btnLoadGamesWrapper <- NULL
		output$dtGames <- NULL
		appState$LeagueGames <- NULL

		if(is.null(input$LeagueId) || input$LeagueId == notSelectedVal)
		{
			appState$SelectedLeagueId <- NULL
		} else {
			appState$SelectedLeagueId <- input$LeagueId
			output$btnLoadGamesWrapper <- renderUI({
				actionButton(ns('btnLoadGames'), 'Load Games')
			})
		}
	})

	observeEvent(input$btnLoadGames, {
		if(is.null(appState$SelectedLeagueId) || appState$SelectedLeagueId == notSelectedVal){
			appState$LeagueGames <- NULL
			output$dtGames <- NULL
		} else {
			rawGames <- get_league_games(isolate(appState$SelectedLeagueId))

			currentRound <- get_current_round_by_league_id(appState$SelectedLeagueId)

			appState$LeagueGames <- leagueGames
			appState$CurrentLeagueRound <- currentRound
		}
	})
}