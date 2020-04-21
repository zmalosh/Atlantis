source('requirements.R')
source('src/sports/soccer/api/get_games_by_league_id.R')
source('src/sports/soccer/get_country_groups.R')
source('src/sports/soccer/get_league_games.R')
source('src/sports/soccer/get_league_standings.R')
source('src/sports/soccer/api/get_all_competitions.R')

competitionChooserUI <- function(id){
	ns <- NS(id)

	return(fluidRow(
		column(4, uiOutput(ns('ddlCountry'))),
		column(4, uiOutput(ns('ddlLeague'))),
		column(2, uiOutput(ns('btnLoadGamesWrapper')), style = "margin-top: 25px;")
	))
}

competitionChooser <- function(input, output, session, appState){
	ns <- session$ns

	notSelectedVal <- -1
	worldCountryCode <- 'XX'

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
				filter((input$CountryCode == country_code | (input$CountryCode == worldCountryCode & country == 'World')) & is_current == 1 & toupper(type) == 'LEAGUE') %>%
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

		if(is.null(input$LeagueId) || input$LeagueId == notSelectedVal){
			appState$SelectedLeagueId <- NULL
		} else {
			appState$SelectedLeagueId <- input$LeagueId
			output$btnLoadGamesWrapper <- renderUI({
				actionButton(ns('btnLoadGames'), 'Load League')
			})
		}
	})

	observeEvent(input$btnLoadGames, {
		if(is.null(appState$SelectedLeagueId) || appState$SelectedLeagueId == notSelectedVal){
			appState$LeagueGames <- NULL
			output$dtGames <- NULL
		} else {
			currentRound <- get_current_round_by_league_id(appState$SelectedLeagueId)
			rawGames <- get_league_games(isolate(appState$SelectedLeagueId))
			rawStandings <- get_league_standings(isolate(appState$SelectedLeagueId))

			ensemblePredModel <- SportPredictR::ensemble_model(gameIds =  rawGames$GameId,
													   homeTeamIds = rawGames$HomeTeam,
													   awayTeamIds = rawGames$AwayTeam,
													   homeScores = rawGames$HomeScore,
													   awayScores = rawGames$AwayScore,
													   isNeutralSite = F)
			allPreds <- ensemblePredModel$predictByIds(rawGames$HomeTeam, rawGames$AwayTeam)
			preds <- allPreds$pred

			pctDecimalPlaces <- 3
			leagueGames <- data.frame(
				GameId = rawGames$GameId,
				Round = rawGames$Round,
				GameTime = rawGames$GameTime,
				HomeTeam = rawGames$HomeTeam,
				AwayTeam = rawGames$AwayTeam,
				HomePct = format(round(preds$HomeWinPct, pctDecimalPlaces), nsmall = pctDecimalPlaces),
				DrawPct = format(round(preds$DrawWinPct, pctDecimalPlaces), nsmall = pctDecimalPlaces),
				AwayPct = format(round(preds$AwayWinPct, pctDecimalPlaces), nsmall = pctDecimalPlaces),
				HomeScore = rawGames$HomeScore,
				AwayScore = rawGames$AwayScore,
				HomeTeamLogoUrl = rawGames$HomeTeamLogoUrl,
				AwayTeamLogoUrl = rawGames$AwayTeamLogoUrl,
				stringsAsFactors = FALSE
			)

			appState$CurrentLeagueRound <- currentRound
			appState$LeagueGames <- leagueGames
			appState$LeagueStandings <- rawStandings
			appState$LeaguePredModel <- ensemblePredModel

			countryGroupData <- countryGroups %>% filter(CountryCode == appState$SelectedCountryCode) %>% top_n(1)
			leagueData <- allLeagues %>% filter(league_id == appState$SelectedLeagueId) %>% slice(1)
			leagueDisplayHtml <- paste0('<span class="h3" style="background-color:#EEEEEE; padding: 5px; height: 60px; display:inline-block;"><img src="', leagueData$flag,'" height="50" />&nbsp;&nbsp;', leagueData$country,' - ', leagueData$season, ' ', leagueData$name, ' (', leagueData$season_start, ' to ', leagueData$season_end, ')</span>')
			appState$LeagueTitleDisplayHtml <- leagueDisplayHtml
		}
	})
}