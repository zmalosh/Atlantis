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
			),
			tabPanel('Quality Quads',
					 imageOutput('imgTeamQualityQuads'),
			),
			tabPanel('Excitement Quads',
					 imageOutput('imgTeamGameExcitementQuads'),
			)
		)
	),
	conditionalPanel(condition='output.ShowGameDetailsPanel',
		gameDetailsPanelUI('gameDetailsPanelElement')
	)
)

minTeamGamesPlayed <- function(leagueGames){
	teamGameCounts <- leagueGames %>%
		filter(!is.na(HomeScore)) %>%
		group_by(HomeTeam) %>%
		summarise(HomeGames = n()) %>%
		full_join(leagueGames %>%
				  	filter(!is.na(HomeScore)) %>%
				  	group_by(AwayTeam) %>%
				  	summarise(AwayGames = n()), by = c('HomeTeam' = 'AwayTeam')) %>%
		mutate(Games = coalesce(HomeGames, 0L) + coalesce(AwayGames, 0L))
	minGameCount <- min(teamGameCounts$Games)
	return(minGameCount)
}

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

	observe({
		if(!is.null(appState) && !is.null(appState$SelectedLeagueId) && !is.null(appState$LeagueGames) && minTeamGamesPlayed(appState$LeagueGames)){
			source('src/vizualizations/team_quality_quadrants.R')
			source('src/vizualizations/team_game_excitement_quadrants.R')
			vizGames <- isolate(appState$LeagueGames %>% filter(!is.na(HomeScore) & !is.na(AwayScore)))
			imgTeamQualityQuads <- team_quality_quadrants(homeTeamIds = vizGames$HomeTeam,
														  awayTeamIds = vizGames$AwayTeam,
														  homeScores = vizGames$HomeScore,
														  awayScores = vizGames$AwayScore,
														  homeLogoUrls = vizGames$HomeTeamLogoUrl,
														  awayLogoUrls = vizGames$AwayTeamLogoUrl)
			teamQualityFileName <- paste0('teamQuads_quality_', appState$SelectedLeagueId, '.png')
			if(!file.exists(teamQualityFileName) || file.info(teamQualityFileName)$mtime < lubridate::now() - minutes(15)){
				ggsave(teamQualityFileName,
					   imgTeamQualityQuads,
					   width = 8,
					   height = 6)
			}
			output$imgTeamQualityQuads <- renderImage({
				filename <- normalizePath(teamQualityFileName)
				list(src = filename,
					 width = 800,
					 height = 600)
			}, deleteFile =  FALSE)

			imgTeamGameExcitementQuads <- team_game_excitement_quadrants(homeTeamIds = vizGames$HomeTeam,
														  awayTeamIds = vizGames$AwayTeam,
														  homeScores = vizGames$HomeScore,
														  awayScores = vizGames$AwayScore,
														  homeLogoUrls = vizGames$HomeTeamLogoUrl,
														  awayLogoUrls = vizGames$AwayTeamLogoUrl)
			teamGameExcitementFileName <- paste0('teamQuads_gameExcitement_', appState$SelectedLeagueId, '.png')
			if(!file.exists(teamGameExcitementFileName) || file.info(teamGameExcitementFileName)$mtime < lubridate::now() - minutes(15)){
				ggsave(teamGameExcitementFileName,
					   imgTeamGameExcitementQuads,
					   width = 8,
					   height = 6)
			}
			output$imgTeamGameExcitementQuads <- renderImage({
				filename <- normalizePath(teamGameExcitementFileName)
				list(src = filename,
					 width = 800,
					 height = 600)
			}, deleteFile =  FALSE)
		} else {
			output$imgTeamQualityQuads <- NULL
			output$imgTeamGameExcitementQuads <- NULL
		}
	})
}

# Run the application
shinyApp(ui = ui, server = server)

