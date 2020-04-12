library(tidyverse)

leagueStandingsUI <- function(id){
	ns <- NS(id)

	return (tagList(
		uiOutput(ns('ddlStandingsGroup')),
		dataTableOutput(ns('dtStandings')))
	)
}

get_form_display_html <- function(form){
	if(form == ''){
		return('')
	}
	result <- get_form_display_html(stringr::str_sub(form, 2))
	thisVal <- stringr::str_sub(form, 1, 1)
	if(thisVal == 'W'){
		thisResult <- '<img src="images/form_green_w.png" />'
	} else if (thisVal == 'D'){
		thisResult <- '<img src="images/form_grey_d.png" />'
	} else if (thisVal == 'L'){
		thisResult <- '<img src="images/form_red_l.png" />'
	} else{
		thisResult <- ''
	}
	result <- paste0(thisResult, result)
	return(result)
}

get_change_image_html <- function(change, imageHeight = 20){
	if(change == 'up'){
		imgSrc <- 'images/green_arrow_up.png'
	} else if (change == 'down') {
		imgSrc <- 'images/red_arrow_down.png'
	} else {
		imgSrc <- 'images/grey_dash.png'
	}
	return(paste0('<img src="', imgSrc,'" height="', imageHeight, '" />'))
}

leagueStandings <- function(input, output, session, appState){
	ns <- session$ns

	output$txtTestText <- renderText('This is populated from the module')
	observe({
		if(is.null(appState$LeagueStandings)){
			output$dtStandings <- NULL
		} else {
			groupOptions <- appState$LeagueStandings %>% select(Group) %>% unique()
			appState$StandingsGroupOptions <- as.list(groupOptions)
			output$ddlStandingsGroup <- renderUI(
				selectInput(ns('StandingsGroup'), 'Standings Group', appState$StandingsGroupOptions)
			)
		}
	})

	observeEvent(input$StandingsGroup,{
		if(is.null(input$StandingsGroup)){
			output$dtStandings <- NULL
		} else {
			rawGroupStandings <- appState$LeagueStandings %>% filter(Group == input$StandingsGroup)
			displayStandings <- data.frame(
				Rank = paste0(sapply(rawGroupStandings$Change, get_change_image_html, imageHeight = 15), '&nbsp;&nbsp;<span>',rawGroupStandings$Rank,'</span>'),
				Team = paste0('<span style="white-space:nowrap"><img src="', rawGroupStandings$TeamLogoUrl, '" height="35" /><span>', rawGroupStandings$TeamName, '</span></span>'),
				Form = paste0('<span style="white-space:nowrap">', sapply(rawGroupStandings$Form, get_form_display_html), '</span>'),
				GP = rawGroupStandings$Played,
				Pts = rawGroupStandings$Points,
				W = rawGroupStandings$Wins,
				D = rawGroupStandings$Draws,
				L = rawGroupStandings$Losses,
				GD = rawGroupStandings$GoalDifference,
				GF = rawGroupStandings$GoalsFor,
				GA = rawGroupStandings$GoalsAgainst,
				CurrentResult = rawGroupStandings$CurrentResult,
				Home = paste(rawGroupStandings$HomeWins, rawGroupStandings$HomeDraws, rawGroupStandings$HomeLosses, sep = '-'),
				Away = paste(rawGroupStandings$AwayWins, rawGroupStandings$AwayDraws, rawGroupStandings$AwayLosses, sep = '-'),
				stringsAsFactors = FALSE
			)

			# SET Form COLUMN TO 80px (3rd COLUMN, ZERO-INDEXING -> c(2))
			dtOptions <- list(lengthMenu = c(10, 50),
							  pageLength = 50,
							  columnDefs = list(list(width = '80px', targets = c(2))),
							  ordering = FALSE,
							  paging = FALSE,
							  searching = FALSE
						 )
			dt <- DT::datatable(displayStandings, escape = FALSE, options = dtOptions, rownames = FALSE)
			output$dtStandings <- DT::renderDataTable(dt)
		}
	})
}