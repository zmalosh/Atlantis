require(ggplot2)
require(ggimage)
require(ggthemes)

team_quality_quadrants <- function(homeTeamIds, awayTeamIds, homeScores, awayScores, homeLogoUrls, awayLogoUrls){
	teamGames <- data.frame(Team = c(homeTeamIds, awayTeamIds),
							ScoreFor = c(homeScores, awayScores),
							ScoreAgainst = c(awayScores, homeScores))

	teamLogos <- data.frame(Team = c(homeTeamIds, awayTeamIds), Logo = c(homeLogoUrls, awayLogoUrls)) %>% distinct()

	scores <- teamGames %>%
		group_by(Team) %>%
		summarise(ScoreFor = sum(ScoreFor),
				  ScoreAgainst = sum(ScoreAgainst),
				  Games = n()) %>%
		mutate(AvgScoreFor = ScoreFor / Games,
			   AvgScoreAgainst = ScoreAgainst / Games) %>%
		inner_join(teamLogos, by = 'Team') %>%
		select(Team, AvgScoreFor, AvgScoreAgainst, Games, ScoreFor, ScoreAgainst, Logo) %>%
		arrange(-AvgScoreFor, -AvgScoreAgainst)

	avgScore <- mean(scores$AvgScoreFor)

	maxDecimalPlaces <- 2
	ql.col <- 'red' # Quadrant Label Color
	ql.sz <- 10 # Quadrant Label (Font) Size
	ql.alpha = 0.3 # Quadrant Label Alpha
	rl.size = 0.25

	avgScoreLabel <- format(round(avgScore, digits = maxDecimalPlaces), nsmall = maxDecimalPlaces)

	p <- ggplot(scores, aes(AvgScoreFor, AvgScoreAgainst)) +
		geom_image(aes(image = Logo), size = 0.075) +
		geom_abline(intercept =  0.0, slope = -1, color = ql.col, size = rl.size) +
		geom_vline(xintercept = avgScore, color = 'blue', size = rl.size) +
		geom_hline(yintercept = avgScore, color = 'blue', size = rl.size) +
		coord_fixed(ratio = 1) +
		scale_x_continuous(breaks = seq(from = 0, to = 200, by = 0.25)) +
		scale_y_continuous(breaks = seq(from = 0, to = 200, by = 0.25), trans = 'reverse') +
		labs(
			title = 'Team Scoring: Avg Score For vs Avg Score Against',
			subtitle = paste0('Average League Score: ', avgScoreLabel),
			caption = 'Teams above/to the right of the red line have a positive goal differential.'
		) +
		xlab('Average Score For') +
		ylab('Average Score Against (desc)') +
		annotation_custom(grobTree(textGrob('Good', x = 0.95, y = 0.95, gp = gpar(col = ql.col, fontsize = ql.sz, alpha = ql.alpha)))) +
		annotation_custom(grobTree(textGrob('Bad', x = 0.05, y = 0.05, gp = gpar(col = ql.col, fontsize = ql.sz, alpha = ql.alpha)))) +
		annotation_custom(grobTree(textGrob('Exciting', x = 0.95, y = 0.05, gp = gpar(col = ql.col, fontsize = ql.sz, alpha = ql.alpha)))) +
		annotation_custom(grobTree(textGrob('Boring', x = 0.05, y = 0.95, gp = gpar(col = ql.col, fontsize = ql.sz, alpha = ql.alpha)))) +
		theme_fivethirtyeight(base_size = 10, base_family = 'sans') +
		scale_color_fivethirtyeight() +
		theme(axis.title = element_text())

	return(p)
}
