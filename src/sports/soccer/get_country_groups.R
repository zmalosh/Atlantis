library(tidyverse)
getCountryGroups <- function(){
	countries <- read_csv('data/soccer/countries.csv',
						  col_types = cols(
						  	CountryName = col_character(),
						  	CountryAbbr = col_character(),
						  	FlagUrl = col_character(),
						  	ApiFootballCountryName = col_character()
						  ))
	countryGroups <- read_csv('data/soccer/countryGroups.csv',
							  col_types = cols(
							  	GroupName = col_character(),
							  	GroupPriority = col_integer(),
							  	CountryPriority = col_integer(),
							  	CountryCode = col_character()
							  	))
	result <- countryGroups %>%
		inner_join(countries, by = c('CountryCode'='CountryAbbr')) %>%
		arrange(GroupPriority, CountryPriority, CountryName)

	return(result)
}
