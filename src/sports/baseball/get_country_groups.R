library(tidyverse)
get_country_groups <- function(){
	countries <- read_csv('data/baseball/countries.csv',
						  col_types = cols(
						  	CountryId = col_integer(),
						  	CountryName = col_character(),
						  	CountryAbbr = col_character(),
						  	FlagUrl = col_character(),
						  	ApiBaseballCountryName = col_character()
						  ))

	countryGroups <- read_csv('data/baseball/countryGroups.csv',
							  col_types = cols(
							  	GroupName = col_character(),
							  	GroupPriority = col_integer(),
							  	CountryPriority = col_integer(),
							  	CountryCode = col_character()
							  ))

	result <- countryGroups %>%
		inner_join(countries, by = c('CountryCode' = 'CountryAbbr')) %>%
		select(GroupName, GroupPriority, CountryCode, CountryName, CountryPriority, FlagUrl)

	return(result)
}
