#data clean up
#This is where we separate countries from regions in our dataset 
countries_regions <- unique(indicators$CountryName)
regions <- countries_regions[1:33]
countries <- countries_regions[34:247]

countrycode <- unique(indicators$CountryCode)

#this is where we separate our dataset into 2 data set: one for countries and one for regions (data_with_countries is a csv file)
model_data_countries <- data_with_countries[data_with_countries$CountryName %in% countries, ]
model_data_regions <- data_with_countries[data_with_countries$CountryName %in% regions, ]