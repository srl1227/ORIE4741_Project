#Subset selection: mainly testing out our subset selection algorithms

d <- model_data_countries
indic_subset <- subset(model_data_countries, select = c(`CO2 emissions (metric tons per capita)`,`Energy use (kg of oil equivalent per capita)`, `Employment to population ratio, 15+, male (%) (modeled ILO estimate)`,
  `Lifetime risk of maternal death (1 in: rate varies by country)`,
  `Renewable energy consumption (% of total final energy consumption)`,
  `Age dependency ratio, old (% of working-age population)`,
  `Unemployment, male (% of male labor force)`,
  `Forest area (% of land area)`,
  `GDP per capita, PPP (constant 2011 international $)`,
  `Fossil fuel energy consumption (% of total)`,
  `Employment to population ratio, 15+, total (%) (modeled ILO estimate)`,
  `Death rate, crude (per 1,000 people)`,
  `Population ages 65 and above (% of total)`,
  `Terrestrial and marine protected areas (% of total territorial area)`,
  `Alternative and nuclear energy (% of total energy use)`,
  `Arable land (% of land area)`,
  `Fixed telephone subscriptions (per 100 people)`,
  `Changes in inventories (current LCU)`,
  `Military expenditure (current LCU)`,
  `Improved sanitation facilities (% of population with access)`,
  `Life expectancy at birth, female (years)`,
  `Merchandise exports to developing economies in Middle East & North Africa (% of total merchandise exports)`,
  `Agriculture value added per worker (constant 2005 US$)`,
  `Population density (people per sq. km of land area)`))

#here we compared best subset selection with forwards stepwise and backward stepwise subset selection
regfit.fwd=regsubsets(`CO2 emissions (metric tons per capita)`~.,data=indic_subset, nvmax = 23, method = "forward")
reg.fwd.summary = summary(regfit.fwd)
names(reg.fwd.summary)
reg.fwd.adjr2 = reg.fwd.summary$adjr2
plot(reg.fwd.adjr2 ,xlab="Number of Variables ",
     ylab="Adjusted RSq",type="o", xlim = c(0,23), ylim = c(0.70,0.90), main = "Forward Subset Selection")
#axis(side=1, at=seq(0, 20, by=2))
#axis(side=2, at=seq(0.70, 0.90, by=.05))

regfit.bwd = regsubsets(`CO2 emissions (metric tons per capita)`~.,data=indic_subset,nvmax = 23, method = "backward")
bwd.summary = summary(regfit.bwd)
names(bwd.summary)
bwd.summary.adjr2 = bwd.summary$adjr2
plot(bwd.summary.adjr2, xlab="Number of Variables ",
     ylab="Adjusted RSq",type="o",xlim = c(0,23), ylim = c(0.70,0.90), main = "Backward Subset Selection")

regfit.full = regsubsets(`CO2 emissions (metric tons per capita)`~., data=indic_subset, nvmax = 23, method = "exhaustive")
full.summary = summary(regfit.full)
names(full.summary)
full.summary.adjr2 = full.summary$adjr2
plot(full.summary.adjr2, xlab="Number of Variables ",
     ylab="Adjusted RSq",type="o",xlim = c(0,23), ylim = c(0.70,.90), main = "Best Subset Selection Adj. R-Sq.")

print(paste0('Max Adj. R2 for Best Subset = ', max(full.summary$adjr2)))
print(paste0('Max Adj. R2 for Fwd. Subset = ', max(reg.fwd.summary$adjr2)))
print(paste0('Max Adj. R2 for Bwd. Subset = ', max(bwd.summary$adjr2)))

print(paste0('Full. Adj. R2 for 15 Features: ', full.summary$adjr2[15])) #.849
print(paste0('Fwd. Adj. R2 for 15 Features: ', reg.fwd.summary$adjr2[15])) #.843
print(paste0('Bwd. Adj. R2 for 15 Features: ', bwd.summary$adjr2[15])) #.849
#best subset and backwards subset selection output the same results 
#--> so reduce computational complexity, we go with backwards subset selection


