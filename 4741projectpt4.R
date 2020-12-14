#analysis on different country values compared to the whole world.
tt <- subset(model_data_countries, CountryName == 'Trinidad and Tobago')
lborg <- subset(model_data_countries, CountryName == 'Luxembourg')
india <- subset(model_data_countries, CountryName == 'India')

plot(tt$Year, tt$`Age dependency ratio, old (% of working-age population)`, type = 'l')

avgs <- matrix(,nrow = 2, ncol = 22)

vars <-colnames(indic_subset)
vars <- vars[-c(12,19)]
#for the world
avgs[1,] = vars
s = 1
for(i in vars){
  avgs[2,s] = mean(model_data_countries[[i]])
  s = s+1
  #print(i)
}

values <- t(avgs)
colnames(values) = c('Features','World Avg.')

#for tt
ttavgs = rep(NA,22)
t = 1
for(i in vars){
  ttavgs[t] = mean(tt[[i]])
  t = t+1
}
ttavgs = t(ttavgs)
colnames(ttavgs) = c('Trinidad and Tobago Avg')

values <- cbind(values, ttavgs)
worldavgs = values[,c(1,2)]


#for lborg
lborgavgs = rep(NA,22)
n = 1
for(i in vars){
  lborgavgs[n]=mean(lborg[[i]])
  n = n+1
}
lborgavgs = t(lborgavgs)
colnames(lborgavgs) = c('Luxemborg')
worldavgs.l = cbind(worldavgs, lborgavgs)

#india
indiaavgs = rep(NA,22)
n = 1
for(i in vars){
  indiaavgs[n]=mean(india[[i]])
  n = n+1
}
indiaavgs = t(t(indiaavgs))
colnames(indiaavgs) = c('India')
india_avg = cbind(worldavgs, indiaavgs)