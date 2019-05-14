# load big mac data
bm <- read.csv("Big Mac Index (1).csv")
head(bm)

library(tidyverse)
library(data.table)

# Big Mac data calc'd for only certain countries
big_mac_countries = c('ARG', 'AUS', 'BRA', 'GBR', 'CAN', 'CHL', 'CHN', 'CZE', 'DNK',
                      'EGY', 'HKG', 'HUN', 'IDN', 'ISR', 'JPN', 'MYS', 'MEX', 'NZL',
                      'NOR', 'PER', 'PHL', 'POL', 'RUS', 'SAU', 'SGP', 'ZAF', 'KOR',
                      'SWE', 'CHE', 'TWN', 'THA', 'TUR', 'ARE', 'USA', 'COL', 'CRI',
                      'PAK', 'LKA', 'UKR', 'URY', 'IND', 'VNM', 'GTM', 'HND', # Venezuela removed
                      'NIC', 'AZE', 'BHR', 'HRV', 'JOR', 'KWT', 'LBN', 'MDA', 'OMN',
                      'QAT', 'ROU', 'EUZ')
big_mac_data = fread('big-mac-source-data.csv', na.strings = '#N/A') %>%
  .[!is.na(local_price)] %>%                    # remove lines where the local price is missing
  .[,GDP_dollar := as.numeric(GDP_dollar)] %>%  # convert GDP to a number
  .[order(date, name)]                          # sort by date and then by country name, for easy reading
tail(big_mac_data)

latest_date = big_mac_data$date %>% max
latest_date

# convert all prices to USD
big_mac_data[, dollar_price := local_price / dollar_ex]
tail(big_mac_data)

# five base currencies
base_currencies = c('USD', 'EUR', 'GBP', 'JPY', 'CNY')


# Calculating the index is as simple as dividing the local price by the price in the base currency. 
# We use data.table grouping abilities to do this neatly.
big_mac_index = big_mac_data[
  !is.na(dollar_price) & iso_a3 %in% big_mac_countries
  ,.(date, iso_a3, currency_code, name, local_price, dollar_ex, dollar_price)]

for(currency in base_currencies) {
  big_mac_index[
    ,                           # we don't want a subset, so our first argument is blank
    (currency) :=               # we'll add a new column named for the base set
      dollar_price /          # we divide the dollar price in each row by
      # the dollar price on the *base currency*'s row (.SD is a data.table
      .SD[currency_code == currency]$dollar_price -        # that contains only the current group)
      1,                      # one means parity (neither over- nor under-valued), so we subtract one
    # to get an over/under-valuation value
    by=date                     # and of course, we'll group these rows by date
    ]
}
big_mac_index[, (base_currencies) := round(.SD, 3), .SDcols=base_currencies]
tail(big_mac_index)

# basic plot versus USD
to_plot = big_mac_index[date == latest_date]
to_plot$name = factor(to_plot$name, levels=to_plot$name[order(to_plot$USD)])
ggplot(to_plot[, over := USD > 0], aes(x=name, y=USD, color=over)) +
  geom_hline(yintercept = 0) +
  geom_linerange(aes(ymin=0, ymax=USD)) +
  geom_point() +
  coord_flip()

# save raw index!
fwrite(big_mac_index, 'big-mac-raw-index.csv')

# Adjusted Index #

big_mac_gdp_data = big_mac_data[GDP_dollar > 0] # select countries where we have GDP data
head(big_mac_gdp_data)

# regression to correct for non-traded local inputs

regression_countries = c('ARG', 'AUS', 'BRA', 'GBR', 'CAN', 'CHL', 'CHN', 'CZE', 'DNK',
                         'EGY', 'EUZ', 'HKG', 'HUN', 'IDN', 'ISR', 'JPN', 'MYS', 'MEX',
                         'NZL', 'NOR', 'PER', 'PHL', 'POL', 'RUS', 'SAU', 'SGP', 'ZAF',
                         'KOR', 'SWE', 'CHE', 'TWN', 'THA', 'TUR', 'USA', 'COL', 'PAK',
                         'IND', 'AUT', 'BEL', 'NLD', 'FIN', 'FRA', 'DEU', 'IRL', 'ITA',
                         'PRT', 'ESP', 'GRC', 'EST')
big_mac_gdp_data = big_mac_gdp_data[iso_a3 %in% regression_countries]
head(big_mac_gdp_data)

# calc regressions
ggplot(big_mac_gdp_data, aes(x=GDP_dollar, y=dollar_price)) +
  facet_wrap(~date) +
  geom_smooth(method = lm, color='tomato') +
  geom_point(alpha=0.5)
# separate regression cals for each date
big_mac_gdp_data[,adj_price := lm(dollar_price ~ GDP_dollar) %>% predict,by=date]
tail(big_mac_gdp_data)
# check to see if points are on lines and this was done right
ggplot(big_mac_gdp_data, aes(x=GDP_dollar, y=dollar_price)) +
  facet_wrap(~date) +
  geom_smooth(method = lm, color='tomato') +
  geom_linerange(aes(ymin=dollar_price, ymax=adj_price), color='royalblue', alpha=0.3) +
  geom_point(alpha=0.1) +
  geom_point(aes(y=adj_price), color='royalblue', alpha=0.5)
# same as above but now for adjusted index
big_mac_adj_index = big_mac_gdp_data[
  !is.na(dollar_price) & iso_a3 %in% regression_countries & iso_a3 %in% big_mac_countries
  ,.(date, iso_a3, currency_code, name, local_price, dollar_ex, dollar_price, GDP_dollar, adj_price)]

for(currency in base_currencies) {
  big_mac_adj_index[
    ,                           # we don't want a subset, so our first argument is blank
    (currency) :=               # we'll add a new column named for the base set
      (                       # we divide the dollar price by the adjusted price to get
        dollar_price / adj_price  # the deviation from our expectation by
      ) /
      # the same figure from the *base currency*'s row
      (
        .SD[currency_code == currency]$dollar_price /
          .SD[currency_code == currency]$adj_price
      ) -
      1,                      # one means parity (neither over- nor under-valued), so we subtract one
    # to get an over/under-valuation value
    by=date                     # and of course, we'll group these rows by date
    ]
}
big_mac_adj_index[, (base_currencies) := round(.SD, 3), .SDcols=base_currencies]

tail(big_mac_adj_index)
# simple plot
to_plot = big_mac_adj_index[date == latest_date]
to_plot$name = factor(to_plot$name, levels=to_plot$name[order(to_plot$USD)])
ggplot(to_plot[, over := USD > 0], aes(x=name, y=USD, color=over)) +
  geom_hline(yintercept = 0) +
  geom_linerange(aes(ymin=0, ymax=USD)) +
  geom_point() +
  coord_flip()

# save adjusted index file 
fwrite(big_mac_adj_index, 'big-mac-adjusted-index.csv')

###### Plots for Project ########
# average price in USD over time
apotid <- data.table(big_mac_adj_index)
apotid$year <- substr(apotid$date,0,4)
head(apotid)
apotid <- apotid[,mean(dollar_price), by = year]
head(apotid)
colnames(apotid) <- c("year","Globe_Avg_USD_Price")
library(ggplot2)
g <- ggplot(data=apotid, aes(x=year,y=Globe_Avg_USD_Price, group=1)) +
  geom_line() +
  geom_point() +
  labs(title='Global Average Price of Big Macs (USD)',x='Year',y='Average Price (USD)') +
  theme_minimal()
g

dt <- data.table(big_mac_adj_index)
head(dt)
dt$year <- substr(dt$date,0,4)
library(dplyr)
dt1 <- group_by(dt,name)
dt2 <- group_by(dt1,name)
dt2
g1 <- ggplot(data=dt1,aes(x=year,y=GDP_dollar,group=name,colour=name)) +
  geom_line() +
  geom_point() +
  labs(title="GDP Per Capita (USD) by Country of Time",x='Year',y='GDP Per Capita (USD)')
g1
# brazil, colombia, sweden, switzerland, usa, china, new zealand, australia, egypt, turkey
target <- c("Brazil", "Colombia", "Sweden", "Switzerland", "United States", 
            "China", "New Zealand", "Australia", "Egypt", "Turkey")
dt3 <- filter(dt, name %in% target)
dt3.agg <- aggregate(dt3$GDP_dollar,by=list(dt3$year,dt3$name,dt3$dollar_price),FUN=mean)
colnames(dt3.agg) <- c("Year","Country","dollar_price","GDP_dollar")
g2 <- ggplot(data=dt3.agg,aes(x=Year,y=GDP_dollar,group=Country,color=Country)) +
  geom_line(size=1) +
  labs(title='GDP Per Capita (USD) by Country of Time',x='Year',y='GDP Per Capita (USD)') +
  scale_colour_brewer(palette = "Spectral") +
  theme_classic()
g2

dt3.country.agg <- dcast(dt3.agg, Country~Year,mean)
dt3.country.agg$pct_change <- ((dt3.country.agg$`2019`-dt3.country.agg$`2011`)/(dt3.country.agg$`2011`))*100
dt3.country.agg.pct <- dt3.country.agg[,c(0:1,11)]
dt3.country.agg.pct

dt3.bm.agg <- aggregate(dt3$dollar_price,by=list(dt3$year,dt3$name),FUN=mean)
colnames(dt3.bm.agg) <- c('Year','Country','dollar_price')
dt3.bm.agg
dt3.bm.country.agg <- dcast(dt3.bm.agg,Country~Year,mean)
dt3.bm.country.agg$pct_change <- ((dt3.bm.country.agg$`2019`-dt3.bm.country.agg$`2011`)/(dt3.bm.country.agg$`2011`))*100
dt3.bm.country.agg.pct <- dt3.bm.country.agg[,c(0:1,11)]
dt3.bm.country.agg.pct

dt4 <- merge(dt3.country.agg.pct,dt3.bm.country.agg.pct,by='Country')
colnames(dt4) <- c('Country','GDP_Capita_PCT_Change','BM_Price_Pct_Change')
dt4

dt5 <- melt(dt4, id.vars='Country')
g3 <- ggplot(data=dt5,aes(x=Country,y=value,fill=variable)) +
  geom_bar(stat="identity",position='dodge') +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ylab("Percent Change from 2011 - 2019") +
  ggtitle('Percent Change in GDP and BM Prices (USD) from 2011-2019') +
  guides(fill=guide_legend(title="Percent Change in:")) +
  scale_color_manual(labels=c('GDP Per Capita (USD)','Big Mac Price (USD)'))
g3
