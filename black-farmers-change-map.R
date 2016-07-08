#Packages to install. Uncomment if you need to.
#install.packages("ggplot2", "maps", "dplyr", "magrittr")
#devtools::install_github("hrbrmstr/albersusa")

library(ggplot2)
library(maps)
library(albersusa) 
library(dplyr)
library(magrittr)

#Visit this URL, and download the data by clicking on "spreadsheet." 
#https://quickstats.nass.usda.gov/results/A68E27D5-E9B2-3621-8F1E-58829A551F32
#Rename the file "ag_census.csv".

#Create a data table from the downloaded data.
df <- tbl_df(read.csv("ag_census.csv"))

#The domain category "NOT SPECIFIED" is the total for each county, so we need to throw out
#other categories that duplicate farmers. Then we filter for 2002 and 2012 only, group by 
#county FIPS number, and then sort by year. At that point we can subtract the 2002 figure 
#from the 2012 number using the "lag" function. We create two new columns to record the
#change over time: "delta" and "change."

df <- df %>%
        filter(Domain == "TOTAL", Domain.Category == "NOT SPECIFIED", Year == 2002 | Year == 2012) %>%
        mutate(fips=sprintf("%02d%03d", State.ANSI, County.ANSI)) %>%
        group_by(fips) %>%
        arrange(Year) %>%
        mutate(delta=Value-lag(Value),
               delta=ifelse(is.na(delta), 0, delta)) %>%
        mutate(change = delta) %>%
        select(Year, State, County, fips, Value, delta, change)


#Here we break the "delta" column up into bins.
df$delta <- cut(df$delta, include.lowest=FALSE,
                breaks=c(-50, -1, 1, 50, 200),
                labels=c("-50 to -1 (losses)", 
                        "no gains/losses", 
                        "+1 to 50", "+51 to 165 (gains)"))

#Here we throw away all the data from 2002 and 2007, since we just want to
#map one value for each county. 
df <- df %>%
        filter(Year == 2012)
        
    
#Here we create a map of counties.
counties <- counties_composite()
c <- c("Alaska", "Hawaii")
counties <- counties[!(counties$state %in% c), ]

#Here we bring in a state map as well so we can show state boundaries. 
states <- usa_composite()
states <- states[!(states$name %in% c), ]

counties_map <- fortify(counties, region="fips")
states_map <- fortify(states)


gg <- ggplot()
gg <- gg + geom_map(data=counties_map, map=counties_map,
                    aes(x=long, y=lat, map_id=id),
                    #color=NA, size=0.15, 
                    fill="white"
                    )

gg <- gg + geom_map(data=df, map=counties_map,
                    aes(fill=delta, map_id=fips),
                    color=NA, lwd=0
                    #color="#b3b3b3", size=0
                    )

gg <- gg + scale_fill_manual(name="Change since 2002\n(white = no data)",
                             values=c("#b2abd2", 
                                      "#e1e1e1",
                                      "#e08214", "#b35806"),
                             guide=guide_legend(reverse=TRUE))

gg <- gg + coord_proj(us_laea_proj)

gg <- gg + labs(x="Grey == no data", y=NULL)

gg <- gg + theme_map()

gg <- gg + theme(legend.position=c(0.85, 0.2))

gg <- gg + theme(legend.key=element_blank())

gg <- gg + labs(title = "Where We’ve Gained—And Lost—Black Farmers")

gg <- gg + geom_path(data = states_map, aes(x=long, y=lat, group = group), colour = "dark grey", size = .25)

gg