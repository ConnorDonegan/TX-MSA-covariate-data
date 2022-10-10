library(sf)
library(tidyverse)

df <- read_csv("tracts-1970-data/nhgis0004_ds95_1970_tract.csv")
df <- filter(df, STATE == "Texas")

d.tmp <- select(df, starts_with('CEB'))
black <- with(d.tmp, `CEB002` + `CEB011`)
pop <- rowSums(d.tmp)
black.pct <- 100 * black / pop

data <- data.frame(GISJOIN = df$GISJOIN,
                   black = black,
                   pop = pop,
                   black.pct = black.pct
                   )

shp <- st_read("tracts-1970-shp/US_tract_1970_conflated.shp")
shp <- inner_join(shp, data, by = "GISJOIN") %>%
    select(GISJOIN,
           black, black.pct, pop)

st_write(shp, "tracts-1970-shp/texas-tracts-1970.shp")
