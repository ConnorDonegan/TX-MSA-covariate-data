##
## Model of population based on 2000 Census count and 5-year ACS estimates
##  aggregated to urban zones based on historic segregation; black residents only
##

library(tidyverse)
library(tidybayes)
library(rstan)

# Loop over these
MSAs <- c("Houston", "Dallas")
AGEs <- c("55-64", "65-74", "75-84")
ZONEs <- c(0, 1)

# Compile Stan model
mod <- stan_model("pop-interpolation.stan")

for (msa_name in seq_along(MSAs)) { 
    for (age_group in seq_along(AGEs)) {
        for (seg_zone in seq_along(ZONEs)) {
            df <- readr::read_csv("zone-analysis-denominator-raw.csv")
            df <- dplyr::filter(df,
                                Age == AGEs[age_group] &
                                MSA == MSAs[msa_name] &
                                zone == ZONEs[seg_zone]
                                )
            
            dl <- list(
                d = df$pop[1],
                x = df$pop[2:4],
                s = df$moe[2:4]/1.645,
                n = length(1999:2019)
            )
            
            dl$idx <- t(data.frame(
                a = 8:12,
    b = 13:17,
    c = 17:21
))
            
# Draw samples from model
S <- sampling(mod, dl, iter = 3e3, chains = 4, cores = 1,
              control = list(adapt_delta = .99, max_treedepth = 12))

# Summarize samples

res <- gather_draws(S, pop[id]) %>%
    group_by(id) %>%
    mean_qi(.value, .width = .90) %>%
    inner_join(
        data.frame(id = 1:21,
                   Year = 1999:2019
                   ),
        by = "id"
    ) %>%
    transmute(
        Year = Year,
        MSA = MSAs[msa_name],
        Age = AGEs[age_group],
        zone = ZONEs[seg_zone],
        pop = .value,
        lwr = .lower,
        upr = .upper,
        CV = (upr - lwr) / pop,
        .width = .width
    ) %>%
    as.data.frame

write_csv(res, paste0("population-estimates/annual-",
                      paste(MSAs[msa_name], AGEs[age_group], ZONEs[seg_zone], sep = "-"),
                      ".csv")
          )

####
####  aggregate to grouped time periods
####
time_periods <- c(rep(1, 7), rep(2, 5), rep(3, 5), rep(4, 4))
nms <- c(
    rep('1999-2005', 7),
    rep('2006-2010', 5),
    rep('2011-2015', 5),
    rep('2016-2019', 4)
)

gdf <- data.frame(
    id = 1:21,
    year = 1999:2019,
    group = time_periods,
    Period = nms
)

res2 <- gather_draws(S, pop[id]) %>%
    inner_join(gdf, by = "id") %>%
    group_by(.iteration, Period) %>%
    summarise(
        pop = mean(.value)
    ) %>%
    ungroup %>%
    group_by(Period) %>%
    mean_qi(pop, .width = .90) %>%
    transmute(
        Period = Period,
        MSA = MSAs[msa_name],
        Age = AGEs[age_group],
        zone = ZONEs[seg_zone],
        pop = pop,
        lwr = .lower,
        upr = .upper,
        CV = (upr - lwr) / pop,
        .width = .width
    )

            write_csv(res2, paste0("population-estimates/period-",
                                   paste(MSAs[msa_name], AGEs[age_group], ZONEs[seg_zone], sep = "-"),
                      ".csv")
          )
}}}
 

##
## combine results
##

## annual
fs <- list.files("population-estimates", full.names = TRUE, pattern = "annual")
ds <- lapply(fs, read_csv)
adf <- bind_rows(ds)
write_csv(adf, "zone-analysis-annual-population-estimates.csv")

# see results, compare with ACS estimates
raw.df <- readr::read_csv("zone-analysis-denominator-raw.csv")
raw.df$Year <- ifelse(raw.df$Period == 2019, 2017, ifelse(raw.df$Period == 2015, 2013, ifelse(raw.df$Period == 2010, 2008, raw.df$Period)))
raw.df <- transmute(raw.df, Year, MSA, Age, zone, pop, lwr = pop - moe, upr = pop + moe) 

adf %>%
    ggplot(aes(Year, pop,
               ymin = lwr, ymax = upr,
               fill = Age,
               group = paste(zone, Age, MSA))) +
    geom_line() +        
    geom_ribbon(aes(ymin = lwr, ymax= upr),
                alpha = .75) +
    geom_linerange(
        data = raw.df,
        aes(Year, pop, ymin = lwr, ymax = upr)
        ) +
    facet_wrap(~ MSA + zone, scale = "free") +
    theme_bw()

## period
fs <- list.files("population-estimates", full.names = TRUE, pattern = "period")
ds <- lapply(fs, read_csv)
pdf <- bind_rows(ds)

write_csv(pdf, "zone-analysis-period-population-estimates.csv")

# see results
pdf %>%
    ggplot(aes(group = paste(zone, Age, MSA),
               col = Age)) +
    geom_pointrange(aes(Period, pop, ymin = lwr, ymax = upr)) +
    facet_wrap(~ MSA + zone, scale = "free")


## res %>%
##     ggplot() +
##     geom_ribbon(aes(year, ymin=lwr, ymax=upr),
##                 alpha = .5,
##                 fill = 'navy') +
##     geom_line(aes(year, pop)) +
##     geom_linerange(data= data.frame(x=c(2008,2013,2017), ymin = dl$x - 2*dl$s, ymax = dl$x + 2 *dl$s),
##                aes(x, ymin=ymin, ymax=ymax)
##                ) +
##     scale_y_continuous(
##         labels = scales::comma
##     ) +
##     scale_x_continuous(
##         breaks = seq(1999, 2019, by = 5)
##     ) +
##theme_bw()

