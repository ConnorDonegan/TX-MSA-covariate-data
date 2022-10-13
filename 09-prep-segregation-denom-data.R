##
## Model of population based on 2000 Census count and 5-year ACS estimates
##  aggregated to urban zones based on historic segregation; black residents only
##

library(tidyverse)
library(tidybayes)
library(rstan)

# Filter data to these parameters
args <- commandArgs(trailingOnly = FALSE)
msa_name <- args[1]
age_group <- args[2]
seg_zone <- as.numeric( args[3] )

# Compile Stan model
rstan_options(auto_write = TRUE)
mod <- stan_model("stan/pop-interpolation.stan")

df <- read_csv("Data-Working/zone-analysis-denominator-raw.csv")
df <- filter(df, Age == age_group &
                 MSA == msa_name &
                 zone == seg_zone)

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
S <- sampling(mod, dl, iter = 4e3, chains = 4)

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
        MSA = msa_name,
        Age = age_group,
        zone = seg_zone,
        pop = .value,
        lwr = .lower,
        upr = .upper,
        CV = (upr - lwr) / pop,
        .width = .width
    ) %>%
    as.data.frame

write_csv(res, paste0("Data-Working/population-estimates/annual-",
                      paste(msa_name, age_group, seg_zone, sep = "-"),
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
        MSA = msa_name,
        Age = age_group,
        zone = seg_zone,
        pop = pop,
        lwr = .lower,
        upr = .upper,
        CV = (upr - lwr) / pop,
        .width = .width
    )

write_csv(res2, paste0("Data-Working/population-estimates/period-",
                      paste(msa_name, age_group, seg_zone, sep = "-"),
                      ".csv")
          )




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
