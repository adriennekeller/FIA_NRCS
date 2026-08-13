library(dplyr)
# identify harvested plots
mn_harvest <- mn$COND %>%
  filter(COND_STATUS_CD == 1) %>%   # forested conditions
  filter(TRTCD1 == 10) %>%
  select(PLT_CN, CONDID,INVYR,STATECD,UNITCD,COUNTYCD,PLOT,
    TRTCD1, TRTCD2, TRTCD3,
    TRTYR1, TRTYR2, TRTYR3
  ) %>%
  distinct()

# build remeasurement history
plot_history <- mn$PLOT %>%
  select(CN, PREV_PLT_CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, MEASYEAR, LAT, LON)

get_plot_root <- function(cn, plot_lookup) { # function to connect remeasured plots with IDs
  current <- cn
  visited <- numeric(0)
  while (!is.na(current) && !(current %in% visited)) { # keep looping until current is not missing and current has not been visited 
    visited <- c(visited, current)
    previous <- plot_lookup$PREV_PLT_CN[
      match(current, plot_lookup$CN)
    ]
    if (length(previous) == 0 || is.na(previous)) {
      break
    }
    current <- previous
  }
  return(current)
}

plot_history <- plot_history %>%
  mutate(
    PLOT_ROOT = vapply(
      CN,
      get_plot_root,
      numeric(1),
      plot_lookup = plot_history
    )
  )

# attach history to harvested plots
mn_harvest <- mn_harvest %>%
  left_join(
    plot_history %>%
      select(CN, PLOT_ROOT, LAT, LON),
    by = c("PLT_CN" = "CN")
  )

# identify plots harvested more than once
multiple_harvest <- mn_harvest %>%
  group_by(PLOT_ROOT) %>%
  summarise(
    LAT = first(LAT), # LAT and LON are unique to PLOT_ROOT (I checked)
    LON = first(LON),
    n_harvest_measurements = n_distinct(INVYR),
    harvest_years = paste(
      sort(unique(INVYR)),
      collapse = ", "
    ),
    n_harvest_records = n(),
    .groups = "drop"
  ) %>%
  filter(n_harvest_measurements > 1)

# map total harvested plots with multiple harvests/plot overlaid
ggplot() + 
  geom_point(data = mn_harvest, aes(x = LON, y = LAT), colour = "grey") + 
  geom_point(data = multiple_harvest, aes(x = LON, y = LAT), colour = "green") 

