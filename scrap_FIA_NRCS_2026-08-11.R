# load packages
library(rFIA)
library(dplyr)
library(ggplot2)

### Get basal area for MN plots that are single condition and that condition is forest

# load data
options(timeout = 3600)
#mn <- getFIA(states = 'MN', dir = "mn_FIA", tables = c("PLOT", "COND", "TREE"))
mn <- readFIA("mn_FIA/")
names(mn)

# filter for plots that are forested with single condition
mn_plots <- mn$COND %>%
  filter(COND_STATUS_CD ==1) %>% # forest condition
  group_by(PLT_CN) %>%
  filter(n() == 1) %>% # exactly one condition
  ungroup
head(mn_plots)

# calculate basal area of each plot
plot_ba <- mn$TREE %>%
  inner_join(
    mn_plots %>% select(PLT_CN, CONDID),
    by = c("PLT_CN", "CONDID")
  ) %>%
  filter(
    STATUSCD == 1,
    !is.na(DIA),
    !is.na(TPA_UNADJ)
  ) %>%
  mutate(
    ba_acre = 0.005454154 * DIA^2 * TPA_UNADJ
  ) %>%
  group_by(PLT_CN) %>%
  summarise(
    basal_area_ac = sum(ba_acre, na.rm = TRUE),
    .groups = "drop"
  )
# visualize
names(mn$PLOT)
plot_map <- plot_ba %>%
  left_join(
    mn$PLOT %>% select(CN, LAT, LON),
    by = c("PLT_CN" = "CN")
  )

ggplot(plot_map, aes(x = LON, y = LAT)) +
  geom_point(aes(size = basal_area_ac), alpha = 0.6, color = "darkgreen") +
  scale_size_continuous(name = "basal area\n(ft2/acre)") +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Minnesota FIA Plot Basal Area w/ single cond and forested",
       x = "Longitude", y = "Latitude")

### Select out plots in MN (with single condition, forested) with harvest at any remeasurement
mn_harvest <- mn_plots %>%
  filter(TRTCD1 == 10 | TRTCD2 == 10 | TRTCD3 == 10) %>%
  left_join(mn$PLOT %>% select(CN, LAT, LON), by = c("PLT_CN" = "CN"))

ggplot() +
  geom_point(data = plot_map, aes(x = LON, y = LAT), shape = 1, colour = "darkgreen") +
  geom_point(data = mn_harvest, aes(x = LON, y = LAT), colour = "brown")

### Play around with how tables are linked ----
head(mn$PLOT)
mn$PLOT %>% filter(MANUAL != 0 & COUNTYCD == 75 & PLOT == 29443) # PLOT is unique only within COUNTYCD

View(mn$COND %>% filter(COUNTYCD == 75 & PLOT == 29443))

mn$COND %>% filter(TRTCD1 == 10) %>%
  left_join(mn$PLOT %>% select(CN, LAT, LON, MEASYEAR), by = c("PLT_CN" = "CN")) %>%
  ggplot() + geom_point(aes(x = LON, y = LAT, colour = MEASYEAR)) + 
  geom_point(data = mn_plots, aes(x = LON, y = LAT), shape = 1)

# plotting all forested plots in MN with annual inventory that also have been cut and disturbed
pdat <- mn$COND %>% filter(COND_STATUS_CD == 1) %>% # pdat = only forested condition
  left_join(mn$PLOT %>% select(CN, LAT, LON, MEASYEAR, MANUAL), by = c("PLT_CN" = "CN")) %>%
  filter(MANUAL > 1) 
ggplot() + 
  geom_point(data = pdat, aes(x = LON, y = LAT), colour = "lightgrey") + 
  geom_point(data = pdat %>% filter(TRTCD1 == 10 | TRTCD2 == 10 | TRTCD3 == 10), # filtered for those with cutting
             aes(x = LON, y = LAT), shape = 1, colour = "green") +
  geom_point(data = pdat %>% filter(DSTRBCD1 == 10), aes(x = LON, y = LAT), shape = 1, colour = "red")
  

### 
# select all plots with harvest
mn_harvest <- mn$COND %>% filter(COND_STATUS_CD == 1) %>% # only forested condition
  filter(TRTCD1 == 10 | TRTCD2 == 10 | TRTCD3 == 10) %>% # harvested
  select(c(CN, PLT_CN, INVYR, COUNTYCD, PLOT))
  filter(DSTRBYR1 > )

  
  