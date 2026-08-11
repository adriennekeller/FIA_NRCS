# load packages
library(rFIA)
library(dplyr)
library(ggplot2)

### Get basal area for MN plots that are single condition and that condition is forest

# load data
options(timeout = 3600)
mn <- getFIA(states = 'MN', dir = "mn_FIA", tables = c("PLOT", "COND", "TREE"))
names(mn)

# filter for plots that are forested with single condition
mn_plots <- mn$COND %>%
  filter(COND_STATUS_CD ==1) %>% # forest condition
  group_by(PLT_CN) %>%
  filter(n() == 1) %>% # exactly one condition
  ungroup
head(mn_plots)

# calculate basal area of each plot
library(dplyr)

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
