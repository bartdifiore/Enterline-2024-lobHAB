




# Predict

pred_data <- readRDS(here::here("Data/Derived/pred_glorys_with_covs.rds"))

# Now for the model matrix trickery
mm_season <- model.matrix(~ 0 + factor(season), data = pred_data)
# mm_year <- model.matrix(~ 0 + factor(est_year), data = dat) 

pred_data <- readRDS(here::here("Data/Derived/pred_glorys_with_covs.rds")) |>
  filter(between(year, year_min, year_max) & season %in% c("Spring", "Summer", "Fall")) |>
  mutate(
    Depth_scaled = (Depth - column_means["Depth"]) / column_sds["Depth"],
    BT_seasonal_scaled = (BT_seasonal - column_means["BT_seasonal"]) / column_sds["BT_seasonal"]
  ) |>
  mutate(season = factor(season, levels = seasons),
         year_season_fac = factor(paste(year, season, sep = "_"), levels = time_fac_levels),
         year_season_int = as.numeric(year_season_fac)) %>%
  dplyr::select(!contains("factor")) %>%
  cbind(mm_season) %>%
  as_tibble() %>%
  add_utm_columns(ll_names = c("x", "y")) %>%
  mutate(survey = "ME_NH") %>%
  drop_na()

fit6_preds <- predict(fit6, newdata = pred_data, type = "response", se = FALSE)
str(fit6_preds)

# Nest and map
fit6_preds <- fit6_preds |>
  group_by(season, year, Year_Season, Date) |>
  nest()

map_nested <- function(pred_df, time, region_use = region, states_use = states, xlim_use = lon_lims, ylim_use = lat_lims) {
  ggplot() +
    geom_raster(data = pred_df, aes(x = x, y = y, fill = est)) +
    geom_sf(data = region_use, fill = "#f0f0f0") +
    geom_sf(data = states_use, color = "dark gray", lwd = 0.2, na.rm = TRUE) +
    coord_sf(xlim = xlim_use, ylim = ylim_use, expand = FALSE) +
    scale_fill_viridis_c(name = "Predicted biomass", trans = "log") +
    theme_minimal() +
    labs(
      fill = "Predicted biomass",
      title = time
    )
}

fit6_preds <- fit6_preds |>
  mutate("Pred_Map" = map2(data, Year_Season, map_nested))

cowplot::plot_grid(fit6_preds$Pred_Map[[1]], 
                   fit6_preds$Pred_Map[[10]], 
                   fit6_preds$Pred_Map[[20]], 
                   fit6_preds$Pred_Map[[30]], 
                   fit6_preds$Pred_Map[[40]], 
                   fit6_preds$Pred_Map[[50]], 
                   fit6_preds$Pred_Map[[60]],
                   fit6_preds$Pred_Map[[70]], 
                   fit6_preds$Pred_Map[[80]], 
                   fit6_preds$Pred_Map[[90]], ncols = 3, nrows = 3)












