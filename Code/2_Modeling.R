#----------------------------------
## Libraries and preliminaries
#----------------------------------
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sdmTMB)
library(sdmTMBextra)
library(ggeffects)


# Base map land info
region <- ne_countries(scale = "medium", continent = "North America", returnclass = "sf")
states <- ne_states(country = c("United States of America", "Canada"), returnclass = "sf")

lat_lims <- c(35.2, 48)
lon_lims<- c(-76, -56.2)

#----------------------------------
## Get data 
#----------------------------------

mod_data <- readRDS(here::here("Data/Derived/model_data.rds")) %>%
  filter(year <= 2019) # FV com only runs to 2019, so for now we have to subset
summary(mod_data)

# Scale/center covariates
# Get means and sds
column_means <- colMeans(mod_data[, c("depth", "BT_seasonal")], na.rm = TRUE)
column_sds <- apply(mod_data[, c("depth", "BT_seasonal")], 2, sd, na.rm = TRUE)

# Scale the data
mod_data <- mod_data |>
  mutate(across(
    c(depth, BT_seasonal),
    ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
    .names = "{.col}_scaled"
  ))
summary(mod_data)

#----------------------------------
## Model Data Prep for fitting seasonal model
#----------------------------------

# Going to want to have a continuous time column
all_years<- seq(from = min(mod_data$year), to = max(mod_data$year))
seasons<- c("Spring", "Fall")
time_fac_levels<- paste(rep(all_years, each = length(unique(seasons))), seasons, sep = "_")
time_ints<- as.numeric(factor(time_fac_levels, levels = time_fac_levels))

mod_data <- mod_data |>
  mutate(season = factor(season, levels = seasons),
         year_season_fac = factor(paste(year, season, sep = "_"), levels = time_fac_levels),
         year_season_int = as.numeric(year_season_fac)) %>%
  arrange(year_season_int)

#----------------------------------
## Make mesh
#----------------------------------

sdmTMB_mesh <- sdmTMB::make_mesh(mod_data, xy_cols = c("X", "Y"), cutoff = 5)
plot(sdmTMB_mesh)

#----------------------------------
## Fit model
#----------------------------------
#| label: Exclude space-invariant temporal autocorrelation.
#| include: false
#| echo: false
#| warning: false

tictoc::tic()
fit1 <- sdmTMB(
  weight_kg ~ factor(season) + factor(habitat) + factor(sed_type) + s(depth, k = 4) + s(BT_seasonal, k = 4),
  data = mod_data,
  spatial = "on",
  # offset = dat_mod$swept,
  anisotropy = TRUE,
  share_range = FALSE,
  spatiotemporal = "ar1",
  spatial_varying = ~season,
  time = "year_season_int",
  # time_varying = ~ 0 + year_season_int,
  # time_varying_type = "ar1",
  extra_time = time_ints,
  mesh = sdmTMB_mesh,
  family = tweedie(),
  silent = FALSE,
  do_fit = TRUE
)
tictoc::toc()

tidy(fit1, effects = "fixed")
tidy(fit1, effects = "ran_pars")
sanity(fit1)

write_rds(fit1,"Data/Derived/Model_output/fit1.rds", compress = "gz")


# Fit 1 doesn't seem to converge very well. This model drops the spacially varying coefficient. 

tictoc::tic()
fit2 <- sdmTMB(
  weight_kg ~ factor(season) + factor(habitat) + factor(sed_type) + s(depth, k = 4) + s(BT_seasonal, k = 4),
  data = mod_data,
  spatial = "on",
  # offset = dat_mod$swept,
  anisotropy = TRUE,
  share_range = FALSE,
  spatiotemporal = "ar1",
  #spatial_varying = ~season,
  time = "year_season_int",
  # time_varying = ~ 0 + year_season_int,
  # time_varying_type = "ar1",
  # extra_time = time_ints,
  mesh = sdmTMB_mesh,
  family = tweedie(),
  silent = FALSE,
  do_fit = TRUE
)
tictoc::toc()

sanity(fit2)
tidy(fit2, effects = "fixed")
tidy(fit2, effects = "ran_pars")

write_rds(fit2,"Data/Derived/Model_output/fit2.rds", compress = "gz")

# Still having convergence issues with fit 2. Going to try and simplify further here. 

tictoc::tic()
fit3 <- sdmTMB(
  weight_kg ~ factor(season) + factor(habitat) + factor(sed_type) + s(depth, k = 4) + s(BT_seasonal, k = 4),
  data = mod_data,
  spatial = "on",
  # offset = dat_mod$swept,
  share_range = TRUE,
  spatiotemporal = "ar1",
  #spatial_varying = ~season,
  time = "year",
  mesh = sdmTMB_mesh,
  family = tweedie(),
  silent = FALSE
)
tictoc::toc()

sanity(fit3)
tidy(fit3, effects = "fixed")
tidy(fit3, effects = "ran_pars")

write_rds(fit3,"Data/Derived/Model_output/fit3.rds", compress = "gz")

# Fit 3 seems to not have any of the convergence issues with the previous models. Going to stick with this one for now.

#--------------------------
## Time varying effects
#--------------------------

# Claire in particularly interested in how habitat use has changed over time. The following model allows the effect of habitat to vary with year-season. This will allow us to estimate the biomass of lobster in each habitat type in each year-season. See this tutorial for an example at "Time-varying effects" https://pbs-assess.github.io/sdmTMB/articles/basic-intro.html#spatial-predictions

tictoc::tic()
fit4 <- sdmTMB(
  weight_kg ~ factor(season) + factor(habitat) + factor(sed_type) + s(depth, k = 4) + s(BT_seasonal, k = 4),
  data = mod_data,
  spatial = "on",
  # offset = dat_mod$swept,
  anisotropy = TRUE,
  share_range = T,
  spatiotemporal = "IID",
  #spatial_varying = ~season,
  time = "year",
  time_varying = ~ 0 + factor(habitat),
  time_varying_type = "rw0",
  mesh = sdmTMB_mesh,
  family = tweedie(),
  silent = FALSE,
)
tictoc::toc()

sanity(fit4)
tidy(fit4, effects = "fixed")
tidy(fit4, effects = "ran_pars")

write_rds(fit4,"Data/Derived/Model_output/fit4.rds", compress = "gz")


nd <- expand.grid(
  habitat = unique(mod_data$habitat), 
  year = as.numeric(unique(mod_data$year)), 
  sed_type = "mud", 
  depth = mean(mod_data$depth), 
  BT_seasonal = mean(mod_data$BT_seasonal), 
  season = "Spring"
)


p <- predict(fit4, newdata = nd, se_fit = TRUE, re_form = NA)

ggplot(p, aes(year, exp(est),
              ymin = exp(est - 1.96 * est_se),
              ymax = exp(est + 1.96 * est_se)))+
  geom_line(aes(color = habitat))+
  geom_ribbon(aes(group = habitat), alpha = 0.25)+
  facet_wrap(~habitat)


# Building upon the previous model, Claire in interested in both shifts in habitat and depth. This model digs into those more fully.

tictoc::tic()
fit5 <- sdmTMB(
  weight_kg ~ factor(season) + factor(habitat) + factor(sed_type) + depth_scaled + BT_seasonal_scaled,
  data = mod_data,
  spatial = "on",
  share_range = T,
  spatiotemporal = "IID",
  #spatial_varying = ~season,
  time = "year",
  time_varying = ~ 0 + depth_scaled + I(depth_scaled^2),
  time_varying_type = "rw0",
  mesh = sdmTMB_mesh,
  family = tweedie(),
  silent = FALSE,
)
tictoc::toc()

sanity(fit5)
tidy(fit5, effects = "fixed")
tidy(fit5, effects = "ran_pars")

write_rds(fit5,"Data/Derived/Model_output/fit5.rds", compress = "gz")


nd <- expand.grid(
  habitat = "crest", 
  year = as.numeric(unique(mod_data$year)), 
  sed_type = "gravel", 
  depth_scaled = seq(min(mod_data$depth_scaled), max(mod_data$depth_scaled), length.out = 100), 
  BT_seasonal_scaled = mean(mod_data$BT_seasonal_scaled), 
  season = "Spring"
)


p <- predict(fit5, newdata = nd, se_fit = T, re_form = NA)

# ggplot(p, aes(depth_scaled, exp(est),
#               ymin = exp(est - 1.96 * est_se),
#               ymax = exp(est + 1.96 * est_se)))+
#   geom_line()+
#   # geom_ribbon(alpha = 0.25)+
#   facet_wrap(~year)

p %>% 
  mutate(depth = unscale(depth_scaled, column_means[1], column_sds[1])) %>%
  ggplot(aes(depth, exp(est)))+
  geom_line(aes(color = as.factor(year)))+
  facet_wrap(~year)


#---------------------------
## New one
#---------------------------


# Building upon the previous model, Claire in interested in both shifts in habitat and depth. This model digs into those more fully.

mod_data2 <- mod_data %>%
  mutate(hab_sed = as.factor(paste(habitat, sed_type, sep = "-")), 
         depth_scaled2 = depth_scaled^2)

tictoc::tic()
fit6 <- sdmTMB(
  weight_kg ~ factor(season) + hab_sed + depth_scaled + depth_scaled2 + BT_seasonal_scaled,
  data = mod_data2,
  spatial = "on",
  share_range = T,
  spatiotemporal = "IID",
  #spatial_varying = ~season,
  time = "year",
  time_varying = ~ 0 + depth_scaled + depth_scaled2 + hab_sed,
  time_varying_type = "rw0",
  mesh = sdmTMB_mesh,
  family = tweedie(),
  silent = FALSE,
)
tictoc::toc()

sanity(fit6)
tidy(fit6, effects = "fixed")
tidy(fit6, effects = "ran_pars")

write_rds(fit6,"Data/Derived/Model_output/fit6.rds", compress = "gz")


nd <- expand.grid(
  hab_sed = unique(mod_data2$hab_sed),
  year = as.numeric(unique(mod_data2$year)), 
  depth_scaled = seq(min(mod_data2$depth_scaled), max(mod_data2$depth_scaled), length.out = 50),
  BT_seasonal_scaled = mean(mod_data2$BT_seasonal_scaled), 
  season = "Spring"
) %>% mutate(depth_scaled2 = depth_scaled^2)


p <- predict(fit6, newdata = nd, se_fit = F, re_form = NA)

# ggplot(p, aes(depth_scaled, exp(est),
#               ymin = exp(est - 1.96 * est_se),
#               ymax = exp(est + 1.96 * est_se)))+
#   geom_line()+
#   # geom_ribbon(alpha = 0.25)+
#   facet_wrap(~year)

p %>% 
  mutate(depth = unscale(depth_scaled, column_means[1], column_sds[1]), 
         year = as.factor(year)) %>%
  ggplot(aes(depth, exp(est)))+
  geom_line(aes(color = hab_sed))+
  facet_wrap(~year)+
  labs(x = "Predicted lobster biomass", y = "Depth (m)")+
  theme_bw()
ggsave("Figures/timevarying_depth_and_hab_sed.png", width = 12, height = 12)

p %>% 
  mutate(depth = unscale(depth_scaled, column_means[1], column_sds[1]), 
         year = as.factor(year)) %>%
  ggplot(aes(depth, exp(est)))+
  geom_line(aes(color = hab_sed))+
  facet_grid(hab_sed~year)
ggsave("Figures/timevarying_depth_and_hab_sed-grid.png", width = 20, height = 20)
