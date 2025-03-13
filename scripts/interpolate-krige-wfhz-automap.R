################################################################################
#                           INTERPOLATE WITH `{automap}`                       #
################################################################################

## ---- Automatically fit a variogram ------------------------------------------
auto_exp_variogram_wfhz <- autofitVariogram(
  formula = est ~ 1,
  input_data = wrangled_wfhz,
  model = c("Sph", "Exp", "Gau", "Ste"),
  fix.values = c(NA, NA, NA),
  verbose = FALSE,
  GLS.model = NA,
  start_vals = c(NA, NA, NA),
  miscFitOptions = list(),
  cutoff = dist_max,
  width = dist_min
)

## ---- Perform automatic interpolation ----------------------------------------
auto_interp_wfhz <- autoKrige(
  formula = est ~ 1,
  input_data = wrangled_wfhz,
  new_data = grid,
  block = 0,
  model = c("Sph", "Exp", "Gau", "Ste"),
  fix.values = c(NA, NA, NA),
  GLS.model = NA,
  start_vals = c(NA, NA, NA),
  miscFitOptions = list(),
  nmin = 3,
  nmax = 4
)

## ---- Perfom automatic cross-validation --------------------------------------
auto_cv_wfhz <- autoKrige.cv(
  formula = est ~ 1,
  input_data = wrangled_wfhz,
  model = c("Sph", "Exp", "Gau", "Ste"),
  fix.values = c(NA, NA, NA),
  GLS.model = NA,
  start_vals = c(NA, NA, NA),
  miscFitOptions = list(),
  nmin = 3,
  nmax = 4,
  maxdist = 9000,
  verbose = c(FALSE, TRUE)
)

### ------------------------------------------- Cross-validation statistics ----
auto_cv_wfhz_stats <- auto_cv_wfhz[[1]] |>
  as_tibble() |>
  summarise(
    mean_error = mean(residual, na.rm = TRUE), ## be as close to zero as possible
    MSPE = mean(residual^2, na.rm = TRUE), ## Ideally small
    MSNR = mean(zscore^2, na.rm = TRUE), ## Mean squared normalized error, should be close to 1
    r2_obspred = cor(observed, observed - residual, use = "complete.obs"), ## Ideally 1
    r2_predobs = cor(observed - residual, residual, use = "complete.obs") ## Ideally should be close to 0
  )

### --------------------------------------------- Plot predicted ~ observed ----
ggplot(auto_cv_wfhz[[1]], aes(x = var1.pred, y = observed)) +
  geom_point(size = 1.2, color = "#BA4A00") +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "#566573",
    linewidth = 0.3
  ) +
  geom_smooth(
    method = "lm",
    color = "blue",
    linewidth = 0.9,
    se = FALSE
  ) +
  theme_minimal() +
  labs(
    title = "A scatterplot of observed values against predicted",
    x = "Predicted",
    y = "Observed"
  ) +
  theme(
    plot.title = element_text(size = 11),
    plot.subtitle = element_text(size = 9, colour = "#706E6D")
  )

### ------------------------------------------------- Visualize map surface ----

#### Static map ----
ggplot() +
  geom_stars(
    data = auto_interp_wfhz[[1]],
    aes(fill = var1.pred, x = x, y = y)
  ) +
  scale_fill_gradientn(
    colors = ipc_colours(),
    na.value = "transparent",
    name = "GAM Prevalence (%)",
    limits = c(0, 30),
    breaks = c(0, 5, 10, 15, 30),
    labels = c("<5.0", "5.0-9.9", "10.0-14.9", "15.0-29.9", "≥30.0"),
    values = scales::rescale(c(0, 5, 10, 15, 30), from = c(0, 30))
  ) +
  geom_sf(
    data = st_cast(karamoja_admn4, "MULTILINESTRING"),
    linewidth = 0.2,
    color = "grey"
  ) +
  labs(
    title = "A surface map of the predicted prevalence of GAM by WHZ"
  ) +
  theme(
    plot.title = element_text(colour = "#706E6D", size = 10)
  ) +
  theme_void()

### Interactive map ----
auto_interp_wfhz[[1]] |>
  mapview(
    alpha = 1,
    alpha.regions = 0.2,
    col.regions = ipc_colours(.map_type = "interactive", indicator = "wfhz"),
    na.color = "transparent",
    trim = TRUE
  )

### -------------------------------------------- Predicting standard errors ----

## TO BE ADDED...

### ------------------------------------------------------- Get areal means ----
#### At district level (ADM2_EN) ----
auto_pred_mean_admn2 <- krige(
  formula = est ~ 1,
  locations = wrangled_wfhz,
  nmin = 3,
  nmax = 4,
  model = auto_exp_variogram_wfhz[[2]],
  newdata = karamoja_admn2
)

##### Cloropleth map of the mean predicted prevalence at district level ----
ggplot() +
  geom_sf(
    data = auto_pred_mean_admn2,
    aes(fill = var1.pred),
    color = "black",
    size = 0.2
  ) +
  scale_fill_gradientn(
    colours = ipc_colours(),
    na.value = "transparent",
    name = "GAM Prevalence (%)",
    limits = c(0, 30),
    breaks = c(0, 5, 10, 15, 30),
    labels = c("<5.0", "5.0-9.9", "10.0-14.9", "15.0-29.9", "≥30.0"),
    values = scales::rescale(c(0, 5, 10, 15, 30), from = c(0, 30))
  ) +
  geom_sf(
    data = karamoja_admn2,
    fill = NA,
    color = "#F2F3F4",
    size = 0.8
  ) +
  geom_sf_text(
    data = karamoja_admn2,
    mapping = aes(label = factor(ADM2_EN)),
    show.legend = TRUE,
    color = "#34495E",
    size = 3,
  ) +
  labs(
    title = "Mean predicted prevalence of GAM by WFHZ at district level",
    fill = "Predicted Values"
  ) +
  theme(
    plot.title = element_text(size = 9)
  ) +
  theme_void()

##### Get minimum and maximum predicted prevalence values by district -----
auto_min_max <- auto_interp_wfhz[[1]] |>
  st_as_sf() |>
  st_join(karamoja_admn2, left = FALSE) |> # each grid cell to a polygon
  group_by(ADM2_EN) |>
  summarise(
    min_value = min(var1.pred, na.rm = TRUE),
    max_value = max(var1.pred, na.rm = TRUE)
  )

##### Compare mean predicted prevalence against original survey results -----
auto_pred_vs_original <- wfhz_data |>
  rename(cluster = enumArea) |>
  mw_estimate_prevalence_wfhz(
    wt = NULL,
    edema = ChildOedema,
    .by = district
  ) |>
  select(district, gam_p) |>
  arrange(factor(district)) |>
  mutate(
    survey = gam_p * 100,
    interp = auto_pred_mean_admn2[["var1.pred"]],
    bias = interp - survey,
    min_interp = min_max$min_value,
    max_interp = min_max$max_value
  ) |>
  select(-gam_p)

#### At county level (ADM4_EN) ----
auto_pred_mean_admn4 <- krige(
  formula = est ~ 1,
  locations = wrangled_wfhz,
  nmin = 3,
  nmax = 4,
  model = auto_exp_variogram_wfhz[[2]],
  newdata = karamoja_admn4
)

#### Cloropleth map of the mean predicted prevalence at county level ----
ggplot() +
  geom_sf(
    data = auto_pred_mean_admn4,
    aes(fill = var1.pred),
    color = "black",
    size = 0.2
  ) +
  scale_fill_gradientn(
    colours = ipc_colours(),
    na.value = "transparent",
    name = "GAM Prevalence (%)",
    limits = c(0, 30),
    breaks = c(0, 5, 10, 15, 30), # Define range breakpoints
    labels = c("<5.0", "5.0-9.9", "10.0-14.9", "15.0-29.9", "≥30.0"),
    values = scales::rescale(c(0, 5, 10, 15, 30), from = c(0, 30))
  ) +
  geom_sf(
    data = karamoja_admn4,
    fill = NA,
    color = "#F2F3F4"
  ) +
  geom_sf(
    data = karamoja_admn2,
    fill = NA,
    color = "#3F4342",
    size = 0.8
  ) +
  geom_sf_text(
    data = karamoja_admn2,
    mapping = aes(label = factor(ADM2_EN)),
    show.legend = TRUE,
    color = "#34495E",
    size = 3,
  ) +
  labs(
    title = "Mean predicted prevalence of GAM by WFHZ at county level"
  ) +
  theme(
    plot.title = element_text(size = 8)
  ) +
  theme_void()
################################ End of workflow ###############################
