################################################################################
#                   INTERPOLATE GAM by MUAC  WITH `{automap}`                  #
################################################################################

## ---- Automatically fit a variogram ------------------------------------------
auto_exp_variogram_muac <- autofitVariogram(
  formula = est ~ 1,
  input_data = wrangled_muac,
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
auto_interp_muac <- autoKrige(
  formula = est ~ 1,
  input_data = wrangled_muac,
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
### --- Bin interpolated GAM prevalence into IPC AMN Phase categories ----
auto_interp_muac$krige_output <- auto_interp_muac$krige_output |> 
  mutate(
    var1.pred.cat = cut(
      var1.pred,
      breaks = c(-Inf, 5, 10, 15, Inf),
      labels = c("<5.0%", "5.0-9.9%", "10.0-14.9%", "≥15.0%")
    )
  )

## ---- Perfom automatic cross-validation --------------------------------------
auto_cv_muac <- autoKrige.cv(
  formula = est ~ 1,
  input_data = wrangled_muac,
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
auto_cv_muac_stats <- auto_cv_muac[[1]] |>
  as_tibble() |>
  summarise(
    mean_error = mean(residual, na.rm = TRUE), ## be as close to zero as possible
    MSPE = mean(residual^2, na.rm = TRUE), ## Ideally small
    MSNR = mean(zscore^2, na.rm = TRUE), ## Mean squared normalized error, should be close to 1
    r2_obspred = cor(observed, observed - residual, use = "complete.obs"), ## Ideally 1
    r2_predobs = cor(observed - residual, residual, use = "complete.obs") ## Ideally should be close to 0
  )

### --------------------------------------------- Plot predicted ~ observed ----
ggplot(auto_cv_muac[[1]], aes(x = var1.pred, y = observed)) +
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
    data = auto_interp_muac$krige_output,
    aes(fill = var1.pred.cat, x = x, y = y)
  ) +
  scale_fill_manual(
      values = apply_ipc_colours(indicator = "muac", .map_type = "static"),
      name = "", 
      na.translate = FALSE
  ) +
  geom_sf(
    data = st_cast(uga4_county, "MULTILINESTRING"),
    linewidth = 0.2,
    color = "grey"
  ) +
  labs(
    title = "Surface map of the predicted prevalence of GAM by MUAC"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(colour = "#706E6D", size = 10)
  )

### Interactive map ----
auto_interp_muac[[1]] |>
  mapview(
    alpha = 1,
    alpha.regions = 0.2,
    col.regions = apply_ipc_colours(.map_type = "interactive", indicator = "muac"),
    na.color = "transparent",
    trim = TRUE
  )

### -------------------------------------------- Predicting standard errors ----

#### Interpolate standardized standard errors ----
auto_zse_muac <- krige(
  formula = zscore ~ 1, 
  locations = auto_cv_muac[[1]] |> filter(!is.na(zscore)), 
  nmin = 3, 
  nmax = 4, 
  model = auto_exp_variogram_muac[[2]], 
  newdata = grid
)

#### Surface map of standardized prediction standard errors ----
ggplot() +
  geom_stars(
    data = auto_zse_muac,
    aes(fill = var1.pred, x = x, y = y)
  ) +
  scale_fill_gradient2(
    low = "#5E3C99",
    mid = "white",
    high = "#008837",
    midpoint = 0, 
    na.value = NA,
    name = ""
  ) +
  geom_sf(
    data = st_cast(uga2_district, "MULTILINESTRING"), 
    linewidth = 0.5,
    color = "orange"
  ) +
  geom_sf(
    data = st_cast(uga4_county, "MULTILINESTRING"),
    linewidth = 0.2,
    color = "grey"
  ) +
  geom_sf_text(
    data = uga2_district,
    mapping = aes(label = factor(ADM2_EN)),
    show.legend = FALSE,
    color = "black",
    size = 3,
  ) +
  geom_sf(
    data = wrangled_wfhz,
    size = 1.0, 
    color = "#7FB3D5",
  ) +
  labs(
    title = "Surface map of the standardized prediction standard errors of GAM by MUAC"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(colour = "#706E6D", size = 11)
  )

### ------------------------------------------------------- Get areal means ----
#### At district level (ADM2_EN) ----
auto_pred_mean_district_muac <- krige(
  formula = est ~ 1,
  locations = wrangled_muac,
  nmin = 3,
  nmax = 4,
  model = auto_exp_variogram_muac[[2]],
  newdata = uga2_district
) |> 
  mutate(
    var1.pred.cat = cut(
      x = var1.pred,
      breaks = c(-Inf, 5, 10, 15, Inf),
      labels = c("<5.0%", "5.0-9.9%", "10.0-14.9%", "≥15.0%"), 
      include.lowest = TRUE
    )
  )

##### Cloropleth map of the mean predicted prevalence at district level ----
ggplot() +
  geom_sf(
    data = auto_pred_mean_district_muac,
    aes(fill = var1.pred.cat),
    color = "black",
    size = 0.2
  ) +
    scale_fill_manual(
      values = apply_ipc_colours(indicator = "muac", .map_type = "static"),
      name = "", 
      na.translate = FALSE
  ) +
  geom_sf(
    data = uga2_district,
    fill = NA,
    color = "#F2F3F4",
    size = 0.8
  ) +
  geom_sf_text(
    data = uga2_district,
    mapping = aes(label = factor(ADM2_EN)),
    show.legend = FALSE,
    color = "#34495E",
    size = 3,
  ) +
  labs(
    title = "Mean predicted prevalence of GAM by MUAC at district level",
    fill = "Predicted Values"
  ) +
  theme(
    plot.title = element_text(size = 9)
  ) +
  theme_void()

##### Get minimum and maximum predicted prevalence values by district -----
auto_min_max_muac <- auto_interp_muac[[1]] |>
  st_as_sf() |>
  st_join(uga2_district, left = FALSE) |> # each grid cell to a polygon
  group_by(ADM2_EN) |>
  summarise(
    min_value = min(var1.pred, na.rm = TRUE),
    max_value = max(var1.pred, na.rm = TRUE)
  )

##### Compare mean predicted prevalence against original survey results -----
auto_pred_vs_original_muac <- muac_data |>
  rename(cluster = enum_area) |>
  mw_estimate_prevalence_muac(
    wt = NULL,
    edema = child_oedema,
    .by = district
  ) |>
  select(district, gam_p) |>
  arrange(factor(district)) |>
  mutate(
    survey = gam_p * 100,
    interp = auto_pred_mean_district_muac[["var1.pred"]],
    bias = interp - survey,
    min_interp = min_max$min_value,
    max_interp = min_max$max_value
  ) |>
  select(-gam_p)

#### At county level (ADM4_EN) ----
auto_pred_mean_county_muac <- krige(
  formula = est ~ 1,
  locations = wrangled_muac,
  nmin = 3,
  nmax = 4,
  model = auto_exp_variogram_muac[[2]],
  newdata = uga4_county
) |> 
  mutate(
    var1.pred.cat = cut(
      x = var1.pred,
      breaks = c(-Inf, 5, 10, 15, Inf),
      labels = c("<5.0%", "5.0-9.9%", "10.0-14.9%", "≥15.0%"), 
      include.lowest = TRUE
    )
  )

#### Cloropleth map of the mean predicted prevalence at county level ----
ggplot() +
  geom_sf(
    data = auto_pred_mean_county_muac,
    aes(fill = var1.pred.cat),
    color = "black",
    size = 0.2
  ) +
    scale_fill_manual(
      values = apply_ipc_colours(indicator = "muac", .map_type = "static"),
      name = "", 
      na.translate = FALSE
  ) +
  geom_sf(
    data = uga4_county,
    fill = NA,
    color = "#F2F3F4"
  ) +
  geom_sf(
    data = uga2_district,
    fill = NA,
    color = "#3F4342",
    size = 0.8
  ) +
  geom_sf_text(
    data = uga2_district,
    mapping = aes(label = factor(ADM2_EN)),
    show.legend = FALSE,
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
