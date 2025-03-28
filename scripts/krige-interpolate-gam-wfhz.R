################################################################################
#                            INTERPOLATE GAM by WFHZ                           #
################################################################################

## ---- Create a surface to interpolate on -------------------------------------
grid <- uga4_county |>
  st_bbox() |>
  st_as_stars(dx = 2000) |>
  st_crop(uga4_county)

## ---- Fit a variogram model --------------------------------------------------
### ------------------------------------------------ Experimental variogram ----

#### Check the maximum and minimum distance between sampling points ----
dist_max <- max(dist(st_coordinates(wrangled_wfhz))) / 2
dist_min <- min(dist(st_coordinates(wrangled_wfhz)))

#### Experimental variogram ----
exp_variogram_wfhz <- variogram(
  object = est ~ 1,
  data = wrangled_wfhz,
  cutoff = dist_max,
  width = dist_min
)

#### Plot experimental variogram ----
ggplot(exp_variogram_wfhz, aes(x = dist, y = gamma)) +
  geom_point(color = "#566573") +
  scale_x_continuous(
    limits = c(0, 1.055 * max(exp_variogram_wfhz$dist)),
    breaks = seq(0, 1.055 * max(exp_variogram_wfhz$dist), length.out = 6)
  ) +
  labs(
    x = "Distance (h)",
    y = expression(gamma(h))
  )

#### Fit variogram model ----
empirical_variogram_wfhz <- fit.variogram(
  object = exp_variogram_wfhz,
  model = vgm(model = c("Exp", "Sph", "Gau", "Mat"))
)

#### Plot variogram ----
v_model <- variogramLine(
  object = empirical_variogram_wfhz,
  maxdist = max(exp_variogram_wfhz$dist),
  n = 100
)
ggplot() +
  geom_point(
    data = exp_variogram_wfhz,
    aes(x = dist, y = gamma),
    color = "#566573",
    size = 1.2
  ) +
  scale_x_continuous(
    limits = c(0, 1.055 * max(exp_variogram_wfhz$dist)),
    breaks = seq(0, 1.055 * max(exp_variogram_wfhz$dist), length.out = 6)
  ) +
  geom_line(
    data = v_model,
    aes(x = dist, y = gamma),
    color = "#D15310",
    linewidth = 0.6
  ) +
  labs(
    x = "Distance (h)",
    y = expression(gamma(h)),
    title = "Empirical & Fitted Variogram"
  )

### -------------------------------- Cross-validation: leave-one-out method ----
cv_wfhz <- krige.cv(
  formula = est ~ 1,
  model = empirical_variogram_wfhz,
  locations = wrangled_wfhz,
  nmin = 3,
  nmax = 4
)

### ------------------------------------------- Cross-validation statistics ----
cv_wfhz_stats <- cv_wfhz |>
  as_tibble() |>
  summarise(
    mean_error = mean(residual, na.rm = TRUE), ## be as close to zero as possible
    MSPE = mean(residual^2, na.rm = TRUE), ## Ideally small
    MSNR = mean(zscore^2, na.rm = TRUE), ## Mean squared normalized error, should be close to 1
    r2_obspred = cor(observed, observed - residual, use = "complete.obs"), ## Ideally 1
    r2_predobs = cor(observed - residual, residual, use = "complete.obs") ## Ideally should be close to 0
  )

### --------------------------------------------- Plot predicted ~ observed ----
uga_scatterplot_wfhz <- ggplot(cv_wfhz, aes(x = var1.pred, y = observed)) +
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
    x = "Predicted GAM rate (%)",
    y = "Observed GAM rate (%)"
  )

## ---- Interpolate ------------------------------------------------------------
interp_wfhz <- krige(
  formula = est ~ 1,
  locations = wrangled_wfhz,
  nmin = 3,
  nmax = 4,
  model = empirical_variogram_wfhz,
  newdata = grid
) |> 
  mutate(
    var1.pred.cat = cut(
      x = var1.pred,
      breaks = c(-Inf, 5.0, 9.9, 14.9, 29.9, Inf),
      labels = c("<5.0%", "5.0-9.9%", "10.0-14.9%", "15.0-29.9%", "≥30.0%"),
      include.lowest = TRUE
    )
  )

### ------------------------------------------------- Visualize map surface ---- 

#### Static map ----
uga_surface_wfhz <- ggplot() +
  geom_stars(
    data = interp_wfhz,
    aes(fill = var1.pred.cat, x = x, y = y)
  ) +
    scale_fill_manual(
      values = apply_ipc_colours(indicator = "wfhz", .map_type = "static"),
      name = "", 
      na.translate = FALSE
  ) +
  geom_sf(
    data = st_cast(uga4_county, "MULTILINESTRING"),
    linewidth = 0.2,
    color = "grey"
  ) +
  # labs(
  #   title = "A surface map of the predicted prevalence of GAM by WHZ"
  # ) +
  # theme(
  #   plot.title = element_text(colour = "#706E6D", size = 10)
  # ) +
  theme_void()

#### Interactive map ----
mv_wfhz <- interp_wfhz |>
  mapView(
    alpha = 1,
    alpha.regions = 0.8,
    col.regions = apply_ipc_colours(.map_type = "interactive", indicator = "wfhz"),
    na.color = "transparent",
    trim = TRUE
  )

#### Save `HTML` file ----
mapshot2(
  x = mv_wfhz,
  title = "Interactive of the interpolated map surface of GAM by WFHZ", 
  selfcontained = TRUE,
  url = "surface-map-GAM-by-WFHZ.html"
)

### -------------------------------------------- Prediction standard errors ----

#### Interpolate standardized standard errors ----
zse_wfhz <- krige(
  formula = zscore ~ 1, 
  locations = cv_wfhz |> filter(!is.na(zscore)), 
  nmin = 3, 
  nmax = 4, 
  model = empirical_variogram_wfhz, 
  newdata = grid
)

#### Surface map of prediction standard errors ----
uga_se_wfhz <- ggplot() +
  geom_stars(
    data = zse_wfhz,
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
  geom_sf(
    data = wrangled_wfhz,
    size = 1.0, 
    color = "#7FB3D5",
  ) +
  geom_sf_text(
    data = uga2_district,
    mapping = aes(label = factor(ADM2_EN)),
    show.legend = TRUE,
    color = "black",
    size = 3,
  ) +
  theme_void()

### ------------------------------------------------------- Get areal means ----

#### At district level (ADM2_EN) ----
pred_mean_district_wfhz <- krige(
  formula = est ~ 1,
  locations = wrangled_wfhz,
  nmin = 3,
  nmax = 4,
  model = empirical_variogram_wfhz,
  newdata = uga2_district
) |> 
  mutate(
    var1.pred.cat = cut(
      x = var1.pred,
      breaks = c(-Inf, 5.0, 9.9, 14.9, 29.9, Inf),
      labels = c("<5.0%", "5.0-9.9%", "10.0-14.9%", "15.0-29.9%", "≥30.0%"),
      include.lowest = TRUE
    )
  )

##### Choropleth map of the mean predicted prevalence at district level ----
uga_choropleth_wfhz_district <- ggplot() +
  geom_sf(
    data = pred_mean_district_wfhz, 
    aes(fill = var1.pred.cat), 
    color = "black", 
    size = 0.2
  ) +
    scale_fill_manual(
      values = apply_ipc_colours(indicator = "wfhz", .map_type = "static"),
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
  theme_void()

##### Get minimum and maximum predicted prevalence values by district -----
min_max <- interp_wfhz |>
  st_as_sf() |>
  st_join(uga2_district, left = FALSE) |> # each grid cell to a polygon
  group_by(ADM2_EN) |>
  summarise(
    min_value = min(var1.pred, na.rm = TRUE),
    max_value = max(var1.pred, na.rm = TRUE), 
    median = median(var1.pred, na.rm = TRUE)
  )

##### Compare mean predicted prevalence against original survey results -----
pred_vs_original_wfhz <- wfhz_data |>
  rename(cluster = enum_area) |>
  mw_estimate_prevalence_wfhz(
    wt = NULL,
    edema = child_oedema,
    .by = district
  ) |>
  select(district, gam_p) |>
  arrange(factor(district)) |>
  mutate(
    survey = gam_p * 100,
    interp = pred_mean_district_wfhz$var1.pred,
    bias = interp - survey,
    min_interp = min_max$min_value,
    max_interp = min_max$max_value,
    median_interp = min_max$median
  ) |>
  select(-gam_p)

#### At county level (ADM4_EN) ----
pred_mean_county_wfhz <- krige(
  formula = est ~ 1,
  locations = wrangled_wfhz,
  nmin = 3,
  nmax = 4,
  model = empirical_variogram_wfhz,
  newdata = uga4_county
) |> 
  mutate(
    var1.pred.cat = cut(
      x = var1.pred,
      breaks = c(-Inf, 5.0, 9.9, 14.9, 29.9, Inf),
      labels = c("<5.0%", "5.0-9.9%", "10.0-14.9%", "15.0-29.9%", "≥30.0%"),
      include.lowest = TRUE
    )
  )

#### Cloropleth map of the mean predicted prevalence at county level ----
uga_choropleth_wfhz_county <- ggplot() +
  geom_sf(
    data = pred_mean_county_wfhz,
    aes(fill = var1.pred.cat),
    color = "black",
    size = 0.2
  ) +
    scale_fill_manual(
      values = apply_ipc_colours(indicator = "wfhz", .map_type = "static"),
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
  theme_void()

################################ End of workflow ###############################
