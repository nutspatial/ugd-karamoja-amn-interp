################################################################################
#                                INTERPOLATE                                   #
################################################################################

## ---- Create a surface to interpolate on -------------------------------------
grid <- karamoja_admn4 |>
  st_bbox() |>
  st_as_stars(dx = 2000) |>
  st_crop(karamoja_admn4)

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
    limits = c(0, 1.055 * max(v0$dist)),
    breaks = seq(0, 1.055 * max(v0$dist), length.out = 6)
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
  nmax = 4,
  maxdist = 9000
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
ggplot(cv_wfhz, aes(x = var1.pred, y = observed)) +
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

## ---- Interpolate ------------------------------------------------------------
interp <- krige(
  formula = est ~ 1,
  locations = wrangled_wfhz,
  nmin = 3,
  nmax = 4,
  model = empirical_variogram_wfhz,
  newdata = grid
)

### ------------------------------------------------- Visualize map surface ----

#### Static map ----
ggplot() +
  geom_stars(
    data = interp,
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
interp |>
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
pred_mean_admn2 <- krige(
  formula = est ~ 1,
  locations = wrangled_wfhz,
  nmin = 3,
  nmax = 4,
  model = empirical_variogram_wfhz,
  newdata = karamoja_admn2
)

##### Cloropleth map of the mean predicted prevalence at district level ----
ggplot() +
  geom_sf(data = pred_mean_admn2, aes(fill = var1.pred), color = "black", size = 0.2) +
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
min_max <- interp |>
  st_as_sf() |>
  st_join(karamoja_admn2, left = FALSE) |> # each grid cell to a polygon
  group_by(ADM2_EN) |>
  summarise(
    min_value = min(var1.pred, na.rm = TRUE),
    max_value = max(var1.pred, na.rm = TRUE)
  )

##### Compare mean predicted prevalence against original survey results -----
pred_vs_original <- wfhz_data |>
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
    interp = pred_mean_admn2$var1.pred,
    bias = interp - survey,
    min_interp = min_max$min_value,
    max_interp = min_max$max_value
  ) |>
  select(-gam_p)

#### At county level (ADM4_EN) ----
pred_mean_admn4 <- krige(
  formula = est ~ 1,
  locations = wrangled_wfhz,
  nmin = 3,
  nmax = 4,
  model = empirical_variogram_wfhz,
  newdata = karamoja_admn4
)

#### Cloropleth map of the mean predicted prevalence at county level ----
ggplot() +
  geom_sf(
    data = pred_mean_admn4,
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