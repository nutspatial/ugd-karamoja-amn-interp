################################################################################
#                      PREDICT GAM TO UNSURVEYED LOCATIONS                     #
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
v0 <- variogram(
  object = est ~ 1,
  data = wrangled_wfhz,
  cutoff = dist_max,
  width = dist_min
)

#### Plot experimental variogram ----
ggplot(v0, aes(x = dist, y = gamma)) +
  geom_point(color = "#566573") +
  scale_x_continuous(
    limits = c(0, 1.055 * max(v0$dist)), 
    breaks = seq(0, 1.055 * max(v0$dist), length.out = 6)
  ) +
  scale_y_continuous(
    limits = c(0, 0.020)
  ) +
  labs(
    x = "Distance (h)", 
    y = expression(gamma(h))
  )

### --------------------------------------------------- Fit variogram model ----
v <- fit.variogram(
  object = v0,
  model = vgm(model = c("Exp", "Sph", "Gau", "Mat"))
)

#### Plot variogram ----
v_model <- variogramLine(v, maxdist = max(v0$dist), n = 100)
ggplot() +
  geom_point(
    data = v0, 
    aes(x = dist, y = gamma), 
    color = "#566573", 
    size = 1.2
  ) +
  scale_x_continuous(
    limits = c(0, 1.055 * max(v0$dist)), 
    breaks = seq(0, 1.055 * max(v0$dist), length.out = 6)
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

### ---------------------------------- Cross-validate: leave-one-out method ----
cv_wfhz <- krige.cv(
  formula = est ~ 1, 
  model = v,
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

### --------------------------------- A scatterplot of predicted ~ observed ----
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
  model = v, 
  newdata = grid
)

### ------------------------------------------------- Visualize map surface ----
ggplot() + 
  geom_stars(
    data = interp, 
    aes(fill = var1.pred, x = x, y = y)
  ) + 
  scale_fill_viridis_c(option = "viridis", na.value = "transparent") + 
  geom_sf(
    data = st_cast(karamoja_admn4, "MULTILINESTRING"), 
    linewidth = 0.2, 
    color = "grey"
  ) +
  theme_void()

### -------------------------------------------- Predicting standard errors ----



### ------------------------------------------------------- Get areal means ----
#### At district level ----
pred_mean_admn3 <- krige(
  formula = est ~ 1, 
  locations = wrangled_wfhz,
  nmin = 3, 
  nmax = 4,
  model = v, 
  newdata = karamoja_admn2
)

#### Cloropleth map of the mean predicted prevalence at district level ----
ggplot() +
  geom_sf(data = pred_mean_admn3, aes(fill = var1.pred), color = "black", size = 0.2) +
  scale_fill_gradientn(colors = custom_colors) +
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
    label.size = 1,
    size = 3,
  ) +
  labs(
    title = "Mean predicted prevalence at district level",
    fill = "Predicted Values"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 9, colour = "#706E6D")
  ) +
  theme_void()

#### At county level ----
pred_mean_admn4 <- krige(
  formula = est ~ 1, 
  locations = wrangled_wfhz,
  nmin = 3, 
  nmax = 4,
  model = v, 
  newdata = karamoja_admn4
)

#### Cloropleth map of the mean predicted prevalence at county level ----
ggplot() +
  geom_sf(data = pred_mean_admn4, aes(fill = var1.pred), color = "black", size = 0.2) +
  scale_fill_gradientn(colors = custom_colors) +
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
    label.size = 1,
    size = 3,
  ) +
  labs(
    title = "Mean predicted prevalence at county level",
    fill = "Predicted Values"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 9, colour = "#706E6D")
  ) +
  theme_void()
