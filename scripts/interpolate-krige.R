################################################################################
#                      PREDICT GAM TO UNSURVEYED LOCATIONS                     #
################################################################################

## ---- Create a surface to interpolate on -------------------------------------
grid <- karamoja_admn4 |> 
  st_bbox() |> 
  st_as_stars(dx = 5000) |> 
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
##### Convert fitted model to a data frame for plotting ----
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