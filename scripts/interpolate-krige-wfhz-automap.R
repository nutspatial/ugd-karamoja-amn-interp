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


