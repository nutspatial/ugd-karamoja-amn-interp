################################################################################
#            WORKFLOW TO WRANGLE DATA THROUGH SPATIAL ATTRIBUTES               #
################################################################################


## ---- Set data as an `sf` object and reproject CRS (WFHZ) --------------------
wfhz_data <- wfhz_data |> 
  filter(!flag_wfhz == 1) |> 
  select(enumArea, X, Y, gam) |> 
  filter(!is.na(X)) |> 
  st_as_sf(
    coords = c("X", "Y"),
    dim = "XY"
  ) |> 
  st_set_crs(value = "EPSG:4326") |> 
  st_transform(crs = "EPSG:32636")

## ---- Calculate rates --------------------------------------------------------
### ---------------------- Raw rates aggregated at the survey cluster level ----
w <- wfhz_data |> 
  summarise(
    cases = sum(gam, na.rm = TRUE), 
    pop = n(),
    raw_rates = cases / pop,
    geometry = st_centroid(st_union(geometry)),
    .by = enumArea
  )

### -------------- Compute aspatial (global) empirical bayes smoothed rates ----
w[["smoothed_rates"]] <- eb_rates(w[c("cases", "pop")])[["EB.Rate"]]

## ---- Map rates --------------------------------------------------------------
### --------------------------------------------------------- Map raw rates ----

#### Create a categorical variable with custom breakpoints ----
w$rate_category <- cut(
  w$raw_rates, 
  breaks = c(-Inf, 0.05, 0.09, 0.149, 0.299, Inf),
  labels = c("<0.05", "0.05-0.09", "0.10-0.149", "0.15-0.299", "≥0.3"),
  include.lowest = TRUE
)

#### Color-codes ----
custom_colors <- c(
  "<0.05" = "#40E0D0",  
  "0.05-0.09" = "#DFFF00",
  "0.10-0.149" = "#FFBF00",
  "0.15-0.299" = "#FF7F50",
  "≥0.3" = "#f03b20"
)

#### Plot ----
ggplot(data = karamoja) +
  geom_sf(fill = "white") +
  geom_sf(data = w, aes(color = rate_category)) +
  scale_color_manual(
    values = custom_colors, 
    name = "Raw Rates" 
  ) +
  theme_void()

### ---------------------------------------------------- Map smoothed rates ----
#### Create a categorical variable with custom breakpoints ----
w$smoothed_rate_category <- cut(
  w$smoothed_rates, 
  breaks = c(-Inf, 0.05, 0.09, 0.149, 0.299, Inf),
  labels = c("<0.05", "0.05-0.09", "0.10-0.149", "0.15-0.299", "≥0.3"),
  include.lowest = TRUE
)

#### Plot ----
ggplot(data = karamoja) +
  geom_sf(fill = "white") +
  geom_sf(data = w, aes(color = smoothed_rate_category)) +
  scale_color_manual(
    values = custom_colors, 
    name = "Smoothed rates"
  ) +
  theme_void()

## ---- Set data as an `sf` object and reproject CRS (MUAC) --------------------
muac_data <- muac_data |> 
  filter(!flag_mfaz == 1) |> 
  select(enumArea, X, Y, gam) |> 
  filter(!is.na(X)) |> 
  st_as_sf(
    coords = c("X", "Y"),
    dim = "XY"
  ) |> 
  st_set_crs(value = "EPSG:4326") |> 
  st_transform(crs = "EPSG:32636")

## ---- Calculate rates --------------------------------------------------------
### ---------------------- Raw rates aggregated at the survey cluster level ----
m <- muac_data |> 
  summarise(
    cases = sum(gam, na.rm = TRUE), 
    pop = n(),
    raw_rates = cases / pop,
    geometry = st_centroid(st_union(geometry)),
    .by = enumArea
  )

### -------------- Compute aspatial (global) empirical bayes smoothed rates ----
m[["smoothed_rates"]] <- eb_rates(m[c("cases", "pop")])[["EB.Rate"]]

## ---- Map rates --------------------------------------------------------------
### --------------------------------------------------------- Map raw rates ----

#### Create a categorical variable with custom breakpoints ----
m$rate_category <- cut(
  m$raw_rates, 
  breaks = c(-Inf, 0.05, 0.09, 0.149, 0.299, Inf),
  labels = c("<0.05", "0.05-0.09", "0.10-0.149", "0.15-0.299", "≥0.3"),
  include.lowest = TRUE
)

#### Color-codes ----
custom_colors <- c(
  "<0.05" = "#40E0D0",  
  "0.05-0.09" = "#DFFF00",
  "0.10-0.149" = "#FFBF00",
  "0.15-0.299" = "#FF7F50",
  "≥0.3" = "#f03b20"
)

#### Plot ----
ggplot(data = karamoja) +
  geom_sf(fill = "white") +
  geom_sf(data = m, aes(color = rate_category)) +
  scale_color_manual(
    values = custom_colors, 
    name = "Raw Rates" 
  ) +
  theme_void()

### ---------------------------------------------------- Map smoothed rates ----
#### Create a categorical variable with custom breakpoints ----
m$smoothed_rate_category <- cut(
  m$smoothed_rates, 
  breaks = c(-Inf, 0.05, 0.09, 0.149, 0.299, Inf),
  labels = c("<0.05", "0.05-0.09", "0.10-0.149", "0.15-0.299", "≥0.3"),
  include.lowest = TRUE
)

#### Plot ----
ggplot(data = karamoja) +
  geom_sf(fill = "white") +
  geom_sf(data = m, aes(color = smoothed_rate_category)) +
  scale_color_manual(
    values = custom_colors, 
    name = "Smoothed rates"
  ) +
  theme_void()
################################ End of workflow ###############################
