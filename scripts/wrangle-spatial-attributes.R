################################################################################
#             WORKFLOW TO WRANGLE DATA THROUGH SPATIAL ATTRIBUTES              #
################################################################################


## ---- Set WFHZ data as an `sf` object  ---------------------------------------
wfhz_data <- wfhz_data |> 
  filter(!flag_wfhz == 1) |> 
  select(enumArea, X, Y, gam) |> 
  filter(!is.na(X)) |> 
  st_as_sf(
    coords = c("X", "Y"),
    dim = "XY"
  ) |> 
  st_set_crs(value = "EPSG:4326")

## ---- Workflow to calculate Spatial Empirical Bayesian Rates (SEBSR) ---------
dissolved_wfhz_data <- wfhz_data |> 
  mutate(
    long_x = st_coordinates(geometry)[, 1], 
    lati_y = st_coordinates(geometry)[, 2]
  ) |>  
  group_by(enumArea) |> 
  summarise(
    cases = sum(gam, na.rm = TRUE),
    pop = n(),
    X = mean(long_x, na.rm = TRUE),
    Y = mean(lati_y, na.rm = TRUE)
  ) |> 
  as_tibble() |> 
  select(-geometry) |> 
  st_as_sf(
    coords = c("X", "Y"),
    dim = "XY", 
    crs = "EPSG:4326"
  ) |> 
  st_transform(crs = st_crs(karamoja))

### ---------------------------------------------- Create Thiessen Polygons ----
voronoi <- dissolved_wfhz_data |> 
  st_union() |> 
  st_voronoi() |> 
  st_collection_extract(type = "POLYGON") |> 
  st_sf()

#### Visualize Thiessen Polygons  ----
voronoi |> 
  ggplot() +
  geom_sf(fill = NA, color = "grey", linetype = "solid") + 
  geom_sf(data = dissolved_wfhz_data, color = "#148F77", size = 1.3) +
  theme_minimal() +
  ggtitle("Thiessen polygons of WFHZ data around Karamoja")

### -------------------------- Calculate spatial weights: K-Near Neighbours ----
spatial_weights <- dissolved_wfhz_data |> 
  knearneigh(k = 4, longlat = T) |> 
  knn2nb(row.names = c[["enumArea"]])

### ------------------------------------------------------- Calculate rates ----
sebsr <- EBlocal(
  ri = c[["cases"]],
  ni = c[["pop"]],
  nb = spatial_weights
)

#### Bind data.frames -----
wrangled_wfhz <- cbind(dissolved_wfhz_data, sebsr)

## ---- Map rates --------------------------------------------------------------
### --------------------------------------------------------- Map raw rates ----

#### Create a categorical variable with custom breakpoints ----
wrangled_wfhz$rate_category <- cut(
  wrangled_wfhz$raw, 
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
  geom_sf(fill = "white", color = "grey") +
  geom_sf(data = wrangled_wfhz, aes(color = rate_category)) +
  scale_color_manual(
    values = custom_colors, 
    name = "Raw rates" 
  ) +
  theme_void() +
  labs(
    title = "Spatial distribution GAM rates by sampling points across the Karamoja region",
    subtitle = "Raw rates: cases / total number of children surveyed"
  ) +
  theme(
    plot.title = element_text(size = 11),
    plot.subtitle = element_text(size = 9, colour = "#706E6D")
  )

### ---------------------------------------------------- Map smoothed rates ----
#### Create a categorical variable with custom breakpoints ----
wrangled_wfhz[["sebsr_cat"]] <- cut(
  wrangled_wfhz[["est"]], 
  breaks = c(-Inf, 0.05, 0.09, 0.149, 0.299, Inf),
  labels = c("<0.05", "0.05-0.09", "0.10-0.149", "0.15-0.299", "≥0.3"),
  include.lowest = TRUE
)

#### Plot ----
ggplot(data = karamoja) +
  geom_sf(fill = "white", color = "grey") +
  geom_sf(data = wrangled_wfhz, aes(color = sebsr_cat)) +
  scale_color_manual(
    values = custom_colors, 
    name = "Smoothed rates"
  ) +
  theme_void() + 
  labs(
    title = "Spatial distribution of smoothed GAM rates by sampling points across the Karamoja region",
    subtitle = "Rates smoothed using Spatial Empirical Bayesian"
  ) +
  theme(
    plot.title = element_text(size = 11),
    plot.subtitle = element_text(size = 9, colour = "#706E6D")
  )

## ---- Set data as an `sf` object and reproject CRS (MUAC) --------------------
# muac_data <- muac_data |> 
#   filter(!flag_mfaz == 1) |> 
#   select(enumArea, X, Y, gam) |> 
#   filter(!is.na(X)) |> 
#   st_as_sf(
#     coords = c("X", "Y"),
#     dim = "XY"
#   ) |> 
#   st_set_crs(value = "EPSG:4326")

# ## ---- Calculate rates --------------------------------------------------------
# ### ---------------------- Raw rates aggregated at the survey cluster level ----
# m <- muac_data |> 
#   summarise(
#     cases = sum(gam, na.rm = TRUE), 
#     pop = n(),
#     raw_rates = cases / pop,
#     geometry = st_centroid(st_union(geometry)),
#     .by = enumArea
#   )

# ### -------------- Compute aspatial (global) empirical bayes smoothed rates ----
# m[["smoothed_rates"]] <- eb_rates(m[c("cases", "pop")])[["EB.Rate"]]

# ## ---- Map rates --------------------------------------------------------------
# ### --------------------------------------------------------- Map raw rates ----

# #### Create a categorical variable with custom breakpoints ----
# m$rate_category <- cut(
#   m$raw_rates, 
#   breaks = c(-Inf, 0.05, 0.09, 0.149, 0.299, Inf),
#   labels = c("<0.05", "0.05-0.09", "0.10-0.149", "0.15-0.299", "≥0.3"),
#   include.lowest = TRUE
# )

# #### Color-codes ----
# custom_colors <- c(
#   "<0.05" = "#40E0D0",  
#   "0.05-0.09" = "#DFFF00",
#   "0.10-0.149" = "#FFBF00",
#   "0.15-0.299" = "#FF7F50",
#   "≥0.3" = "#f03b20"
# )

# #### Plot ----
# ggplot(data = karamoja) +
#   geom_sf(fill = "white") +
#   geom_sf(data = m, aes(color = rate_category)) +
#   scale_color_manual(
#     values = custom_colors, 
#     name = "Raw Rates" 
#   ) +
#   theme_void()

# ### ---------------------------------------------------- Map smoothed rates ----
# #### Create a categorical variable with custom breakpoints ----
# m$smoothed_rate_category <- cut(
#   m$smoothed_rates, 
#   breaks = c(-Inf, 0.05, 0.09, 0.149, 0.299, Inf),
#   labels = c("<0.05", "0.05-0.09", "0.10-0.149", "0.15-0.299", "≥0.3"),
#   include.lowest = TRUE
# )

# #### Plot ----
# ggplot(data = karamoja) +
#   geom_sf(fill = "white") +
#   geom_sf(data = m, aes(color = smoothed_rate_category)) +
#   scale_color_manual(
#     values = custom_colors, 
#     name = "Smoothed rates"
#   ) +
#   theme_void()
# ################################ End of workflow ###############################
