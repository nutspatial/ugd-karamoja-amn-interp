################################################################################
#                                 READ IN DATASET                              #
################################################################################


## ---- Read in SMART survey dataset -------------------------------------------

### --------------------------------------------- Extract Excel sheet names ----
file_sheets <- excel_sheets("data-raw/anthro-data.xlsx")

### ------------------- Read in household roster to extract GPS coordinates ----
data <- read_xlsx(
  path = "data-raw/anthro-data.xlsx",
  sheet = file_sheets[2]
) |>
  select(-c(parish, `Flag-WHO`, `WHZ-WHO`, Team, ID, HH))

################################ End of workflow ###############################
