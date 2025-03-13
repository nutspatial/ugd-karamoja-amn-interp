#'
#' 
#'  Utility function to apply IPC Acute Malnutriton color codes based indicator 
#' 
#' 
#' 

ipc_colours <- function(indicator = c("wfhz", "muac")){

  # Enforce options in indicator ----
  x <- match.arg(indicator)

  switch (x,
    # Color codes for GAM by MUAC-based thresholds ----
    "muac" = {
      c(
        "<0.05" = "#CDFACD",  
        "0.05-0.09" = "#FAE61E",
        "0.10-0.149" = "#E67800",
        "≥0.15" = "#640000"
      )
    }, 
    # Color codes for GAM by WFHZ-based thresholds ----
    "wfhz" = {
      c(
        "<5.0%" = "#CDFACD",  
        "5.0-9.9%" = "#FAE61E",
        "10.0-14.9%" = "#E67800",
        "15.0-29.9%" = "#C80000",
        "≥30.0%" = "#640000"
      )
    }
  )
}
