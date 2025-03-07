################################################################################
#                    RUN PLAUSIBILITY CHECK OF ANTHRO DATA                     #
################################################################################


## ---- WFHZ data --------------------------------------------------------------
quality_wfhz <- wfhz_data |> 
  group_by(district) |> 
  mw_plausibility_check_wfhz(
    sex = sex,
    age = age, 
    weight = weight,
    height = height,
    flags = flag_wfhz
  )

## ---- MUAC data through MUAC-for-age z-scores --------------------------------
quality_mfaz <- muac_data |> 
  group_by(district) |> 
  mw_plausibility_check_mfaz(
    sex = sex,
    muac = muac,
    age = age, 
    flags = flag_mfaz
  )

################################ End of workflow ###############################