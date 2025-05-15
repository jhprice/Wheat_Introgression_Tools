#' @export
Intro_test_map <- function(Chrm, Start1, End1, Phenodata, trait, Genodata, ID) {

  # Filter Genodata for the specified chromosome and region
  INTRO <- dplyr::filter(Genodata, Chrom == Chrm & Start > Start1 & End < End1)

  # Dynamically identify columns based on patterns
  hw_col <- names(Genodata)[stringr::str_detect(names(Genodata), "HW") & stringr::str_detect(names(Genodata), "Count")]
  we_col <- names(Genodata)[stringr::str_detect(names(Genodata), "WE") & stringr::str_detect(names(Genodata), "Count")]

  # If multiple options exist, stop and warn
  if (length(hw_col) != 1 | length(we_col) != 1) {
    stop("Cannot uniquely identify HW_count and WE_count columns.")
  }

  # Group and summarize using dynamically identified columns
  INTROmean <- INTRO %>%
    dplyr::group_by(!!sym(ID)) %>%
    dplyr::summarise(
      HWsum = sum(.data[[hw_col]], na.rm = TRUE),
      WEsum = sum(.data[[we_col]], na.rm = TRUE)
    )

  # Calculate mean and merge with Phenodata
  INTROmean <- INTROmean %>% dplyr::mutate(mean = WEsum / (HWsum + WEsum))
  INTROB <- dplyr::left_join(Phenodata, INTROmean, by = ID)

  # Count records based on mean threshold
  count <- dplyr::filter(INTROB, mean > 0.5) %>% nrow()
  count2 <- dplyr::filter(INTROB, mean < 0.5) %>% nrow()

  # Early exit conditions
  if (count < 2 | count2 < 2) {
    return(NA)
  } else {
    # Fit linear model and extract results
    mod <- lm(get(trait) ~ mean, data = INTROB) %>% summary()

    # Check model coefficients
    if (dim(mod[["coefficients"]])[1] == 1) {
      return(NA)
    } else {
      p <- mod[["coefficients"]][2, 4]
      change <- mod[["coefficients"]][2, 1]
      return(c(p, change))
    }
  }
}
