
#' @export
Intro_test_map <- function(Chrm, Start1,End1, Phenodata, trait, Genodata){

  INTRO <- dplyr::filter(Genodata, Chrom == Chrm & Start > Start1 & End < End1)

  INTROmean <- INTRO %>% dplyr::group_by(Acc) %>% dplyr::summarise(HWsum = sum(HW_count, na.rm =T), WEsum = sum(WE_count, na.rm = T))
  INTROmean <- INTROmean %>% dplyr::mutate(mean = WEsum/ (HWsum+WEsum))
  INTROB <- dplyr::left_join(Phenodata, INTROmean)
  count = dplyr::filter(INTROB, mean > .5)
  count = nrow(count)
  count2 = dplyr::filter(INTROB, mean < .5)
  count2 = nrow(count2)
  if(count < 2 | count2 < 2){
      return(NA)
    }else{

    mod <- lm(get(trait)~mean, data = INTROB) %>% summary()

  if(dim(mod[["coefficients"]])[1]==1){
      return(NA)
    }else{
      p = mod[["coefficients"]][2,4]
      change <- mod[["coefficients"]][2,1]
      return(c(p, change))
    }}}

