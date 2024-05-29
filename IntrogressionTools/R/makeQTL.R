#' @export
make_QTL <- function(input, threshold){
  ex <- input %>% group_by(Chrom) %>% mutate(ID = rleid(P < threshold)) %>% ungroup()
  ex <- ex %>% filter(P < threshold & is.na(P)==F) %>% group_by(Chrom, ID) %>% summarise(QTLStart = min(Pos), QTLEnd = max(Pos), est_effect = mean(Effect))
  ex <- ex %>% mutate(length = QTLEnd - QTLStart)
  ex <- ex %>% filter(length > 0)
  return(ex)
}
