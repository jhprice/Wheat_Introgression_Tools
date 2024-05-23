
#' @export
IntroMap <- function(Phenodata, trait, Genodata, cores, ID, window = 2000000){

  chrom <- unique(Genodata$Chrom)
  doParallel::registerDoParallel(cores)
  datause <- Phenodata %>% dplyr::select(ID, trait) %>% na.omit()

  mapped <- foreach::foreach(k=1:length(chrom), .combine = 'rbind', .packages = 'dplyr', .export = c('Intro_test_map')) %dopar%{
    Qlist <- as.data.frame(matrix(ncol=3))
    colnames(Qlist) <- c("Chrom", "Pos", "P", "Effect")
    j = 1
    for( i in seq(from = 1000000, to = (860000000-window), by = window) ){
      Qlist[j,c(3,4)] <- Intro_test_map(chrom[k], i-1, (i+window+1), datause, trait, Genodata)
      Qlist[j,2] <- i
      Qlist[j,1] <- chrom[k]
      j = j+1
    }
    Qlist}

  doParallel::stopImplicitCluster()
  return(mapped)
}
