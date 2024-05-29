#' @export
check_QTL_interaction <- function(input_QTL, pheno, trait){
results<- data.frame()
for(i in 1:nrow(input_QTL)){
ob <- coreset_geno %>% filter(Chrom == as.character(input_QTL[i,1]) & Start >= as.numeric(input_QTL[i,3]) & End <= as.numeric(input_QTL[i,4])) %>%
  group_by(Acc, WE) %>% summarise(WE_mean = mean(WE_av2, na.rm = T)) %>% left_join(.,pheno) %>% group_by(WE) %>% filter(max(WE_mean)> .8 & min(WE_mean) < .2) %>% ungroup() %>%
  lm(get(trait) ~ WE_mean*WE, data = .)
coef <- summary(ob)[["coefficients"]] %>% as.data.frame()
coef$Chrom <- as.character(input_QTL[i,1])
coef$Start <- as.numeric(input_QTL[i,3])
coef$End <- as.numeric(input_QTL[i,4])
coef$ID <- row.names(coef)
colnames(coef)[4] <- "P"
results <- rbind(results, coef)

}
return(results)}
