#Scale each 

#' @name scalePhenos
#' @title scale phenotypes by stddev for each  combination of experiment facility and variable
#' @param dat is a dataframe in long format
#' @param pheno is a character vector of phenotypes
#' @param classifier is a character vector of classifying columns in the dataframe (exp, facility, etc)
#' @param lineid the name of the column that contains Accessions (e.g. SALK lines or CS numbers)
#' @param center passed to scale
#' @param scale passed to scale
#' @export
#'

scalePhenos <- function(dat, pheno, classifier, lineid="accession") {

    dat <- dat[dat$variable %in% pheno,]
    dat <- dat[!is.na(dat$value),] #don't mess with NAs

    filter.cond <- paste0("grepl('60000|70000|col|COL|columbia|Columbia',",lineid,")")
    select.cond <- paste0(c(classifier,"variable","value"))
    group.cond <-  paste0(c(classifier,"variable"))

    #get summary stats by classifiers+variable
    ExpFacStats = dat%>%group_by_(.dots=group.cond)%>%
        summarise(std=sd(value),mean=mean(value))

    dat <- merge(dat,ExpFacStats)
    dat$value=dat$value/dat$std
    dat%>%select(-std,-mean)
    
}
