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

scalePhenos <- function(dat, pheno=NULL, classifier, lineid="accession") {

    dat <- dat[!is.na(dat$value),] #don't mess with NAs
    if (!is.null(pheno)) dat <- dat[dat$variable%in%pheno,]

    select.cond <- paste0(c(classifier,"variable","value"))
    group.cond <-  paste0(c(classifier,"variable"))

    #get summary stats by classifiers+variable
    ExpFacStats = dat%>% dplyr::group_by_at(group.cond)%>%
        dplyr::summarise(std=sd(value),mean=mean(value))

    dat <- merge(dat,ExpFacStats)
    dat$value=dat$value/dat$std
    dat%>%dplyr::select(-std,-mean)
    
}
