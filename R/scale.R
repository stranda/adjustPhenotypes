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

scalePhenos = function(phenolong)
{
    ExpFacStats = phenolong%>%group_by(experiment,facility,variable)%>%
        summarise(std=sd(value),mean=mean(value)) %>% 
            arrange(variable,experiment,facility)
    phenolong = merge(phenolong,ExpFacStats)
    phenolong$value=phenolong$value/phenolong$std
    phenolong%>%select(-std,-mean)
}

