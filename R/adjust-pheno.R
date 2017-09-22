#
# functions to take a phenotype along with classifying information to create an adjusted phenotype
#  either by the phytometers or by the means in each growth chamber/greenhouse of all s
#
## the "dat" dataframes each function takes are in long, or melted format.

#require(dplyr)

#' @name colcorrect
#' @title Correct phenotypes by the mean of col plants grown in each growth chamber in each Experiment
#' @param dat is a dataframe in long format
#' @param pheno is a character vector of phenotypes
#' @param classifier is a character vector of classifying columns in the dataframe (exp, facility, etc)
#' @param lineid the name of the column that contains Accessions (e.g. SALK lines or CS numbers)
#' @param op which operation to perform. "trans" means translate by the comparison mean. anything else means scale by comparison mean
#' @export
colcorrect <- function(dat, classifier, pheno=NULL, lineid="accession",op="trans") {
  if (!is.null(pheno)) dat <- dat[dat$variable%in%pheno,]
    dat <- dat[!is.na(dat$value),] #don't mess with NAs
  
    filter.cond <- paste0("grepl('60000|70000|Columbia|COL|ancestor',",lineid,")")
    select.cond <- paste0(c(classifier,"variable","value"))
    group.cond <-  paste0(c(classifier,"variable"))

    phytmn <- filter_(dat,filter.cond)%>%
        select_(.dots=select.cond)%>%
        group_by_(.dots=group.cond) %>% summarise_all(funs(mean(.,na.rm=T)))
    names(phytmn)[names(phytmn)=="value"] <- "mean"
#    if ("plantID" %in% names(phytmn)) {phytmn <- phytmn[,-grep("plantID",names(phytmn))]}

   ### adj dat by phytometer means
    select.cond <- paste0(c(classifier,lineid,"variable","value"))

    adjdat <- merge(dat,phytmn)
    adjdat$value <- adjdat$value-adjdat$mean
#    adjdat <- adjdat %>% select_(.dots=select.cond)
    adjdat %>% select(-mean)
}

#' @name phytcorrect
#' @title Correct phenotypes by the mean of the phytometers in each growth chamber in each Experiment
#' @param dat is a dataframe in long format
#' @param pheno is a character vector of phenotypes
#' @param classifier is a character vector of classifying columns in the dataframe (exp, facility, etc)
#' @param lineid the name of the column that contains Accessions (e.g. SALK lines or CS numbers)
#' @param op which operation to perform. "trans" means translate by the comparison mean. anything else means scale by comparison mean
#' @export
phytcorrect <- function(dat, classifier, pheno=NULL, lineid="accession",op="trans") {
  if (!is.null(pheno)) dat <- dat[dat$variable%in%pheno,]
        dat <- dat[!is.na(dat$value),] #don't mess with NAs
    
  filter.cond <- paste0("grepl('CS|COL|ancestor',",lineid,")")
  select.cond <- paste0(c(classifier,"variable","value"))
  group.cond <-  paste0(c(classifier,"variable"))
 ### mean all phyts by classifiers
  phytmn <- filter_(dat,filter.cond)%>% #filter_(.dots=filter.cond2)%>%
      select_(.dots=select.cond)%>%
      group_by_(.dots=group.cond) %>% summarise_all(funs(mean(.,na.rm=T)))
  names(phytmn)[names(phytmn)=="value"] <- "mean"
#        if ("plantID" %in% names(phytmn)) {phytmn <- phytmn[,-grep("plantID",names(phytmn))]}
       
### adj dat by phytometer means
  select.cond <- paste0(c(classifier,lineid,"variable","value"))
  adjdat <- merge(dat,phytmn)
  adjdat$value <- adjdat$value-adjdat$mean
#        adjdat <- adjdat %>% select_(.dots=c(select.cond,"meta.experiment","plantID"))
###        adjdat
  adjdat %>% select(-mean)
}

#this one adjusts the phenotype by the means of all plants in each
#growth chamber
#
#' @name allcorrect
#' @title description Correct phenotypes by the mean of all plants in each growth chamber in each Experiment
#' @param dat is a dataframe in long format
#' @param pheno is a character vector of phenotypes
#' @param classifier is a character vector of classifying columns in the dataframe (exp, facility, etc)
#' @param lineid the name of the column that contains Accessions (e.g. SALK lines or CS numbers)
#' @param op which operation to perform. "trans" means translate by the comparison mean. anything else means scale by comparison mean
#' @export
allcorrect <- function(dat, classifier, pheno=NULL, lineid,op="trans") {

    if (!is.null(pheno)) dat <- dat[dat$variable%in%pheno,]
    dat <- dat[!is.na(dat$value),] #don't mess with NAs
        
    select.cond <- paste0(c(classifier,"variable","value"))
    group.cond <-  paste0(c(classifier,"variable"))
 ### mean all phyts by classifiers
        phytmn <- dat %>%
            select_(.dots=select.cond)%>%
                group_by_(.dots=group.cond) %>% summarise_all(funs(mean(.,na.rm=F)))
        names(phytmn)[names(phytmn)=="value"] <- "mean"  
#    if ("plantID" %in% names(phytmn)) {phytmn <- phytmn[,-grep("plantID",names(phytmn))]}

    
### adj dat by phytometer means
        select.cond <- paste0(c(classifier,lineid,"variable","value"))
        adjdat <- merge(dat,phytmn)
        adjdat$value <- adjdat$value-adjdat$mean
###        adjdat <- adjdat %>% select_(.dots=c(select.cond,"plantID"))
###        adjdat
      adjdat %>% select(-mean)      
    }
