# options(download.file.method = "wget"); # For Ubuntu 14.04
package <- function(p) { 
  if (!is.element(p, installed.packages()[,1])) {
    install.packages(p); 
  }
  library(p, character.only = TRUE)
} # loads packages with automatical install if needed

package('gdata')

setwd("/var/www/beliefs_climate_policies")

communes_agglo <- read.xls("table-appartenance-geo-communes-18_V2.xls", pattern="CODGEO") # 2018
communes_PLM <- read.xls("table-appartenance-geo-communes-18_V2.xls", sheet=2, pattern="CODGEO") # Paris Lyon Marseille
communes_data <- read.csv("correspondance-code-insee-code-postal.csv", sep=";") # 2013
communes_agglo <- communes_agglo[,c('CODGEO', 'TUU2015')]
communes_PLM <- communes_PLM[,c('CODGEO', 'TUU2015')]
colnames(communes_agglo) <- c('Code.INSEE', 'taille_agglo')
colnames(communes_PLM) <- c('Code.INSEE', 'taille_agglo')
communes_agglo$Code.INSEE <- as.character(communes_agglo$Code.INSEE)
communes_PLM$Code.INSEE <- as.character(communes_PLM$Code.INSEE)
communes_data <- communes_data[,c('Code.INSEE', "Code.Postal", 'Population')]
communes <- merge(merge(communes_agglo, communes_PLM, all=T), communes_data, all=T)
sum(communes$Population[is.na(communes$taille_agglo)], na.rm=T) # 750k missing because of Code.INSEE renaming
taille_agglo <- aggregate(1000*Population ~ taille_agglo, communes, sum)
colnames(taille_agglo) <- c('taille_agglo', 'pop')
taille_agglo$share <- taille_agglo$pop / sum(taille_agglo$pop)
taille_agglo # total: 63.76 M
taille_agglo$share[taille_agglo$taille_agglo==0] # rural
sum(taille_agglo$share[taille_agglo$taille_agglo<=3 & taille_agglo$taille_agglo>0]) # <20k
sum(taille_agglo$share[taille_agglo$taille_agglo<=5 & taille_agglo$taille_agglo>3]) # <100k
sum(taille_agglo$share[taille_agglo$taille_agglo<8 & taille_agglo$taille_agglo>5]) # >100k
taille_agglo$share[taille_agglo$taille_agglo==8] # Paris
