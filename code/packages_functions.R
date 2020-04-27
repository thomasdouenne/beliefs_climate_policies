# options(download.file.method = "wget"); # For Ubuntu 14.04
package <- function(p) { 
  if (!is.element(p, installed.packages()[,1])) {
    install.packages(p); 
  }
  library(p, character.only = TRUE)
} # loads packages with automatical install if needed

library(utils)
package("xtable")
package('tidyverse')
package("rms")
package('pwr')
package("foreign")
package("memisc")
package("DT")
package("pastecs")
package("lsr")
package("ggplot2")
package("stringr")
package("survey")
package("plotly")
package('gdata')
package("Hmisc")
package("quantreg")
package("rcompanion")
package("DescTools")
package("VCA")
package("glmnet")
# package("installr") # not for linux
package("plotly")
package("processx")
package("readstata13")
package("permute")
package("AER")
package("ivmodel")
package("rattle")
package("data.table")
package("reshape2")
package("margins")
package("rddtools")
package("rddapp")
# package("mets")
package("plyr")
package("descr")
package("stargazer")
package("clipr")
package("ergm") # wtd.median
package("mfx")
package("margins")
package("plotrix")
package("grDevices")
package("colorspace")
package("RColorBrewer")
package("colorRamps")
package("ordinal")
package("oglmx")
package("logistf")
package("emmeans")
package("ggeffects")
package("snakecase")
package("rdd")
package("corrplot")
package("psy")
package("devtools")
install_github("jdstorey/qvalue")
package("qvalue")
package("car")
# install_github(repo = "MatthieuStigler/RCompAngrist", subdir = "RCompAngrist")
# package("RCompAngrist")


package("lavaan")
# package("psych") # library(psych, exclude = "describe")
# package("semTools")

# package("interplot")
# package("jtools")
# package("effects")
# package("sjplot")
# package("doMC") # for parallel computing, does not work on Windows

# Fs <- function(QID) { s[QID][[1]] }
# Vs <- function(QID) { as.vector(Fs(QID))  } 
n <- function(var) { as.numeric(as.vector(var)) }
NSPs <- function(QID) { length(V(QID)[V(QID) == "NSP (Je ne veux pas répondre)"])/length(V(QID)) }
nsps <- function(id) { length(v(id)[v(id) == "NSP (Je ne veux pas répondre)"])/length(v(id)) }
Label <- function(var) {
  if (length(annotation(var))==1) { annotation(var)[1] }
  else { label(var)  }
}
decrit <- function(variable, miss = FALSE, weights = NULL, numbers=FALSE) { 
  if (length(annotation(variable))>0 & !numbers) {
    if (!miss) {
      # if (is.element("Oui", levels(as.factor(variable))) | grepl("(char)", annotation(variable)) | is.element("quotient", levels(as.factor(variable)))  | is.element("Pour", levels(as.factor(variable))) | is.element("Plutôt", levels(as.factor(variable))) ) { describe(as.factor(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
      # else { describe(variable[variable!="" & !is.na(variable)], weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
      if (length(which(!is.na(suppressWarnings(as.numeric(levels(as.factor(variable)))))))==0) { describe(as.factor(variable[variable!=""]), weights = weights[variable!=""], descript=Label(variable)) } # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
      else { describe(as.numeric(as.vector(variable[variable!=""])), weights = weights[variable!=""], descript=Label(variable)) } # avant:  & !is.na(variable)
    }
    else {
      if (length(which(suppressWarnings(!is.na(as.numeric(levels(as.factor(variable)))))))>10) describe(include.missings(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
      else describe(as.factor(include.missings(variable[variable!="" & !is.na(variable)])), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
  }
  else {  
    if (length(annotation(variable))>0) {
      if (miss) describe(variable[variable!=""], weights = weights[variable!=""], descript=Label(variable))
      else describe(variable[variable!="" & !is.missing(variable)], weights = weights[variable!="" & !is.missing(variable)], descript=paste(length(which(is.missing(variable))), "missing obs.", Label(variable)))
    } else describe(variable[variable!=""], weights = weights[variable!=""])  }
}
CImedian <- function(vec) { # 95% confidence interval
  res <- tryCatch(unlist(ci.median(vec[!is.na(vec) & vec!=-1])), error=function(e) {print('NA')})
  return(paste(res[paste('ci.lower')], res[paste('ci.median')], res[paste('ci.upper')], length(which(!is.na(vec) & vec!=-1)))) 
}
clean_number <- function(vec, high_numbers='') { 
   numeric_vec <- as.numeric(gsub(",", ".", gsub("[[:alpha:]  !#$%&')?/(@:;€_-]","",vec)))
   if (high_numbers=='remove') { is.na(numeric_vec) <- numeric_vec>10000 }
   else if (high_numbers=='divide') { numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)] <- numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)]/12 }
   else if (high_numbers=='divide&remove') { 
     numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)] <- numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)]/12
     is.na(numeric_vec) <- numeric_vec>6000 }
   return(numeric_vec)
}
uc <- function(nb_pers, nb_14_et_plus) {
  # https://www.insee.fr/fr/metadonnees/definition/c1802
  return(1 + 0.5 * pmax(0, nb_14_et_plus - 1) + 0.3 * pmax(0, nb_pers - nb_14_et_plus))
}
quotient <- function(nb_pers, nb_adultes) {
	# https://droit-finances.commentcamarche.com/contents/907-quotient-familial-calcul-du-nombre-de-parts-fiscales
	# nb de parts fiscales en fonction de la situation et de nb_pers = 1 / 2 / 3 / 4 / 5
	# marie: x / 2 / 2.5 / 3 / 4 --- en concubinage: x / 1 / 1.5 /2 / 3 (= marie - 1) --- seul: 1 / 2 / 2.5 / 3.5 / 4.5
	return((nb_pers == 1) + (nb_pers == 2)*2 + (nb_pers == 3)*2.5 + (nb_pers == 4)*3 + (nb_pers > 4)*pmin(6, nb_pers - 1) + (nb_adultes==1)*(nb_pers > 3)*0.5 )
}
irpp <- function(rev, nb_adultes, nb_pers) {
	# quotient <- (nb_pers < 2) + (nb_pers == 2) * 2 + (nb_pers == 3) * 2.5 + (nb_pers == 4) * 3 + (nb_pers > 4) * pmin(6, nb_pers - 1)
	income <- 0.9334 * rev / quotient(nb_pers, nb_adultes) # (1 + (0.029 * 1.28))*0.9 : passage au brut (+28% en moyenne), CSG+CRDS non déductibles (2,90%), puis abattement de 10%
	ir <- 0
	ir <- ir + (income - 12815.25*12) * 0.45 * (income > 12815.25*12)
	ir <- ir + (pmin(income, 12676*12) - 6051.42*12) * 0.41  * (income > 6051.42*12)
	ir <- ir + (pmin(income, 6051.42*12) - 2257.17*12) * 0.3  * (income > 2257.17*12)
	ir <- ir + (pmin(income, 2257.17*12) - 817.25*12) * 0.14  * (income > 817.25*12)
	
	ir <- quotient(nb_pers, nb_adultes) * ir
	seuil_decote <- (nb_adultes>1)*2585/12 + (nb_adultes<=1)*1569/12
	# decote <- (1920 - 0.75 * ir) * (marie & ir<2560) + (1165 - 0.75 * ir) * (!(marie) & ir<1553)
	decote <- (ir < seuil_decote) * 0.75 * (seuil_decote - ir)
	return(pmax((ir-decote),0)) # vrai calcul
}



##### Graphiques #####
stack_bars <- function(vars, data=s, miss=T, labels=NA, title=NA, accord=FALSE, en = FALSE, margin=c(2.5,17,0,3), cex=1, width=0.77/length(vars), weights=FALSE) {
  matrice <- c()
  colors <-   c(rainbow(4, end=4/15)[1:3], "green", "forestgreen") # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  for (var in vars) {
    if (miss) {
      mat <- c(length(which(data[[var]]==-2))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==-1))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==0))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==1))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==2))/length(which(!is.missing(data[[var]]))),length(which(is.missing(data[[var]]) & !is.na(data[[var]])))/length(which(!is.missing(data[[var]]) & !is.na(data[[var]]))))
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]==-2)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==-1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==0)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==2)])/sum(data[['weight']][!is.missing(data[[var]])]),sum(data[['weight']][which(is.missing(data[[var]]) & !is.na(data[[var]]))])/sum(data[['weight']][!is.missing(data[[var]])])) }
      colors <- c(colors, "lightgrey")    }
    else {
      mat <- c(length(which(data[[var]]==-2))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==-1))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==0))/length(which(!is.missing(data[[var]]))),  length(which(data[[var]]==1))/length(which(!is.missing(data[[var]]))),  length(which(data[[var]]==2))/length(which(!is.missing(data[[var]]))))    
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]==-2)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==-1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==0)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==2)])/sum(data[['weight']][!is.missing(data[[var]])])) } }
    matrice <- c(matrice, mat) }
  matrice <- matrix(c(matrice), ncol=length(vars))
  if (is.na(labels)) { labels <- vars }
  if (accord) { values <- c("Pas du tout", "Pas vraiment d'accord", "Indifférent-e", "Assez", "Tout à fait d'accord")
  if (miss) { widths <- c(0.16,0.16,0.13,0.125,0.145,0.05) }
  else { widths <- c(0.18,0.185,0.15,0.14,0.2) } }
  else { values <- c("Baisser fortement", "légèrement", "Maintenir", "Augmenter légèrement", "fortement")
  if (miss) { widths <- c(0.153,0.14,0.14,0.15,0.083,0.05) }
  else { widths <- c(0.173,0.16,0.165,0.19,0.095) } }
  if (en) {values <- c("Strongly decrease", "Slightly decrease", "Maintain", "Slightly increase", "Strongly increase")
  if (accord) values <- c("Totally disagree", "Disagree", "Indifferent", "Agree", "Totally agree")
  if (miss) { widths <- c(0.16,0.15,0.14,0.13,0.12,0.06) }
  else { widths <- c(0.173,0.16,0.165,0.19,0.095) } }
  if (miss) { 
    if (en) values <- c(values, "PNR")
    else values <- c(values, "NSP") }
  # if (accord) { values <- c("Pas du tout d'accord", "Pas vraiment d'accord", "Indifférent-e", "Assez d'accord", "Tout à fait d'accord") }
  # else { values <- c("Baisser fortement", "Baisser légèrement", "Maintenir au niveau actuel", "Augmenter légèrement", "Augmenter fortement") }
  # if (miss) { values <- c(values, "NSP (Ne sait pas, ne se prononce pas)")} # TODO: trouver widths pour ceux-là et les mettre
  before_par <- par()
  titre <- 0
  if (!is.na(title)) { titre <- 1.5 }
  par(mar=margin, oma=c(0,0,titre,0))
  frame()
  abline(v=seq(0,1,by=0.1), lty=3, col="grey")
  axis(1, at = seq(0,1,by=0.1))
  barplot(matrice, width=width, horiz=TRUE, add=TRUE, col=colors, names.arg = labels, cex.names = cex, border=NA, ylim=c(0,1), legend.text=values, las=1, args.legend=list(horiz=TRUE, bty='o', box.lwd=0, xjust=1, text.width=widths, x.intersp=0.3, x="topright")) # ncol=3, inset=-0.3
  title(title, outer=TRUE)
  par(before_par)
  # legend("topright", fill=colors, legend=values, ncol=2)
}
oui_non <- function(vars, file, labels = vars, data = s, display_value = T, sort=T, colors=color(2), weights=T, margin_r=0, margin_l=NA, title="", en=FALSE, NSP=FALSE) { # 250 l
  margin_t <- 30
  if (title!="") { margin_t <- 80 }
  if (grepl("<br>", title)) { margin_t <- 130 }
  if (is.na(margin_l)) { margin_l <- 4.7*max(nchar(labels)/(1 + str_count(labels, '<br>'))) }
  oui <- non <- nsp <- c()
  for (var in vars) {
    if (weights) {
      oui <- c(oui, sum(data[['weight']][which(data[[var]]==T | data[[var]]=="Oui" | data[[var]]=="Pour" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="quotient")])/sum(data[['weight']][which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")]) )
      non <- c(non, sum(data[['weight']][which(data[[var]]==FALSE | data[[var]]=="Non" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="indiv")])/sum(data[['weight']][which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")]) )
      nsp <- c(nsp, sum(data[['weight']][which(data[[var]]=="NSP" | data[[var]]==-1)])/sum(data[['weight']][which(data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")]) ) #  | data[[var]]==-1 | data[[var]]=="NSP"
    }
    else {
      oui <- c(oui, length(which(data[[var]]==T | data[[var]]=="Oui" | data[[var]]=="Pour" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="quotient"))/length(which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")) )
      non <- c(non, length(which(data[[var]]==FALSE | data[[var]]=="Non" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="indiv"))/length(which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")) )
      nsp <- c(nsp, length(which(data[[var]]=="NSP" | data[[var]]==-1))/length(which(data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")) )
    }  
  }
  true_nsp <- round(100 * nsp*(oui+non))
  oui <- round(100 * oui)
  non <- round(100 * non)
  nsp <- round(100 * nsp)
  if (sort) order_as <- order(oui/(oui+non))
  else order_as <- 1:length(oui)
  y <- labels[order_as]
  non <- non[order_as]
  nsp <- nsp[order_as]
  true_nsp <- true_nsp[order_as]
  if (sort) oui <- sort(oui)
  o <- round(100 * oui / (oui + non))
  n <- round(100 * non / (oui + non))
  
  if (en==T) {
    hover_oui <- paste('Yes<br>', oui, '% of answers<br>', o, '% of expressed answers')
    hover_non <- paste('No<br>', non, '% of answers<br>',n, '% of expressed answers')
    hover_nsp <- paste('PNR<br>', true_nsp, '% of answers')  
    Text <- c("Yes", "No", "PNR")      }
  else if (en==FALSE) {
    hover_oui <- paste('Oui<br>', oui, '% des réponses<br>', o, '% des réponses exprimées')
    hover_non <- paste('Non<br>', non, '% des réponses<br>',n, '% des réponses exprimées')
    hover_nsp <- paste('NSP<br>', true_nsp, '% des réponses')  
    Text <- c("Oui", "Non", "NSP") }
  else {
    hover_oui <- paste('Oui<br>', oui, '% des réponses<br>', o, '% des réponses exprimées')
    hover_non <- paste('Non<br>', non, '% des réponses<br>',n, '% des réponses exprimées')
    hover_nsp <- paste('NSP<br>', true_nsp, '% des réponses')  
    Text <- en
    if (length(Text ==2)) Text <- c(Text, 'PNR')}
  if (display_value) {
    hover_oui <- paste(oui, '%')
    hover_non <- paste(non, '%')
    hover_nsp <- paste(true_nsp, '%')
  }
  if (!(NSP)) Text[3] <- ''
  print(oui)
  print(non)
  print(nsp)
  print(o)
  print(n)
  data <- data.frame(y, oui, non, nsp, o, n)
  data$y <- factor(data$y, levels = data[["y"]])
  y <- c(y, '')
  bars <- plot_ly(data, x = ~o, y = ~y, type = 'bar', orientation = 'h', text = hover_oui, textposition = 'auto', # last one displays values; colors were forestgreen and darkred
                  hoverinfo = 'text', marker = list(color = colors[1], line = list(color = 'white', width = 1))) %>%
    add_trace(x = ~n, text = hover_non, hoverinfo = 'text', marker = list(color = colors[2])) %>%
    add_trace(x = ~nsp, text = hover_nsp, hoverinfo = 'text', marker = list(color = 'lightgrey')) %>%
    layout(xaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE,
                        domain = c(0.15, 1)),
           yaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE),
           hovermode = 'closest',
           barmode = 'stack',
           title = title,
           titlefont = list(color='black'),
           font = list(color='black'),
           # paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
           margin = list(l = margin_l, r = margin_r, t = margin_t, b = 0),
           showlegend = FALSE) %>%
    # labeling the y-axis
    add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
                    xanchor = 'right',
                    text = y,
                    font = list(family = 'Arial', size = 14, color = 'black'),
                    showarrow = FALSE, align = 'right') %>%
    # labeling the first Likert scale (on the top)
    add_annotations(xref = 'x', yref = 'paper',
                    x = c(10, 90, 110),
                    y = 1.1,
                    text = Text,
                    font = list(family = 'Arial', size = 15, color = 'black'),
                    showarrow = FALSE) # %>%
  # labeling the percentages of each bar (x_axis)
  # add_annotations(xref = 'x', yref = 'y',
  #                 x = o / 2, y = y,
  #                 text = paste(data[,"oui"], '%'),
  #                 font = list(family = 'Arial', size = 14, color = 'white'),
  #                 showarrow = FALSE) %>%
  # add_annotations(xref = 'x', yref = 'y',
  #                 x = o + n / 2, y = y,
  #                 text = paste(data[,"non"], '%'),
  #                 font = list(family = 'Arial', size = 14, color = 'white'),
  #                 showarrow = FALSE) %>%
  # add_annotations(xref = 'x', yref = 'y',
  #                 x = o + n + nsp / 2, y = y,
  #                 text = paste(data[,"nsp"], '%'),
  #                 font = list(family = 'Arial', size = 14, color = 'white'),
  #                 showarrow = FALSE) %>%
  # api_create(bars, filename=file, sharing="public")
  return(bars) # bugs most often than not
}
data5 <- function(vars, data=s, miss=T, weights=T, rev=FALSE) {
  matrice <- c()
  colors <-  c(rainbow(4, end=4/15), "forestgreen") # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  for (var in vars) {
    if (miss) {
      if (is.null(annotation(data[[var]]))) {
        mat <- c(length(which(n(data[[var]])==-2))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==-1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==0))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==2))/length(which(!is.missing(n(data[[var]])))),length(which(is.na(data[[var]])))/length(which(!is.missing(n(data[[var]]))))) # removed "n()"
        if (weights) { mat <- c(sum(data[['weight']][which(n(data[[var]])==-2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==-1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==0)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]),sum(data[['weight']][which(is.na(data[[var]]))])/sum(data[['weight']][!is.missing(n(data[[var]]))])) } }
      else {
        mat <- c(length(which(n(data[[var]])==-2))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==-1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==0))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==2))/length(which(!is.missing(n(data[[var]])))),length(which(is.missing(data[[var]]) & !is.na(data[[var]])))/length(which(!is.missing(data[[var]])))) # removed "n()"
        if (weights) { mat <- c(sum(data[['weight']][which(n(data[[var]])==-2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==-1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==0)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]),sum(data[['weight']][which(is.missing(data[[var]]) & !is.na(data[[var]]))])/sum(data[['weight']][!is.missing(data[[var]])])) } }
      colors <- c(colors, "lightgrey")    }
    else {
      mat <- c(length(which(n(data[[var]])==-2))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==-1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==0))/length(which(!is.missing(n(data[[var]])))),  length(which(n(data[[var]])==1))/length(which(!is.missing(n(data[[var]])))),  length(which(n(data[[var]])==2))/length(which(!is.missing(n(data[[var]])))))    
      if (weights) { mat <- c(sum(data[['weight']][which(n(data[[var]])==-2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==-1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==0)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==2)])/sum(data[['weight']][!is.missing(n(data[[var]]))])) } }
    matrice <- c(matrice, mat) }
  matrice <- matrix(c(matrice), ncol=length(vars))
  if (rev & !(miss)) return(matrice[5:1,])
  else if (rev & miss) return(matrice[c(5:1,6),])
  else return(matrice)
  # return(as.data.frame(matrice))
}
data1 <- function(vars, data=s, weights=T) {
  res <- c()
  for (var in vars) {
    if (weights) { res <- c(res, sum(data[['weight']][which(data[[var]]==TRUE)])/sum(data[['weight']][which(data[[var]]==TRUE | data[[var]]==FALSE)])) }
    else { res <- c(res, length(which(data[[var]]==T)))/length(which(data[[var]]==T | data[[var]]==FALSE)) }
  }
  return( matrix(res, ncol=length(vars)) )
}
dataN <- function(var, data=s, miss=T, weights = T, return = "", fr=T, rev=FALSE) {
  mat <- c()
  if (is.character(data[[var]]) | (is.numeric(data[[var]]) & !grepl("item", class(data[[var]])))) v <- as.factor(data[[var]])
  else v <- data[[var]]
  if (is.null(annotation(v))) levels <- levels(v)
  else levels <- labels(v)@.Data
  levels <- levels[!(levels %in% c("NSP", "PNR", "Non concerné·e"))]
  for (val in levels) { # before: no %in% nowherer below
    if (weights) mat <- c(mat, sum(data[['weight']][which(v==val)])/sum(data[['weight']][!is.missing(v) & (!(v %in% c("NSP", "Non concerné·e")))]))
    else mat <- c(mat, length(which(v==val))/length(which(!is.missing(v) & (!(v %in% c("NSP", "Non concerné·e")))))) }
  if (rev) mat <- rev(mat)
  if (miss) {
    if (is.null(annotation(v))) {
      if (weights) mat <- c(mat, sum(data[['weight']][which(is.na(v) | v %in% c("NSP", "Non concerné·e"))])/sum(data[['weight']][!is.missing(v) & (!(v %in% c("NSP", "Non concerné·e")))]))
      else mat <- c(mat, length(which(is.na(v) | v %in% c("NSP", "Non concerné·e")))/length(which(!is.missing(v) & (!(v %in% c("NSP", "Non concerné·e"))))))
    } else  {
      if (weights) mat <- c(mat, sum(data[['weight']][which(is.missing(v) & !is.na(v))])/sum(data[['weight']][!is.missing(v)]))
      else mat <- c(mat, length(which(is.missing(v) & !is.na(v)))/length(which(!is.missing(v)))) } }
  if (max(nchar(levels))==3 & 'Oui' %in% levels & 'Non' %in% levels) { if (which(levels=='Non') < which(levels=='Oui')) mat[2:1] <- mat[1:2]; levels[c(which(levels=='Oui'),which(levels=='Non'))] <- c('Non', 'Oui') }
  if ((return %in% c("levels", "legend")) & miss & fr) return(c(levels, 'NSP'))
  else if ((return %in% c("levels", "legend")) & miss & (!(fr))) return(c(levels, 'PNR'))
  else if ((return %in% c("levels", "legend")) & (!(miss))) return(levels)
  else return(matrix(mat, ncol=1))
}
dataKN <- function(vars, data=s, miss=T, weights = T, return = "", fr=T, rev=FALSE) {
  res <- c()
  for (var in vars) res <- c(res, dataN(var, data, miss, weights, return, fr, rev))
  return(matrix(res, ncol=length(vars)))
}
color5 <- c(rainbow(4, end=4/15)[1:3], "#00FF00", "#228B22") # the last two are: green, forestgreen
color <- function(v, grey=FALSE, grey_replaces_last = T, rev_color = FALSE, theme='RdBu') {
  if (is.matrix(v)) n <- nrow(v)
  else if (length(v) > 1) n <- length(v)
  else n <- v # cf. http://research.stowers.org/mcm/efg/R/Color/Chart/ColorChart.pdf
  if (grey & grey_replaces_last & n > 1) n <- n-1
  if (theme=='rainbow') {
    if (n == 1) cols <- c("#66B3B3") # "brown": #A52A2A Presentation Teal: #008096 (title) #1A8C8C (dark) #66B3B3 #99CCCC (light)
    else if (n == 2) cols <- c("#66B3B3", "#A52A2A") # c("lightgreen", "plum") = c("#90EE90", "#DDA0DD")
    else if (n == 3) cols <- color5[c(1,3,5)]
    else if (n == 4) cols <- c(rainbow(4, end=4/15)[1:3], "#228B22")
    else if (n == 5) cols <- c(rainbow(4, end=4/15)[1:3], "#00FF00", "#228B22") # the last two are: green, forestgreen
    else if (n == 6) cols <- rainbow(6)
    else if (n == 7) cols <- c("#000000", rainbow(7)[c(1:3,5:7)])
    else cols <- rainbow(n) # diverge_hcl green2red brewer.pal(n, Spectral/RdBu...)  https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
  } else {
    cols <- rev(brewer.pal(n, theme))
    if (n == 1) cols <- cols[1]
    # if (n == 2) cols <- cols[c(1,3)]
    else if (n %% 2 == 0) cols <- rev(brewer.pal(n+2, theme))[c(1:(n/2),(n/2+2):(n+1))] }
  if (rev_color) cols <- rev(cols)
  if (grey & n > 1) return(c(cols, "#D3D3D3")) # lightgrey
  else return(cols)
}
# accord5 <- c("Pas du tout d'accord", "Pas vraiment d'accord", "Indifférent-e", "Assez d'accord", "Tout à fait d'accord")
oui_non5 <- c("Non, pas du tout", "Non, pas vraiment", "Indifférent-e/NSP", "Oui, plutôt", "Oui, tout à fait")
yes_no5 <- c("Not at all", "Not really", "Indifferent/PNR", "Rather yes", "Yes, completely")
# agree5 <- c("Strongly disagree", "Disagree", "Indifferent", "Agree", "Strongly agree")
# evol5 <- c("Baisser fortement", "Baisser légèrement", "Maintenir au niveau", "Augmenter légèrement", "Augmenter fortement")
# evolve5 <- c("Strongly decrease", "Slightly decrease", "Maintain", "Slightly increase", "Strongly increase")
barres <- function(data, file, title="", labels, color=c(), rev_color = FALSE, hover=legend, nsp=TRUE, sort=TRUE, legend=hover, showLegend=T, margin_r=0, margin_l=NA, online=FALSE, display_values=T, thin=FALSE, legend_x=NA, show_ticks=T, xrange=NA) {
  if (length(color)==0) color <- color(data, nsp, rev_color = rev_color)
  margin_t <- 0 + 25*(!(thin))
  if (title!="") { margin_t <- 100 }
  if (grepl("<br>", title)) { margin_t <- 150 }
  legendSize <- 15 # 10, 13
  legendY <- 1.1  + 0.3*thin/(ncol(data)-1) # last term may be problematic
  legendX <- 0.2
  # legendFont <- 'Open Sans'
  if (is.na(margin_l)) { margin_l <- 4.7*max(nchar(labels)/(1 + str_count(labels, '<br>'))) }
  if (max(nchar(labels)) > 25) { legendSize <- 15 } # 9, 13
  # if (max(nchar(labels)) > 50) { legendSize <- 8 }
  # if (max(nchar(labels)) > 60) { legendSize <- 7 }
  if (max(nchar(labels)) > 50) { # 70
    legendSize <- 13 # 11
    # legendY = 1.2
    legendX= -0.2 # 1
    # if (ncol(data)>1) margin_t = 170
  }
  if (!is.na(legend_x)) legendX <- legend_x
  if (!showLegend) { margin_t <- max(0, margin_t - 70) }
  if (ncol(data)==1) legendY = 1.5 + 0.3*thin
  if (sort) {
    agree <- c()
    if (nrow(data)==5 | nrow(data)==6) { for (i in 1:length(labels)) { agree <- c(agree, data[4, i] + data[5, i]) } }
    else if (nrow(data)==7) { for (i in 1:length(labels)) { agree <- c(agree, data[6, i] + data[7, i]) } }
    else { for (i in 1:length(labels)) { agree <- c(agree, data[1, i]) } }
    labels <- labels[order(agree)]
    data <- matrix(data[, order(agree)], nrow=nrow(data))
  }
  if (nrow(data)==1 & sort) {  
    hover <- hover[order(agree)]
    value <- c()
    for (i in 1:length(hover)) { 
      hover[i] <- paste(hover[i], "<br>Choisi dans ", round(100*data[1, i]), "% des réponses", sep="")
      value[i] <- paste(round(100*data[1, i]), '%', sep='') }
    hovers <- matrix(hover, nrow=length(hover))
    values <- matrix(value, nrow=length(hover))
  }
  else {
    hovers <- values <- c()
    if (nsp) {
      for (i in 1:(length(hover)-1)) { 
        for (j in 1:length(labels)) {
          hovers <- c(hovers, paste(hover[i], '<br>', round(100*data[i, j]/(1+data[length(hover), j])), '% des réponses<br>', round(100*data[i, j]), '% des réponses exprimées') )
          values <- c(values, paste(round(100*data[i, j]/(1+data[length(hover), j])), '%', sep=''))
        }
      }
      for (j in 1:length(labels)) {
        hovers <- c(hovers, paste(hover[length(hover)], '<br>', round(100*data[length(hover), j]/(1+data[length(hover), j])), '% des réponses<br>') )
          values <- c(values, paste(round(100*data[length(hover), j]/(1+data[length(hover), j])), '%', sep=''))
      }
    }
    else {
      if (is.element(hover[length(hover)],c("PNR", "NSP"))) hover <- hover[1:(length(hover)-1)]
      if (is.element(legend[length(legend)],c("PNR", "NSP"))) legend <- legend[1:(length(legend)-1)]
      for (i in 1:length(hover)) { 
        for (j in 1:length(labels)) {
          hovers <- c(hovers, paste(hover[i], '<br>', round(100*data[i, j]), '% des réponses exprimées<br>') )
          values <- c(values, paste(round(100*data[i, j]), '%', sep=''))
        }
      }  
    }
    hovers <- matrix(hovers, ncol=length(hover))
    values <- matrix(values, ncol=length(hover))
  }
  if (!(display_values)) values <- replace(values, T, '')
  
  bars <- plot_ly(x = data[1,], y = labels, type = 'bar', orientation = 'h', text = values[,1], textposition = 'auto', # sort=FALSE, 
                  hoverinfo = hovers[,1], name=legend[1], marker = list(color = color[1], line = list(color = 'white'))) %>% # , width = 0
    
    layout(xaxis = list(title = "",
                        showgrid = show_ticks,
                        showline = FALSE,
                        showticklabels = show_ticks,
                        gridcolor = toRGB("gray70"), # + noir, + proche de 0
                        gridwidth = 1,
                        griddash = "dot",
                        autotick = FALSE,
                        ticks = "outside",
                        tick0 = 0,
                        dtick = 0.1,
                        ticklen = 5*show_ticks,
                        tickwidth = 1,
                        tickcolor = toRGB("gray70"),
                        zeroline = T, 
                        range = xrange,
                        domain = c(0.01 + 0.14*(!(" " %in% labels)), 1)
    ),
    yaxis = list(title = "",
                 showgrid = FALSE,
                 showline = FALSE,
                 showticklabels = FALSE,
                 categoryorder = "trace",
                 # automargin = T,
                 zeroline = FALSE),
    hovermode = 'closest',
    barmode = 'stack',
    title = title,
    titlefont = list(color='black'),
    font = list(color='black', size=legendSize-1),
    # paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
    margin = list(l = margin_l, r = margin_r, t = margin_t, b = 24, autoexpand = thin), # 21, autoexpand=FALSE removes useless margin at bottom but creates bug with legend
    # margin = list(b = 20, t = margin_t),
    legend = list(orientation='h', y=legendY, x=legendX, traceorder='normal', font=list(size=legendSize, color='black')), # family='Balto',  , family=legendFont
    showlegend = (showLegend & (!("Yes" %in% legend) & !("Oui" %in% legend)))) %>%
    
    # labeling the y-axis
    add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = labels,
                    xanchor = 'right',
                    text = labels,
                    font = list(family = 'Arial', size = 14, color = 'black'),
                    showarrow = FALSE, align = 'right') # %>%
    # Legend in the Yes/No case
    if (("Yes" %in% legend) | ("Oui" %in% legend)) { 
      bars <- bars %>% add_annotations(xref = 'x', yref = 'paper',
                    x = c(0.1, 0.9, 1.1),
                    y = 1.5,
                    text = legend,
                    font = list(family = 'Arial', size = 16, color = 'black'),
                    showarrow = FALSE) } # %>%
  # print(nrow(data))
  # print(nrow(hovers))
  # print(ncol(hovers))
  if (nrow(data)>1) { for (i in 2:nrow(data)) {
    bars <- add_trace(bars, evaluate=TRUE, x = data[i,], name=legend[i], text = values[,i], hoverinfo = 'text', hovertext = hovers[,i], marker = list(color = color[i]))
  } }
  if (online) { api_create(bars, filename=file, sharing="public") }
  return(bars)
}
CImedian <- function(vec) { # 95% confidence interval
  res <- tryCatch(unlist(ci.median(vec[!is.na(vec) & vec!=-1])), error=function(e) {print('NA')})
  return(paste(res[paste('ci.lower')], res[paste('ci.median')], res[paste('ci.upper')], length(which(!is.na(vec) & vec!=-1)))) }

# from http://pcwww.liv.ac.uk/~william/R/crosstab.r http://rstudio-pubs-static.s3.amazonaws.com/6975_c4943349b6174f448104a5513fed59a9.html
Crosstab <- function (..., dec.places = NULL, type = NULL, style = "wide", row.vars = NULL, col.vars = NULL, percentages = TRUE,  addmargins = TRUE, subtotals=TRUE) {
  #Declare function used to convert frequency counts into relevant type of proportion or percentage
  mk.pcnt.tbl <- function(tbl, type) {
    a <- length(row.vars)
    b <- length(col.vars)
    mrgn <- switch(type, column.pct = c(row.vars[-a], col.vars), 
                   row.pct = c(row.vars, col.vars[-b]),
                   joint.pct = c(row.vars[-a], col.vars[-b]),
                   total.pct = NULL)
    tbl <- prop.table(tbl, mrgn)
    if (percentages) {
      tbl <- tbl * 100
    }
    tbl
  }
  
  #Find no. of vars (all; row; col) for use in subsequent code
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n.vars <- n.row.vars + n.col.vars
  
  
  #Check to make sure all user-supplied arguments have valid values
  stopifnot(as.integer(dec.places) == dec.places, dec.places > -1)
  #type: see next section of code
  stopifnot(is.character(style))    
  stopifnot(is.logical(percentages))
  stopifnot(is.logical(addmargins))
  stopifnot(is.logical(subtotals))
  stopifnot(n.vars>=1)
  
  #Convert supplied table type(s) into full text string (e.g. "f" becomes "frequency")
  #If invalid type supplied, failed match gives user automatic error message
  types <- NULL
  choices <- c("frequency", "row.pct", "column.pct", "joint.pct", "total.pct")
  for (tp in type) types <- c(types, match.arg(tp, choices))
  type <- types
  
  #If no type supplied, default to 'frequency + total' for univariate tables and to
  #'frequency' for multi-dimenstional tables
  
  #For univariate table....
  if (n.vars == 1) {
    if (is.null(type)) {
      # default = freq count + total.pct  
      type <- c("frequency", "total.pct")
      #row.vars <- 1
    } else {
      #and any requests for row / col / joint.pct must be changed into requests for 'total.pct'
      type <- ifelse(type == "frequency", "frequency", "total.pct")
    }
    #For multivariate tables...
  } else if (is.null(type)) {
    # default = frequency count  
    type <- "frequency"
  }
  
  
  
  #Check for integrity of requested analysis and adjust values of function arguments as required
  
  if ((addmargins==FALSE) & (subtotals==FALSE)) {
    warning("WARNING: Request to suppress subtotals (subtotals=FALSE) ignored because no margins requested (addmargins=FALSE)")
    subtotals <- TRUE
  }
  
  if ((n.vars>1) & (length(type)>1) & (addmargins==TRUE)) {
    warning("WARNING: Only row totals added when more than one table type requested")
    #Code lower down selecting type of margin implements this...
  }
  
  if ((length(type)>1) & (subtotals==FALSE)) { 
    warning("WARNING: Can only request supply one table type if requesting suppression of subtotals; suppression of subtotals not executed")
    subtotals <- TRUE
  }
  
  if ((length(type)==1) & (subtotals==FALSE)) {
    choices <- c("frequency", "row.pct", "column.pct", "joint.pct", "total.pct")
    tp <- match.arg(type, choices)
    if (tp %in% c("row.pct","column.pct","joint.pct")) {
      warning("WARNING: subtotals can only be suppressed for tables of type 'frequency' or 'total.pct'")
      subtotals<- TRUE
    }
  }
  
  if ((n.vars > 2) & (n.col.vars>1) & (subtotals==FALSE)) 
    warning("WARNING: suppression of subtotals assumes only 1 col var; table flattened accordingly")
  
  
  if ( (subtotals==FALSE) & (n.vars>2) )  {
    #If subtotals not required AND total table vars > 2
    #Reassign all but last col.var as row vars
    #[because, for simplicity, Crosstabs assumes removal of subtotals uses tables with only ONE col var]
    #N.B. Subtotals only present in tables with > 2 cross-classified vars...
    if (length(col.vars)>1) {
      row.vars <- c(row.vars,col.vars[-length(col.vars)])
      col.vars <- col.vars[length(col.vars)]
      n.row.vars <- length(row.vars)
      n.col.vars <- 1
    }
  }
  
  #If dec.places not set by user, set to 2 unlesss only one table of type frequency requested,
  #in which case set to 0.  [Leaves user with possibility of having frequency tables with > 0 dp]
  if (is.null(dec.places)) {
    if ((length(type)==1) & (type[1]=="frequency")) {
      dec.places <- 0
    } else {
      dec.places <-2
    }
  }
  
  #Take the original input data, whatever form originally supplied in,
  #convert into table format using requested row and col vars, and save as 'tbl'
  
  args <- list(...)    
  
  if (length(args) > 1) {
    if (!all(sapply(args, is.factor))) 
      stop("If more than one argument is passed then all must be factors")
    tbl <- table(...)
  }
  else {
    if (is.factor(...)) {
      tbl <- table(...)
    }
    else if (is.table(...)) {
      tbl <- eval(...)
    }
    else if (is.data.frame(...)) {
      #tbl <- table(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        tbl <- table(...)
      }
      else {
        var.names <- c(row.vars,col.vars)
        A <- (...)
        tbl <- table(A[var.names])
        if(length(var.names==1)) names(dimnames(tbl)) <- var.names
        #[table() only autocompletes dimnames for multivariate Crosstabs of dataframes]
      }
    }
    else if (class(...) == "ftable") {
      tbl <- eval(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        row.vars <- names(attr(tbl, "row.vars"))
        col.vars <- names(attr(tbl, "col.vars"))
      }
      tbl <- as.table(tbl)
    }
    else if (class(...) == "ctab") {
      tbl <- eval(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        row.vars <- tbl$row.vars
        col.vars <- tbl$col.vars
      }
      for (opt in c("dec.places", "type", "style", "percentages", 
                    "addmargins", "subtotals")) if (is.null(get(opt))) 
                      assign(opt, eval(parse(text = paste("tbl$", opt, 
                                                          sep = ""))))
      tbl <- tbl$table
    }
    else {
      stop("first argument must be either factors or a table object")
    }
  }
  
  #Convert supplied table style into full text string (e.g. "l" becomes "long")
  style <- match.arg(style, c("long", "wide"))
  
  #Extract row and col names to be used in creating 'tbl' from supplied input data
  nms <- names(dimnames(tbl))
  z <- length(nms)
  if (!is.null(row.vars) && !is.numeric(row.vars)) {
    row.vars <- order(match(nms, row.vars), na.last = NA)
  }
  if (!is.null(col.vars) && !is.numeric(col.vars)) {
    col.vars <- order(match(nms, col.vars), na.last = NA)
  }
  if (!is.null(row.vars) && is.null(col.vars)) {
    col.vars <- (1:z)[-row.vars]
  }
  if (!is.null(col.vars) && is.null(row.vars)) {
    row.vars <- (1:z)[-col.vars]
  }
  if (is.null(row.vars) && is.null(col.vars)) {
    col.vars <- z
    row.vars <- (1:z)[-col.vars]
  }
  
  #Take the original input data, converted into table format using supplied row and col vars (tbl)
  #and create a second version (Crosstab) which stores results as percentages if a percentage table type is requested.
  if (type[1] == "frequency") 
    Crosstab <- tbl
  else 
    Crosstab <- mk.pcnt.tbl(tbl, type[1])
  
  
  #If multiple table types requested, create and add these to 
  if (length(type) > 1) {
    tbldat <- as.data.frame.table(Crosstab)
    z <- length(names(tbldat)) + 1
    tbldat[z] <- 1
    pcntlab <- type
    pcntlab[match("frequency", type)] <- "Count"
    pcntlab[match("row.pct", type)] <- "Row %"
    pcntlab[match("column.pct", type)] <- "Column %"
    pcntlab[match("joint.pct", type)] <- "Joint %"
    pcntlab[match("total.pct", type)] <- "Total %"
    for (i in 2:length(type)) {
      if (type[i] == "frequency") 
        Crosstab <- tbl
      else Crosstab <- mk.pcnt.tbl(tbl, type[i])
      Crosstab <- as.data.frame.table(Crosstab)
      Crosstab[z] <- i
      tbldat <- rbind(tbldat, Crosstab)
    }
    tbldat[[z]] <- as.factor(tbldat[[z]])
    levels(tbldat[[z]]) <- pcntlab
    Crosstab <- xtabs(Freq ~ ., data = tbldat)
    names(dimnames(Crosstab))[z - 1] <- ""
  }
  
  
  #Add margins if required, adding only those margins appropriate to user request
  if (addmargins==TRUE) {
    
    vars <- c(row.vars,col.vars)
    
    if (length(type)==1) {
      if (type=="row.pct") 
      { Crosstab <- addmargins(Crosstab,margin=c(vars[n.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.vars]))
      }
      else 
      { if (type=="column.pct") 
      { Crosstab <- addmargins(Crosstab,margin=c(vars[n.row.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.row.vars]))
      }
        else 
        { if (type=="joint.pct") 
        { Crosstab <- addmargins(Crosstab,margin=c(vars[(n.row.vars)],vars[n.vars])) 
        tbl <- addmargins(tbl,margin=c(vars[(n.row.vars)],vars[n.vars])) 
        }
          else #must be total.pct OR frequency
          { Crosstab <- addmargins(Crosstab)
          tbl <- addmargins(tbl)
          }
        }
      } 
    }
    
    #If more than one table type requested, only adding row totals makes any sense...
    if (length(type)>1) {
      Crosstab <- addmargins(Crosstab,margin=c(vars[n.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.vars]))
    }
    
  }  
  
  
  #If subtotals not required, and total vars > 2, create dataframe version of table, with relevent
  #subtotal rows / cols dropped [Subtotals only present in tables with > 2 cross-classified vars]
  t1 <- NULL
  if ( (subtotals==FALSE) & (n.vars>2) )  {
    
    #Create version of Crosstab in ftable format
    t1 <- Crosstab 
    t1 <- ftable(t1,row.vars=row.vars,col.vars=col.vars)
    
    #Convert to a dataframe
    t1 <- as.data.frame(format(t1),stringsAsFactors=FALSE)
    
    #Remove backslashes from category names AND colnames
    t1 <- apply(t1[,],2, function(x) gsub("\"","",x))
    #Remove preceding and trailing spaces from category names to enable accurate capture of 'sum' rows/cols
    #[Use of grep might extrac category labels with 'sum' as part of a longer one or two word string...]
    t1 <- apply(t1,2,function(x) gsub("[[:space:]]*$","",gsub("^[[:space:]]*","",x)))
    
    #Reshape dataframe to that variable and category labels display as required
    #(a) Move col category names down one row; and move col variable name one column to right
    t1[2,(n.row.vars+1):ncol(t1)] <- t1[1,(n.row.vars+1):ncol(t1)]
    t1[1,] <- ""
    t1[1,(n.row.vars+2)] <- t1[2,(n.row.vars+1)]    
    #(b) Drop the now redundant column separating the row.var labels from the table data + col.var labels
    t1 <- t1[,-(n.row.vars+1)]
    
    #In 'lab', assign category labels for each variable to all rows (to allow identification of sub-totals) 
    lab <- t1[,1:n.row.vars]
    for (c in 1:n.row.vars) {
      for (r in 2:nrow(lab)) {
        if (lab[r,c]=="") lab[r,c] <- lab[r-1,c]  
      }
    }
    
    lab <- (apply(lab[,1:n.row.vars],2,function(x) x=="Sum"))
    lab <- apply(lab,1,sum)
    #Filter out rows of dataframe containing subtotals
    
    t1 <- t1[((lab==0) | (lab==n.row.vars)),]
    
    #Move the 'Sum' label associated with last row to the first column; in the process
    #setting the final row labels associated with other row variables to ""
    t1[nrow(t1),1] <- "Sum"
    t1[nrow(t1),(2:n.row.vars)] <- ""
    
    #set row and column names to NULL
    rownames(t1) <- NULL
    colnames(t1) <- NULL
    
  }
  
  
  
  #Create output object 'result' [class: Crosstab]
  result <- NULL
  #(a) record of argument values used to produce tabular output
  result$row.vars <- row.vars
  result$col.vars <- col.vars
  result$dec.places <- dec.places
  result$type <- type
  result$style <- style
  result$percentages <- percentages
  result$addmargins <- addmargins
  result$subtotals <- subtotals
  
  #(b) tabular output [3 variants]
  result$table <- tbl  #Stores original cross-tab frequency counts without margins [class: table]
  result$Crosstab <- Crosstab #Stores cross-tab in table format using requested style(frequency/pct) and table margins (on/off)
  #[class: table]  
  result$Crosstab.nosub <- t1  #Crosstab with subtotals suppressed [class: dataframe; or NULL if no subtotals suppressed]  
  class(result) <- "Crosstab"    
  
  #Return 'result' as output of function
  result
  
}

print.Crosstab <- function(x,dec.places=x$dec.places,subtotals=x$subtotals,...) {
  
  row.vars <- x$row.vars
  col.vars <- x$col.vars
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n.vars <- n.row.vars + n.col.vars
  
  if (length(x$type)>1) {
    z<-length(names(dimnames(x$Crosstab)))
    if (x$style=="long") {
      row.vars<-c(row.vars,z) 
    } else {
      col.vars<-c(z,col.vars)
    }
  }
  
  if (n.vars==1) {
    if (length(x$type)==1) {
      tmp <- data.frame(round(x$Crosstab,x$dec.places))
      colnames(tmp)[2] <- ifelse(x$type=="frequency","Count","%")
      print(tmp,row.names=FALSE)
    } else {
      print(round(x$Crosstab,x$dec.places))
    }
  }
  
  
  #If table has only 2 dimensions, or subtotals required for >2 dimensional table,
  #print table using ftable() on x$Crosstab
  if ((n.vars == 2) | ((subtotals==TRUE) & (n.vars>2))) {
    
    tbl <- ftable(x$Crosstab,row.vars=row.vars,col.vars=col.vars)
    
    if (!all(as.integer(tbl)==as.numeric(tbl))) tbl <- round(tbl,dec.places)
    print(tbl,...)
    
  }
  
  #If subtotals NOT required AND > 2 dimensions, print table using write.table() on x$Crosstab.nosub
  if ((subtotals==FALSE) & (n.vars>2))  {
    
    t1 <- x$Crosstab.nosub
    
    #Convert numbers to required decimal places, right aligned
    width <- max( nchar(t1[1,]), nchar(t1[2,]), 7 )
    dec.places <- x$dec.places
    number.format <- paste("%",width,".",dec.places,"f",sep="")
    t1[3:nrow(t1),((n.row.vars+1):ncol(t1))] <- sprintf(number.format,as.numeric(t1[3:nrow(t1),((n.row.vars+1):ncol(t1))]))
    
    #Adjust column variable label to same width as numbers, left aligned, padding with trailing spaces as required
    col.var.format <- paste("%-",width,"s",sep="")
    t1[1,(n.row.vars+1):ncol(t1)] <- sprintf(col.var.format,t1[1,(n.row.vars+1):ncol(t1)])
    #Adjust column category labels to same width as numbers, right aligned, padding with preceding spaces as required
    col.cat.format <- paste("%",width,"s",sep="")
    t1[2,(n.row.vars+1):ncol(t1)] <- sprintf(col.cat.format,t1[2,(n.row.vars+1):ncol(t1)])
    
    #Adjust row labels so that each column is of fixed width, using trailing spaces as required
    for (i in 1:n.row.vars) {
      width <- max(nchar(t1[,i])) + 2
      row.lab.format <- paste("%-",width,"s",sep="")
      t1[,i] <- sprintf(row.lab.format,t1[,i])
    }
    
    write.table(t1,quote=FALSE,col.names=FALSE,row.names=FALSE)
    
  }
  
}

##### Template #####

# relabel_and_rename <- function(s) {
#   # Notation: ~ means that it's a random variant; * means that another question is exactly the same (in another random branch)
#   
#   # The commented lines below should be executed before creating relabel_and_rename, to ease the filling of each name and label
#   # for (i in 1:length(s)) {
#   #   label(s[[i]]) <- paste(names(s)[i], ": ", label(s[[i]]), s[[i]][1], sep="");
#   #   print(paste(i, label(s[[i]])))
#   # }
#   
#   label(s[[1]]) <- ""
#   label(s[[2]]) <- ""
#   label(s[[3]]) <- ""
#   label(s[[4]]) <- ""
#   label(s[[5]]) <- ""
#   label(s[[6]]) <- ""
#   label(s[[7]]) <- ""
#   label(s[[8]]) <- ""
#   label(s[[9]]) <- ""
#   label(s[[10]]) <- ""
#   label(s[[11]]) <- ""
#   label(s[[12]]) <- ""
#   label(s[[13]]) <- ""
#   label(s[[14]]) <- ""
#   label(s[[15]]) <- ""
#   label(s[[16]]) <- ""
#   label(s[[17]]) <- ""
#   label(s[[18]]) <- ""
#   label(s[[19]]) <- ""
#   label(s[[20]]) <- ""
#   label(s[[21]]) <- ""
#   label(s[[22]]) <- ""
#   label(s[[23]]) <- ""
#   label(s[[24]]) <- ""
#   label(s[[25]]) <- ""
#   label(s[[26]]) <- ""
#   label(s[[27]]) <- ""
#   label(s[[28]]) <- ""
#   label(s[[29]]) <- ""
#   label(s[[30]]) <- ""
#   label(s[[31]]) <- ""
#   label(s[[32]]) <- ""
#   label(s[[33]]) <- ""
#   label(s[[34]]) <- ""
#   label(s[[35]]) <- ""
#   label(s[[36]]) <- ""
#   label(s[[37]]) <- ""
#   label(s[[38]]) <- ""
#   label(s[[39]]) <- ""
#   label(s[[40]]) <- ""
#   label(s[[41]]) <- ""
#   label(s[[42]]) <- ""
#   label(s[[43]]) <- ""
#   label(s[[44]]) <- ""
#   label(s[[45]]) <- ""
#   label(s[[46]]) <- ""
#   label(s[[47]]) <- ""
#   label(s[[48]]) <- ""
#   label(s[[49]]) <- ""
#   label(s[[50]]) <- ""
#   label(s[[51]]) <- ""
#   label(s[[52]]) <- ""
#   label(s[[53]]) <- ""
#   label(s[[54]]) <- ""
#   label(s[[55]]) <- ""
#   label(s[[56]]) <- ""
#   label(s[[57]]) <- ""
#   label(s[[58]]) <- ""
#   label(s[[59]]) <- ""
#   label(s[[60]]) <- ""
#   label(s[[61]]) <- ""
#   label(s[[62]]) <- ""
#   label(s[[63]]) <- ""
#   label(s[[64]]) <- ""
#   label(s[[65]]) <- ""
#   label(s[[66]]) <- ""
#   label(s[[67]]) <- ""
#   label(s[[68]]) <- ""
#   label(s[[69]]) <- ""
#   label(s[[70]]) <- ""
#   label(s[[71]]) <- ""
#   label(s[[72]]) <- ""
#   label(s[[73]]) <- ""
#   label(s[[74]]) <- ""
#   label(s[[75]]) <- ""
#   label(s[[76]]) <- ""
#   label(s[[77]]) <- ""
#   label(s[[78]]) <- ""
#   label(s[[79]]) <- ""
#   label(s[[80]]) <- ""
#   label(s[[81]]) <- ""
#   label(s[[82]]) <- ""
#   label(s[[83]]) <- ""
#   label(s[[84]]) <- ""
#   label(s[[85]]) <- ""
#   label(s[[86]]) <- ""
#   label(s[[87]]) <- ""
#   label(s[[88]]) <- ""
#   label(s[[89]]) <- ""
#   label(s[[90]]) <- ""
#   label(s[[91]]) <- ""
#   label(s[[92]]) <- ""
#   label(s[[93]]) <- ""
#   label(s[[94]]) <- ""
#   label(s[[95]]) <- ""
#   label(s[[96]]) <- ""
#   label(s[[97]]) <- ""
#   label(s[[98]]) <- ""
#   label(s[[99]]) <- ""
#   label(s[[100]]) <- ""
#   label(s[[101]]) <- ""
#   label(s[[102]]) <- ""
#   label(s[[103]]) <- ""
#   label(s[[104]]) <- ""
#   label(s[[105]]) <- ""
#   label(s[[106]]) <- ""
#   label(s[[107]]) <- ""
#   label(s[[108]]) <- ""
#   label(s[[109]]) <- ""
#   label(s[[110]]) <- ""
#   label(s[[111]]) <- ""
#   label(s[[112]]) <- ""
#   label(s[[113]]) <- ""
#   label(s[[114]]) <- ""
#   label(s[[115]]) <- ""
#   label(s[[116]]) <- ""
#   label(s[[117]]) <- ""
#   label(s[[118]]) <- ""
#   label(s[[119]]) <- ""
#   label(s[[120]]) <- ""
#   label(s[[121]]) <- ""
#   label(s[[122]]) <- ""
#   label(s[[123]]) <- ""
#   label(s[[124]]) <- ""
#   label(s[[125]]) <- ""
#   label(s[[126]]) <- ""
#   label(s[[127]]) <- ""
#   label(s[[128]]) <- ""
#   label(s[[129]]) <- ""
#   label(s[[130]]) <- ""
#   label(s[[131]]) <- ""
#   label(s[[132]]) <- ""
#   label(s[[133]]) <- ""
#   label(s[[134]]) <- ""
#   label(s[[135]]) <- ""
#   label(s[[136]]) <- ""
#   label(s[[137]]) <- ""
#   label(s[[138]]) <- ""
#   label(s[[139]]) <- ""
#   label(s[[140]]) <- ""
#   label(s[[141]]) <- ""
#   label(s[[142]]) <- ""
#   label(s[[143]]) <- ""
#   label(s[[144]]) <- ""
#   label(s[[145]]) <- ""
#   label(s[[146]]) <- ""
#   label(s[[147]]) <- ""
#   label(s[[148]]) <- ""
#   label(s[[149]]) <- ""
#   label(s[[150]]) <- ""
#   label(s[[151]]) <- ""
#   label(s[[152]]) <- ""
#   label(s[[153]]) <- ""
#   label(s[[154]]) <- ""
#   label(s[[155]]) <- ""
#   label(s[[156]]) <- ""
#   label(s[[157]]) <- ""
#   label(s[[158]]) <- ""
#   label(s[[159]]) <- ""
#   label(s[[160]]) <- ""
#   label(s[[161]]) <- ""
#   label(s[[162]]) <- ""
#   label(s[[163]]) <- ""
#   label(s[[164]]) <- ""
#   label(s[[165]]) <- ""
#   label(s[[166]]) <- ""
#   label(s[[167]]) <- ""
#   label(s[[168]]) <- ""
#   label(s[[169]]) <- ""
#   label(s[[170]]) <- ""
#   label(s[[171]]) <- ""
#   label(s[[172]]) <- ""
#   label(s[[173]]) <- ""
#   label(s[[174]]) <- ""
#   label(s[[175]]) <- ""
#   label(s[[176]]) <- ""
#   label(s[[177]]) <- ""
#   label(s[[178]]) <- ""
#   label(s[[179]]) <- ""
#   label(s[[180]]) <- ""
#   label(s[[181]]) <- ""
#   label(s[[182]]) <- ""
#   label(s[[183]]) <- ""
#   label(s[[184]]) <- ""
#   label(s[[185]]) <- ""
#   label(s[[186]]) <- ""
#   label(s[[187]]) <- ""
#   label(s[[188]]) <- ""
#   label(s[[189]]) <- ""
#   label(s[[190]]) <- ""
#   label(s[[191]]) <- ""
#   label(s[[192]]) <- ""
#   label(s[[193]]) <- ""
#   label(s[[194]]) <- ""
#   label(s[[195]]) <- ""
#   label(s[[196]]) <- ""
#   label(s[[197]]) <- ""
#   label(s[[198]]) <- ""
#   label(s[[199]]) <- ""
#   label(s[[200]]) <- ""
#   label(s[[201]]) <- ""
#   label(s[[202]]) <- ""
#   label(s[[203]]) <- ""
#   label(s[[204]]) <- ""
#   label(s[[205]]) <- ""
#   label(s[[206]]) <- ""
#   label(s[[207]]) <- ""
#   label(s[[208]]) <- ""
#   label(s[[209]]) <- ""
#   label(s[[210]]) <- ""
#   label(s[[211]]) <- ""
#   label(s[[212]]) <- ""
#   label(s[[213]]) <- ""
#   label(s[[214]]) <- ""
#   label(s[[215]]) <- ""
#   label(s[[216]]) <- ""
#   label(s[[217]]) <- ""
#   label(s[[218]]) <- ""
#   label(s[[219]]) <- ""
#   label(s[[220]]) <- ""
#   label(s[[221]]) <- ""
#   label(s[[222]]) <- ""
#   label(s[[223]]) <- ""
#   label(s[[224]]) <- ""
#   label(s[[225]]) <- ""
#   label(s[[226]]) <- ""
#   label(s[[227]]) <- ""
#   label(s[[228]]) <- ""
#   label(s[[229]]) <- ""
#   label(s[[230]]) <- ""
#   label(s[[231]]) <- ""
#   label(s[[232]]) <- ""
#   label(s[[233]]) <- ""
#   label(s[[234]]) <- ""
#   label(s[[235]]) <- ""
#   label(s[[236]]) <- ""
#   label(s[[237]]) <- ""
#   label(s[[238]]) <- ""
#   label(s[[239]]) <- ""
#   label(s[[240]]) <- ""
#   label(s[[241]]) <- ""
#   label(s[[242]]) <- ""
#   label(s[[243]]) <- ""
#   label(s[[244]]) <- ""
#   label(s[[245]]) <- ""
#   label(s[[246]]) <- ""
#   label(s[[247]]) <- ""
#   label(s[[248]]) <- ""
#   label(s[[249]]) <- ""
#   label(s[[250]]) <- ""
#   label(s[[251]]) <- ""
#   label(s[[252]]) <- ""
#   label(s[[253]]) <- ""
#   label(s[[254]]) <- ""
#   label(s[[255]]) <- ""
#   label(s[[256]]) <- ""
#   label(s[[257]]) <- ""
#   label(s[[258]]) <- ""
#   label(s[[259]]) <- ""
#   label(s[[260]]) <- ""
#   label(s[[261]]) <- ""
#   label(s[[262]]) <- ""
#   label(s[[263]]) <- ""
#   label(s[[264]]) <- ""
#   label(s[[265]]) <- ""
#   label(s[[266]]) <- ""
#   label(s[[267]]) <- ""
#   label(s[[268]]) <- ""
#   label(s[[269]]) <- ""
#   label(s[[270]]) <- ""
#   label(s[[271]]) <- ""
#   label(s[[272]]) <- ""
#   label(s[[273]]) <- ""
#   label(s[[274]]) <- ""
#   label(s[[275]]) <- ""
#   label(s[[276]]) <- ""
#   label(s[[277]]) <- ""
#   label(s[[278]]) <- ""
#   label(s[[279]]) <- ""
#   label(s[[280]]) <- ""
#   label(s[[281]]) <- ""
#   label(s[[282]]) <- ""
#   label(s[[283]]) <- ""
#   label(s[[284]]) <- ""
#   label(s[[285]]) <- ""
#   label(s[[286]]) <- ""
#   label(s[[287]]) <- ""
#   label(s[[288]]) <- ""
#   label(s[[289]]) <- ""
#   label(s[[290]]) <- ""
#   label(s[[291]]) <- ""
#   label(s[[292]]) <- ""
#   label(s[[293]]) <- ""
#   label(s[[294]]) <- ""
#   label(s[[295]]) <- ""
#   label(s[[296]]) <- ""
#   label(s[[297]]) <- ""
#   label(s[[298]]) <- ""
#   label(s[[299]]) <- ""
#   label(s[[300]]) <- ""
#   label(s[[301]]) <- ""
#   label(s[[302]]) <- ""
#   label(s[[303]]) <- ""
#   label(s[[304]]) <- ""
#   label(s[[305]]) <- ""
#   label(s[[306]]) <- ""
#   label(s[[307]]) <- ""
#   label(s[[308]]) <- ""
#   label(s[[309]]) <- ""
#   label(s[[310]]) <- ""
#   label(s[[311]]) <- ""
#   label(s[[312]]) <- ""
#   label(s[[313]]) <- ""
#   label(s[[314]]) <- ""
#   label(s[[315]]) <- ""
#   label(s[[316]]) <- ""
#   label(s[[317]]) <- ""
#   label(s[[318]]) <- ""
#   label(s[[319]]) <- ""
#   
#   return(s)
# }