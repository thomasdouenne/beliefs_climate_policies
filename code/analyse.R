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
package("installr")
package("plotly")
package("processx")
# package("doMC") # for parallel computing, does not work on Windows

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
oui_non <- function(vars, file, labels = vars, data = s, display_value = T, weights=T, margin_r=0, margin_l=250, title="", en=FALSE, NSP=FALSE) {
  margin_t <- 30
  if (title!="") { margin_t <- 80 }
  if (grepl("<br>", title)) { margin_t <- 130 }
  
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
  order_as <- order(oui/(oui+non))
  y <- labels[order_as]
  non <- non[order_as]
  nsp <- nsp[order_as]
  true_nsp <- true_nsp[order_as]
  oui <- sort(oui)
  o <- round(100 * oui / (oui + non))
  n <- round(100 * non / (oui + non))
  
  if (en) {
    hover_oui <- paste('Yes<br>', oui, '% of answers<br>', o, '% of expressed answers')
    hover_non <- paste('No<br>', non, '% of answers<br>',n, '% of expressed answers')
    hover_nsp <- paste('PNR<br>', true_nsp, '% of answers')  
    Text <- c("Yes", "No", "PNR")      }
  else {
    hover_oui <- paste('Oui<br>', oui, '% des réponses<br>', o, '% des réponses exprimées')
    hover_non <- paste('Non<br>', non, '% des réponses<br>',n, '% des réponses exprimées')
    hover_nsp <- paste('NSP<br>', true_nsp, '% des réponses')  
    Text <- c("Oui", "Non", "NSP") }
  if (display_value) {
    hover_oui <- paste(oui, '%')
    hover_non <- paste(non, '%')
    hover_nsp <- paste(true_nsp, '%')
  }
  print(oui)
  print(non)
  print(nsp)
  print(o)
  print(n)
  data <- data.frame(y, oui, non, nsp, o, n)
  data$y <- factor(data$y, levels = data[["y"]])
  y <- c(y, '')
  bars <- plot_ly(data, x = ~o, y = ~y, type = 'bar', orientation = 'h', text = hover_oui, textposition = 'auto', # last one displays values; colors were forestgreen and darkred
                  hoverinfo = 'text', marker = list(color = 'lightgreen', line = list(color = 'white', width = 1))) %>%
    add_trace(x = ~n, text = hover_non, hoverinfo = 'text', marker = list(color = 'plum')) %>%
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
                    y = 1.05,
                    text = Text,
                    font = list(family = 'Arial', size = 16, color = 'black'),
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
data5 <- function(vars, data=m, miss=T, weights=T) {
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
  return(matrice)
  # return(as.data.frame(matrice))
}
data1 <- function(vars, data=m, weights=T) {
  res <- c()
  for (var in vars) {
    if (weights) { res <- c(res, sum(data[['weight']][which(data[[var]]==TRUE)])/sum(data[['weight']][which(data[[var]]==TRUE | data[[var]]==FALSE)])) }
    else { res <- c(res, length(which(data[[var]]==T)))/length(which(data[[var]]==T | data[[var]]==FALSE)) }
  }
  return( matrix(res, ncol=length(vars)) )
}
color5 <- c(rainbow(4, end=4/15)[1:3], "#00FF00", "#228B22") # the last two are: green, forestgreen
# accord5 <- c("Pas du tout d'accord", "Pas vraiment d'accord", "Indifférent-e", "Assez d'accord", "Tout à fait d'accord")
oui_non5 <- c("Oui, tout à fait", "Oui, plutôt", "Indifférent-e/NSP", "Non, pas vraiment", "Non, pas du tout")
yes_no5 <- c("Yes, completely", "Rather yes", "Indifferent/PNR", "Not really", "Not at all")
# agree5 <- c("Strongly disagree", "Disagree", "Indifferent", "Agree", "Strongly agree")
# evol5 <- c("Baisser fortement", "Baisser légèrement", "Maintenir au niveau", "Augmenter légèrement", "Augmenter fortement")
# evolve5 <- c("Strongly decrease", "Slightly decrease", "Maintain", "Slightly increase", "Strongly increase")
barres <- function(data, file, title="", labels, color, hover=legend, nsp=TRUE, sort=TRUE, legend=hover, showLegend=T, margin_r=0, margin_l=NA, online=FALSE) {
  margin_t <- 0
  if (title!="") { margin_t <- 100 }
  if (grepl("<br>", title)) { margin_t <- 150 }
  legendSize <- 10
  legendY <- 1.1
  legendX <- 0.2
  # legendFont <- 'Open Sans'
  if (is.na(margin_l)) { margin_l <- 4.7*max(nchar(labels)) }
  if (max(nchar(labels)) > 25) { legendSize <- 9 }
  # if (max(nchar(labels)) > 50) { legendSize <- 8 }
  # if (max(nchar(labels)) > 60) { legendSize <- 7 }
  if (max(nchar(labels)) > 50) { # 70
    legendSize <- 11 
    legendY = 1.2
    legendX=1
    margin_t = 170
  }
  if (!showLegend) { margin_t <- margin_t - 70}
  
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
    for (i in 1:length(hover)) { hover[i] <- paste(hover[i], "<br>Choisi dans ", round(100*data[1, i]), "% des réponses", sep="")}
    hovers <- matrix(hover, nrow=length(hover))
  }
  else {
    hovers <- c()
    if (nsp) {
      for (i in 1:(length(hover)-1)) { 
        for (j in 1:length(labels)) {
          hovers <- c(hovers, paste(hover[i], '<br>', round(100*data[i, j]/(1+data[length(hover), j])), '% des réponses<br>', round(100*data[i, j]), '% des réponses exprimées') )
        }
      }
      for (j in 1:length(labels)) {
        hovers <- c(hovers, paste(hover[length(hover)], '<br>', round(100*data[length(hover), j]/(1+data[length(hover), j])), '% des réponses<br>') )
      }
    }
    else {
      if (is.element(hover[length(hover)],c("PNR", "NSP"))) hover <- hover[1:(length(hover)-1)]
      if (is.element(legend[length(legend)],c("PNR", "NSP"))) legend <- legend[1:(length(legend)-1)]
      for (i in 1:length(hover)) { 
        for (j in 1:length(labels)) {
          hovers <- c(hovers, paste(hover[i], '<br>', round(100*data[i, j]), '% des réponses exprimées<br>') )
        }
      }  
    }
    hovers <- matrix(hovers, ncol=length(hover))
  }
  
  bars <- plot_ly(x = data[1,], y = labels, type = 'bar', orientation = 'h', text = hovers[,1], # sort=FALSE, 
                  hoverinfo = 'text', name=legend[1], marker = list(color = color[1], line = list(color = 'white', width = 0))) %>%
    
    layout(xaxis = list(title = "",
                        showgrid = T,
                        showline = FALSE,
                        showticklabels = T,
                        gridcolor = toRGB("gray70"), # + noir, + proche de 0
                        gridwidth = 1,
                        griddash = "dot",
                        autotick = FALSE,
                        ticks = "outside",
                        tick0 = 0,
                        dtick = 0.1,
                        ticklen = 5,
                        tickwidth = 1,
                        tickcolor = toRGB("gray70"),
                        zeroline = T,
                        domain = c(0.15, 1)
    ),
    yaxis = list(title = "",
                 showgrid = FALSE,
                 showline = FALSE,
                 showticklabels = FALSE,
                 categoryorder = "trace",
                 zeroline = FALSE),
    hovermode = 'closest',
    barmode = 'stack',
    title = title,
    titlefont = list(color='black'),
    font = list(color='black'),
    # paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
    margin = list(l = margin_l, r = margin_r, t = margin_t, b = 20),
    # margin = list(b = 20, t = margin_t),
    legend = list(orientation='h', y=legendY, x=legendX, traceorder='normal', font=list(size=legendSize, color='black')), # family='Balto',  , family=legendFont
    showlegend = showLegend) %>%
    
    # labeling the y-axis
    add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = labels,
                    xanchor = 'right',
                    text = labels,
                    font = list(family = 'Arial', size = 14, color = 'black'),
                    showarrow = FALSE, align = 'right') # %>%  
  print(nrow(data))
  print(nrow(hovers))
  print(ncol(hovers))
  if (nrow(data)>1) { for (i in 2:nrow(data)) {
    bars <- add_trace(bars, evaluate=TRUE, x = data[i,], name=legend[i], text = hovers[,i], hoverinfo = 'text', marker = list(color = color[i]))
  } }
  
  # labeling the first Likert scale (on the top)
  # add_annotations(xref = 'x', yref = 'paper',
  #                 x = c(10, 90, 110),
  #                 y = 1.05,
  #                 text = c("Oui", "Non", "NSP"),
  #                 font = list(family = 'Arial', size = 16, color = 'black'),
  #                 showarrow = FALSE) # %>%
  # labeling the percentages of each bar (x_axis)
  # add_annotations(xref = 'x', yref = 'y',
  #                 x = o / 2, y = labels,
  #                 text = paste(data[,"oui"], '%'),
  #                 font = list(family = 'Arial', size = 14, color = 'white'),
  #                 showarrow = FALSE) %>%
  # add_annotations(xref = 'x', yref = 'y',
  #                 x = o + n / 2, y = labels,
  #                 text = paste(data[,"non"], '%'),
  #                 font = list(family = 'Arial', size = 14, color = 'white'),
  #                 showarrow = FALSE) %>%
  # add_annotations(xref = 'x', yref = 'y',
  #                 x = o + n + nsp / 2, y = labels,
  #                 text = paste(data[,"nsp"], '%'),
  #                 font = list(family = 'Arial', size = 14, color = 'white'),
  #                 showarrow = FALSE) %>%
  if (online) { api_create(bars, filename=file, sharing="public") }
  return(bars)
}
CImedian <- function(vec) { # 95% confidence interval
  res <- tryCatch(unlist(ci.median(vec[!is.na(vec) & vec!=-1])), error=function(e) {print('NA')})
  return(paste(res[paste('ci.lower')], res[paste('ci.median')], res[paste('ci.upper')], length(which(!is.na(vec) & vec!=-1)))) }



##### Distributions de revenus #####
decrit(s$revenu, weights = s$weight)
round(deciles_erfs_inflates_weighted)
distribution_revenu <- wtd.Ecdf(s$revenu, weights = s$weight)
plot(distribution_revenu_erfs_weighted$x/12, distribution_revenu_erfs_weighted$ecdf, xlim=c(0,5000), type='l', lwd=2, col="blue", xlab = "Net individual income (in €/month)", ylab="Distribution of incomes")
lines(distribution_revenu$x, distribution_revenu$ecdf, col="orange", type='l', lwd=2) + grid()
legend("bottomright", lwd=2, col=c("blue", "orange"), title = "Income distribution from:", title.col = "black", text.col = c("blue", "orange"), legend=c("ERFS (true)", "BCP (our survey)"))

decrit(s$rev_tot, weights = s$weight)
round(deciles_menage_erfs_inflates_weighted)
distribution_rev_tot <- wtd.Ecdf(s$rev_tot, weights = s$weight)
plot(distribution_rev_tot_erfs_weighted$x/12, distribution_rev_tot_erfs_weighted$ecdf, xlim=c(0,5000), type='l', lwd=2, col="blue", xlab = "Net household income (in €/month)", ylab="Distribution of incomes")
lines(distribution_rev_tot$x, distribution_rev_tot$ecdf, col="orange", type='l', lwd=2) + grid()
legend("bottomright", lwd=2, col=c("blue", "orange"), title = "Income distribution from:", title.col = "black", text.col = c("blue", "orange"), legend=c("ERFS (true)", "BCP (our survey)"))


##### Durées #####
decrit(s$duree/60) # 18 min (moyenne 24)
decrit(s$duree_depenses/60) # 2 min (moyenne 3.5)
decrit(s$duree_champ_libre/60) # 0.1 min (moyenne 1)
decrit(s$duree_info_CC_PM/60) # 0.4 min (moyenne 1)
decrit(s$duree_info_CC/60) # 0.3 min (moyenne 1.7)
decrit(s$duree_info_PM/60) # 0.2 min (moyenne 0.5)
decrit(s$duree_no_info/60) # 0.05 min (moyenne 0.1)
length(which(!is.na(s$depenses_confiant)))/nrow(s) # 32%
plot(density(sa$duree/60), xlim=c(0,30))
plot(Ecdf(sa$duree/60)$x, Ecdf(sa$duree/60)$ecdf)
sa$duree_min <- sa$duree/60
Ecdf(sa$duree_min) 


##### Caractéristiques énergétiques #####
decrit(s$surface, weights = s$weight) # 120
decrit(s$chauffage, weights = s$weight) # 50% gaz ou fioul
decrit(s$km, weights = s$weight) # 11k (moyenne 15k)
decrit(s$conso, weights = s$weight) # 6
decrit(s$nb_vehicules, weights = s$weight)


##### Gain questions générales (TVA, transports, logement) #####
decrit(s$perte_tva, weights = s$weight)
decrit(s$perte_fuel, weights = s$weight)
decrit(s$perte_chauffage, weights = s$weight) # proportions similaires pour les 3, environ 60% pensent perdre plus que la moyenne
temp1 <- temp2 <- s[,c("perte_tva", "perte_fuel", "perte_chauffage", "perte_partielle", "variante_partielle", "weight")]
temp1$perte <- temp1$perte_tva
temp1$variante_perte <- "tva"
temp2$perte <- temp2$perte_partielle
temp2$variante_perte <- temp2$variante_partielle
s_perte <- merge(temp1, temp2, all=T)
# s_perte$variante_perte <- relevel(as.factor(s_perte$variante_perte), "tva")
summary(lm(perte ~ variante_perte, data = s_perte, weights = s_perte$weight)) # *** -0.31 chauffage, -0.18 fuel: les gens s'estiment plus perdants avec hausse TVA
# TODO: restreindre aux seuls gagnants/perdants


##### Gain (dividende - hausse dépenses énergétiques) #####
decrit(110 * s$nb_adultes - s$hausse_depenses, weights = s$weight) # Ok !
decrit(s$gain, weights = s$weight) 
decrit(s$gain_taxe, weights = s$weight)
decrit(s$gain_taxe_feedback, weights = s$weight)
decrit(s$gain_taxe_feedback[s$hausse_depenses < 110 * s$nb_adultes], weights = s$weight[s$hausse_depenses < 110 * s$nb_adultes]) 
# On ne les convainc pas !!!
length(which(s$gain_taxe_feedback=='Gagnant' & s$hausse_depenses < 110 * s$nb_adultes))/length(which(!is.na(s$gain_taxe_feedback) & s$hausse_depenses < 110 * s$nb_adultes))
length(which(s$gain_taxe_feedback=='Gagnant' & s$gain_taxe != 'Gagnant' & s$hausse_depenses < 110 * s$nb_adultes))/length(which(!is.na(s$gain_taxe_feedback) & s$hausse_depenses < 110 * s$nb_adultes & s$gain_taxe != 'Gagnant'))
decrit(s$gain_taxe_progressif, weights = s$weight)

## Plot CDF
cdf_gain <- wtd.Ecdf(s$gain, weights = s$weight)
plot(cdf_gain$x, cdf_gain$ecdf, type='s', lwd=2, col='orange', xlab="Category of subjective gain", ylab="Distribution of answers") + grid()


##### Approbation #####
decrit(s$taxe_approbation) # 11% Dur !!!
decrit(s$taxe_approbation[s$gilets_jaunes_soutien==TRUE])
decrit(s$taxe_approbation[s$gilets_jaunes_oppose==TRUE])
decrit(s$taxe_feedback_approbation) # 17%
decrit(s$taxe_efficace) # 18%
decrit(s$taxe_progressif_approbation) # 19%
summary(lm((taxe_feedback_approbation=='Oui') ~ (gain_taxe!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_progressif_approbation=='Oui') ~ (gain_taxe!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_approbation=='Oui') ~ (taxe_efficace!='Non') + (gain_taxe!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_approbation=='Oui') ~ score_climate_call + score_polluants + (gain_taxe!='Perdant'), data=s, weights = s$weight))


##### OLS Approbation ####
summary(lm((taxe_approbation=='Oui') ~ revenu + rev_tot + hausse_carburants + hausse_chauffage + score_climate_call + score_ges
              + Gauche_droite + emission_cible + effets_CC + ecologiste + conservateur + liberal + humaniste + patriote + apolitique
              + sexe + age + diplome4 + statut_emploi + csp + region, 
            data=s))



###### Approbation: ANOVA #####
# ANalysis Of VAriance partitions sequentially the error terms of a regression where regressors are categorical variables. 
#   This gives the Sum of squares (SS) of each variable, which depends on the rank of the variable in the list (I think that variables 
#   have higher sum of squares when they are treated first). (SS are rank-independent when the sample is balanced, i.e. when there are the same number of observations in each category)
#   This generalizes from OLS to logit (and other) using deviance; to continuous regressors (TODO: how? if we don't know, we could still make them categorical)
# Is there a canonical way to decompose the variance for unbalanced samples? Like averaging SS from random draws on the ordering of the variables?
#   I haven't found. en.wikipedia.org/wiki/Repeated_measures is something else. The more I dig, the more I realize ANOVA is fit for another
#   purpose: when we can decompose the sample into several groups (as if there were only one categorical variable, or different but with all interaction terms)
variables_big_regression <- c("revenu", "rev_tot", "hausse_carburants", "hausse_chauffage", "score_climate_call", "score_ges", "Gauche_droite", 
                              "emission_cible", "effets_CC", "ecologiste", "conservateur", "liberal", "humaniste", "patriote", "apolitique", 
                              "sexe", "age", "diplome4", "statut_emploi", "csp", "region") # TODO: complete the list
ols <- summary(lm(as.formula(paste("(taxe_approbation=='Oui') ~", paste(variables_big_regression, collapse=' + '))), data=s))
ols
sum(ols$residuals^2)
anova <- summary(aov(as.formula(paste("(taxe_approbation=='Oui') ~", paste(variables_big_regression, collapse=' + '))), data=s))
anova
sum_sq <- anova[[1]]$'Sum Sq'
explained_variance <- sum(sum_sq[-length(sum_sq)])
print(paste("Share of explained variance: ", round(explained_variance/sum(sum_sq), 2), ". Among which, share of explained variance explained by each variable:", sep=""))
for (i in 1:length(variables_big_regression)) print(paste(variables_big_regression[order(sum_sq[-length(sum_sq)], decreasing = T)][i], round(sort(sum_sq[-length(sum_sq)], decreasing = T)[i]/explained_variance,3)))
summary(lm((taxe_approbation=='Oui') ~ ecologiste + conservateur, data=s))

anovaVCA(as.formula(paste("(taxe_approbation=='Oui') ~", paste(variables_big_regression, collapse=' + '))), Data=s)
st <- s[,c("revenu", "Gauche_droite", "humaniste")]
st$revenu <- n(st$revenu)
st$humaniste <- factor(st$humaniste)
anovaMM(revenu ~ Gauche_droite + humaniste, Data=st)
data(dataEP05A2_2)
dt <- dataEP05A2_2[-1,]
st <- cbind(st[1:79,], dt)
anovaMM(revenu ~ day, st)

##### Approbation: Model Selection #####
# Other method of model selection exists (like forward/backward stepwise selection), but seem less reliable as they do not
#   explore exhaustively all possible models nor do they minimize anything. Best-subset selection is exhaustive, but I am
#   afraid that it be too computationally intensive. TODO: test them (cf. medium.com/cracking-the-data-science-interview/1ef6dbd531f7)
# (as far as I understood) There are two ways to use LASSO: (cf. web.stanford.edu/~hastie/glmnet/glmnet_alpha.html, wikipedia for lasso and CV)
#   - glmnet (without cross-validation) decreases lambda (i.e. increases the norm of beta and the number of regressors)
#     until the minimum of (null) deviance explained (~ max adj-R^2) is reached, "lassoing" variables one after the other
#   - cv.glmnet (with cross-validation (CV)) uses 10-fold CV (cf. wikipedia>CV) to select the lambda that minimizes
#     "out-of-the-sample" deviance* (avoiding too low lambda that induce overfitting): lambda.min. lambda.1se is an
#     alternative choice for lambda that allow less regressors but stays with 1 s.e. of deviance relative to lambda.min
#       * actually type.measure = class/mae/deviance/auc minimizes misclassification/mean absolute error/deviance/area under ROC curve
# Elastic net is a compromise between Ridge regression (L^2 norm penalty) and lasso (L^1 penalty), with a penalty
#   alpha*lasso + (1-alpha)*Ridge. Ridge bias all estimates to 0 instead of removing some of them (and biasing some others)
#   as lasso does. But Ridge allows to keep variables that are correlated (while lasso would then to keep only of one them). 
#   How to choose alpha? Define a 10-fold, use it to run cv.glmnet on different values for alpha, and select the one 
#   whose lambda.min minimizes deviance (cf. Zou & Hastie, 2005). 
# The Composite Absolute Penalties allows to group categorical variables: all in or all out of the model for a given variable 
#   (introduced by Zhao, Rocha &  Yu (2009) who only coded it in matlab; R package quadrupen provides a related method cran.r-project.org/web/packages/quadrupen/quadrupen.pdf)
# Multinomial regression is an alternative to logit, as we have 3 categories in our dependant variable (Oui/Non/NSP)
#   This is family="multinomial". type.multinomial="grouped" forces that the same variables be taken for our categories (Oui/Non/NSP)

for (v in variables_big_regression) { # display and remove variables with missing values
  na_v <- length(which(is.na(s[[v]])))
  # if ("labelled" %in% class(s[[v]])) na_v <- length(which(is.na(s[[v]]))) #
  # else na_v <- length(which(is.missing(s[[v]]))) 
  if (na_v>0) {
    print(paste(v, na_v))
    variables_big_regression <- variables_big_regression[variables_big_regression!=v] }
}
  
x <- model.matrix(as.formula(paste("(taxe_approbation=='Oui') ~", paste(variables_big_regression, collapse=' + '))),  data=s)
y <- ifelse(s$taxe_approbation=="Oui", 1, 0)
#perform grid search to find optimal value of lambda
#family= binomial => logistic regression, alpha=1 => lasso 
# check docs to explore other type.measure options
fit <- glmnet(x, y, alpha=1, family="binomial", weights = s$weight, type.multinomial = "grouped")
cv.out <- cv.glmnet(x, y, alpha=1, family="binomial", weights = s$weight, type.multinomial = "grouped") # TODO: how to choose alpha?; run parallel computing: parallel = T
plot(cv.out)
coefs_lasso <- coef(cv.out, s="lambda.1se") # lambda.min lambda.1se
coefs_lasso <- coef(cv.out, s="lambda.min") # TODO: group variables
data.frame(name = coefs_lasso@Dimnames[[1]][coefs_lasso@i + 1], coefficient = coefs_lasso@x) # doesn't work for multinomial
selected_variables <- coefs_lasso@i - 1
selected_variables <- selected_variables[selected_variables > 0 & selected_variables <= length(variables_big_regression)]
selected_variables <- variables_big_regression[selected_variables]
summary(glm(as.formula(paste("(taxe_approbation=='Oui') ~", paste(selected_variables, collapse=' + '))), binomial, data=s, weights=s$weight))
# lasso <- glmnet(x, y, alpha=1, family="binomial")

# find alpha
foldid=sample(1:10,size=length(y),replace=TRUE)
cv1=cv.glmnet(x,y,foldid=foldid,alpha=1)
cv.5=cv.glmnet(x,y,foldid=foldid,alpha=.5)
cv0=cv.glmnet(x,y,foldid=foldid,alpha=0)

##### Logit Approbation #####
# All variables we can think of (TODO: complete the list)
logit_all <- glm((taxe_approbation=='Oui') ~ revenu + rev_tot + hausse_carburants + hausse_chauffage + score_climate_call + score_ges
              + Gauche_droite + emission_cible + effets_CC + ecologiste + conservateur + liberal + humaniste + patriote + apolitique
              + sexe + age + diplome4 + statut_emploi + csp + region, 
            family = "binomial", data=s)
summary(logit_all)
PseudoR2(logit_all)

# Only significant variables
summary(glm((taxe_approbation=='Oui') ~ hausse_chauffage + score_climate_call + ecologiste + conservateur + humaniste + sexe , 
            binomial, data=s))

# Only demographics
summary(glm((taxe_approbation=='Oui') ~ revenu + rev_tot + sexe + age + region, 
            family = "binomial", data=s)) # *: -age, +revenu, -rev_tot, +Homme, qq regions
summary(glm((taxe_approbation=='Oui') ~ revenu + rev_tot + sexe + age + diplome4 + statut_emploi + csp + region, 
            family = "binomial", data=s)) # *: +revenu, -rev_tot, +Homme, qq regions
summary(glm((taxe_approbation=='Oui') ~ revenu + rev_tot + sexe + Age + Diplome + region, 
            family = "binomial", data=s)) # *: +revenu, -rev_tot, +Homme, qq regions

# Demographics + politics
summary(glm((taxe_approbation=='Oui') ~ revenu + rev_tot + sexe + age + diplome4 + statut_emploi + csp + region
            + Gauche_droite + ecologiste + conservateur + liberal + humaniste + patriote + apolitique, 
            family = "binomial", data=s)) # *: apolitique, ecologiste, conservateur, +revenu, -rev_tot, +Homme, qq regions
summary(glm((taxe_approbation=='Oui') ~ revenu + rev_tot + sexe + age + diplome4 + statut_emploi + csp + region
            + Gauche_droite + ecologiste + conservateur + liberal + humaniste + patriote + apolitique, 
            family = "binomial", data=s)) # *: apolitique, ecologiste, conservateur, +revenu, -rev_tot, +Homme, qq regions


##### Probit Approbation #####
summary(glm(, family = binomial(link = "probit", data=s)))


##### Elasticites #####
decrit(s$Elasticite_fuel[!is.na(s$Elasticite_fuel)])
decrit(s$Elasticite_fuel_perso[!is.na(s$Elasticite_fuel_perso)]) #
decrit(s$Elasticite_chauffage[!is.na(s$Elasticite_chauffage)]) #
decrit(s$Elasticite_chauffage_perso[!is.na(s$Elasticite_chauffage_perso)]) #

summary(lm(Elasticite_fuel ~ (taxe_efficace=='Oui'), data=s, weights = s$weight))
summary(lm(Elasticite_chauffage ~ (taxe_efficace=='Oui'), data=s, weights = s$weight)) # Aucun lien évident élasticité / efficacité environnementale

cor(s$Elasticite_fuel[!is.na(s$Elasticite_fuel)], s$Elasticite_fuel_perso[!is.na(s$Elasticite_fuel_perso)])
cor(s$Elasticite_chauffage[!is.na(s$Elasticite_chauffage)], s$Elasticite_chauffage_perso[!is.na(s$Elasticite_chauffage_perso)])
# Correlation positive entre �lasticit� perso et �lasticit� globale


##### Ciblage #####
decrit(s$categorie_cible, weights = s$weight)
decrit(s$taxe_cible_approbation, weights = s$weight)
summary(lm((taxe_cible_approbation=='Oui') ~ (cible==20), data=s, subset=categorie_cible=='20_30', weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ (cible==30), data=s, subset=categorie_cible=='30_40', weights = s$weight)) # *
summary(lm((taxe_cible_approbation=='Oui') ~ (cible==40), data=s, subset=categorie_cible=='40_50', weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ (cible==20), data=s, subset=(categorie_cible=='20_30' & taxe_cible_approbation!='NSP'), weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ (cible==30), data=s, subset=(categorie_cible=='30_40' & taxe_cible_approbation!='NSP'), weights = s$weight)) # *
summary(lm((taxe_cible_approbation=='Oui') ~ (cible==40), data=s, subset=(categorie_cible=='40_50' & taxe_cible_approbation!='NSP'), weights = s$weight))

summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==20, weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==30, weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==40, weights = s$weight)) # *
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==50, weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==20 & taxe_cible_approbation!='NSP', weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==30 & taxe_cible_approbation!='NSP', weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==40 & taxe_cible_approbation!='NSP', weights = s$weight)) # *
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==50 & taxe_cible_approbation!='NSP', weights = s$weight))


##### Gaz de schiste #####
decrit(s$schiste_approbation, weights = s$weight)
decrit(s$schiste_approbation, miss=T, weights = s$weight)
decrit(s$schiste_traite, weights = s$weight)
decrit(s$schiste_avantage, weights = s$weight)
decrit(s$schiste_CC, weights = s$weight)
summary(lm((schiste_approbation=='Oui') ~ (schiste_traite==0), data=s, weights = s$weight)) # Encore rien de très significatif
summary(lm((schiste_avantage=='Cela permettrait de créer des emplois et dynamiser les départements concernés') ~ (schiste_traite==1), data=s, subset=schiste_approbation=='Oui' & schiste_avantage !='Aucune de ces deux raisons', weights = s$weight))
summary(lm((schiste_CC=='Elle est valable : toute baisse des émissions va dans la bonne direction') ~ (schiste_traite==1), data=s, subset=schiste_approbation=='Oui' & schiste_CC !='NSP (Ne sait pas, ne se prononce pas)', weights = s$weight))


##### Engagement personnel ######
decrit(s$mode_vie_ecolo, weights = s$weight) # 79% !
decrit(s$mode_vie_ecolo, miss=T, weights = s$weight) # 65%
decrit(s$enfant_CC, weights = s$weight)
decrit(s$enfant_CC_pour_CC[s$enfant_CC=='Oui'], weights = s$weight[s$enfant_CC=='Oui'])
decrit(s$enfant_CC_pour_lui[s$enfant_CC=='Oui'], weights = s$weight[s$enfant_CC=='Oui'])
summary(lm((enfant_CC=='Oui') ~ sexe, data=s))


##### Connaissances et opinions CC #####
decrit(s$ges_avion, weights = s$weight) # 47%
decrit(s$ges_boeuf, weights = s$weight) # 47%
decrit(s$ges_nucleaire, weights = s$weight) # 48%
decrit(s$ges_co2, weights = s$weight) # 81%
decrit(s$ges_ch4, weights = s$weight) # 48%
decrit(s$ges_o2, weights = s$weight) # 6%
decrit(s$ges_pm, weights = s$weight) # 58%
decrit(s$score_polluants, weights = s$weight)
decrit(s$score_climate_call, weights = s$weight)
decrit(s$emission_cible, weights = s$weight) # 5
decrit(s$cause_CC, miss=T, weights = s$weight)


##### Orientation politiques #####
decrit(s$gauche_droite, weights = s$weight, miss=T)
decrit(s$gauche_droite, miss=T) # TODO: pb (should not be number, and missing should be here)
decrit(s$apolitique, weights = s$weight) # 21%
decrit(s$ecologiste, weights = s$weight) # 15%
decrit(s$humaniste, weights = s$weight) # 11%
decrit(s$patriote, weights = s$weight) # 8%
decrit(s$liberal, weights = s$weight) # 5%
decrit(s$conservateur, weights = s$weight) # 2%


##### Transferts inter #####
summary(lm((transferts_inter=='Oui') ~ transferts_inter_info*apres_modifs, data = s, weights = s$weight)) # 0 !
summary(lm((transferts_inter=='Oui') ~ transferts_inter_info*apres_modifs, data = s, subset = transferts_inter!='NSP', weights = s$weight)) # 0 !
decrit(s$variation_aide, weights = s$weight)
load('p_data.RData')
t <- merge(s, t_transferts_inter_a, all=T)
t$transferts_inter[!is.na(t$taille_foyer)] <- t$transferts_inter_a[!is.na(t$taille_foyer)] 
decrit(t$transferts_inter, weights = t$weight)
decrit(t$transferts_inter, weights = t$weight, miss=T)
decrit(t$transferts_inter[t$apres_modifs==T], weights = t$weight[t$apres_modifs==T], miss=T)
decrit(t$transferts_inter[t$apres_modifs==FALSE], weights = t$weight[t$apres_modifs==FALSE], miss=T)
decrit(t$transferts_inter[t$transferts_inter_info==T], weights = t$weight[t$transferts_inter_info==T], miss=T)
decrit(t$transferts_inter[t$transferts_inter_info==FALSE], weights = t$weight[t$transferts_inter_info==FALSE], miss=T)
decrit(t$transferts_inter[t$apres_modifs==T & t$transferts_inter_info==T], weights = t$weight[t$apres_modifs==T & t$transferts_inter_info==T], miss=T)
decrit(t$transferts_inter[t$apres_modifs==T & t$transferts_inter_info==FALSE], weights = t$weight[t$apres_modifs==T & t$transferts_inter_info==FALSE], miss=T)
decrit(t$transferts_inter[t$apres_modifs==FALSE & t$transferts_inter_info==T], weights = t$weight[t$apres_modifs==FALSE & t$transferts_inter_info==T], miss=T)
decrit(t$transferts_inter[t$apres_modifs==FALSE & t$transferts_inter_info==FALSE], weights = t$weight[t$apres_modifs==FALSE & t$transferts_inter_info==FALSE], miss=T)
decrit(t$transferts_inter, miss=T)
binconf(sum(t$weight[!is.na(t$transferts_inter) & t$transferts_inter=='Oui']), sum(t$weight[!is.missing(t$transferts_inter)]))
binconf(sum(t$weight[!is.na(t$transferts_inter) & t$transferts_inter=='Oui']), sum(t$weight[!is.na(t$transferts_inter)]))
summary(lm((transferts_inter=='Oui') ~ transferts_inter_info*apres_modifs, data = t)) # 0
summary(lm((transferts_inter=='Oui') ~ transferts_inter_info*apres_modifs, data = t, subset = transferts_inter!='NSP')) # 0
summary(lm((transferts_inter=='Oui') ~ transferts_inter_info*apres_modifs, data = t, weights = t$weight)) # 0
summary(lm((transferts_inter=='Oui') ~ transferts_inter_info*apres_modifs, data = t, subset = transferts_inter!='NSP', weights = t$weight)) # 0

# use m_global (in enquete/codes) to redo graph with new data for transferts_inter

##### Dépenses publiques #####
categories_depenses <- c("sante", "education", "retraites", "securite", "recherche", "justice", "armee", "protection", "infrastructures", "loisirs", "aide")
i <- 0
for (v in categories_depenses) {
  print(summary(lm(s[[paste('variation', v, sep='_')]] ~ s[[paste('dep', i, 'en_position', sep='_')]], weights=s$weight)))
  i <- i+1 }
# *** pour justice, loisirs, éducation
t_depenses$aleatoire <- FALSE
s$aleatoire <- TRUE
d <- merge(s, t_depenses, all=T)
for (v in categories_depenses) {  print(summary(lm(d[[paste('variation', v, sep='_')]] ~ d$aleatoire))) } # * -: armee, securite, aide, 
summary(lm(d$variation_aide ~ d$aleatoire, weights = d$weight))
summary(rq(d$variation_aide ~ d$aleatoire, weights = d$weight))
decrit(d$variation_aide, weights = d$weight)
decrit(s$variation_aide)
decrit(s$depenses_confiant)
decrit(s$depenses_confiant[is.na(s$variation_aide)])
decrit(t_depenses$variation_aide)
for (v in categories_depenses) { # why not use tidyverse's gather?
  temp <- d[, c(paste('variation', v, sep='_'), 'weight')]
  temp$categorie <- v
  temp$variation <- temp[[paste('variation', v, sep='_')]]
  if (exists('dep')) dep <- merge(dep, temp, all=T)
  else dep <- temp
}
dep$categorie <- relevel(as.factor(dep$categorie), "infrastructures")
summary(lm(variation ~ categorie, data=dep)) # answers are not random, i.e. average depends significantly on category


##### Miscellanous #####
decrit(s$rattrapage_diesel, miss = T, weights = s$weight)
decrit(s$hausse_diesel, weights = s$weight) 
summary(lm((rattrapage_diesel!='Non') ~ (diesel==TRUE), data=s, weights = s$weight))
for (j in names(s)) if (grepl('gilets_jaunes', j)) print(decrit(s[[j]], weights=s$weight))


##### Approbation politiques environnementales #####
for (j in names(s)) if (grepl('si_', j)) print(decrit(s[[j]], weights=s$weight))
for (j in 141:148) print(decrit(s[[j]], weights=s$weight))


##### Bénéfices/problèmes taxe carbone #####
variables_benefices <- names(s)[which(grepl("benefice", names(s)))[which(grepl("benefice", names(s)))>300]]
variables_problemes <- names(s)[which(grepl("problemes", names(s)))[which(grepl("problemes", names(s)))>300]]
labels_benefices <- c()
values_benefices <- c()
for (v in variables_benefices[1:(length(variables_benefices)-2)]) {
  labels_benefices <- c(labels_benefices, gsub(" - .*", "", gsub(".*: ", "", Label(s[[v]]))))
  values_benefices <- c(values_benefices, sum(s$weight[which(s[[v]]==T)])/sum(s$weight)) }
labels_problemes <- c()
values_problemes <- c()
for (v in variables_problemes[1:(length(variables_problemes)-2)]) {
  labels_problemes <- c(labels_problemes, gsub(" - .*", "", gsub(".*: ", "", Label(s[[v]]))))
  values_problemes <- c(values_problemes, sum(s$weight[which(s[[v]]==T)])/sum(s$weight)) }

oui_non(margin_l=430, variables_benefices[1:(length(variables_benefices)-2)], "barres_benefices", labels_benefices)
oui_non(margin_l=430, variables_problemes[1:(length(variables_problemes)-2)], "barres_problemes", labels_problemes)

# TODO: créer variables avec nombre de bénéfices / problèmes cochés

barres_benefices <- barres(file="benefices", title="<b>Bénéfices d'une taxe carbone compensée</b><br>(choix multiples)", data=matrix(values_benefices, ncol=length(values_benefices)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_benefices, hover=labels_benefices, legend="empty")
barres_problemes <- barres(file="problemes", title="<b>Problèmes d'une taxe carbone compensée</b><br>(choix multiples)", data=matrix(values_problemes, ncol=length(values_problemes)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_problemes, hover=labels_problemes, legend="empty")
barres_benefices
dev.print(png, '../images/benefice.png')
dev.copy(png, filename="../images/benefice.png")
dev.off()
barres_problemes
dev.print(png, '../images/probleme.png')
# orca(barres_problemes, "../images/problemes.png")
# orca(barres_benefices, "../images/benefices.png")
