
# load packages ----

library(shinythemes)
library(vcd)
library(RColorBrewer)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(dplyr)


# load data ----

load(file = "data/simulatr.RData")


# global functions ----

theme_nothing <- function(base_size = 12, base_family = "Helvetica"){
  theme_bw(base_size = base_size, base_family = base_family)
  theme(rect = element_blank(),
        line = element_blank(),
        text = element_blank(),
        legend.position = "none"
  )
}


ScaleRnorm <- function(n, mu, sigma){
  x <- rnorm(n = n, mean = mu, sd = sigma)
  xDisp <- sigma * x / sd(x)
  xScaled <- xDisp - mean(xDisp) + mu
  return(xScaled)
}



# mean estimation ----

GetOneSampleMean <- function(df, sampsize, val){
  df$TAILLE <- val
  sampIndex <- sample(x = 1:1000, size = sampsize, replace = FALSE)
  sampMean <- mean(df[sampIndex, "TAILLE"])
  return(list(MEAN = sampMean, SAMP = sampIndex))
}

GetTenSampleMean <- function(df, sampsize, val){
  df$TAILLE <- val
  meanList <- vector()
  for(i in 1:10){
    oneMean <- mean(df[sample(x = 1:1000, size = sampsize, replace = FALSE), "TAILLE"])
    meanList <- append(meanList, oneMean)
  }
  return(list(MEAN = meanList))
}

GetHundredSampleMean <- function(df, sampsize, val){
  df$TAILLE <- val
  meanList <- vector()
  for(i in 1:100){
    oneMean <- mean(df[sample(x = 1:1000, size = sampsize, replace = FALSE), "TAILLE"])
    meanList <- append(meanList, oneMean)
  }
  return(list(MEAN = meanList))
}

PlotPopMean <- function(df){
  popPlot <- ggplot(df) + 
    geom_point(aes(X, Y, size = TAILLE), color = "firebrick") +
    #scale_color_manual(values = c("chocolate", "chartreuse4")) +
    scale_size_continuous(range = c(0.5, 6)) +
    coord_equal() + theme_nothing()
  return(popPlot)
}

PlotOneSampleMean <- function(df, sampone, val){
  df$TAILLE <- val
  sampPlot <- ggplot() + 
    geom_point(data = df, aes(X, Y, size = TAILLE), color = "grey70") +
    geom_point(data = df[sampone, ], aes(X, Y, size = TAILLE), color = "firebrick") +
    scale_size_continuous(range = c(0.5, 6)) +
    coord_equal() + theme_nothing()
  return(sampPlot)
}

PlotHist <- function(df, sel, val, sigma){
  df$TAILLE <- val
  if(is.null(sel)){
    selecSample <- 1:1000
  } else {
    selecSample <- sel
  }
  
  meanPop <- mean(df$TAILLE)
  sdPop <- sqrt(mean((df$TAILLE - mean(df$TAILLE))^2))
  meanSamp <- mean(df[selecSample, "TAILLE"])
  sdSamp <- sd(df[selecSample, "TAILLE"])
  
  pTot <- ggplot(df) + geom_histogram(aes(TAILLE), color = "white", binwidth = 5) + 
    geom_vline(xintercept = meanPop, color = "firebrick") +
    scale_x_continuous(limits = c(25, 175)) + scale_y_continuous("Fréquence") +
    ggtitle(paste("Population (mu = ", round(meanPop, 1), " ; sigma = ", round(sdPop, 1), ")", sep = "")) +
    theme_bw()
  pSel <- ggplot(df[selecSample, ]) + geom_histogram(aes(TAILLE), color = "white", binwidth = 5) + 
    geom_vline(xintercept = meanSamp, color = "firebrick") +
    scale_x_continuous(limits = c(25, 175)) + scale_y_continuous("Fréquence") +
    ggtitle(paste("Échantillon (moy = ", round(meanSamp, 1), " ; ecart-type = ", round(sdSamp, 1), ")", sep = "")) +
    theme_bw()
  
  grid.arrange(pTot, pSel, nrow = 1, ncol = 2)
}

ShowDataTableMean <- function(val){
  seqNum <- seq(1, length(val), 1)
  seqChar <- ifelse(seqNum < 10, paste("000", seqNum, sep = ""),
                    ifelse(seqNum < 100, paste("00", seqNum, sep = ""),
                           ifelse(seqNum < 1000, paste("0", seqNum, sep = ""),
                                  as.character(seqNum))))
  df <- data.frame(ECHANTILLON = paste("ECHANTILLON", seqChar, sep = "_"),
                   MOYENNE = round(val, digits = 2))
  df <- df[order(df$ECHANTILLON, decreasing = TRUE), ]
  return(df)
}

PlotMeanDistrib <- function(meanvalues, levalpha, mu, sigma, sampsize){
  
  # compute probabilities
  zValue <- round(qnorm(p = 1 - levalpha / 2), digits = 2)
  standError <- sigma / sqrt(sampsize)
  icError <- zValue * sigma / sqrt(sampsize)
  seqValues <- mu + seq(-3 * standError, 3 * standError, by = 0.01)
  seqProba <- dnorm(x = seqValues, mean = mu, sd = standError)
  
  # create tables
  tabNorm <- data.frame(SEQ = seqValues, PROBA = seqProba)
  
  # plot distribution
  meanPlot <- ggplot() + 
    geom_density(data = data.frame(MEAN = meanvalues), aes(x = MEAN), fill = "grey", color = "grey30", alpha = 0.5) + 
    geom_line(data = tabNorm, aes(x = SEQ, y = PROBA), color = "firebrick") +
    geom_vline(xintercept = c(mu - icError, mu + icError), color = "firebrick2") + 
    annotate(geom = "text", 
             x = c(mu - 1.1 * icError, mu + 1.1 * icError), 
             y = 0.05, 
             label = c(formatC(mu - icError, digits = 2, format = "f"), formatC(mu + icError, digits = 2, format = "f")), 
             color = "firebrick2", fontface = 2) +
    scale_x_continuous("Valeur de la statistique moyenne empirique") +
    scale_y_continuous("Densité de probabilité") + 
    theme_bw()
  
  return(meanPlot)
}


# Mean comparison ---- 

PlotPopComp <- function(df){
  popPlot <- ggplot(df) + 
    geom_point(aes(X, Y, size = TAILLE, color = COULEUR)) +
    scale_color_manual(values = c("chocolate", "chartreuse4")) +
    scale_size_continuous(range = c(0.5, 6)) +
    coord_equal() + theme_nothing()
  return(popPlot)
}

PlotOneSampleComp <- function(df, sampone, val){
  df$TAILLE <- val
  sampIndex <- c(sampone$SAMP1, sampone$SAMP2)
  sampPlot <- ggplot() + 
    geom_point(data = df, aes(X, Y, size = TAILLE), color = "grey70") +
    geom_point(data = df[sampIndex, ], aes(X, Y, size = TAILLE, color = COULEUR)) +
    scale_color_manual(values = c("chocolate", "chartreuse4")) +
    scale_size_continuous(range = c(0.5, 6)) +
    coord_equal() + theme_nothing()
  return(sampPlot)
}

PlotHistComp <- function(df, sel, val){
  df$TAILLE <- val
  mean1 <- mean(df[sel$SAMP1, "TAILLE"])
  mean2 <- mean(df[sel$SAMP2, "TAILLE"])
  
  pDens <- ggplot() + 
    geom_density(data = df[sel$SAMP1, ], aes(TAILLE), fill = "chocolate", color = "grey30", alpha = 0.3) + 
    geom_density(data = df[sel$SAMP2, ], aes(TAILLE), fill = "chartreuse4", color = "grey30", alpha = 0.3) + 
    geom_vline(xintercept = c(mean1, mean2), color = c("chocolate", "chartreuse4"), size = 2) +
    scale_x_continuous("Taille", limits = c(50, 150)) + scale_y_continuous("Densité") +
    ggtitle(paste("Distribution de la taille par groupe : orange (moy = ", round(mean1, 2), ") ; vert (moy = ", round(mean2, 2), ")", sep = "")) +
    theme_bw()
  
  return(pDens)
}

GetOneSampleComp <- function(df, sampsize, val){
  df$TAILLE <- val
  sampIndex1 <- sample(x = 1:500, size = sampsize, replace = FALSE)
  sampIndex2 <- sample(x = 501:1000, size = sampsize, replace = FALSE)
  oneSamp1 <- df[sampIndex1, "TAILLE"]
  oneSamp2 <- df[sampIndex2, "TAILLE"]
  tVal <- t.test(x = oneSamp1, y = oneSamp2, paired = FALSE, var.equal = TRUE)$statistic
  return(list(TVAL = tVal, SAMP = list(SAMP1 = sampIndex1, SAMP2 = sampIndex2)))
}

GetTenSampleComp <- function(df, sampsize, val){
  df$TAILLE <- val
  tList <- vector()
  for(i in 1:10){
    sampIndex1 <- sample(x = 1:500, size = sampsize, replace = FALSE)
    sampIndex2 <- sample(x = 501:1000, size = sampsize, replace = FALSE)
    oneSamp1 <- df[sampIndex1, "TAILLE"]
    oneSamp2 <- df[sampIndex2, "TAILLE"]
    tVal <- t.test(x = oneSamp1, y = oneSamp2, paired = FALSE, var.equal = TRUE)$statistic
    tList <- append(tList, tVal)
  }
  return(list(TVAL = tList))
}

GetHundredSampleComp <- function(df, sampsize, val){
  df$TAILLE <- val
  tList <- vector()
  for(i in 1:100){
    sampIndex1 <- sample(x = 1:500, size = sampsize, replace = FALSE)
    sampIndex2 <- sample(x = 501:1000, size = sampsize, replace = FALSE)
    oneSamp1 <- df[sampIndex1, "TAILLE"]
    oneSamp2 <- df[sampIndex2, "TAILLE"]
    tVal <- t.test(x = oneSamp1, y = oneSamp2, paired = FALSE, var.equal = TRUE)$statistic
    tList <- append(tList, tVal)
  }
  return(list(TVAL = tList))
}


ShowDataTableComp <- function(val){
  seqNum <- seq(1, length(val), 1)
  seqChar <- ifelse(seqNum < 10, paste("000", seqNum, sep = ""),
                    ifelse(seqNum < 100, paste("00", seqNum, sep = ""),
                           ifelse(seqNum < 1000, paste("0", seqNum, sep = ""),
                                  as.character(seqNum))))
  df <- data.frame(ECHANTILLON = paste("ECHANTILLON", seqChar, sep = "_"),
                   StudentT = round(val, digits = 2))
  df <- df[order(df$ECHANTILLON, decreasing = TRUE), ]
  return(df)
}


PlotTDistrib <- function(tvalues, levalpha, sampsize){
  
  # compute probabilities
  lowerBound <- 0
  upperBound <- round(qt(p = 1 - (levalpha/2), df = sampsize - 1), digits = 2)
  maxX <- round(qt(p = 0.999, df = sampsize - 1), digits = 2)
  seqValues <- sort(c(-seq(0, maxX, .01), seq(0, maxX, .01)))
  seqValuesAbs <- seq(0, maxX, .01)
  seqProba <- c(rev(dt(x = seqValuesAbs, df = sampsize - 1)), dt(x = seqValuesAbs, df = sampsize - 1))
  
  # create tables
  tabT <- data.frame(SEQ = seqValues, PROBA = seqProba)
  
  # plot distribution
  tPlot <- ggplot() + 
    geom_density(data = data.frame(TVAL = tvalues), aes(x = TVAL), fill = "grey", color = "grey30", alpha = 0.5) + 
    geom_line(data = tabT, aes(x = SEQ, y = PROBA), color = "firebrick") +
    geom_vline(xintercept = c(-upperBound, upperBound), color = "firebrick2") + 
    annotate(geom = "text", 
             x = c(-upperBound, upperBound), 
             y = 0.05, 
             label = c(formatC(-upperBound, digits = 2, format = "f"), formatC(upperBound, digits = 2, format = "f")), 
             fontface = 2) +
    scale_x_continuous("Valeur du T de Student") +
    scale_y_continuous("Densité de probabilité") +
    theme_bw()
  
  return(tPlot)
}





# chi2 estimation ----

GetOneSampleChi <- function(df, sampsize){
  sampIndex <- sample(x = 1:1000, size = sampsize, replace = FALSE)
  oneSamp <- df[sampIndex, ]
  contTab <- table(oneSamp$FORME, oneSamp$COULEUR)
  contTabMat <- dcast(as.data.frame(contTab), formula = Var2 ~ Var1, value.var = "Freq")[, -1]
  row.names(contTabMat) <- c("N", "O")
  resChi <- chisq.test(oneSamp$FORME, oneSamp$COULEUR, correct = FALSE)
  chi2 <- resChi$statistic
  contTabExp <- resChi$expected
  return(list(CONT = contTab, CONTEXP = contTabExp, CONTMAT = contTabMat, CHI = chi2, SAMP = sampIndex))
}

GetTenSampleChi <- function(df, sampsize){
  chiList <- vector()
  for(i in 1:10){
    oneSamp <- df[sample(x = 1:1000, size = sampsize, replace = FALSE), ]
    chi2Temp <- chisq.test(oneSamp$FORME, oneSamp$COULEUR, correct = FALSE)$statistic
    chiList <- append(chiList, chi2Temp)
  }
  return(list(CHI = chiList))
}

GetHundredSampleChi <- function(df, sampsize){
  chiList <- vector()
  for(i in 1:100){
    oneSamp <- df[sample(x = 1:1000, size = sampsize, replace = FALSE), ]
    chi2Temp <- chisq.test(oneSamp$FORME, oneSamp$COULEUR, correct = FALSE)$statistic
    chiList <- append(chiList, chi2Temp)
  }
  return(list(CHI = chiList))
}

ShowDataTableChi <- function(val){
  seqNum <- seq(1, length(val), 1)
  seqChar <- ifelse(seqNum < 10, paste("000", seqNum, sep = ""),
                    ifelse(seqNum < 100, paste("00", seqNum, sep = ""),
                           ifelse(seqNum < 1000, paste("0", seqNum, sep = ""),
                                  as.character(seqNum))))
  df <- data.frame(ECHANTILLON = paste("ECHANTILLON", seqChar, sep = "_"),
                   CHI2 = round(val, digits = 5))
  df <- df[order(df$ECHANTILLON, decreasing = TRUE), ]
  return(df)
}

PlotPopChi <- function(df){
  popPlot <- ggplot(df) + 
    geom_point(aes(X, Y, shape = FORME, color = COULEUR), size = 4) +
    scale_color_manual(values = c("chocolate", "chartreuse4")) +
    coord_equal() + theme_nothing()
  return(popPlot)
}

PlotOneSampleChi <- function(df, sampone){
  sampPlot <- ggplot() + 
    geom_point(data = df, aes(X, Y, shape = FORME), color = "grey70", size = 4) +
    geom_point(data = df[sampone, ], aes(X, Y, shape = FORME, color = COULEUR), size = 4) +
    scale_color_manual(values = c("chocolate", "chartreuse4")) +
    coord_equal() + theme_nothing()
  return(sampPlot)
}


PlotChiDistrib <- function(chivalues, levalpha){
  
  # compute probabilities
  lowerBound <- 0
  upperBound <- round(qchisq(p = 1 - levalpha, df = 1), digits = 2)
  maxX <- round(qchisq(p = 0.999, df = 1), digits = 2)
  seqValues <- seq(0, maxX, .01)
  seqProba <- dchisq(x = seqValues, df = 1)
  
  # create tables
  tabChi <- data.frame(SEQ = seqValues, PROBA = seqProba)
  
  # plot distribution
  chiPlot <- ggplot() + 
    geom_density(data = data.frame(CHI = chivalues), aes(x = CHI), fill = "grey", color = "grey30", alpha = 0.5) + 
    geom_line(data = tabChi, aes(x = SEQ, y = PROBA), color = "firebrick") +
    geom_vline(xintercept = upperBound, color = "firebrick2") + 
    annotate(geom = "text", x = upperBound+0.4, y = 0.5, label = formatC(upperBound, digits = 2, format = "f"), color = "firebrick2", fontface = 2) +
    scale_x_continuous("Valeur de la statistique Chi2") +
    scale_y_continuous("Densité de probabilité", limits = c(0, 1.2)) + 
    theme_bw()
  
  return(chiPlot)
}

PlotMosaic <- function(varx, vary, df){
  tabCont <- structable(formula = formula(eval(paste(vary, "~", varx, sep = " "))), data = df)
  valLabs <- round(tabCont, 0)
  mosaic(tabCont)
}



