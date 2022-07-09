#' Esta funcao serve para calcular os coeficientes das equacoes de
#' chuvas intensas

#' @export

ScriptIDF = function(ArquivPrec){

  rm(list = ls())

  AnoChuvaMaxima <- ArquivPrec %>%
    dplyr::mutate(Ano = lubridate::year(lubridate::dmy(Data))) %>%
    dplyr::group_by(Ano) %>%
    dplyr::arrange(-Prec) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  ArquivPrecCrescente <- sort(c(ArquivPrec$Prec))

  TamanhoArquivPrec <- length(ArquivPrec$Prec)

  ArquivTr <- c(2,5,10,15,20,25,50,75,100)
  TamanhoArquivTr <- length(ArquivTr)

  Probab <- 1/ArquivTr

  ArquivDuracoes <- c(5,10,15,20,30,60,360,480,720,1440) #(em minutos)
  TamanhoArquivDuracoes <- length(ArquivDuracoes)

  valcrit <- c(0.254)

  #Identifica a distribuicao probabilistica que melhor se ajusta a serie historica
  TesteAderencia = function(ArquivPrecCrescente){

    Fr <- matrix(0,length(ArquivPrecCrescente),1)

    for (j in 1:length(ArquivPrecCrescente)) {

      Fr[j] <- j/(length(ArquivPrecCrescente)+1)

    }

    modweibull <- fitdist(ArquivPrecCrescente, "weibull")
    dstweibull <- pweibull(ArquivPrecCrescente,modweibull$estimate[1],modweibull$estimate[2])

    modgumbel <- evd::fgev(ArquivPrecCrescente,shape=0)
    dstgumbel <- evd::pgev(ArquivPrecCrescente,modgumbel$estimate[1],modgumbel$estimate[2])

    modGEV <- evd::fgev(ArquivPrecCrescente)
    dstGEV <- evd::pgev(ArquivPrecCrescente,modGEV$estimate[1],modGEV$estimate[2],modGEV$estimate[3])

    dstpearson <- PearsonDS::ppearson0(ArquivPrecCrescente,mean=mean(ArquivPrecCrescente),sd=sd(ArquivPrecCrescente))
    dstpearsonIII <- smwrBase::ppearsonIII(ArquivPrecCrescente,mean(ArquivPrecCrescente),sd(ArquivPrecCrescente),
                                           e1071::skewness(ArquivPrecCrescente,type=1))
    dstlpearsonIII <- smwrBase::plpearsonIII(ArquivPrecCrescente,mean(log(ArquivPrecCrescente)),sd(log(ArquivPrecCrescente)),
                                             e1071::skewness(log(ArquivPrecCrescente),type=1))

    fxweibull <- matrix(0,length(ArquivPrecCrescente),1)
    fxgumbel <- matrix(0,length(ArquivPrecCrescente),1)
    fxGEV <- matrix(0,length(ArquivPrecCrescente),1)
    fxpearson <- matrix(0,length(ArquivPrecCrescente),1)
    fxpearsonIII <- matrix(0,length(ArquivPrecCrescente),1)
    fxlpearsonIII <- matrix(0,length(ArquivPrecCrescente),1)

    for (l in 1:length(ArquivPrecCrescente)) {

      fxweibull[l] <- abs(dstweibull[l]-Fr[l])
      fxgumbel[l] <- abs(dstgumbel[l]-Fr[l])
      fxGEV[l] <- abs(dstGEV[l]-Fr[l])
      fxpearson[l] <- abs(dstpearson[l]-Fr[l])
      fxpearsonIII[l] <- abs(dstpearsonIII[l]-Fr[l])
      fxlpearsonIII[l] <- abs(dstlpearsonIII[l]-Fr[l])

    }

    ksweibull <- max(fxweibull)
    ksGumbel <- max(fxgumbel)
    ksGEV <- max(fxGEV)
    ksPearson <- max(fxpearson)
    ksPearsonIII <- max(fxpearsonIII)
    ksLogPearsonIII <- max(fxlpearsonIII)

    dfTestAdr <- data.frame(
      Distribuicoes = c("Weibull","Gumbel","GEV","Pearson","PearsonIII","LogPearsonIII"),
      Resultados = c(round(ksweibull,4),round(ksGumbel,4),round(ksGEV,4),round(ksPearson,4),
                     round(ksPearsonIII,4),round(ksLogPearsonIII,4)))

    dfTA <- dfTestAdr %>%
      dplyr::filter(Resultados == min(Resultados))

    my_list <- list(dfTA=dfTA,modweibull=modweibull,modgumbel=modgumbel,modGEV=modGEV)

    return(my_list)

  }

  dfTAR <- TesteAderencia(ArquivPrecCrescente)$dfTA$Resultados
  dfTAD <- TesteAderencia(ArquivPrecCrescente)$dfTA$Distribuicoes

  if(dfTAR > valcrit){

    stop("ATENCAO, nenhuma distribuicao se ajustou a serie historica de chuva!")

  }else{

    modweibull <- TesteAderencia(ArquivPrecCrescente)$modweibull
    modgumbel <- TesteAderencia(ArquivPrecCrescente)$modgumbel
    modGEV <- TesteAderencia(ArquivPrecCrescente)$modGEV

    #Determina as intensidades maximas observadas a partir da distribuicao probabilistica que melhor se ajustou a serie historica
    DistProb = function(dfTAD,Probab,modweibull,modgumbel,modGEV,ArquivPrecCrescente,TamanhoArquivDuracoes,TamanhoArquivTr,ArquivDuracoes){

      result <- switch(dfTAD,
                       "Weibull"= cat(X <- c(qweibull(1-Probab,modweibull$estimate[1],modweibull$estimate[2]))),
                       "Gumbel"= cat(X <- c(evd::qgev(1-Probab,modgumbel$estimate[1],modgumbel$estimate[2]))),
                       "GEV"= cat(X <- c(evd::qgev(1-Probab,modGEV$estimate[1],modGEV$estimate[2],modGEV$estimate[3]))),
                       "Pearson"= cat(X <- c(PearsonDS::qpearson0(1-Probab,mean(ArquivPrecCrescente),sd(ArquivPrecCrescente)))),
                       "PearsonIII"= cat(X <- c(smwrBase::qpearsonIII(1-Probab,mean(ArquivPrecCrescente),sd(ArquivPrecCrescente),
                                                                      e1071::skewness(ArquivPrecCrescente,type=1)))),
                       "LogPearsonIII"= cat(X <- c(smwrBase::qlpearsonIII(1-Probab,mean(log(ArquivPrecCrescente)),sd(log(ArquivPrecCrescente)),
                                                                          e1071::skewness(log(ArquivPrecCrescente),type=1)))))

      PMax <- matrix(0,TamanhoArquivDuracoes-1,TamanhoArquivTr)
      PMax24h <- matrix(0,1,TamanhoArquivTr)

      for (b in 1:TamanhoArquivTr) {

        PMax24h[b] <- X[b]*1.14

        for (f in 1:(TamanhoArquivDuracoes-1)) {

          PMax[f,b] <- PMax24h[b]*(exp(1.5*log((log(ArquivDuracoes[f])/7.3))))

        }

      }

      PMaxCorrig <- matrix(0,TamanhoArquivDuracoes,TamanhoArquivTr)

      for (l in 1:TamanhoArquivTr) {

        PMaxCorrig[1,l] <- ifelse(PMax[1,l]<8,8,PMax[1,l])
        PMaxCorrig[2,l] <- ifelse(PMax[2,l]<10,10,PMax[2,l])
        PMaxCorrig[3,l] <- ifelse(PMax[3,l]<15,15,PMax[3,l])
        PMaxCorrig[4,l] <- ifelse(PMax[4,l]<15,15,PMax[4,l])
        PMaxCorrig[5,l] <- ifelse(PMax[5,l]<20,20,PMax[5,l])
        PMaxCorrig[6,l] <- ifelse(PMax[6,l]<25,25,PMax[6,l])
        PMaxCorrig[7,l] <- ifelse(PMax[7,l]<40,40,PMax[7,l])
        PMaxCorrig[8,l] <- ifelse(PMax[8,l]<40,40,PMax[8,l])
        PMaxCorrig[9,l] <- ifelse(PMax[9,l]<47,47,PMax[9,l])
        PMaxCorrig[10,l] <- ifelse(PMax24h[l]<55,55,PMax24h[l])

      }

      IMaxObs <- matrix(0,TamanhoArquivDuracoes,TamanhoArquivTr)

      for (f in 1:TamanhoArquivTr) {

        for (g in 1:TamanhoArquivDuracoes) {

          IMaxObs[g,f] <- (PMaxCorrig[g,f]/ArquivDuracoes[g])*60

        }

      }

      my_list1 <- list(IMaxObs=IMaxObs)

      return(my_list1)

    }

    IMaxObs <- DistProb(dfTAD,Probab,modweibull,modgumbel,modGEV,ArquivPrecCrescente,TamanhoArquivDuracoes,TamanhoArquivTr,ArquivDuracoes)$IMaxObs

    #Otimiza os coeficientes das equacoes de chuvas intensas
    Otimiza = function(TamanhoArquivDuracoes,TamanhoArquivTr,ArquivTr,ArquivDuracoes,IMaxObs){

      IMaxSim <- matrix(0,TamanhoArquivDuracoes,TamanhoArquivTr)
      erro <- matrix(0,TamanhoArquivDuracoes,TamanhoArquivTr)
      Sum.erroInicial <- matrix(0,1,TamanhoArquivTr)

      function.min <- function(par) {
        for (i in 1:TamanhoArquivTr) {
          for (j in 1:TamanhoArquivDuracoes) {
            IMaxSim[j,i] <- (par[1]*(ArquivTr[i])^par[2])/((ArquivDuracoes[j]+par[3])^par[4])
            erro[j,i] <- abs(IMaxSim[j,i]-IMaxObs[j,i])/IMaxObs[j,i]
          }
          Sum.erroInicial[1,i] <- sum(erro[,i])
        }
        sum(Sum.erroInicial[1,])
      }

      optmin <- nlminb(c(0,0,0,0),function.min,control=list(trace=TRUE,
                                                            iter.max=100000,eval.max=20000),
                       lower=c(0,0,0,0),upper=c(Inf,Inf,Inf,Inf))

      kinicial <- optmin$par[1]
      minicial <- optmin$par[2]
      t0inicial <- optmin$par[3]
      ninicial <- optmin$par[4]

      erro1 <- matrix(0,TamanhoArquivDuracoes*TamanhoArquivTr,1)
      erro2 <- matrix(0,TamanhoArquivDuracoes*TamanhoArquivTr,1)

      iter <- 0

      function.NS <- function(par) {
        for (k in 1:TamanhoArquivTr) {
          for (l in 1:TamanhoArquivDuracoes) {
            IMaxSim[l,k] <- (par[1]*(ArquivTr[k])^par[2])/((ArquivDuracoes[l]+par[3])^par[4])
            erro1[l+iter,1] <- (IMaxSim[l,k]-IMaxObs[l,k])^2
            erro2[l+iter,1] <- (IMaxObs[l,k]-mean(IMaxObs[,k]))^2
          }
          iter <- iter+1
        }
        (1-(sum(erro1)/sum(erro2)))
      }

      function.max <- function(par) -function.NS(par)

      optmax <- nlminb(c(kinicial,minicial,t0inicial,ninicial),function.max,control=list(trace=TRUE,
                                                                                         iter.max=100000,eval.max=20000),
                       lower=c(0.01,0.01,0.01,0.01),upper=c(Inf,Inf,Inf,Inf))

      kotim <- optmax$par[1]
      motim <- optmax$par[2]
      t0otim <- optmax$par[3]
      notim <- optmax$par[4]

      my_list2 <- list(kotim=kotim,motim=motim,t0otim=t0otim,notim=notim)

      return(my_list2)

    }

    kotim <- Otimiza(TamanhoArquivDuracoes,TamanhoArquivTr,ArquivTr,ArquivDuracoes,IMaxObs)$kotim
    motim <- Otimiza(TamanhoArquivDuracoes,TamanhoArquivTr,ArquivTr,ArquivDuracoes,IMaxObs)$motim
    t0otim <- Otimiza(TamanhoArquivDuracoes,TamanhoArquivTr,ArquivTr,ArquivDuracoes,IMaxObs)$t0otim
    notim <- Otimiza(TamanhoArquivDuracoes,TamanhoArquivTr,ArquivTr,ArquivDuracoes,IMaxObs)$notim

    #Gera uma tabela contendo os principais resultados
    Saida = function(TamanhoArquivDuracoes,TamanhoArquivTr,kotim,ArquivTr,motim,ArquivDuracoes,t0otim,notim,ArquivPrec,AnoChuvaMaxima,IMaxObs,dfTAD){

      IMaxSim <- matrix(0,TamanhoArquivDuracoes,TamanhoArquivTr)

      for (l in 1:TamanhoArquivTr) {

        for (w in 1:TamanhoArquivDuracoes) {

          IMaxSim[w,l] <- ((kotim)*(ArquivTr[l])^(motim))/((ArquivDuracoes[w]+(t0otim))^(notim))

        }

      }

      AnoSerieInicio <- min(ArquivPrec$Data)
      AnoSerieFim <- max(ArquivPrec$Data)

      TamanhoSerie <- c((AnoSerieFim-AnoSerieInicio)+1)

      AnoMaxPrec <- c(AnoChuvaMaxima$Data)
      MaxPrec <- c(AnoChuvaMaxima$Prec)

      K <- c(round(kotim,2))
      m <- c(round(motim,3))
      t0 <- c(round(t0otim,2))
      n <- c(round(notim,3))

      Distribuicao <- c(dfTAD)

      NS <- c(abs(round(min(NSE(IMaxSim,IMaxObs)),3)))
      R2 <- c(round(min(br2(IMaxSim,IMaxObs)),3))
      RMSE <- c(round(max(rmse(IMaxSim,IMaxObs)),3))
      MAE <- c(round(max(mae(IMaxSim,IMaxObs)),3))

      TabFinal <- data.frame(AnoSerieInicio,AnoSerieFim,TamanhoSerie,MaxPrec,AnoMaxPrec,K,m,t0,n,Distribuicao,
                             NS,R2,RMSE,MAE) %>%
        setNames(.,c("Beginning of the historical series","End of the historical series","Historical series size",
                     "Maximum daily precipitation","Year of maximum precipitation","Coefficient - K","Coefficient - m",
                     "Coefficient - t0","Coefficient - n","Probability distribution","Statistic - NS","Statistic - R2",
                     "Statistic - RMSE","Statistic - MAE"))

      my_list3 <- list(TabFinal=TabFinal)

      return(my_list3)

    }

    TabelaResultados <- Saida(TamanhoArquivDuracoes,TamanhoArquivTr,kotim,ArquivTr,motim,ArquivDuracoes,t0otim,notim,ArquivPrec,AnoChuvaMaxima,IMaxObs,dfTAD)$TabFinal

    View(TabelaResultados)

    #Gera graficos de Intensidade-Duracao-Frequencia e de Precipitacao-Duracao-Frequencia
    GraficosIDF = function(ArquivTr,kotim,motim,t0otim,notim){

      D <- data.frame(seq(6,1140,1)) %>%
        setNames(c("Duracoes"))

      Int <- matrix(0,length(D[,1]),length(ArquivTr[]))

      for (i in 1:length(ArquivTr[])) {

        for (j in 1:length(D[,1])) {

          Int[j,i] <- (kotim*ArquivTr[i]^motim)/((D[j,1]+t0otim)^notim)

        }

      }

      plot(Int[,1],
           xlim = c(0,420),
           ylim = c(0,300),
           main="Intensity-Duration-Frequency Curves",
           xlab="Time (min)",
           ylab="Intensity (mm/h)",
           type="l",
           col="blue")
      lines(Int[,4], col="red")
      lines(Int[,7], col="orange")
      lines(Int[,9], col="black")
      legend("topright",
             c("2 years","15 years","50 years","100 years"),
             fill=c("blue","red","orange","black"))

      Prec <- matrix(0,length(D[,1]),length(ArquivTr[]))

      for (i in 1:length(ArquivTr[])) {

        for (j in 1:length(D[,1])) {

          Prec[j,i] <- (Int[j,i]*D[j,1])/60

        }

      }

      plot(Prec[,1],
           xlim = c(0,420),
           ylim = c(0,300),
           main="Precipitation-Duration-Frequency Curves",
           xlab="Time (min)",
           ylab="Precipitation (mm)",
           type="l",
           col="blue")
      lines(Prec[,4], col="red")
      lines(Prec[,7], col="orange")
      lines(Prec[,9], col="black")
      legend("topleft",
             c("2 years","15 years","50 years","100 years"),
             fill=c("blue","red","orange","black"))

      return()

    }

    PlotGraficos <- GraficosIDF(ArquivTr,kotim,motim,t0otim,notim)

  }

  cat("\014")

}
