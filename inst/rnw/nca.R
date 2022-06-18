# non-compartmental analysis powered by package ncappc


df <- read.csv("hamster_lung.csv", check.names = FALSE)

colnames(df) <- gsub("[^[:alnum:]]", "", colnames(df))



res <- data.frame(
  ncappc::ncappc(obsFile = df,
                 doseAmtNm = 'AMT',
                 doseType = 'ns',
                 adminType = "extravascular",
                 doseTime = 0,
                 Tau = 5,
                 LambdaTimeRange = c(1,4),
                 LambdaExclude = 5,
                 onlyNCA = TRUE,
                 extrapolate = FALSE,
                 printOut = FALSE,
                 evid = FALSE,
                 noPlot = TRUE)
)

colnames(res) <- gsub("ncaOutput.", "",colnames(res))

#plotly
p <- ggplot2::ggplot(df, aes(x=as.numeric(TIME), y=as.numeric(DV))) +
  ggplot2::geom_point()

 
  plotly::rangeslider(plotly::ggplotly(p, dynamicTicks = T), borderwidth = 1)


# make histogram
#res_spark <- cbind(
#  sapply(res, function(x){
#    ifelse(
#      is.na(x),
#      NA,
#      hist(x, plot=F)['counts']
#    )
#  })[1,]
#)
sparkline::sparkline(
  c(ifelse(
    is.na(res$Dose),
    NA,
    hist(res$Dose, plot=FALSE, breaks=seq(min(res$Dose),max(res$Dose),length=10))['counts']
  )[[1]])
  ,type='bar'
)

hist(res$Dose, plot=FALSE, breaks=seq(min(res$Dose),max(res$Dose),length=10))['counts'][[1]]


res_spark <- ifelse(
        is.na(res$Dose),
        NA,
        hist(res$Dose, plot=F)['counts'])[[1]]

res_spark <- res_spark[1,]
res_spark <- cbind(res_spark)

res <- rbind(res, t(res_spark))

#ncappc(obsFile = "nca_original.npctab.dta",
#       simFile = "nca_simulation.1.npctab.dta.zip",
#       str1Nm = NULL,
#       str1 = NULL,
#       str2Nm = NULL,
#       str2 = NULL,
#       str3Nm = NULL,
#       str3 = NULL, 
#       concUnit = NULL,
#       timeUnit = NULL,
#       doseUnit = NULL,
#       obsLog = FALSE,
#       simLog = obsLog,
#       psnOut = TRUE,
#       idNmObs = "ID",
#       timeNmObs = "TIME",
#       concNmObs = "DV",
#       idNmSim = idNmObs,
#       timeNmSim = timeNmObs,
#       concNmSim = concNmObs,
#       onlyNCA = FALSE,
#       AUCTimeRange = NULL,
#       backExtrp = FALSE,
#       LambdaTimeRange = NULL,
#       LambdaExclude = NULL,
#       doseAmtNm = NULL,
#       adminType = "extravascular",
#       doseType = "ns",
#       doseTime = NULL,
#       Tau = NULL,
#       TI = NULL,
#       method = "linearup-logdown",
#       blqNm = NULL,
#       blqExcl = 1,
#       evid = TRUE,
#       evidIncl = 0,
#       mdv = FALSE,
#       filterNm = NULL,
#       filterExcl = NULL,
#       negConcExcl = FALSE,
#       param = c("AUClast", "Cmax"),
#       timeFormat = "number",
#       dateColNm = NULL,
#       dateFormat = NULL,
#       spread = "npi",
#       tabCol = c("AUClast", "Cmax", "Tmax", "AUCINF_obs", "Vz_obs", "Cl_obs",
#                  "HL_Lambda_z"),
#       figFormat = "tiff",
#       noPlot = FALSE,
#       printOut = TRUE,
#       studyName = NULL,
#       new_data_method = TRUE,
#       overwrite_SIMDATA = NULL,
#       overwrite_sim_est_file = NULL,
#       outFileNm = NULL,
#       out_format = "html",
#       gg_theme = theme_bw(),
#       parallel = FALSE,
#       extrapolate = FALSE,
#       timing = FALSE)