#' SurvivalCalc
#'
#' SurvivalCalc removes nogrowth, contaminates and unexpected growth from experiments and then passes
#' the modified results to both survivalIntegral and survivalPercentage for calculations, followed by statsCLS for statistics.
#'
#' @param firstDay Specify the first day of measurements, default=1
#' @param resultspath System path where the results are stored, default=current directory
#' @param rmflagged This determines if flagged wells are removed from CLS statistics, default=TRUE
#' @param statsCLS Should the code run statsCLS on the CLS results (TRUE/FALSE), default=TRUE
#' @param measureInterval Interval in minutes for each OD measurement, default=15

SurvivalCalc<- function(firstDay = 1, resultspath = getwd(), rmflagged=TRUE, statsCLS=TRUE, measureInterval=15)
{
  if(!dir.exists(resultspath))
  {
    print(paste0("Directory: ", resultspath," not found, confirm it exists and you have permission to access it."))
    return(-1)
  }
  setwd(resultspath)                                              # set directory to resultspath (location of raw results files)
  survivalanalysis <-paste0(getwd(), "/Survivalanalysis")         # create survivalanalysis which identifies the path for the survival analysis dir
  dir.create(paste0("Survivalanalysis"), showWarnings = FALSE)    # creates directory Survivalanalysis within current working directory
  homedir<-RAWpath<-survivalanalysis                              # passes homedir and RAWpath to SurvivalPercentage and SurvivalIntegral
  file_list <-
    list.files(pattern = "[[:alnum:]]*_Day_[[:digit:]]*.csv")
  if(length(file_list)==0)
  {
    print(paste0("No csv files found to analyze in directory ",resultspath))
    return(-1)
  }
  if(length(grep("results-", file_list))==0)
  {
    cat("WARNING: No csv files of the format results-<experiment>_Day_##.csv.
        This could mean the path specified isn't the one with generated results
        from OGA(); check that the path passed is correct. If the files are modified
        and the names are changed to exclude results- in the name, you can ignore this warning.\n")
  }
  empty_list <- vector(mode = "list", length = length(file_list))
  if(rmflagged)
  {
    for (cleaning in 1:length(file_list))
    {
      df<-as.data.frame(read.csv(file_list[cleaning], row.names = 1, stringsAsFactors=FALSE))
      empty_list[[cleaning]]<-which(df[7,]!="No Growth" & df[8,]!="Unexpected Growth" & df[10,]!="CONTAMINATION")
      rm(df)
    }
    xh<-Reduce(intersect, empty_list)
    for (cleaning in 1:length(file_list))
    {
      df<-as.data.frame(read.csv(file_list[cleaning], row.names = 1, stringsAsFactors=FALSE))
      df<-df[,xh]
      write.csv (df, file.path(survivalanalysis, file = paste0("results-flagged-wells-removed",file_list[cleaning])))
      rm(df)
    }
    rm(xh, file_list, empty_list)
  }else
  {
    for (cleaning in 1:length(file_list))
    {
      df<-as.data.frame(read.csv(file_list[cleaning], row.names = 1, stringsAsFactors=FALSE))
      write.csv (df, file.path(survivalanalysis, file = paste0("results-flagged-wells-NOT-removed",file_list[cleaning])))
      rm(df)
    }
  }
  SurvivalPercentage(RAWpath,firstDay,measureInterval)
  SurvivalIntegral(homedir, fileName = "SurvivalPercentage.csv")
  if(statsCLS) { statsCLS() }
}


#' CLSCalc
#'
#' CLSCalc calculates the fraction surviving (sn) between two time points.
#'
#' @param delta is the time shift associated with the measurements.
#' @param measureInterval Time between each OD measurement in minutes. Default = 15
#' @param doubleTime Doubling time for the specific specimen of interest in seconds. Default = 90*60

CLSCalc <- function(delta, measureInterval=15, doubleTime = 90*60)
{
  Sn = 100* (1 / (2^((delta*measureInterval) / doubleTime)))
  return(Sn)
}

#' SurvivalPercentage
#'
#' This function computes percentage of cells still alive. This function looks for files
#' that have the filename of the syntax <experiment>_Day_###.csv , where <experiment> can be
#' any unique ID to define the experiment, _Day_ is a marker to identify when the day is
#' going to be specified and ### is the actual day that growth rate measurements were started.
#'
#' @param RAWpath path where the raw data files are located. Default = current working directory
#' @param firstDay First day of measurements. Default = 1
#' @param measureInterval Time between each OD measurement in minutes. Default = 15

SurvivalPercentage <- function(RAWpath = getwd(), firstDay = 1, measureInterval=15)
{

  if(!dir.exists(RAWpath))
  {
    print(paste0("Directory: ", RAWpath," not found, confirm it exists and you have permission to access it."))
    return(-1)
  }
  setwd(RAWpath)
  file_list <-
    list.files(pattern = "[[:alnum:]]*_Day_[[:digit:]]*.csv") # list all files in current working directory with extension.csv and Day notation

  if(length(file_list)==0)
  {
    print(paste0("No viable files found in ",RAWpath))
    return(-1)
  }
  if(length(grep("results-", file_list))==0)
  {
    cat("WARNING: No csv files of the format results-<experiment>_Day_##.csv.
         This could mean the path specified isn't the one with generated results
         from OGA(); check that the path passed is correct. If the files are modified
        and the names are changed to exclude results- in the name, you can ignore this warning.\n")

  }
  if(length(grep(pattern=paste0("*",firstDay,".csv"),file_list))==0) {
    print(paste0("Could not find firstDay csv file, please confirm you have a csv file for that day. Format: <experiment>_Day_",firstDay,".csv"))
    return(-1)
  }
  print("Calculating Survival Percentage")
  for (i in 1:length (file_list))
    # loop 1:x total number of CSV files in directory
  {
    assign(paste0(sub('.csv', '', basename(file_list[i]))), read.csv(file_list[i])) # read .csv into dataframe with the same name as original csv without extension.csv
  }
  dfs <-
    Filter(f = function(x) is(x, "data.frame"), mget(ls(pattern="_Day_"))) # create nested list of all dataframes
  # This creates a data frame that specifically holds the upper limit(ul.df) and doubling time(dt.df)
  firstDaygrep = paste0("Day_",firstDay)

  fullDt <- ul.df <- data.frame()
  dt.df <- dfs[[grep(paste0(firstDaygrep,"$"),names(dfs))]][1,]
  qvalue <- c()
  strsplit(names(dfs[1]),"_")[[1]][2]

  for(x in seq_along(dfs))
  {
    qvalue[x] <- as.numeric(gsub("([0-9]+).*$", "\\1", strsplit(names(dfs[x]),"_")[[1]][3]))
    ul.df <- rbind(ul.df,dfs[[x]][4,])
    fullDt <- rbind(fullDt,dfs[[x]][1,])
  }
  # sortedDt is a sorted doubling time data frame so that the survival data frame is in a manner
  # that is age+1 is the next element in the data frame so solving the survival function
  # makes sense
  ul.df[,1] <- qvalue
  fullDt[,1] <- qvalue
  colnames(fullDt)[1]<-"Time"
  fullDt <- fullDt[order(fullDt$Time),]
  View(fullDt)
  checkDoubleTimes(fullDt)
  colnames(ul.df)[1]<-"Time"
  sortedDt <- ul.df[order(ul.df$Time),]
  survDt <- sortedDt
  survDt[1,] <- 100
  survDt[1,1] <- 1
  colnames(survDt) <- names(dfs[[1]])
  for(z in 2:length(sortedDt[1,]))
  {
    for(y in 2:length(qvalue))
    {
      Sn <-CLSCalc((as.numeric(sortedDt[y,z])-as.numeric(sortedDt[1,z])), measureInterval, dt.df[1,z] )
      survDt[y,z]<-format(round(Sn,2),nsmall=2)
    }
    write.csv(survDt,file="SurvivalPercentage.csv",row.names = FALSE)
  }
}

#' SurvivalIntegral
#'
#' Calculate the area under the curve as determined by the survival percentage for an experiment.
#'
#' @param homedir Path where a csv containing the percent surviving for an experiment lives. default=getwd()
#' @param fileName Name of the file with the experimental data. default="SurvivalPercentage.csv"

SurvivalIntegral <- function(homedir=getwd(), fileName = "SurvivalPercentage.csv")
{
  setwd(homedir)
  if(!file.exists(fileName))
  {
    print(paste0("Could not find  file: ",fileName))
    print("Confirm that this file is in the working directory or specify a different file with laddername=<file>")
    return(-1)
  }
  print("Calculating Survival Integral")
  ul.df <- read.csv(file = fileName)
  colnames(ul.df)[1]<-"Time"
  ul.df <- ul.df[order(ul.df$Time),]
  SI.mat <- matrix(rep(0,length(names(ul.df))),nrow=1)
  count <- 1
  for(name in names(ul.df))
  {
    SI <- 0
    for(n in 2:length(ul.df[,1]))
    {
      SI <- SI+((((ul.df[n,name])+ul.df[n-1,name])/2)*(ul.df[n,1]-ul.df[n-1,1]))
    }
    SI.mat[count]<-SI
    count <- count + 1
  }
  SI.df <- data.frame(SI.mat)
  colnames(SI.df) <- names(ul.df)
  ul.df <- rbind(ul.df,SI.df)
  ul.df[length(ul.df[,1]),1] <- 0
  write.csv(ul.df,file="SurvivalIntegral.csv",row.names = FALSE)

}

#' checkDoubleTimes
#'
#' Calculate the area under the curve as determined by the survival percentage for an experiment.
#'
#' @param dt.df data frame with the doubling times that need to be checked for high variation
#' @param acceptablePercentage What percentage either up or down that is acceptable in the computed difference between doubling times. default = 25%

checkDoubleTimes <- function(dt.df,acceptablePercentage = 25)
{
  TimeDiff <- matrix("NoDiff",nrow = length(dt.df[,1])-1, ncol = 1)
  for(l in 1:length(TimeDiff))
  {
    TimeDiff[l] <- paste0(dt.df$Time[l+1],"-",dt.df$Time[l])
  }
  results.df <- data.frame(TimeDiff)
  print(typeof(results.df))
  df.colnames <- matrix(colnames(dt.df), nrow = 1)

  for(z in 2:length(dt.df[1,]))
  {
    results.matrix <- matrix(TRUE,nrow = length(dt.df[,1])-1, ncol = 1)
    for(n in 2:length(dt.df[,1]))
    {
      #print(paste0("Calculating: ",dt.df[n,z],"Divided by ",dt.df[n-1,z]))
      dtDiff <- dt.df[n,z]/dt.df[n-1,z]*100
      #print(paste0("Results: ", dtDiff))
      if((dtDiff > 100+acceptablePercentage) || (dtDiff < 100-acceptablePercentage))
      {
        results.matrix[n-1] = FALSE
      }
    }
    newName <- df.colnames[z]
    # print(newName)
    # print(results.matrix)
    results.df[newName] <- as.data.frame(results.matrix)
    # print(results.df)
    # Sys.sleep(10)
  }
  print(results.df)
  print(typeof(results.df))

}


###################### These are some potential other greps depending on final result of files.
# ############## saved for use later to get names more clearly dealt with
# > list.files(pattern = "results-[[:alnum:]]*_Day_[[:digit:]]*.csv")
# character(0)
# > list.files(pattern = "[[:alnum:]]*_Day_[[:digit:]]*\\D*.csv")
# [1] "EMSQCLS_Day_14.csv" "EMSQCLS_Day_21.csv" "EMSQCLS_Day_28.csv" "EMSQCLS_Day_7.csv"
# > list.files(pattern = "[[:alnum:]]*_Day_[[:digit:]]*\\D.csv")
# character(0)
# > list.files(pattern = "[[:alnum:]]*_Day_[[:digit:]]*\\D*.csv")
# [1] "EMSQCLS_Day_14.csv" "EMSQCLS_Day_21.csv" "EMSQCLS_Day_28.csv" "EMSQCLS_Day_7.csv"
# > list.files(pattern = "[[:alnum:]]*_Day_[[:digit:]]*[:graph:]*.csv")
# [1] "EMSQCLS_Day_14.csv" "EMSQCLS_Day_21.csv" "EMSQCLS_Day_28.csv" "EMSQCLS_Day_7.csv"
# > list.files(pattern = "[[:alnum:]]*_Day_[[:digit:]]*[[:graph:]]*.csv")
# [1] "EMSQCLS_Day_14.csv"            "EMSQCLS_Day_1gross78stuff.csv" "EMSQCLS_Day_21.csv"
# [4] "EMSQCLS_Day_28.csv"            "EMSQCLS_Day_7.csv"
# > library(stringr)
# > str_extract(list.files(pattern = "[[:alnum:]]*_Day_[[:digit:]]*[[:graph:]]*.csv"),"Day_[[:digit:]]*")
# [1] "Day_14" "Day_1"  "Day_21" "Day_28" "Day_7
