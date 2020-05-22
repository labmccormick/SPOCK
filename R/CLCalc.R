###
### Commented out print statements are lazy man debugging, uncomment if you want to see the
### code as it goes
### this will eventually be setup as a debugging element


### this just calculates the formula as defined in paper (need paper reference...)

SurvivalCalc<- function(firstDay = 1, resultspath = getwd(), rmflagged=TRUE, stats=TRUE)
{
  setwd(resultspath)#set directory to resultspath (location of raw results files)
  survivalanalysis <-paste0(resultspath, "/Survivalanalysis")#create survivalanalysis which identifies the path for the survival analysis dir
  dir.create(paste0("Survivalanalysis"), showWarnings = FALSE) #creates directory Survivalanalysis within current working directory
  homedir<-RAWpath<-survivalanalysis #passes homedir and RAWpath to SurvivalPercentage and SurvivalIntegral
  file_list <-
    list.files(pattern = "[[:alnum:]]*_Day_[[:digit:]]*.csv")
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
  {  for (cleaning in 1:length(file_list))
      { df<-as.data.frame(read.csv(file_list[cleaning], row.names = 1, stringsAsFactors=FALSE))
    write.csv (df, file.path(survivalanalysis, file = paste0("results-flagged-wells-NOT-removed",file_list[cleaning])))
    rm(df)
      }

  }


  SurvivalPercentage(RAWpath,firstDay)
  SurvivalIntegral(homedir, fileName = "SurvivalPercentage.csv")

  if(stats)
  {statsCLS()}



}

CLSCalc <- function(delta, doubleTime = 90*60)
{
  Sn = 100* (1 / (2^(delta*15 / doubleTime)))
  return(Sn)
}

#setwd("/home/fitz/code/OGA-test/Results/") # this is the location of the results csv files

SurvivalPercentage <- function(RAWpath = getwd(),firstDay = 1)
{
  setwd(RAWpath)
  file_list <-
    list.files(pattern = "[[:alnum:]]*_Day_[[:digit:]]*.csv") #list all files in current working directory with extension.csv and Day notation
  print(file_list)
  for (i in 1:length (file_list))
    #loop 1:x total number of CSV files in directory
  {
    assign(paste0(sub('.csv', '', basename(file_list[i]))), read.csv(file_list[i])) #read .csv into dataframe with the same name as original csv without extension.csv
  }
  dfs <-
    Filter(f = function(x) is(x, "data.frame"), mget(ls(pattern="_Day_"))) #create nested list of all dataframes
  # This creates a data frame that specifically holds the upper limit(ul.df) and doubling time(dt.df)
  firstDaygrep = paste0("Day_",firstDay)

  ul.df <- data.frame()
  dt.df <- dfs[[grep(paste0(firstDaygrep,"$"),names(dfs))]][1,]
  View(dt.df)
  qvalue <- c()
  strsplit(names(dfs[1]),"_")[[1]][2]
  names(dfs[[1]])

  for(x in seq_along(dfs))
  {
    print(names(dfs[x]))
    qvalue[x] <- as.numeric(gsub("([0-9]+).*$", "\\1", strsplit(names(dfs[x]),"_")[[1]][3]))
    ul.df <- rbind(ul.df,dfs[[x]][4,])
  }
  View(ul.df)
  # sortedDt is a sorted doubling time data frame so that the survival data frame is in a manner
  # that is age+1 is the next element in the data frame so solving the survival function
  # makes sense
  ul.df[,1] <- qvalue
  colnames(ul.df)[1]<-"Time"
  sortedDt <- ul.df[order(ul.df$Time),]
  # sortedDt <- sortedDt[-nrow(sortedDt),]
  survDt <- sortedDt
  survDt[1,] <- 100
  survDt[1,1] <- 1
  colnames(survDt) <- names(dfs[[1]])
  View(survDt)
  for(z in 2:length(sortedDt[1,]))
  {
    #print(paste0("Results for ",colnames(ul.df)[z])
    for(y in 2:length(qvalue))
    {
      Sn <-CLSCalc((as.numeric(sortedDt[y,z])-as.numeric(sortedDt[1,z])),dt.df[1,z])
      #print(paste0(paste(paste0("Day ",paste(paste0(sortedDt[y,1],":"),Sn)),"based on:"),dt.df[1,z]))
      survDt[y,z]<-format(round(Sn,2),nsmall=2)
    }
    write.csv(survDt,file="SurvivalPercentage.csv",row.names = FALSE)
  }
}

SurvivalIntegral <- function(homedir=getwd(), fileName = "SurvivalPercentage.csv")
{
  setwd(homedir)
  ul.df <- read.csv(file = fileName)
  colnames(ul.df)[1]<-"Time"
  ul.df <- ul.df[order(ul.df$Time),]
  SI.mat <- matrix(rep(0,length(names(ul.df))),nrow=1)
  count <- 1
  for(name in names(ul.df))
  {
    SI <- 0
    print(paste("Computing: ",name))
    print(paste("Starting SI reset",SI))
    for(n in 2:length(ul.df[,1]))
    {
      SI <- SI+((((ul.df[n,name])+ul.df[n-1,name])/2)*(ul.df[n,1]-ul.df[n-1,1]))
      print(SI)
    }
    print(paste0(paste0(paste0("Final SI for ",name),":"),SI))
    SI.mat[count]<-SI
    count <- count + 1
  }
  SI.df <- data.frame(SI.mat)
  colnames(SI.df) <- names(ul.df)
  ul.df <- rbind(ul.df,SI.df)
  ul.df[length(ul.df[,1]),1] <- 0
  write.csv(ul.df,file="SurvivalIntegral.csv",row.names = FALSE)
  View(ul.df)

}

# if (ul > 5)
# {
#   xaxe<-as.vector(c(1:5))
#   llonemean<-lm(yfilterd[1:5]~xaxe)$coefficients[2]
#
#   for (ll in 1:(ul-5))
#   {
#     llmean <- lm(yfilterd[(ll):(ll+4)]~xaxe)$coefficients[2]
#
#     if (llmean > llonemean*fractionlowerlimit)
#    {
#      lowerlimit[ii - 1] <- low <- (ll+2)
#      break
#    } else
#    {
#      lowerlimit <- low <- 1
#      print("ZERO")
#    }
#  }
#} else
#{
#  lowerlimit <- low <- 1
#  print("ZERO")
#}




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
