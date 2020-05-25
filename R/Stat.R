#stats takes raw results files and calculates the mean, SD, and SEM of all wells with the same name
#This function will fail if RAW csv files from bioscreen (or other plate reader) contain periods in sample names
#locationofresults = path of results folder
#LimitNoGrowth = Limit below which a well is defined as having no growth
#####################################################################################################
#####################################################################################################
#' statsDT
#'
#' statsDT takes raw results files and calculates the mean, SD and SEM of all wells with the same name.
#'
#' @param locationofresults path to the results of OGA analysis.
#' @param LimitNoGrowth Parameter specifying the cutoff value for determining growth. default=0.9
#' @param rmflagged This determines if flagged wells are removed from statistics. default=TRUE
statsDT <- function(locationofresults="Results",LimitNoGrowth=0.9, rmflagged=TRUE)
{
  if(!dir.exists(locationofresults))
  {
    print(paste0("Directory: ", locationofresults," not found, confirm it exists and you have permission to access it."))
    return(-1)
  }

  setwd(locationofresults)
  stats <-
    paste0(locationofresults, "/Replicate_Stats")#create variable replicate_states which identifies the path for the replicate_stats directory

  dir.create(paste0("Replicate_Stats"), showWarnings = FALSE) #creates directory replicate_states within current working directory
  file_list_tostat <-
    list.files(pattern = "*.csv") #create list of all result outputfiles from OGA in dir results
  print ("Calculating Statistics")
  for (analysis in 1:length(file_list_tostat))
  {
    #####################################################################################################
    #Housekeeping
    daf<-as.data.frame(read.csv(file_list_tostat[analysis], row.names = 1, stringsAsFactors=FALSE))
    if(rmflagged) #remove flagged wells if =TRUE
    {
      toolow<-(LimitNoGrowth-(as.numeric(daf[9,1])))
      xh<-which(as.numeric(daf[6,])>toolow)
      daf<-daf[,xh]
      xg<-which(as.numeric(daf[5,])>as.numeric(daf[4,]))
      daf<-daf[,xg]
    }
    dfg<-daf
    dfg[11,]<-colnames(dfg)
    name<-0
    average<-0
    sd<-0
    se<-0
    dfg[11,]<-sub("\\..*", "", dfg[11,])
    for (l in 1:length(colnames(daf)))
    {
      name[l]<-sub("\\..*", "", colnames(dfg)[1]) #take name of first SAMPLE and remove R inserted .# values
      reps<-grep(paste0(name[l], "$"), dfg[11,]) #find all SAMPLES with same name as column 1
      dtval<-(as.numeric(dfg[1,reps]))
      dtval[is.na(dtval)]<-0
      dtval[is.infinite(dtval)]<-0
      if(length(dtval)>0)
      {
        average[l]<-mean(as.numeric(dtval)) #average these columns doubling time
        sd[l]<-sd(as.numeric(dfg[1,reps])) #find SD of these columns doubling time
        se[l]<-(sd[l]/sqrt(length(reps))) #find SEM of these columns doubling time
        if(dim(dfg)[2]>length(reps)) #remove SAMPLE replicate columns from further analysis
        {
          dfg<-dfg[,-reps]
        }
        else
        {
          dfg<-0 #if all columns have been analyzed stop
          break
        }
      }
    }
    avdataout<-as.data.frame(cbind(average,sd,se)) #create dataframe and store stats here
    rownames(avdataout)<-name
    write.csv (avdataout, file.path(stats, file = paste0(sub(".csv", "", file_list_tostat[analysis]), "averagedresults.csv"))) #write out dataframe.csv to replicate_stats dir
  }
  setwd("..") #setwd to dir one level up
}

############################################################################# CLS STATS CALCULATION
#stats takes Survival Percentage and Survival Integral files and calculates the mean, SD, and SEM of all wells with the same name
#This function will fail if RAW csv files from bioscreen (or other plate reader) contain periods in sample names
#locationofresults = path of results folder
#LimitNoGrowth = Limit below which a well is defined as having no growth
#####################################################################################################

#' statsCLS
#'
#' statsCLS takes SurvivalIntegral.csv and calculates the mean, SD and SEM of all wells with the same name from SurvivalPercentage and SurvivalIntegral.
#'

statsCLS <- function()
{
  print ("Calculating CLS Statistics")

  si<-as.data.frame(read.csv("SurvivalIntegral.csv"))
  width<-dim(si)[2]
  title<-si[,1]
  lngth<-dim(si)[1]
  title[lngth]<-"SI"
  si<-si[,2:width]
  lngth<-dim(si)[1]
  si[lngth+1,]<-colnames(si)
  name<-0
  repnum<-0
  average<-vector(mode = "list", length = lngth)
  sd<-vector(mode = "list", length = lngth)
  se<-vector(mode = "list", length = lngth)
  si[lngth+1,]<-sub("\\..*", "", si[lngth+1,])
  for (l in 1:length(colnames(si)))
  {
    name[l]<-sub("\\..*", "", colnames(si)[1]) #take name of first SAMPLE and remove R inserted .# values
    reps<-grep(paste0(name[l], "$"), si[lngth+1,]) #find all SAMPLES with same name as column 1
    repnum[l]<-length(reps)
    dtval<-(as.numeric(si[1,reps]))
    dtval[is.na(dtval)]<-0
    dtval[is.infinite(dtval)]<-0
    if(length(dtval)>0)
    {
      for(times in 1:lngth)
      {
        dtval<-si[times,reps]
        average[[times]][l]<-mean(as.numeric(dtval)) #average these columns doubling time
        sd[[times]][l]<-sd(as.numeric(dtval)) #find SD of these columns doubling time
        se[[times]][l]<-(sd[[times]][l]/sqrt(length(reps))) #find SEM of these columns doubling time
      }
      if(dim(si)[2]>length(reps)) #remove SAMPLE replicate columns from further analysis
      {si<-si[,-reps]
      }else{
        si<-0 #if all columns have been analyzed stop
        break}
    }
  }
  average<-unlist(average)
  sd<-unlist(sd)
  se<-unlist(se)
  titles<-numeric(lngth*3)
  for(binding in 1:lngth)
  {
    x<-(1+(length(name)*(binding-1)))
    xn<-(length(name)*binding)
    xy<-c(1:3)
    xy<-xy+3*(binding-1)
    titles[xy[1]]<-paste(title[binding],"average")
    titles[xy[2]]<-paste(title[binding],"SD")
    titles[xy[3]]<-paste(title[binding],"SE")
    if(exists("avdataout"))
    {
      avdataout<-as.data.frame(cbind(avdataout,average[x:xn],sd[x:xn],se[x:xn])) #create dataframe and store stats here
    } else
    {
      avdataout<-as.data.frame(cbind(average[x:xn],sd[x:xn],se[x:xn])) #create dataframe and store stats here
    }

  }



  colnames(avdataout)<-titles
  rownames(avdataout)<-name
  avdataout<-cbind(repnum,avdataout)
  colnames(avdataout)[1]<-"Replicates"
  View(avdataout)
  write.csv (avdataout, "CLSstats.csv") #write out dataframe.csv to replicate_stats dir
  rm(avdataout)
}




