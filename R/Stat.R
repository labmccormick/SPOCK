#stats takes raw results files and calculates the mean, SD, and SEM of all wells with the same name
#This function will fail if RAW csv files from bioscreen (or other plate reader) contain periods in sample names
#locationofresults = path of results folder
#LimitNoGrowth = Limit below which a well is defined as having no growth
#####################################################################################################
stats <- function(locationofresults=results,LimitNoGrowth=LimitNoGrowth)
{
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
      toolow<-(LimitNoGrowth-(as.numeric(daf[9,1])))
      xh<-which(as.numeric(daf[6,])>toolow)
      daf<-daf[,xh]
      xg<-which(as.numeric(daf[5,])>as.numeric(daf[4,]))
      daf<-daf[,xg]
      dfg<-daf
      name<-0
      average<-0
      sd<-0
      se<-0

            for (l in 1:length(colnames(daf)))
            {
              name[l]<-sub("\\..*", "", colnames(dfg)[1]) #take name of first SAMPLE and remove R inserted .# values
              print(name)
              reps<-grep(name[l], colnames(dfg)) #find all SAMPLES with same name as column 1
              dtval<-(as.numeric(dfg[1,reps]))
              dtval[is.na(dtval)]<-0
              dtval[is.infinite(dtval)]<-0
              if(length(dtval)>0)
              {
              average[l]<-mean(as.numeric(dtval)) #average these columns doubling time
              sd[l]<-sd(as.numeric(dfg[1,reps])) #find SD of these columns doubling time
              se<-(sd/sqrt(length(reps))) #find SEM of these columns doubling time
              if(dim(dfg)[2]>length(reps)) #remove SAMPLE replicate columns from further analysis
              {dfg<-dfg[,-reps]
              }else{
              dfg<-0 #if all columns have been analyzed stop
              break}
              }
            }
      avdataout<-as.data.frame(cbind(average,sd,se)) #create dataframe and store stats here
      rownames(avdataout)<-name
      write.csv (avdataout, file.path(stats, file = paste0(sub(".csv", "", file_list_tostat[analysis]), "averagedresults.csv"))) #write out dataframe.csv to replicate_stats dir
      }
      setwd("..") #setwd to dir one level up
}
