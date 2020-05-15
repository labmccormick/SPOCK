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
      
            for (l in 1:length(colnames(daf)))
            {
              name[l]<-sub("\\..*", "", colnames(dfg)[1])
              reps<-grep(name[l], colnames(dfg))
              average[l]<-mean(as.numeric(dfg[1,reps]))
              sd[l]<-sd(as.numeric(dfg[1,reps]))
              if(dim(dfg)[2]>length(reps))
              {dfg<-dfg[,-reps]
              }else{
              dfg<-0
              break}
            }
      avdataout<-as.data.frame(cbind(average,sd))
      rownames(avdataout)<-name
      write.csv (avdataout, file.path(stats, file = paste0(sub(".csv", "", file_list_tostat[analysis]), "averagedresults.csv")))
      }
      setwd("..")
}
