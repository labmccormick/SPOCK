# WilcoxonCLS looks at the data outputted from SurvivalCalc
# And compares to strains to determine the p-value likelihood for a difference between strains
#####################################################################################################
#####################################################################################################

#' wilcoxonCLS
#'
#' WilcoxonCLS looks at the data outputted from SurvivalCalc
#' And compares to strains to determine the p-value likelihood for a difference between strains

wilcoxonCLS<-function()
{
print(paste("All files in current working directory", getwd(), "with extension .csv will be opened. It is assumed that all such files are the output from SPOCK OGA()-->SurvivalCalc()-->StatsCLS() called CLSstats.csv, files from multiple independent CLS experiments may be opened at the same time." ))
listCLSfiles<-list.files(pattern = "*.csv")
for(i in 1:length(listCLSfiles))
{
  assign(paste0(sub('.csv', '', basename(listCLSfiles[i]))), read.csv(listCLSfiles[i])) # read .csv into dataframe with the same name as original csv without extension.csv
}
dfs <- vector(mode="list")
dfs <-
  Filter(f = function(x) is(x, "data.frame"), mget(ls())) # create nested list of all dataframes
print(paste0("Opened .csv file", names(dfs)))


filex<-"y"
while(filex=="y")
{
print("Please select an open file")
print(names(dfs))
selectfile<-readline(prompt="Please specify which file you wish to analyze: ")
y<-which(names(dfs)==selectfile)
while(length(y)==0)
{print("That is not a valid selection, try again. Hint, do not use quotes")
selectfile<-readline(prompt="Please specify which file you wish to analyze: ")
y<-which(names(dfs)==selectfile)
}
unnested<-as.data.frame(dfs[selectfile])
rownames(unnested)<-unnested[,1]
unnested<-unnested[,-1:-2]
meancolumns<-grep("average",colnames(unnested))
unnested<-unnested[,meancolumns]
length(unnested)
unnested<-unnested[,1:(length(unnested)-1)]
x<-"y"
while(x=="y")
{
print(rownames(unnested))
print("Which two conditions would you like to compare, using a Wilcoxon Rank Sum Test?")
first<-readline(prompt="Please select the first condition: ")
y<-which(rownames(unnested)==first)
while(length(y)==0)
{print("That is not a valid selection, try again. Hint, do not use quotes")
  first<-readline(prompt="Please specify which condition you wish to analyze: ")
  y<-which(rownames(unnested)==first)
}
second<-readline(prompt="Please select the second condition: ")
y<-which(rownames(unnested)==second)
while(length(y)==0)
{print("That is not a valid selection, try again. Hint, do not use quotes")
  second<-readline(prompt="Please specify which condition you wish to analyze: ")
  y<-which(rownames(unnested)==second)
}
print(suppressWarnings(wilcox.test(as.numeric(unnested[first,]),as.numeric(unnested[second,]))))
print(paste0("Pvalue is ",suppressWarnings(wilcox.test(as.numeric(unnested[first,]),as.numeric(unnested[second,])))$p.value))
print("Would you like to analyze any additional conditions?")
x<-readline(prompt= "If yes or no, enter y/n: ")
}
print("Would you like to analyze any additional files?")
filex<-readline(prompt= "If yes or no, enter y/n: ")
}
}

