varScore <- function(Data, Forward, Reverse=NULL, Range = NULL, Prorate = TRUE, MaxMiss = .20)
{
  #select relevant items
  d = Data[,c(Forward, Reverse)]
  
  #check for out of range
  if (!is.null(Range)){
    if (min(d, na.rm=TRUE) < Range[1] || max(d, na.rm=TRUE) > Range[2]){
      stop('Item score(s) out of range')
    }
  }
  
  #check that length of Range == 2 if Reverse is not null
  if (!is.null(Reverse) && length(Range) !=2) {
    stop('Must specify item range (Range) to reverse score items')
  }
  
  #Reverse score relevant items
  if (!is.null(Reverse)){
    for (v in Reverse) {
      d[,v] = (Range[1] + Range[2]) - d[,v]
    }   
  }
  
  if (Prorate){
    Total = rowMeans(d, na.rm=TRUE)*dim(d)[2]
  }
  else{
    Total = rowSums(d, na.rm=TRUE)
  }
  
  #count missing and set > MaxMiss to NA
  MissCount = rowSums(is.na(d))
  MissCount = MissCount/dim(d)[2]
  Total[MissCount > MaxMiss] = NA
  
  return(Total)
}

ATTCdata$ATTC_TOT = varScore(ATTCdata, Forward= c('ATTC4', 'ATTC5', 'ATTC9', 'ATTC10', 
                                                  'ATTC13', 'ATTC14', 'ATTC17', 'ATTC18', 
                                                  'ATTC19'), 
                             Reverse = c('ATTC1', 'ATTC2', 'ATTC3', 
                                         'ATTC6','ATTC7', 'ATTC8', 
                                         'ATTC11', 'ATTC12', 'ATTC15', 'ATTC16', 'ATTC20'), 
                             Range = c(1,4) )
ATTCdata$ATTC_FOC = varScore(ATTCdata, Forward= c('ATTC4', 'ATTC5', 'ATTC9'), 
                             Reverse= c('ATTC1', 'ATTC2', 'ATTC3', 'ATTC6','ATTC7', 'ATTC8'), 
                             Range = c(1,4) )

ATTCdata$ATTC_SHI = varScore(ATTCdata, Forward= c('ATTC10', 'ATTC13', 'ATTC14', 'ATTC17', 'ATTC18', 'ATTC19'), 
                             Reverse= c('ATTC11', 'ATTC12', 'ATTC15', 'ATTC16', 'ATTC20'), 
                             Range = c(1,4) )



