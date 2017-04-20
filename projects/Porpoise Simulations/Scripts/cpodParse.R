
# rewriting this function to use data.table
# and to pull in three years of C-POD data
# calculate no. PPS for each day
# (note: in future may want to update this to calc sunrise to sunset)

require(data.table)
require(lubridate)

acParse <- function(data.dir, file.out){
  
#  data.dir <- "~/Desktop/acSim/Data/CPOD Files/"
#  file.out <- "~/Desktop/acSim/Data/CpodPPS.RData"
  
  filenames <- list.files(data.dir)
  
  # establish list of file paths
  filepaths <- paste(data.dir, filenames, sep="")  
  
  int.2013 <- interval(ymd_hms("20130901_000000", tz="UTC"), ymd_hms("20131201_000000", tz="UTC"))
  int.2014 <- interval(ymd_hms("20140901_000000", tz="UTC"), ymd_hms("20141201_000000", tz="UTC"))
  int.2015 <- interval(ymd_hms("20150901_000000", tz="UTC"), ymd_hms("20151201_000000", tz="UTC"))
  
  pps.data <- list()
  
  for (f in 2:length(filepaths)){
    
    y <- as.numeric(substr(strsplit(filenames[f], split=" ")[[1]][2], 1, 4))
    
    if(y == 2013) int <- int.2013
    if(y == 2014) int <- int.2014
    if(y == 2015) int <- int.2015
    
    raw <- fread(filepaths[f])
    
    raw$new.time <- dmy_hm(raw$Time, tz="UTC")
    
    data <- raw[new.time %within% int & TrClass == "High", ]
   
    T <- int@.Data # t.int@.Data is in seconds
    
    d.int <- seq(int@start, int@start+T, by=24*60*60)
    
    data.out <- data.table("YEAR"=y, 
                           "MOORING"=strsplit(filenames[f], split=" ")[[1]][1],
                           "DATE"=d.int[1:(length(d.int)-1)],
                           "PPS"=NA)
    
    for (d in 1:(length(d.int)-1)){
      
      d.data <- data[new.time %within% interval(d.int[d], d.int[d+1]),]
      
      if (is.null(data)){data.out$PPS[d] <- 0; next} else {
        
        train.int <- interval(d.data$new.time + (d.data$Start/(10^6)), # 10^6 converts usec to sec
                              d.data$new.time + (d.data$Start/(10^6)) + (d.data$TrDur_us/(10^6)))
        
        sec.s <- seq(from=d.int[d], by=1, length.out=(24*60*60)-1)
        sec.e <- seq(from=d.int[d]+1, by=1, length.out=(24*60*60)-1)
        
        sec.int <- interval(sec.s, sec.e)
        
        rS <- rowSums(matrix(int_overlaps(rep(train.int, each=length(sec.int)), 
                                          sec.int), nrow=length(sec.int)))
        
        data.out$PPS[d] <- length(which(rS != 0))
        
    
      } # end else
      } # end for d
    
      pps.data[[f]] <- data.out 
      
      PPS.Data <- rbindlist(pps.data)
      
      save(PPS.Data, file=file.out)
      
      print(paste("PARSED FILE", f, "of", length(filenames)))
          } # end for f
      
      return(PPS.Data)
      
      } # end function
    
