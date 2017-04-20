
### Script to Parse Aerial Survey Data

aerChopR <- function(daspath, outloc, species, eff.out, si.out, flat.out, 
                     length=1, bf.cutoff=4){
  
  # daspath is the location of the DAS aerial survey files
  # species is the two letter code for the species of interest (e.g., "pp")
  # eff.out is the file 
  
  require(lubridate)
  require(stringr)
  require(swfscMisc)
  require(ggplot2)
  require(maptools)
  require(grid)
  require(rgdal)
  require(raster)
  require(dsm)
  
setwd("~/Desktop/acSim/")
daspath <- "./Data/DAS Files/"
outloc <- "./Data/"
species <- "pp"
length <- 1
bf.cutoff <- 4
eff.out <- "effort"
si.out <- "sightings"
flat.out <- "flatfile"

  
# Get aerial survey filenames
filenames <- list.files(daspath)

# establish list of file paths
filepaths <- paste(daspath, filenames, sep="")    

### Read in all DAS files and add to a single data frame

das.data <- data.frame() # initiate empty data frame

for (f in filepaths){
  
  nd <- read.fwf(f, widths=c(3,2,6,7,2,2,1,5,2,3,1,5,5,5,5,5,5,5,5),
                 stringsAsFactors=FALSE, # don't create factors
                 fill=TRUE, # add blank fields at the ends of lines
                 comment.char="", # ignore hashtags
                 colClasses = "character") # read in everything as characters

  das.data <- rbind(das.data, nd) # add new file to the data frame
  
  #rm(f) # i don't know why this was here.  maybe it should have been nd?
  
}

# get rid of leading and trailing white space
das.data <- as.data.frame(apply(das.data, 2, str_trim), stringsAsFactors=FALSE)

# Convert the lat/lon columns to numeric
das.data[,c(6,8,10,12)] <- apply(das.data[,c(6,8,10,12)], 2, as.numeric)

# Get rid of NA lines

#das.data <- das.data[-which(das.data$V1=="\032"),]
das.data <- das.data[-which(das.data$V4==""),]

### Cleaning up the data

# Create a new data frame to store effort info

data <- data.frame(Code = das.data$V2,
                   Lat = das.data$V6+das.data$V8/60,
                   Lon = -1*(das.data$V10+das.data$V12/60),
                   Time = parse_date_time(paste(das.data$V4, das.data$V3), 
                                          orders="mdy HMS", tz="PST8PDT"),
                   Eff = NA, Transect = NA, Seg = NA, SubSeg = NA, 
                   BF = NA, OCast = NA, HorSun = NA, Belly = NA, Alt = NA,
                   O1 = NA, O2 = NA, O3 = NA, Rec = NA,
                   stringsAsFactors=FALSE)


# these codes indicate that effort has changed
change.codes <- c("T.","R.","W.","V.","E","O", "A.", "P.")

seg <- 0 # initialize segment counter

# Loop to add effort info to the object data

for (i in 1:nrow(data)){ # loop through each line of data
  
  if (i==1 & str_detect(data$Code[i], "[.]")){ # if i = 1 and on effort
    data$Eff[i] <- 1} else { # mark that effort = 1
      if(i==1 & str_detect(data$Code[i], "[.]")==FALSE){ # if i = 1 and off effort
        data$Eff[i] <- 0 # mark that effort = 0
        next} else {data[i, 6:17] <- data[i-1, 6:17]} # and skip to next i
    } # end if line 1
  
  if (str_detect(data$Code[i], "[.]")){ # when on effort
    data$Eff[i] <- 1} else {
      data$Eff[i] <- 0
      data$Seg[i] <- 0} # end if .
  
  if (data$Code[i] %in% change.codes){ 
    
    if (data$Code[i]=="T." | data$Code[i]=="R."){
      seg <- seg+1 # advance the segment counter
      data$Seg[i] <- seg
    }
    
    if (data$Code[i]=="T."){data$Transect[i] <- das.data$V13[i]} # Transect
    
    if (data$Code[i]=="W."){ # weather change
      data$BF[i] <- as.numeric(das.data$V15[i])
      data$OCast[i] <- as.numeric(das.data$V14[i])
      data$HorSun[i] <- as.numeric(das.data$V17[i])
      
      if(is.na(data$BF[i]!=data$BF[i-1]) | # determine if this was a change
         is.na(data$OCast[i]!=data$OCast[i-1]) | 
         is.na(data$HorSun[i] != data$HorSun[i-1]) |
         data$BF[i]!=data$BF[i-1] | 
         data$OCast[i]!=data$OCast[i-1] | 
         data$HorSun[i] != data$HorSun[i-1]){
        seg <- seg+1 # advance the segment counter
        data$Seg[i] <- seg}
    } # end if W
    
    if (data$Code[i]=="V.") { # Visibility
      # Convert belly codes (o, p, g, e) to numeric (0, 1, 2, 3) 
      data$Belly[i] <- match(das.data$V15[i], c("o", "p", "g", "e"))-1 
      if(is.na(data$Belly[i] != data$Belly[i-1]) | data$Belly[i] != data$Belly[i-1]){
      seg <- seg+1 # advance the segment counter IF a belly change occurred
      data$Seg[i] <- seg}
    } # end if V
    
    # Changes in altitude or observers don't result in new effort segment
    if (data$Code[i]=="A.") {data$Alt[i] <- as.numeric(das.data$V13[i])} # Altitude
    if (data$Code[i]=="P.") {data[i,14:17] <- das.data[i,13:16]} # Observers
    # if it's the end of the line, copy the seg ID from the previous line
    if (data$Code[i]=="O" | data$Code[i]=="E") {data$Seg[i] <- data$Seg[i-1]}
    
  } # end if change.codes
  
} # end i

### Now chop up data and assign to subsegments

eff.data <- data.frame() # initialize an effort data frame

for (i in 1:max(na.omit(data$Seg))){ # loop through each effort segment/transect
  
  df <- subset(data, Seg==i) # pull out one segment at a time
  
  if(df$Code[nrow(df)] != "O" & df$Code[nrow(df)] != "E"){
    # if there is no E or O line, use the first line of the next segment as the 
    # endpoint of the current segment
    df <- rbind.data.frame(df, subset(data, Seg==(i+1))[1,])
  }
  
  # Calculate the total distance of the segment
  seg.dist <- swfscMisc::distance(df$Lat[1], df$Lon[1],
                       df$Lat[nrow(df)], df$Lon[nrow(df)],
                       units="km", method="vincenty")
  
  if(seg.dist==0) {next} # ignore segments with zero distance
  
  # Determine the lengths of the subsegments
  if ((seg.dist %% length) > (0.5*length)) {
    # if the remainder is > 1/2 the target length, create a new subseg
    n.subseg <- ceiling(seg.dist/length) # round up
    # position the shorter subsegment randomly in the vector
    pos <- sample(1:n.subseg, 1)
    subseg.lengths <- rep(length, n.subseg)
    subseg.lengths[pos] <- seg.dist %% length
    } else {
    # if the segment is shorter than the target length, don't chop it
    if (seg.dist < length){
    n.subseg <- 1
    subseg.lengths <- seg.dist
    } else {
    # if the remainder is < 1/2 the target length, add it to an existing subseg
    n.subseg <- floor(seg.dist/length) # round down
    # position the longer subsegment randomly in the vector
    pos <- sample(1:n.subseg, 1)
    subseg.lengths <- rep(length, n.subseg)
    subseg.lengths[pos] <- length + (seg.dist %% length)
  }}
  
  # Now assign subsegment IDs to each row of data
  # this is important for sightings
  cum.dist <- c(0, cumsum(subseg.lengths))[1:length(subseg.lengths)]
  subseg.ids <- paste(i, 1:n.subseg, sep=".")
  for (k in which(data$Seg==i)){
    dist.k <- swfscMisc::distance(df$Lat[1], df$Lon[1],
                       data$Lat[k], data$Lon[k],
                       units="km", method="vincenty")
    b <- length(which(cum.dist <= dist.k))
    data$SubSeg[k] <- subseg.ids[b]
  }
  
  # Initialize a new data frame for this subsegment (to be appended to eff.data)
  df.out <- data.frame("Date" = floor_date(df$Time[1], "day"), "Transect"=df$Transect[1], 
                       "Seg" = i, "SubSeg" = subseg.ids,
                       "S.Lat" = NA, "S.Lon" = NA, "E.Lat" = NA, "E.Lon" = NA,
                       "M.Lat" = NA, "M.Lon" = NA, "Dist" = subseg.lengths,
                       "BF" = df$BF[1], "OCast" = df$OCast[1], 
                       "HorSun" = df$HorSun[1], "Belly" = df$Belly[1], 
                       stringsAsFactors = FALSE)
  
  # Find the bearings between the start and end of the segment
  seg.bearings <- bearing(df$Lat[1], df$Lon[1],
                          df$Lat[nrow(df)], df$Lon[nrow(df)])
  
  # Get the bearing at each subsegment (interpolate between initial and final)
  subseg.bearings <- seq(seg.bearings[1], seg.bearings[2], length.out=n.subseg)
  
  # Find the start and end coordinates for each subsegment
  for (j in 1:n.subseg){
    # Use the endpoint of the previous subsegment as the start of the next
    if (j == 1) {
      s.lat <- df$Lat[1]
      s.lon <- df$Lon[1]
    } else {
      s.lat <- df.out$E.Lat[j-1]
      s.lon <- df.out$E.Lon[j-1]
    }   
    # Fill in the start of the subsegment
    df.out$S.Lat[j] <- s.lat
    df.out$S.Lon[j] <- s.lon
    # Find the end of the subsegment
    df.out$E.Lat[j] <- destination(s.lat, s.lon, 
                                   subseg.bearings[j], subseg.lengths[j],
                                   units="km")[1]
    df.out$E.Lon[j] <- destination(s.lat, s.lon, 
                                   subseg.bearings[j], subseg.lengths[j],
                                   units="km")[2]
    # Find the midpoint of the subsegment
    df.out$M.Lat[j] <- destination(s.lat, s.lon, 
                                   subseg.bearings[j], subseg.lengths[j]/2,
                                   units="km")[1]
    df.out$M.Lon[j] <- destination(s.lat, s.lon, 
                                   subseg.bearings[j], subseg.lengths[j]/2,
                                   units="km")[2]
  } # end j
  
  # Append this subsegment onto the others
  eff.data <- rbind.data.frame(eff.data, df.out) 
  
} # end i

### Organize sighting information

s.ids <- which(data$Code=="S.")

si.data <- data.frame("Si.ID" = paste(yday(data[s.ids, "Time"]), 
                                      das.data[s.ids, "V13"], sep = "."),
                      "SubSeg"=data[s.ids, "SubSeg"],
                      "P.Lat" = data[s.ids, "Lat"],
                      "P.Lon" = data[s.ids, "Lon"],
                      "Species" = das.data[s.ids, "V17"],
                      "Size" = as.numeric(das.data[s.ids, "V16"]),
                      "Angle" = 90-abs(as.numeric(das.data[s.ids, "V15"])),
                      "Alt" = data$Alt[s.ids]*0.3048,
                      "Dist" = NA, "Detected" = 1, 
                      "BF" = data[s.ids, "BF"])


si.data$Dist <- round(tan(pi/180*si.data$Angle)*si.data$Alt) 

 
### Assign depths to subsegments and sightings

cc.bathy <- readGDAL("./Data/20050622cacentral3sec.asc")
mbathy <- raster(cc.bathy, layer=1, values=TRUE)

eff.data$Depth <- extract(mbathy, data.frame("x"=eff.data$M.Lon, "y"=eff.data$M.Lat))
si.data$Depth <- extract(mbathy, data.frame("x"=si.data$P.Lon, "y"=si.data$P.Lat))

# PpSightings <- data.frame("Lat"=si.data$P.Lat, "Lon"=si.data$P.Lon)
# 
# save(PpSightings, file="PpSightings.RData")

eff.data <- eff.data[which(eff.data$BF < bf.cutoff), ]
si.data <- si.data[which(si.data$Species==species & si.data$BF < bf.cutoff), ]


### Create a Flatfile for distance sampling

flatfile <- data.frame()

for (i in 1:nrow(eff.data)){
  
  if (any(si.data$SubSeg==eff.data$SubSeg[i])==FALSE){
    flatfile <- rbind(flatfile, data.frame("Sample.Label"=eff.data$SubSeg[i],
                                           "Date"=eff.data$Date[i],
                                           "Effort"=eff.data$Dist[i]*1000,
                                           "object"=NA, "distance"=NA, "size"=NA,
                                           "BF"=eff.data$BF[i],
                                           "M.Lat"=eff.data$M.Lat[i], 
                                           "M.Lon"=eff.data$M.Lon[i],
                                           "M.Depth"=eff.data$Depth[i]))
  } else {
    
    m.id <- which(si.data$SubSeg==eff.data$SubSeg[i])
    
    flatfile <- rbind(flatfile, data.frame("Sample.Label"=eff.data$SubSeg[i],
                                           "Date"=eff.data$Date[i],
                                           "Effort"=eff.data$Dist[i]*1000,
                                           "object"=si.data$Si.ID[m.id], 
                                           "distance"=si.data$Dist[m.id], 
                                           "size"=si.data$Size[m.id],
                                           "BF"=eff.data$BF[i],
                                           "M.Lat"=eff.data$M.Lat[i], 
                                           "M.Lon"=eff.data$M.Lon[i],
                                           "M.Depth"=eff.data$Depth[i])) 
    
  }
  
}

C.Lat <- mean(range(flatfile$M.Lat))
C.Lon <- mean(range(flatfile$M.Lon))

xy <- latlong2km(flatfile$M.Lon, flatfile$M.Lat, C.Lon, C.Lat)

flatfile$X <- xy$km.e*1000
flatfile$Y <- xy$km.n*1000

save(eff.data, file = paste(outloc, eff.out, ".RData", sep=""))
save(si.data, file = paste(outloc, si.out, ".RData", sep=""))
save(flatfile, file=paste(outloc, flat.out, ".RData", sep=""))

###


}





