## ----setup, include=TRUE-------------------------------------------------
knitr::opts_chunk$set(cache=FALSE)
knitr::opts_chunk$set(echo=TRUE)
knitr::opts_chunk$set(fig.width=14)
knitr::opts_chunk$set(fig.height=14)
prjpath <- "~/work/citiviz/bikeSharing/bikestats/"
datapath <- paste(prjpath, "data/", sep="")
analpath <- paste(prjpath, "analyses/", sep="")
rcodepath <- paste(analpath, "Rcode/", sep="")
setwd(analpath)

## ----loadData, cache=TRUE------------------------------------------------
source(paste(rcodepath,"datamanip.R", sep=""))
path.zip <- paste(datapath, "hubway/hubway_2011_07_through_2013_11.zip", sep="")
print(paste("we will try to read the file from", path.zip))
print(paste("see if file exists", file.exists(path.zip))) 
hubway.orig <- read.csv.zip(name.csv="hubway_trips.csv", 
                            path.csv=paste(datapath, 
					   "hubway/hubway_trips.csv", sep=""),
                            path.zip=path.zip )


hubway <- hubway.orig
hubway.st <- read.csv.zip(name.csv="hubway_stations.csv",
                          path.csv=paste(datapath, 
					 "hubway/hubway_stations.csv", sep=""),
                          path.zip=path.zip)
print("data read with dimensions")
print(dim(hubway))


## ----hubwayClms, echo=TRUE-----------------------------------------------
print(names(hubway))

## ----renamevars, echo=TRUE, cache=FALSE----------------------------------
source(paste(rcodepath, "varNamingScheme.R", sep=""))
nms.indata <- names(hubway)
nms.touse <- namingScheme(nms.indata,
                          var.user_id = NA,
                          var.start_statn="strt_statn",
                          var.end_statn="end_statn",
                          var.start_time="start_date",
                          var.end_time="end_date",
                          var.bike_id="bike_nr",
                          var.trip_id=NA)
print("we will rename data from")
print(nms.indata)
print("to")
print(nms.touse)
names( hubway ) <- nms.touse

## ----renamevars.st, echo=TRUE, cache=FALSE-------------------------------
nms.indata <- names(hubway.st)
nms.touse <- namingScheme.statn(nms.indata,
                                var.statn_id="id",
                                var.lat="lat",
                                var.long="lng",
                                var.status="status")
print("we will rename the station location meta data from")
print(nms.indata)
print("to")
print(nms.touse)
names(hubway.st) <- nms.touse

## ----datetimefix, cache=FALSE--------------------------------------------
library(lubridate)
hubway$start_time <- mdy_hms(hubway$start_time)
hubway$end_time <- mdy_hms(hubway$end_time)
print("the names of hubway should have changed to ")
print(names(hubway))

## ----sampleWeek----------------------------------------------------------
print("we will extract data from dims")
print(dim(hubway))
source(paste(rcodepath,"datamanip.R", sep=""))
data.sample <- extractWeek(data=hubway, 
                             start=ymd_hms("2012-06-04 0:0:0"),
			     var.start_time="start_time",
			     var.end_time="end_time",
                             weekdays=TRUE)
print(paste("Number of trips during the week starting on June 04 2012", 
            nrow(data.sample)))

## ----dayperiods----------------------------------------------------------
periodOfDay <- function(times){
  dayPeriods <- data.frame(
    period=c("morn.early", "morn", "aftn", "aftn.late", "evn", "nite", "nite.late"),
    start=c(5, 8, 12, 15, 18, 21, 0),
    end=c(8,12,15,18,21,24,5)
  )
  dayPeriods$period <- as.character(dayPeriods$period)
  if (class(times) != "integer") times <- hour(times)
  sapply(times, function(t) {
    with(dayPeriods, period[start <= t & end > t])
  })
}

data.sample$period <- periodOfDay(data.sample$start_time)

## ----disambg-------------------------------------------------------------
source(paste(rcodepath, "userDisambiguation.R", sep=""))
data.sample$pre_user_id <- with(data.sample, paste(gender, birth_date, sep=""))
usrs <- unique(data.sample$pre_user_id)

data.sample <- do.call("rbind", lapply(unique(data.sample$pre_user_id), 
                                function(u) disambguser(user=u, trips=data.sample)))

newUsers <- unique(data.sample$new_user_id)
any( sapply(newUsers, function(u) {
#      print(paste("testing user", u))
      doUserTripsOverlap(user=u, data.sample, test_ids = "new_user_id")
    })
)

## ----hwuid---------------------------------------------------------------
data.sample$user_id <- data.sample$new_user_id

## ----tabuser-------------------------------------------------------------
library(lattice)
tb <- table(data.sample$user_id)
tb <- data.frame(name=names(tb), freq.trips=as.numeric(tb))
histogram(tb$freq.trips, main="Histogram of number of trips made by a users")

## ----transclmns----------------------------------------------------------
names(makeTrxn(data.sample[1,]))

## ----hwmaketrans---------------------------------------------------------
trips.sample <- data.sample
trxns.sample <- makeTrxn(trips.sample)
trxns.sample$period <- periodOfDay( trxns.sample$time)

## ----transpeek-----------------------------------------------------------
head(trxns.sample)

## ----usagetrace----------------------------------------------------------
usagetrace <- function(user, trips, dt=minutes(5)){
	userTrips <- subset(trips, user_id == user)
  #make a sequence by 5 minutes
  t0 <- min(userTrips$start_time)
  tl <- max(userTrips$end_time)
  N <- duration(interval(start=t0, end=tl))/as.duration(dt)
  tvec <- t0 + (0:N)*dt 
  tripIndices <- lapply(1:nrow(trips), function(i) {
    which( tvec > userTrips$start_time[i] & tvec < userTrips$end_time[i])
  })
  tripIndices <- do.call(c, tripIndices)
  inuse <- rep(0, length(tvec))
  inuse[tripIndices] <- 1
  data.frame(time=tvec, inuse=inuse, user_id=user)
}

## ----explusgtrcs---------------------------------------------------------
allusers <- unique(trips.sample$user_id)
eg.ustrs <- do.call(rbind, lapply(allusers[1:16], function(u) usagetrace(u, trips.sample)))
xyplot( inuse ~ time | user_id, data=eg.ustrs, type="l")

## ----userfreq------------------------------------------------------------
library(reshape2)
library(plyr)
user.frequencies <- function(trxns){
  trxn.types <- unique(trxns$drxn )
  users <- unique(trxns$user_id)
  tb <- as.data.frame(table(trxns$user_id, trxns$drxn))
  names(tb) <- c("user_id", "drxn", "freq")
  dcast(data=tb, formula=user_id ~ drxn, value.var="freq", fun.aggregate=sum)
}

ufreqs <- user.frequencies(trxns=trxns.sample)

## ------------------------------------------------------------------------
ufreqs.l <- melt(ufreqs, id.vars = "user_id", 
                 variable.name = "drxn", value.name="count" )
histogram(~ count | drxn, data=ufreqs.l)

## ----periodbars----------------------------------------------------------
library(ggplot2)
print(names(trxns.sample))
prdUsg <- with(trxns.sample, as.data.frame(table(gender, period, drxn)))
levels(prdUsg$period) <- c("morn.early", "morn", 
                          "aftn", "aftn.late", 
                          "evn", "evn.late", 
                          "nite", "nite.late")

gp <- ggplot(data=prdUsg, aes(x=period, y=Freq, fill=gender)) +
        geom_bar(stat="identity", position="dodge") + 
          facet_grid( drxn ~ . ) + 
            ggtitle("Usage going out by period of day across genders")
print(gp)

gp.out <- ggplot(data=subset(prdUsg, drxn =="out"),
                             aes(x=period, y=Freq, fill=gender)) +
            geom_bar(stat="identity", position="dodge") + 
              ggtitle("Usage going out by period of day across genders")
print(gp.out)

gp.in <- ggplot(data=subset(prdUsg, drxn =="in"),
                             aes(x=period, y=Freq, fill=gender)) +
            geom_bar(stat="identity", position="dodge") +
              ggtitle("Usage coming in by period of day across genders")
print(gp.in)

## ----userdayperiod-------------------------------------------------------
user.prdusg <- function(data, 
                        prds){
  users <- unique(data$user_id)
  df <- do.call(rbind, lapply(users, function(u){
					tb <- table(subset(data, user_id == u)$period)
          nu <- rep(0, length(prds))
          names(nu) <- prds
          nu[names(tb)] <- tb
          nu
        }))
  df <- data.frame(users=users, df, stringsAsFactors=FALSE)
  df
}
#eg usage
prds <- c("morn.early", "morn", "aftn", "aftn.late", "evn", "nite", "nite.late")
updu <- user.prdusg(trips.sample, prds)

## ----userinter-----------------------------------------------------------
intmtcOfUser <- function(user, trips){
  #print(paste("computing intermittency for user", user))
	trips.u <- subset(trips, user_id == user)
  trips.u <- trips.u[order(trips.u$start_time),]
  start.u <- trips.u$start_time
  end.u <- trips.u$end_time
  n <- nrow(trips.u)
  if(n > 1) list(user=user, intmtc =difftime(start.u[2:n], end.u[1:(n-1)], units="secs"))
  else c()
}
                                
user.intmtc <-  function(trips,
                         users=NULL,
                         statistics=TRUE,
                         .funcs=c(mean, var),
                         .funcs.names=c("mean", "var")){
  #.func.names needs some improvement, as now it wont generalize
  if(is.null(users)) users <- unique(trips$user_id)
  uintmt <- lapply(users, function(u) {
    intmtcOfUser(u, trips)$intmtc }
	)
  names(uintmt) <- users
  if (statistics){
    df <- as.data.frame(do.call(rbind, lapply(uintmt, function(u) {
        if(length(u) == 0) rep(NA, length(.funcs))
        else sapply(.funcs, function(f) f(u))
      }))
    )
    df <- data.frame(user_id=users, df)
    names(df)[2:3] <- .funcs.names
    df
  }
  else {
    do.call(rbind, lapply(1:length(users), function(i){
        data.frame(user=rep(users[i], length(uintmt[[i]])), intmtc=uintmt[[i]])
    }))
  }
}
  
uintmtc <- user.intmtc(trips=trips.sample)  

## ----userdurs------------------------------------------------------------
user.durations <- function(trips,
                           statistics=TRUE,
                           .funcs=c(mean, var),
                           .funcs.names=c("mean", "var")){
  #.func.names needs some improvement, as now it wont generalize
	users <- unique(trips$user_id)
  udurs <- lapply(users, function(u) {
    trips.u <- subset(trips, user_id == u)
    difftime(trips.u$end_time, trips.u$start_time, units="secs")
  })
  if (statistics){
    df <- as.data.frame(do.call(rbind, lapply(udurs, function(u) {
        if(length(u) == 0) rep(NA, length(.funcs))
        else sapply(.funcs, function(f) f(u))
      }))
    )
    df <- data.frame(user_id=users, df)
    names(df)[2:3] <- .funcs.names
    df
  }
  else udurs
}

## ----eguserdur-----------------------------------------------------------
udurs <- user.durations(trips=trips.sample)  

## ----userstartstatn------------------------------------------------------
#remove this line
trxns <- trxns.sample
entropy <- function(ps){
  ps <- ps/sum(ps)
  ps <- ps[ps > 0]
  -sum(ps*log2(ps))
}
user.stnentropies <- function(trxns){
  users <- unique(trxns$user_id)
  data.frame(user_id=users, 
             strtent = sapply(users, function(u){
																trxns.u <- subset(trxns, user_id == u)
																ss <- subset(trxns.u, drxn == "out")$statn_id
																entropy(table(ss))
															}),
             endent = sapply(users, function(u){
																trxns.u <- subset(trxns, user_id == u)
																es <- subset(trxns.u, drxn == "in")$statn_id
																entropy(table(es))
															}),
             strtend = sapply(users, function(u){
																trxns.u <- subset(trxns, user_id == u)
																ss <- subset(trxns.u, drxn == "out")$statn_id
																es <- subset(trxns.u, drxn == "in")$statn_id
																ses <- paste(ss, es, sep=".")
																entropy(table(ses))
															})
						 )
}
  
uents <- user.stnentropies(trxns=trxns)  

## ----featureDataSet------------------------------------------------------
user.features <- merge(ufreqs, uintmtc, by="user_id")
user.features <- merge(user.features, udurs, by="user_id")
user.features <- merge(user.features, uents, by="user_id")

## ----usrclus-------------------------------------------------------------
user.ftr.sc <- scale(user.features[,-1])
user.ftr.sc <- data.frame(user_id=user.features$user_id, user.ftr.sc)
user.ftr.dis <- dist(user.ftr.sc[,-1], method="euclidean")
user.ftr.mat <- as.matrix(user.ftr.sc[,-1])
hc <- hclust(user.ftr.dis , method="ward.D2")
plot(hc)
#heatmap(user.ftr.mat)
user.ftr.sc <- user.ftr.sc[ order(cutree(hc, h=20)),]
ufm.m <- melt(user.ftr.sc[, -2], id.vars = "user_id")
p <- ggplot(ufm.m, aes(variable, user_id)) + 
      geom_tile(aes(fill=sign(value)*log10(abs(value))), color="white") +
       scale_fill_gradient(low="yellow", high="red")
print(p)

