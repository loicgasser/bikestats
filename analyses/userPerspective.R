## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(cache=TRUE)
knitr::opts_chunk$set(echo=FALSE)

## ----loadData------------------------------------------------------------
hubway.orig <- read.csv("data/hubway/hubway_trips.csv", stringsAsFactor=FALSE)
hubway <- hubway.orig
hubway.st <- read.csv("data/hubway//hubway_stations.csv")

## ----hubwayClms----------------------------------------------------------
names(hubway)

## ----datetimefix---------------------------------------------------------
library(lubridate)
hubway$start_date <- mdy_hms(hubway$start_date)
hubway$end_date <- mdy_hms(hubway$end_date)

## ----sampledata----------------------------------------------------------
N <- 10000
extractGivenDays <- function(data=hubway, start, end){
  subset(hubway, start_date >= start & end_date <= end)
}
extractWeek <- function(data=hubway, start, weekdays=FALSE){
  extractGivenDays(data, start=start, end=start + days(ifelse(weekdays, 5, 7)) )
}

## ----sampleWeek----------------------------------------------------------
hubway.sample <- extractWeek(data=hubway, 
                             start=ymd_hms("2012-06-04 0:0:0"), 
                             weekdays=TRUE)

## ----dayperiods----------------------------------------------------------
dayPeriods <- data.frame(
  period=c("morn.early", "morn", "aftn", "aftn.late", "evn", "nite", "nite.late"),
  start=c(5, 8, 12, 15, 18, 21, 0),
  end=c(8,12,15,18,21,24,5)
)
dayPeriods$period <- as.character(dayPeriods$period)
st <- hour(hubway.sample$start_date)
hubway.sample$period <- sapply(st, function(s){
  with(dayPeriods, period[ start <= s & end > s])
})

## ----validuser-----------------------------------------------------------
validUser <- function(uid, data=hubway.sample){
  hs <- subset(data, user_id == uid)
  hs <- subset(hs, !is.na(start_date) & !is.na(end_date) )
  hs <- hs[order(hs$start_date),]
  n <- nrow(hs)
  s <- hs$start_date
  e <- hs$end_date
  ifelse(n==1, s <= e, all(s[2:n] > e[1:(n-1)]))
}

validUserIds <- function(uids=NULL, 
                         user_id_var = NULL, 
                         data=hubway.sample){
  if(is.null(uids)) uids <- unique(data[, user_id_var])
  all( sapply(uids, function(u) validUser(u, data)))
}

## ----tripoverlap---------------------------------------------------------
ovlpngTrips <- function(trip1, trip2, 
                        var.start_time="start_date",
                        var.end_time="end_date"){
  s1 <- trip1[, var.start_time]; e1 <- trip1[, var.end_time] 
  s2 <- trip2[, var.start_time]; e2 <- trip2[, var.end_time] 
  if ((s2 > e2) & (s1 > e1)) NA
  else{
    nonovlp <- (e2 <= s1) | (s2 >= e1)
    !nonovlp
  }
}

tripOverlaps <- function(trips, 
                        var.end_time="end_date", 
                        var.start_time="start_date",
                        rentry.time = 0) {
  #use a exponential relaxation to re-enter users into the system after a trip.
  #parameter rentry.time is measured in minutes
  n <- nrow(trips)
  as.dist( do.call("rbind", 
                   lapply(1:n, function(i){
                    dt <- trips[1:i, var.end_time] - trips[i, var.start_time]
                    dt <- as.double(dt, units="mins")
                    dis <- c((dt > 0)*(1 - exp(-dt/rentry.time)),  rep(NA, n - i) ) 
                    dis[is.nan(dis)] <- 0
                    dis
                   }))
        )
}

## ----disambg-------------------------------------------------------------
hubway.sample$pre_user_id <- with(hubway.sample, paste(gender, birth_date, sep=""))
usrs <- unique(hubway.sample$pre_user_id)

disambguser <- function(user,
                        pre_user_id_var="pre_user_id",
                        var.end_time="end_date",
                        var.start_time="start_date",
                        rentry.time=0,
                        trips=hubway.sample){
  trips <- trips[ trips[, pre_user_id_var] == user,]
  tol <- tripOverlaps(trips, var.end_time, var.start_time, rentry.time)
  trips$new_user_id <- paste(trips[, pre_user_id_var],
                             if( sum(tol) > 0 ) cutree(hclust(tol,method="ward.D2"), h=0) 
                              else   rep(1, nrow(trips)),
                             sep="."
                       )
  trips
}

hubway.sample <- do.call("rbind", lapply(unique(hubway.sample$pre_user_id), 
                                function(u) disambguser(user=u, trips=hubway.sample)))

## ----testdisambg---------------------------------------------------------
userTripsOverlaps <- function(user, trips=hubway.sample, var.user_id="new_user_id"){
  trips.u <- trips[ trips[, var.user_id] == user, ]
  n <- nrow(trips.u)
  if (n > 1) {
    any(sapply(1:(n-1), function(i) {
        #print(paste( "testing trip ", i))
        any(sapply((i+1):n, function(j){
        #  print(paste("\t against trip", j))
          ovlpngTrips(trips.u[i,], trips.u[j,])
        })) 
      }
    ))
  }
  else FALSE
}

newUsers <- unique(hubway.sample$new_user_id)
any( sapply(newUsers, function(u) {
      #print(paste("testing user", u))
      userTripsOverlaps(user=u)
    })
  )

## ----hwuid---------------------------------------------------------------
hubway.sample$user_id <- hubway.sample$new_user_id

## ----testthatdisambg-----------------------------------------------------
#library(testthat)
#test_that("Disambiguates to a single user", {
 # trips <- data.frame(pre_user_id = rep(1, 11), 
  #                    start_date=seq(0, 10, length=11),
   #                   end_date=seq(0, 10, length=11) + 0.5 )
  #trips.da <- disambguser(user=1, trips=trips)
#  expect_that(length(unique(trips.da$new_user_id)), equals(1))
#})
#test_that("Disambiguated users's trips do not overlap", {
 # trips <- data.frame(pre_user_id = rep(1, 11), 
  #                    start_date=seq(0, 10, length=11),
  #                    end_date=seq(0, 10, length=11) + 1.5 )
  #trips.da <- disambguser(user=1, trips=trips)
  #users <- trips.da$new_user_id
  #expect_that(any( sapply(users, 
   #                       function(u) userTripsOverlaps(user=u, trips=trips.da))
    #          ), 
     #         equals(FALSE)
      #       )
#}

## ----tabuser-------------------------------------------------------------
library(lattice)
tb <- table(hubway.sample$new_user_id)
tb <- data.frame(name=names(tb), freq.trips=as.numeric(tb))
histogram(tb$freq.trips)

## ----makeTransaction-----------------------------------------------------
makeTransaction <- function(trips){
    trxns <- with(trips, data.frame(
      statn_loc=c(strt_statn, end_statn),
      user_id=rep(user_id,2),
      trxn_type=c(rep("out", nrow(trips)), rep("in", nrow(trips))),
      bike_nr=rep(bike_nr, 2),
      time=c(start_date, end_date),
      subsc_type=rep(subsc_type,2),
      birth_date=rep(birth_date,2),
      gender=rep(gender,2) )
    )
    trxns$bike_nr <- as.character(trxns$bike_nr)
    trxns
}

## ----transclmns----------------------------------------------------------
names(makeTransaction(hubway.sample[1,]))

## ----hwmaketrans---------------------------------------------------------
hubway_trans <- makeTransaction(hubway.sample)
hubway_trans$period <- sapply(hour(hubway_trans$time), 
                              function(t) with(dayPeriods, 
                                               period[ t>=start & t<end]) )

## ----transpeek-----------------------------------------------------------
head(hubway_trans)

## ----trxntotripstub------------------------------------------------------
#this function is a stub
makeTrip <- function(trxns, 
                     var.start_statn="strt_statn",
                     var.end_statn="end_statn",
                     var.user_id="user_id"
                     ){
  hubway.sample
}

## ----hwuidftr------------------------------------------------------------
hubway_trips <- makeTrip(trxns=hubway_trans)
users <- unique(hubway_trans$user_id)

## ----userfreq------------------------------------------------------------
library(reshape2)
library(plyr)
user.frequencies <- function(trxns, 
                      var.trxn_type="trxn_type", 
                      var.user_id="user_id"){
  trxn.types <- unique(trxns[, var.trxn_type])
  users <- unique(trxns[, var.user_id])
  tb <- as.data.frame(table(trxns[, var.user_id], trxns[, var.trxn_type]))
  names(tb) <- c("user_id", "drxn", "freq")
  dcast(data=tb, formula=user_id ~ drxn, value.var="freq", fun.aggregate=sum)
}

ufreqs <- user.frequencies(trxns=hubway_trans)

## ------------------------------------------------------------------------
ufreqs.l <- melt(ufreqs, id.vars = "user_id", 
                 variable.name = "drxn", value.name="count" )
histogram(~ count | drxn, data=ufreqs.l)

## ----periodbars----------------------------------------------------------
prdUsg <- with(hubway_trans, as.data.frame(table(gender, period, trxn_type)))
levels(prdUsg$period) <- c("morn.early", "morn", 
                          "aftn", "aftn.late", 
                          "evn", "evn.late", 
                          "nite", "nite.late")

gp <- ggplot(data=prdUsg, aes(x=period, y=Freq, fill=gender)) +
        geom_bar(stat="identity", position="dodge") + 
          facet_grid( trxn_type ~ . ) + 
            ggtitle("Usage going out by period of day across genders")
print(gp)

gp.out <- ggplot(data=subset(prdUsg, trxn_type=="out"),
                             aes(x=period, y=Freq, fill=gender)) +
            geom_bar(stat="identity", position="dodge") + 
              ggtitle("Usage going out by period of day across genders")
print(gp.out)

gp.in <- ggplot(data=subset(prdUsg, trxn_type=="in"),
                             aes(x=period, y=Freq, fill=gender)) +
            geom_bar(stat="identity", position="dodge") +
              ggtitle("Usage coming in by period of day across genders")
print(gp.in)

## ----userdayperiod-------------------------------------------------------

## ----userinter-----------------------------------------------------------
intermittencyOfUser <- function(user, trips,
                               var.user_id="user_id",
                               var.start_time="start_date",
                               var.end_time="end_date"){
  #print(paste("computing intermittency for user", user))
  trips.u <- trips[ trips[, var.user_id] == user,]
  trips.u <- trips.u[order(trips.u[, var.start_time]),]
  start.u <- trips.u[ , var.start_time]
  end.u <- trips.u[, var.end_time]
  n <- nrow(trips.u)
  if(n > 1) difftime(start.u[2:n], end.u[1:(n-1)], units="secs")
  else c()
}
                                
user.intermittencies <- function(trips,
                               var.user_id="user_id",
                               var.start_time="start_date",
                               var.end_time="end_date",
                               statistics=TRUE,
                               .funcs=c(mean, var),
                               .funcs.names=c("mean", "var")){
  #.func.names needs some improvement, as now it wont generalize
  users <- unique(trips[, var.user_id])
  uintmt <- lapply(users, function(u) {
    intermittencyOfUser(u, trips,
                        var.user_id,
                        var.start_time,
                        var.end_time)
            })
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
  else uintmt
}
  
uintmtc <- user.intermittencies(trips=hubway_trips)  

## ----userdurs------------------------------------------------------------
user.durations <- function(trips,
                           var.user_id="user_id",
                           var.start_time="start_date",
                           var.end_time="end_date",
                           statistics=TRUE,
                           .funcs=c(mean, var),
                           .funcs.names=c("mean", "var")){
  #.func.names needs some improvement, as now it wont generalize
  users <- unique(trips[, var.user_id])
  udurs <- lapply(users, function(u) {
    trips.u <- trips[ trips[, var.user_id] == u,]
    difftime(trips.u[ , var.end_time], trips.u[, var.start_time], units="secs")
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
  
udurs <- user.durations(trips=hubway_trips)  

## ----userstartstatn------------------------------------------------------
#remove this line
trxns <- hubway_trans
entropy <- function(ps){
  ps <- ps/sum(ps)
  ps <- ps[ps > 0]
  -sum(ps*log2(ps))
}
user.subset <- function(user, data=hubway_trxns, var.user_id="user_id"){
  data[data[ , var.user_id] == user,]
}
user.stnentropies <- function(trxns,
                             var.user_id="user_id",
                             var.trxn_type="trxn_type",
                             var.statn_id="statn_loc"){
  users <- unique(trxns[, var.user_id])
  data.frame(user_id=users, 
             strtent = sapply(users, function(u){
               trxns.u <- user.subset(u, data=trxns)
               ss <- trxns.u[ trxns.u[, var.trxn_type] == "out", var.statn_id]
               entropy(table(ss))
             }),
             endent = sapply(users, function(u){
               trxns.u <- user.subset(u, data=trxns)
               es <- trxns.u[ trxns.u[, var.trxn_type] == "in", var.statn_id]
               entropy(table(es))
             }),
             strtend = sapply(users, function(u){
               trxns.u <- user.subset(u, data=trxns)
               ss <- trxns.u[ trxns.u[, var.trxn_type] == "out", var.statn_id]
               es <- trxns.u[ trxns.u[, var.trxn_type] == "in", var.statn_id]
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
heatmap(user.ftr.mat)
library(ggplot2)
user.ftr.sc <- user.ftr.sc[ order(cutree(hc, h=20)),]
ufm.m <- melt(user.ftr.sc[, -2], id.vars = "user_id")
p <- ggplot(ufm.m, aes(variable, user_id)) + 
      geom_tile(aes(fill=sign(value)*log10(abs(value))), color="white") +
       scale_fill_gradient(low="yellow", high="red")
print(p)

