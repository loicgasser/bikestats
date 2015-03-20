## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(cache=TRUE)
knitr::opts_chunk$set(echo=FALSE)
knitr::opts_chunk$set(fig.width=14)
knitr::opts_chunk$set(fig.height=12)
prjpath <- "~/work/citiviz/bikeSharing/bikestats/"
datapath <- paste(prjpath, "data/", sep="")
analpath <- paste(prjpath, "analyses/", sep="")
rcodepath <- paste(analpath, "Rcode/", sep="")

library(shiny)
library(lubridate)

## ----tripmaker-----------------------------------------------------------
makeTripGivenCoords <- function(start_time, 
																start_statn,  
																end_time, 
																end_statn,
																user_id){
  data.frame(start_time=start_time, 
						 start_statn=start_statn, 
						 end_time=end_time, 
						 end_statn=end_statn, 
						 user_id=user_id)
}
shiftEndTime <- function(trip1, dt, units="minutes"){
  trip1n <- trip1
  trip1n$end_time <- trip1n$end_time + timeConvFuncs[[units]](dt)
  trip1n
}
shiftStartTime <- function(trip, dt, units="minutes"){
  tripn <- trip
  tripn$start_time <- tripn$start_time + timeConvFuncs[[units]](dt)
  tripn
}
shiftTripTimes <- function(trip, dt, units="minutes"){
  shiftStartTime( shiftEndTime( trip, dt, units), dt, units)
}

relocEndStatn <- function(trip, statn){
	trip2 <- trip
	trip2$end_statn = statn
	trip2
}
relocStartStatn <- function(trip, statn){
	trip2 <- trip
	trip2$start_statn = statn
	trip2
}
relocTrip <- function(trip, start, end){
	trip2 <- trip
	trip2$start_statn <- start
	trip2$end_statn <- end
	trip2
}
reverseStatns <- function(trip) {
	trip2 <- trip
	trip2$start_statn <- trip$end_statn
	trip2$end_statn <- trip$start_statn
	trip2
}



## ----disamgen------------------------------------------------------------
distBwnTrips <- function(trip1, trip2, statnLoc,
			                    wt.dur=1,
                          wt.dis=1,
                          tsep=5,
                          hsep=2){
	if(ovlpngTrips(trip1, trip2)){
		Inf
	} else {
    
    psep12 <- 1/(1 + exp(hsep * (timeSprnTrips(trip1, trip2) - tsep) ) )
		tdrn12 <- abs(drntrip(trip1) - drntrip(trip2))
		d12.s <- distBwnStatns(trip1$start_statn, trip2$start_statn, statnLoc = statnLoc)
		d12.e <- distBwnStatns(trip1$end_statn, trip2$end_statn, statnLoc=statnLoc)
    ds <- c(tdrn12, d12.s, d12.e)
    wts <- c(wt.dur, wt.dis, wt.dis)
    psep12 * sqrt(sum(wts*(ds^2))/sum(wts)) 
   }
}



## ----sampledata----------------------------------------------------------
trips.sample <- read.csv(paste(datapath, "extracted/hubwayTripsSample.csv", sep=""),
															 stringsAsFactors=FALSE)

## ----validuser-----------------------------------------------------------
validUser <- function(uid, data){
	hs <- subset(data, user_id == uid & !is.na(start_time) & !is.na(end_time))
  hs <- hs[order(hs$var.start_time),]
  n <- nrow(hs)
  s <- hs$start_time
  e <- hs$end_time
  ifelse(n==1, s <= e, all(s[2:n] > e[1:(n-1)]))
}

validUserIds <- function(data, uids=NULL){
  if(is.null(uids)) uids <- unique(data$user_id)
  all( sapply(uids, function(u) 
    validUser(u, data)
		)
	)
}

## ----tripoverlap---------------------------------------------------------
ovlpngTrips <- function(trip1, trip2){
  s1 <- trip1$start_time; e1 <- trip1$end_time 
  s2 <- trip2$start_time; e2 <- trip2$end_time 
  if ((s2 > e2) & (s1 > e1)) NA
  else{
    nonovlp <- (e2 <= s1) | (s2 >= e1)
    !nonovlp
  }
}

tripOverlaps <- function(trips, rentry.time = 0) {
  #use a exponential relaxation to re-enter users into the system after a trip.
  #parameter rentry.time is measured in minutes
  n <- nrow(trips)
  as.dist( do.call("rbind", 
                   lapply(1:n, function(i){
                    dt <- trips$end_time[1:i] - trips$start_time[i]
                    dt <- as.double(dt, units="mins")
                    dis <- c((dt > 0)*(1 - exp(-dt/rentry.time)),  rep(NA, n - i)) 
                    dis[is.nan(dis)] <- 0
                    dis
                   }))
        )
}

## ----disambg-------------------------------------------------------------
disambguser <- function(user, trips,
                        var.pre_user_id="pre_user_id",
                        rentry.time=0){
  trips <- trips[ trips[, var.pre_user_id] == user,]
  tol <- tripOverlaps(trips, rentry.time)
  trips$new_user_id <- paste(trips[, var.pre_user_id],
                             if( sum(tol) > 0 ) cutree(hclust(tol,method="ward.D2"), h=0) 
                              else   rep(1, nrow(trips)),
                             sep="."
                       )
  trips
}


## ----testdisambg---------------------------------------------------------
doUserTripsOverlap <- function(user, trips, test_ids){
  trips.u <- trips[ test_ids == user,]
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


## ----probRcrTrip---------------------------------------------------------
probRcrTrip <- function( trcr = 5, trlx = 10, units="minutes"){
	function(t) {
		ifelse(t <= 0,
					 0,
				 	 1/(1 + exp(-(dminutes(t) - dminutes(trcr))/dminutes(trlx)))
		)
	}
}

## ----egProbRcrTrip, cache=FALSE, echo=FALSE, fig.width=14, fig.height=40----
library(shiny)
inputPanel(
					 sliderInput("trcr", label="time of recurrence:", 
											 min=0, max=100, value=5, step=0.5),
					 sliderInput("trlx", label="time of relaxation:", 
											 min=0, max=100, value=5, step=0.5)
)
renderPlot({ 
					 ts <- seq(0,100,length=100)
					 plot(ts, sapply(ts, probRcrTrip(input$trcr, input$trlx)), 
                 type="l", 
                 main="How does the recurrent probability\n
                 depend on relaxation and recurrent times?",
								 xlab="$dt$",
								 ylab="$P$",
                 ylim=c(0,1))
            abline(v=0.5)
            abline(h=input$trcr)
}, height=600, width=800)

## ----probSecTrip.Stn-----------------------------------------------------
prwtLoc2ndTrp <- function(s1, e1, s2, e2, weight=0){
	if(is.na(s1) | is.na(e1) | is.na(s2) | is.na(e2)){
		print("			stations unavailable")
		0
	} else {
		if (s1 == e1){
			exp(-weight * (2 - ((s2 == s1) + (e2 == s1))))
		}
		else {
			exp(-weight * (2 - ((s2 == s1) + (e2 == e1) + (s2 == e1) + (e2 == s1))))
		}
	}
}
	

## ----genSecondStatn------------------------------------------------------
gen2ndStatns <- function(s1, e1){
	st2 <- list()
	st2$same <- c(s1, e1)
	st2$reverse <- c(e1, s1)
	st2$sameStart <- c(s1, s1 + e1 + 1)
	st2$sameEnd <- c(s1 + e1 + 1, e1)
	st2$different <- c(s1 + e1 + 1, s1 + e1 + 2)
  st2
}
gen1stStatns <- function(){
	st1 <- list()
	st1$sameStrtEnd <- c(1,1)
	st1$diffStrtEnd <- c(1,2)
  st1
}

## ----visProbSecTrip.Stn, cache=FALSE-------------------------------------
inputPanel(
					 sliderInput("weight", label="Weight parameter:",
											 min=0, max=10, value=1, step=1)
)
renderPlot({
						pwts.1 <- lapply(gen2ndStatns(1,2), function(s2){
															 prwtLoc2ndTrp(1, 2, s2[1], s2[2], weight=input$weight)
											 })
						pwts.2 <- lapply(gen2ndStatns(1,1), function(s2){
															 prwtLoc2ndTrp(1, 1, s2[1], s2[2], weight=input$weight)
											 })
						plot(x=as.factor(names(pwts.1)), y=as.numeric(pwts.1))
						points(x=as.factor(names(pwts.2)), y=as.numeric(pwts.2), col="red")
}, width=800, height=600)

## ----prob2ndTrip---------------------------------------------------------
	timeConvFuncs <- c(dseconds, dminutes, dhours, ddays)
	names(timeConvFuncs) <- c("seconds", "minutes", "hours", "days")
	timeToSecTrip <- function(trip1, trip2, units="minutes"){
		duration(interval(start=trip1$end_time, end=trip2$start_time))/timeConvFuncs[[units]](1)
	}
	logWtSecTrip <- function(trip1, trip2, 
													trcr=5, trlx=5,
													weight=1, numStatns=10, 
													timeUnits="minutes"){
		wtime <- log(probRcrTrip(trcr, trlx)(timeToSecTrip(trip1, trip2, units=timeUnits)))
		wloc <- log(prwtLoc2ndTrp(trip1$start_statn, trip1$end_statn,
													trip2$start_statn, trip2$end_statn,
													weight=weight))
		wtime + wloc
	}

## ----egProbSecTripTable--------------------------------------------------
t1 <- makeTripGivenCoords(start_time = ymd_hms("2012-01-1 08:00:00"),
													end_time =  ymd_hms("2012-01-1 08:05:00"),
													start_statn = 1,
													end_statn = 2,
													user_id=1)


wtsSecTrip <- function(trips, 
											 trcr=5, trlx=5,
											 weight=1){
  N <- length(trips)
  do.call(rbind, lapply(1:N,
                        function(n){
                          sapply(1:N,
                                 function(m){
                                   logWtSecTrip(trips[[m]], trips[[n]],
																								trcr=trcr, trlx=trlx,
																								weight=1)
                                 })
                        }))
}

makeSecTripClusterAndPlot <- function(wts, withPlot=FALSE){
	dis <- -wts
	dis[ is.infinite(dis)] <- 100
	if(nrow(dis) < 2){
		print("only one trip!")
		NA
	} else {
		hc <- hclust(as.dist(dis), method="complete")
		if(withPlot) plot(hc)
		hc
	}
}

numUserTrips <- as.data.frame(table(trips.sample$user_id), stringsAsFactors=FALSE)
names(numUserTrips) <- c("user_id", "numTrips")
numUserTrips <- numUserTrips[ order(numUserTrips$numTrips, decreasing=TRUE),]
rownames(numUserTrips) <- numUserTrips$user_id



## ----testProbSecTripTable------------------------------------------------
trips.eg <- list(t1, 
								 shiftTripTimes(t1, 1),
								 shiftTripTimes(t1, 2),
								 shiftTripTimes(t1, 3),
								 shiftTripTimes(t1, 4),
								 shiftTripTimes(t1, 5),
                 shiftTripTimes(t1, 6),
                 shiftTripTimes(t1, 7),
                 shiftTripTimes(t1, 8),
                 shiftTripTimes(t1, 9),
                 shiftTripTimes(t1, 10),
                 shiftTripTimes(t1, 11)
								)
wts <- wtsSecTrip(trips.eg)
hc <- makeSecTripClusterAndPlot(wts)
plot(hc)

## ----testProbSecTripTable2-----------------------------------------------
trips.eg <- list(t1, 
									shiftTripTimes(t1, 5.1),
									shiftTripTimes(t1, 6),
									shiftTripTimes(t1, 6.1),
									shiftTripTimes(t1, 10),
									shiftTripTimes(t1, 10.2)
									)
wts <- wtsSecTrip(trips.eg)
hc <- makeSecTripClusterAndPlot(wts)

## ----testProbSecTripTable3-----------------------------------------------
trips.eg <- list(t1, 
									shiftTripTimes(t1, 5.1),
									shiftTripTimes(t1, 11.0),
									shiftTripTimes(t1, 17.0),
									shiftTripTimes(t1, 24.0),
									shiftTripTimes(t1, 32.0)
									)
wtSecTripTable <- wtsSecTrip(trips.eg)
hc <- makeSecTripClusterAndPlot(wtSecTripTable)

## ----testProbSecTripTabelHubway------------------------------------------
trips.sample <- trips.sample[ order(trips.sample$start_time),]

users <- unique(trips.sample$user_id) 

clusterTrips <- function(data, withPlot=FALSE){
	trips.list <- lapply(1:nrow(data), function(n) data[n,])
	wts <- wtsSecTrip(trips.list)
	makeSecTripClusterAndPlot(wts, withPlot=FALSE)
}

clusterTripsOfPreUser <- function(data, user_id, withPlot=FALSE){
	data.user <- subset(data, user_id == user_id)
  print(paste("for pre user", user_id, "trips to disambiguate", nrow(data.user)))
	clusterTrips(data.user)
}


disambiguateWithClustering <- function(user, trips, 
																			var.pre_user_id="pre_user_id", 
                                      h=2){
	vars.statns <- c("start_statn", "end_statn")
	if(sum(is.na(trips[, vars.statns]))) {
		print("--------trip stations unavailable for user")
		list(clustering=NA, rep(user, nrow(trips)))
	} else {
		hc <- clusterTrips(trips, withPlot=FALSE)
		if(is.na(hc)) {
			k <- 1
		} else {
			k <- cutree(hc, h=h)
		}
		list(user_id=user, clustering=hc, new_user_ids=paste(rep(user, nrow(trips)), k, sep="."))
	}
}

## ----someHubwayExamples--------------------------------------------------
#hc1 <- clusterTripsOfPreUser(trips.sample, numUserTrips$user_id[1])
# hc2 <- clusterTripsOfPreUser(trips.sample, numUserTrips$user_id[2])
# hc.Female1960.1 <- clusterTripsOfPreUser(trips.sample, "Female1960.1")
# hc.Male1990.1 <- clusterTripsOfPreUser(trips.sample, "Male1990.1")
hc.allUsers <- lapply(numUserTrips$user_id,
											function(u){
												print(paste("disambiguating user", u,
																		"who made", numUserTrips[u, "numTrips"], "trips"))
												hc <- disambiguateWithClustering(u, subset(trips.sample, user_id == u))
												print(paste("disambiguated ", length(unique(hc$new_user_id)), "users"))
												hc
											}
											)


## ----probSameUserOfLocs--------------------------------------------------
probSameUserGvnStatns <- function(s1, e1, s2, e2, omega=1){
	if((s1 == e1) | (s2 == e2)) stop("Trip starts and ends at the same point")
	omega^( (s2 == s1) + (e2 == e1) + (s2 == e1) + (e2 == s1) )/(2*(1 + omega)^2)
}

## ----timeSprnTrips-------------------------------------------------------
timeSprnTrips <- function(trip1, trip2, units="minutes"){
  convFuncs <- c(eseconds, eminutes, ehours, edays)
  names(convFuncs) <- c("seconds", "minutes", "hours", "days")
  if(trip1$start_time < trip2$start_time){
    duration(interval(start=trip1$end_time, end=trip2$start_time))/convFuncs[[units]](1)
  } else{
    duration(interval(start=trip2$end_time, end=trip1$start_time))/convFuncs[[units]](1)
  }
}

probSepTrips <- function(trip1, trip2, tsep=5, hsep=5, units="minutes"){
  if(ovlpngTrips(trip1, trip2)) {0}
  else{
    tsep12 <- timeSprnTrips(trip1, trip2, units=units)
    1 - 1/(1 + exp((tsep12 - tsep)/hsep))
  }
}

## ----distBwnStatns-------------------------------------------------------
distBwnStatns <- function(stn1, stn2, statnLoc){
  l12 <- statnLoc[ statnLoc$statn %in% c(stn1, stn2), c("long", "lat")]
  as.numeric(dist(l12))
}
  

## ----durationTrip--------------------------------------------------------
drntrip <- function(trip, units="minutes"){
  duration(interval(start=trip$start_time, end=trip$end_time))/timeConvFuncs[[units]](1)
}

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
   #                       function(u) doUserTripsOverlaps(user=u, trips=trips.da))
    #          ), 
     #         equals(FALSE)
      #       )
#}

