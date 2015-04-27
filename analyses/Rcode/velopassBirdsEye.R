## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(cache=FALSE)
knitr::opts_chunk$set(echo=FALSE)
knitr::opts_chunk$set(include=TRUE)
knitr::opts_chunk$set(results="asis")
knitr::opts_chunk$set(fig.width=12)
knitr::opts_chunk$set(fig.height=14)
prjpath <- "~/work/citiviz/bikeSharing/bikestats/"
datapath <- paste(prjpath, "data/velopass_data/", sep="")
analpath <- paste(prjpath, "analyses/", sep="")
rcodepath <- paste(analpath, "Rcode/", sep="")
setwd(analpath)
library(lattice)

## ----loadLibsAndSource, include=FALSE------------------------------------
reqdpkgs <- c("lattice", "latticeExtra", "ggplot2", "reshape2", "plyr", "corrplot",
							"Hmisc", "corrplot", "rgdal", "sp")
lapply(reqdpkgs, library, character.only=TRUE)
source(paste(rcodepath, "datamanip.R", sep=""))

## ----readdata------------------------------------------------------------
dd <- paste(datapath, "transactions_lausanne-morges_2010/", sep="")
vlp2010 <- read.csv(paste(dd, "velopass_lausanne-morges_combined.csv", sep=""),
									 	stringsAsFactors=FALSE)

## ----datetime------------------------------------------------------------
vlp2010 <- convertDateTime(vlp2010, c("start_time", "end_time"))
vlp2010 <- vlp2010[ order(vlp2010$start_time), ]

## ----tripsbyday----------------------------------------------------------
iswknd <- c(TRUE, rep(FALSE, 5), TRUE)
names(iswknd) <- c("Sunday", "Monday", "Tuesday", "Wednesday", 
											"Thursday", "Friday", "Saturday")
tripsByDay <- as.data.frame(table(floor_date(vlp2010$start_time, unit="day")),
													 	stringsAsFactors=FALSE)
names(tripsByDay) <- c("day", "num.trips")
tripsByDay$dayOfWeek <- factor(weekdays(ymd(tripsByDay$day)),
															 levels=c("Sunday", "Monday", "Tuesday",
																			 	"Wednesday", "Thursday", "Friday",
																			 	"Saturday") )
											
tripsByDay$day <- ymd(tripsByDay$day)
tripsByDay$weekdayOrEnd <- with(tripsByDay,
																		c("weekday", "weekend")[iswknd[dayOfWeek] + 1])

## ----peektripsbyday, results="markup"------------------------------------
print(head(tripsByDay))


## ----tripsbyweek---------------------------------------------------------

tripsByDay$week <- floor_date(ymd(tripsByDay$day), unit="week")
tripsByWeek <- ddply(tripsByDay, .(week, weekdayOrEnd), summarise, 
														 num.trips = sum(num.trips))

## ----tripsbymonth--------------------------------------------------------
tripsByDay$month <- floor_date(ymd(tripsByDay$day), unit="month")
tripsByMonth <- ddply(tripsByDay, .(month, weekdayOrEnd), summarise, 
														 num.trips = sum(num.trips))

## ----tripsbydayaggregatemonth--------------------------------------------
tripsByWeekday.month <- ddply(tripsByDay, .(month, dayOfWeek), summarise,
													 mean.num.trips = mean(num.trips),
													 var.num.trips = var(num.trips) )

tripsByWeekday.month$lower.num.trips <- with(tripsByWeekday.month,
																						 mean.num.trips - sqrt(var.num.trips))
tripsByWeekday.month$upper.num.trips <- with(tripsByWeekday.month,
																						 mean.num.trips + sqrt(var.num.trips))

tripsByWeekday.month$num.dayOfWeek <- as.numeric(tripsByWeekday.month$dayOfWeek)
xYplot(Cbind(mean.num.trips, lower.num.trips, upper.num.trips) ~ num.dayOfWeek | month,
			 data=tripsByWeekday.month,
			 type='b',
			 main="Number of trips for day of the week")

## ----timeofdayinsecs-----------------------------------------------------
timeOfDayInSeconds <- function(ts, format){
	if (all(class(ts) == c("POSIXct","POSIXt"))){
		3600*hour(ts) + 60*minute(ts) + second(ts)
	}
	else if (class(ts) == "character"){
		if (format == "hms"){
			sapply(ts, function(t) {
							sum(c(3600, 60, 1) * as.numeric(strsplit(x=t, split=":")[[1]]))
						})
		}
		else if (format == "ymd hms"){
			ts <- do.call(rbind, strsplit(x=ts, split=" "))[,2]
			timeOfDayInSeconds( ts, format="hms")
		}
		else {
			stop("UNKNOWN FORMAT: allowed 'hms', 'ymd hms'")
		}
	}
	else {
		stop("t of UNKNOWN CLASS: allowed character and POSIXct/t")
	}
}

## ----tripsAtHour---------------------------------------------------------

tripsStartingInPeriod <- function(trips, start_prd, end_prd, format="hms"){
	spsecs = timeOfDayInSeconds(start_prd, format="hms")
	epsecs = timeOfDayInSeconds(end_prd, format="hms")
	stsecs = timeOfDayInSeconds(trips$start_time)
	trips[ stsecs >= spsecs & stsecs <= epsecs,]
}

tripsEndingInPeriod <- function(trips, start_prd, end_prd, format="hms"){
	spsecs = timeOfDayInSeconds(start_prd, format="hms")
	epsecs = timeOfDayInSeconds(end_prd, format="hms")
	etsecs = timeOfDayInSeconds(trips$end_time)
	trips[ etsecs >= spsecs & etsecs <= epsecs,]
}

withByWeekAndMonth <- function(tbd, varDateTime="day"){
	cls <- class(tbd[, varDateTime])
	if (all(cls == c("POSIXct", "POSIXt"))) {
		tbd$day = floor_date( tbd[ , varDateTime], unit="day")
	}
	else if (cls != "character") {
		stop(paste("class", class(tbd), "of provided data not implemented"))
	}
	tbd$day <- ymd(tbd$day)
	tbd$dayOfWeek <- factor(weekdays(tbd[, varDateTime]), 
													levels=c("Sunday", "Monday", "Tuesday", 
																	 "Wednesday", "Thursday", "Friday", "Saturday") )
	tbd$wkdayOrEnd <- c("weekday", "weekend")[iswknd[tbd$dayOfWeek] + 1]
	tbd$week <- floor_date(tbd[, varDateTime], unit="week")
	tbd$month <- floor_date(tbd[, varDateTime], unit="month")
	tbd
}


countTripsBwnHours <- function(trips, start_prd, end_prd, by="day", plot=FALSE){
                    
	starting <- tripsStartingInPeriod(trips, start_prd, end_prd)
	ending <- tripsEndingInPeriod(trips, start_prd, end_prd)
	startingByDay <- as.data.frame(table(floor_date(starting$start_time, unit="day")),
																 stringsAsFactors=FALSE)
	names(startingByDay) <- c("day", "num.trips")
	startingByDay <- withByWeekAndMonth(startingByDay)
	endingByDay <- as.data.frame(table(floor_date(ending$start_time, unit="day")),
																 stringsAsFactors=FALSE)
	names(endingByDay) <- c("day", "num.trips")
	endingByDay <- withByWeekAndMonth(endingByDay)
	counts <- rbind(data.frame(ddply(startingByDay, c(by, "wkdayOrEnd"), summarise,
																		num.trips = sum(num.trips)),
															drxn="out", start_prd=start_prd, end_prd=end_prd),
									data.frame(ddply(endingByDay, c(by, "wkdayOrEnd"), summarise,
																	 num.trips = sum(num.trips)),
														 drxn="in", start_prd=start_prd, end_prd=end_prd) 
									)

	counts.p <- counts
	counts.p$period <- counts[, by]
	if(plot){
		p  <-  xyplot(num.trips ~ period | drxn, group=wkdayOrEnd, 
								 data=counts.p,
								 type="b",
								 auto.key=TRUE,
								 main=paste("total trips by ", by, " between ", 
														start_prd, " and ", end_prd, sep="") 
								)
		print(p)
	}
	counts
}


## ----cth-----------------------------------------------------------------
countTripsHourly <- function(trips, offset = 0, by="day", plot=FALSE) {
	hrs <- 	0:23 
	do.call(rbind, 
					lapply(0:23, function(h) countTripsBwnHours(trips, paste(h, offset, 0, sep=":"),
																										  paste(h+1, offset, 0, sep=":"),
																										  by=by, plot=plot))
				 )
}

## ----dailyusagebythehour-------------------------------------------------
usageByTheHour.aggDay <- countTripsHourly(vlp2010)

## ----hourlyusage---------------------------------------------------------
hourlyUsage <- withByWeekAndMonth(vlp2010, "start_time")
hourlyUsage$start_prd <- hour(hourlyUsage$start_time)
hourlyUsage$end_prd <- hour(hourlyUsage$end_time)
hourlyOutUsage.aggYear <- data.frame(ddply(hourlyUsage, .(start_prd), .fun="nrow"), 
															 					 drxn="out")
names(hourlyOutUsage.aggYear) <- c("hour", "num.trips", "drxn")
hourlyInUsage.aggYear <- data.frame(ddply(hourlyUsage, .(end_prd), .fun="nrow"),
																		drxn="in")
names(hourlyInUsage.aggYear) <- c("hour", "num.trips", "drxn")

hourlyUsage.aggYear <- rbind(hourlyOutUsage.aggYear, hourlyInUsage.aggYear)

## ----hourlyusagemonth----------------------------------------------------
hourlyOutUsage.aggMonth <- data.frame(ddply(hourlyUsage, .(start_prd, wkdayOrEnd, month), .fun="nrow"),
																						drxn="out")
names(hourlyOutUsage.aggMonth) <- c("hour", "wkdayOrEnd", "month", "num.trips", "drxn")
hourlyInUsage.aggMonth <- data.frame(ddply(hourlyUsage, .(end_prd, wkdayOrEnd, month), .fun="nrow"),
																						drxn="in")
names(hourlyInUsage.aggMonth) <- c("hour", "wkdayOrEnd", "month", "num.trips", "drxn")
hourlyUsage.aggMonth <- rbind(hourlyOutUsage.aggMonth, hourlyInUsage.aggMonth)
hourlyUsage.aggMonth <- subset(hourlyUsage.aggMonth, !is.na(hour))


## ----tripdurations-------------------------------------------------------
vlp2010 <- withByWeekAndMonth(vlp2010, "start_time")
vlp2010$duration <- duration(new_interval(vlp2010$start_time, vlp2010$end_time), units="seconds")

## ----trafficbwnstns------------------------------------------------------
traffic <- table(vlp2010$start_statn, vlp2010$end_statn)

## ----stnloc--------------------------------------------------------------
stnlocs <- read.csv(paste(datapath, "stations.csv", sep=""), sep=",", 
										stringsAsFactors=FALSE)
stnlocs.spatial <- stnlocs
coordinates(stnlocs.spatial) <- c("lon", "lat")
proj4string(stnlocs.spatial) <- CRS("+init=epsg:4326")

## ----stnmap--------------------------------------------------------------
b <- bbox(stnlocs.spatial)
b[1, ] <- (b[1,] - mean(b[1,])) * 1.05 + mean(b[1,])
b[2, ] <- (b[2,] - mean(b[2,])) * 1.05 + mean(b[2,])
stn.map <- ggmap(get_map(location=b, source="stamen", maptype="toner", crop=TRUE))
stn.map <- ggmap(get_map(location=b, source="google",  crop=TRUE, zoom=14))
stn.map + geom_point(data=stnlocs, aes(x=lon, y=lat), size=6, col="red")


