## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(cache=TRUE)
knitr::opts_chunk$set(echo=FALSE)

## ----setvarnames---------------------------------------------------------
namingScheme <- function(nms.orig,
			 var.user_id=NA,
			 var.trip_id=NA,
			 var.start_statn=NA,
			 var.end_statn=NA,
			 var.bike_id=NA,
			 var.start_time=NA,
			 var.end_time=NA){

	nms.rplc <- nms.orig
	names(nms.rplc) <- nms.orig

	nms.touse  <- c("user_id", "trip_id", "bike_id", 
			"start_statn", "end_statn",
			"start_time", "end_time")
			
	nms.torplc <- c(var.user_id, var.trip_id, var.bike_id,
			var.start_statn, var.end_statn,
			var.start_time, var.end_time)
	nms.touse <- nms.touse[ !is.na(nms.torplc)]
	nms.torplc <- nms.torplc[ !is.na(nms.torplc)]
	nms.rplc[ nms.torplc] <- nms.touse
	nms.rplc
}

## ----setvarnames.statn---------------------------------------------------
namingScheme.statn <- function(nms.orig,
                      			 var.statn_id=NA,
                      			 var.lat=NA,
                      			 var.long=NA,
                      			 var.status=NA){

	nms.rplc <- nms.orig
	names(nms.rplc) <- nms.orig

  nms.touse <- c("statn", "long", "lat", "status" )
			
	nms.torplc <- c(var.statn_id, var.long, var.lat, var.status)
	nms.touse <- nms.touse[ !is.na(nms.torplc)]
	nms.torplc <- nms.torplc[ !is.na(nms.torplc)]
	nms.rplc[ nms.torplc] <- nms.touse
	nms.rplc
}

