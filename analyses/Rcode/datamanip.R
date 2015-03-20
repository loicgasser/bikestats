## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(cache=TRUE)
knitr::opts_chunk$set(echo=FALSE)
library(lubridate)

## ----loadcsvOrzip--------------------------------------------------------
read.csv.zip <- function(name.csv, path.csv, path.zip){
  if(file.exists(path.csv)){
    read.csv(path.csv, stringsAFactors=FALSE)
  } else {
    if(file.exists(path.zip)){
      read.csv(unz(path.zip, name.csv))
    }
  }
}

## ----convertDateTime-----------------------------------------------------
convertDateTime <- function(data, vars.time, convFunc=ymd_hms){
	data.frame(data[, setdiff(names(data), vars.time)], 
						 data.frame(lapply(data[, vars.time], convFunc)))
}
											


##Extract data by time periods

## ----sampledata----------------------------------------------------------
library(lubridate)
extractGivenDays <- function(data,
														 start, 
														 end,
														 var.start_time="start_time",
														 var.end_time="end_time"){
	extidx <- (data[ , var.start_time] >= start) & ( data[, var.end_time] <= end)
	extidx[ is.na(extidx)] <- FALSE
	data[extidx, ]
}
extractWeek <- function(data, start, 
												var.start_time="start_time",
												var.end_time="end_time",
												weekdays=FALSE){
	extractGivenDays(data, 
	  						   start=start, 
									 end=start + days(ifelse(weekdays, 5, 7)),
									 var.start_time=var.start_time,
									 var.end_time=var.end_time)
}

## ----makeTransaction-----------------------------------------------------
makeTrxn <- function(trips){
	n <- nrow(trips)
  allvars <- names(trips)
  tocopy <- setdiff(allvars, c("start_time", "end_time", 
                               "start_statn", "end_statn"))
  df <- rbind( trips[, tocopy], trips[, tocopy])
	cbind(df, with(trips, data.frame(time=c(start_time, end_time),
                    			         statn=c(start_statn, end_statn),
                    			         drxn=c(rep("out", n), rep("in", n))
                                  )
                 )
        )
}

