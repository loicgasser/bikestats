library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

shinyServer(function(input, output, session) {
	output$plotTrafficStartToEnd <- renderPlot({
		ss <- input$start_statn
		es <- input$end_statn
		stoe <- subset(vlp2010,
									 start_statn == ss &
									 end_statn == es )
		stoeByDay <- as.data.frame(table(floor_date(stoe$start_time, unit="day")),
																	 stringsAsFactors=FALSE)
		names(stoeByDay) <- c("day", "num.trips")
		stoeByDay <- withByWeekAndMonth(stoeByDay)
		stoeByWeek <- ddply(stoeByDay, .(week, wkdayOrEnd), summarise,
												num.trips = sum(num.trips))
		stoeByMonth <- ddply(stoeByDay, .(month, wkdayOrEnd), summarise,
												num.trips = sum(num.trips))
		if(input$by == "day"){
			xyplot(num.trips ~ day | wkdayOrEnd,
						 data=stoeByDay,
						 type="b",
						 auto.key=TRUE)
		}
		else if (input$by == "week"){
			xyplot(num.trips ~ week | wkdayOrEnd,
						 data=stoeByWeek,
						 type="b",
						 auto.key=TRUE)
		}else {
			xyplot(num.trips ~ month | wkdayOrEnd,
						 data=stoeByMonth,
						 type="b",
						 auto.key=TRUE)
		}
			

	})
	output$plotAverageTraffic <- renderPlot({
		print(paste("make a new plot for inputs ", input$start_statn,
								input$end_statn,
								input$aggBy,
								input$wkdayOrEnd,
								input$month))
		if (input$statn == "all"){
			stoe <- vlp2010
		} else {
			stoe <- subset(vlp2010,
										 start_statn == input$statn)
		}
		hourlyStoe <- withByWeekAndMonth(stoe, "start_time")
		hourlyStoe$start_prd <- hour( hourlyStoe$start_time)
		hourlyStoe$end_prd <- hour( hourlyStoe$end_time)

		## ----hourlyusageyear----------------------------------------------------
		hourlyStoeOut.aggYear <- data.frame(ddply(hourlyStoe, .(start_prd, wkdayOrEnd), .fun="nrow"),
																			 drxn="out")
		names(hourlyStoeOut.aggYear) <- c("hour", "wkdayOrEnd", "num.trips", "drxn")
		hourlyStoeIn.aggYear <- data.frame(ddply(hourlyStoe, .(end_prd, wkdayOrEnd), .fun="nrow"),
																			  drxn="in")
		names(hourlyStoeIn.aggYear) <- c("hour", "wkdayOrEnd", "num.trips", "drxn")
		hourlyStoe.aggYear <- rbind(hourlyStoeOut.aggYear, hourlyStoeIn.aggYear)
	
		## ----hourlyusagemonth----------------------------------------------------
		hourlyStoeOut.aggMonth <- data.frame(ddply(hourlyStoe, 
																							 .(start_prd, wkdayOrEnd, month), .fun="nrow"),
																				 drxn="out")
		names(hourlyStoeOut.aggMonth) <- c("hour", "wkdayOrEnd", "month", "num.trips", "drxn")
		hourlyStoeIn.aggMonth <- data.frame(ddply(hourlyStoe, 
																							.(end_prd, wkdayOrEnd, month), .fun="nrow"),
																				drxn="in")
		names(hourlyStoeIn.aggMonth) <- c("hour", "wkdayOrEnd", "month", "num.trips", "drxn")
		hourlyStoe.aggMonth <- rbind(hourlyStoeOut.aggMonth, hourlyStoeIn.aggMonth)
		hourlyStoe.aggMonth <- subset(hourlyStoe.aggMonth, !is.na(hour))

		hourlyStoe.aggMonth$mon <- as.character(month(hourlyStoe.aggMonth$month, label=TRUE))

		if(input$aggBy == "year"){
			xyplot(num.trips ~ hour | wkdayOrEnd,
						 group=drxn,
						 data=subset(hourlyStoe.aggYear),
						 type="b",
						 auto.key=TRUE,
						 layout=c(1,2))
		} else if (input$month=="all"){
			xyplot(num.trips ~ hour | month,
						 group=drxn,
						 data=subset(hourlyStoe.aggMonth,
												 wkdayOrEnd==input$wkdayOrEnd),
						 layout=c(4,3),
						 type="b",
						 auto.key=TRUE)
		} else {
			xyplot(num.trips ~ hour | wkdayOrEnd,
						 group=drxn,
						 data=subset(hourlyStoe.aggMonth,
												 mon==input$month),
						 type="b",
						 auto.key=TRUE,
						 main=paste("Hourly usage for the month of ", input$month) )
		}
	}, height=800, width=800)

	output$plotAvgTrfBwnStns <- renderPlot({
		print(paste("make a new plot for inputs ", input$start_statn,
								input$end_statn,
								input$aggBy.bs,
								input$wkdayOrEnd.bs,
								input$month.bs))
		stoe <- subset(vlp2010,
									 start_statn == input$start_statn &
									 end_statn == input$end_statn)


		hourlyStoe <- withByWeekAndMonth(stoe, "start_time")
		hourlyStoe$start_prd <- hour( hourlyStoe$start_time)
		hourlyStoe$end_prd <- hour( hourlyStoe$end_time)

		hourlyStoe.aggYear <- aggregatedHourlyUsage(hourlyStoe, aggBy="year")
		hourlyStoe.aggMonth <- aggregatedHourlyUsage(hourlyStoe, aggBy="month")
		hourlyStoe.aggMonth$mon <- as.character(month(hourlyStoe.aggMonth$month, label=TRUE))

		if(input$aggBy.bs == "year"){
			xyplot(num.trips ~ hour | year,
						 group=drxn,
						 data=subset(hourlyStoe.aggYear,
												 wkdayOrEnd==input$wkdayOrEnd.bs ),
						 type="b",
						 auto.key=TRUE,
						 main="Hourly usage over a year" )

		} else if (input$month.bs=="all"){
			xyplot(num.trips ~ hour | month,
						 group=drxn,
						 data=subset(hourlyStoe.aggMonth,
												 wkdayOrEnd==input$wkdayOrEnd.bs ),
						 layout=c(4,3),
						 type="b",
						 auto.key=TRUE,
						 main="Hourly usage over a month")
		} else {
			xyplot(num.trips ~ hour | wkdayOrEnd,
						 group=drxn,
						 data=subset(hourlyStoe.aggMonth,
												 mon==input$month.bs),
						 type="b",
						 auto.key=TRUE,
						 main=paste("Hourly usage for the month of ", input$month.bs) )
		}
	},height=800, width=800)
})

		

	




