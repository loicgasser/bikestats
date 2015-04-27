library(shiny)

##----to make this visualization work, source *stationUsage.R* first.

shinyUI(navbarPage("Bike sharing", id="nav",

	tabPanel("Historical Usage",

					 sidebarLayout(
												 sidebarPanel(
																			selectInput("start_statn", 
																									"Start Station",
																									choices = as.list(stations),
																									selected=1),
																			selectInput("end_statn", 
																									"End Station",
																									choices = as.list(stations),
																									selected=1),
																			selectInput("by",
																									"Time period to sum over",
																									choices =list("day", "week", "month"),
																									selected=1)
																		),
												 mainPanel(
																	 plotOutput("plotTrafficStartToEnd")
																	)
											)
				 ),

	tabPanel("Hourly usage between stations",

					 sidebarLayout(
												 sidebarPanel(
																			selectInput("start_statn",
																									"Start Station",
																									choices=as.list(stations),
																									selected=1),
																			selectInput("end_statn",
																									"End Station",
																									choices=as.list(stations),
																									selected=1),
																			selectInput("aggBy.bs",
																									"Aggregate trips by",
																									choices=list("year", "month"),
																									selected=1),
																			selectInput("wkdayOrEnd.bs",
																									"Weekend/Weekday",
																									choices=list("weekday", "weekend"),
																									selected=1),
																			selectInput("month.bs",
																									"Month",
																									choices=list("all",
																															 "Jan", "Feb", "Mar", "Apr",
																															 "May", "Jun", "Jul", "Aug",
																															 "Sep", "Oct", "Nov", "Dec"),
																									selected=1)
		
																		),
												 mainPanel(
																	 plotOutput("plotAvgTrfBwnStns")
																	)
												 )
					 ),

	
	tabPanel( "Hourly usage in a period",
					 
					 sidebarLayout(
												 sidebarPanel(
																			selectInput("statn",
																									"Station",
																									choices=as.list(c("all", stations)),
																									selected=1),
																			selectInput("aggBy",
																									"Aggregate trips by",
																									choices=list("year", "month"),
																									selected=1),
																			selectInput("wkdayOrEnd",
																									"Weekend/Weekday",
																									choices=list("weekday", "weekend"),
																									selected=1),
																			selectInput("month",
																									"Month",
																									choices=list("all",
																															 "Jan", "Feb", "Mar", "Apr",
																															 "May", "Jun", "Jul", "Aug",
																															 "Sep", "Oct", "Nov", "Dec"),
																									selected=1)
		
																		),
												 mainPanel(
																	 plotOutput("plotAverageTraffic")
																	)
												 )
					 )

))







