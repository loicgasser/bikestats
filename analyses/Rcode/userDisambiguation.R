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

