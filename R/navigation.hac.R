navigation.hac <-
function( pos ){
  #require(geosphere)
  vn <- names(pos)
  if (!'lon' %in% vn | !'lat' %in% vn )
    stop ("missing longitude or latitude data" )
  np <- nrow(pos)
  if (np == 1){
    ans <- data.frame(pos[ , -1], bearing=NA, navdist=NA, time.dif=NA, navspeed=NA)
  } else {  
    p1 <- pos[1:(np-1), c('lon', 'lat')]
    p2 <- pos[2:(np), c('lon', 'lat')]
    bear <- c(geosphere::bearingRhumb( p1, p2 ), NA)
    navdist <- c(0, geosphere::distVincentyEllipsoid(p1, p2)/1852)
    time.dif <- c(0, difftime(pos[2:np, 'time.cpu'], pos[1:(np-1), 'time.cpu'], units = 'hours'))
    navspeed <- navdist / time.dif
    navspeed[1] <- 0
    ans <- data.frame(pos[ , -1], bearing=bear, navdist, time.dif, navspeed)
  }  
  ans
}
