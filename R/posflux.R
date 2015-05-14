posflux <-
function( v1,v2 ){
  rotation = function( rad ) matrix( c(cos(rad),sin(rad),sin(-rad),cos(rad)), nrow=2, ncol=2) # Counterclockwise rotation in radians
  rotate90 = round( rotation(pi/2), digits=1e-10)
  ifelse(dotprod(rotate90%*%v1,v2)>0,1,0)*abs(crossprod_fn(v1,v2)) # Flux outward from a polygon, given unit vectors and counterclockwise edges
}
