Loc2Tri_Fn <-
function( locmat, TriList, RemoveMissing=FALSE, Verbose=TRUE ){
  r_loc = rep(NA, nrow(locmat))
  for(li in 1:nrow(locmat)){
    Which = NULL
    for( ri in 1:nrow(TriList$E0)){
      v1 = rbind( TriList$E0[ri,], TriList$E1[ri,], TriList$E2[ri,] )
      v2 = outer(rep(1,3),as.matrix(locmat)[li,]) - rbind( TriList$V2[ri,], TriList$V0[ri,], TriList$V1[ri,] )
      crossprod_vec = sapply( 1:3, FUN=function(i){ crossprod_fn(v1[i,],v2[i,]) })
      if( all(crossprod_vec>0) ) Which = c(Which,ri)
    }
    if( length(Which)>=2 ) stop( paste("location",li,"in 2 triangles") )
    if( length(Which)==0 ){
      if( RemoveMissing==FALSE ) stop( paste("location",li,"in 0 triangles") )
      if( RemoveMissing==TRUE ) Which = NA
    }
    r_loc[li] = Which
    if( Verbose==TRUE && (li%%1000)==0 ) print( paste("location",li,"located") )
  }
  return( r_loc )
}

Loc2Tri_Fn2 <-
function( locmat, TriList, RemoveMissing=FALSE, Verbose=TRUE ){
  # Define function
  Within_Fn <- function( loc, locmat ){
    ifelse( loc[1]>min(locmat[,1]) && loc[1]<max(locmat[,1]) && loc[2]>min(locmat[,2]) && loc[2]<max(locmat[,2]), TRUE, FALSE )
  }
  # Search
  r_loc = rep(NA, nrow(locmat))
  for(li in 1:nrow(locmat)){
    Tri2Check = which( sapply(1:n_r, FUN=function(ri){Within_Fn(loc=locmat[li,],locmat=rbind(TriList$V0[ri,],TriList$V1[ri,],TriList$V2[ri,]))}) ) 
    Which = NULL
    for( ri in Tri2Check){
      v1 = rbind( TriList$E0[ri,], TriList$E1[ri,], TriList$E2[ri,] )
      v2 = outer(rep(1,3),as.matrix(locmat)[li,]) - rbind( TriList$V2[ri,], TriList$V0[ri,], TriList$V1[ri,] )
      crossprod_vec = sapply( 1:3, FUN=function(i){ crossprod_fn(v1[i,],v2[i,]) })
      if( all(crossprod_vec>0) ) Which = c(Which,ri)
    }
    if( length(Which)>=2 ) stop( paste("location",li,"in 2 triangles") )
    if( length(Which)==0 ){
      if( RemoveMissing==FALSE ) stop( paste("location",li,"in 0 triangles") )
      if( RemoveMissing==TRUE ) Which = NA
    }
    r_loc[li] = Which
    if( Verbose==TRUE && (li%%1000)==0 ) print( paste("location",li,"located") )
  }
  return( r_loc )
}
