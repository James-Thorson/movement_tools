Loc2Tri_Fn <-
function( locmat, TriList, RemoveMissing=FALSE ){
  r_loc = rep(NA, nrow(locmat))
  for(li in 1:nrow(locmat)){
    Which = NULL
    for( ri in 1:nrow(TriList$E0)){
      v1 = rbind( TriList$E0[ri,], TriList$E1[ri,], TriList$E2[ri,] )
      v2 = outer(rep(1,3),locmat[li,]) - rbind( TriList$V2[ri,], TriList$V0[ri,], TriList$V1[ri,] )
      crossprod_vec = sapply( 1:3, FUN=function(i){ crossprod_fn(v1[i,],v2[i,]) })
      if( all(crossprod_vec>0) ) Which = c(Which,ri)
    }
    if( length(Which)>=2 ) stop("location in 2 triangles")
    if( length(Which)==0 ){
      if( RemoveMissing==FALSE ) stop("location in 2 triangles")
      if( RemoveMissing==TRUE ) Which = NA
    }
    r_loc[li] = Which
  }
  return( r_loc )
}
