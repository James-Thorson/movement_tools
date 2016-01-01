MovementMatrix_Fn <-
function( mesh, TriList){
  M1 = M2 = M3 = M4 = matrix(0, ncol=length(TriList$Tri_Area), nrow=length(TriList$Tri_Area))
  for( i in 1:length(TriList$Tri_Area) ){
    #plot( rbind(V0[i,],V1[i,],V2[i,]), xlim=c(0,1), ylim=c(0,1) )
    #text( rbind(V0[i,],V1[i,],V2[i,]), labels=0:2)
    # Movement across three edges in each of 4 cardinal directions
    for(j in 1:3){
      if( !is.na(mesh$graph$tt[i,j]) ){  # Reflective boundaries
        v2 <- list(TriList$E0, TriList$E1, TriList$E2)[[j]][i,]
        M1[mesh$graph$tt[i,j],i] = M1[mesh$graph$tt[i,j],i] + posflux( c(1,0), v2) / TriList$Tri_Area[i]
        M2[mesh$graph$tt[i,j],i] = M2[mesh$graph$tt[i,j],i] + posflux( c(0,1), v2) / TriList$Tri_Area[i]
        M3[mesh$graph$tt[i,j],i] = M3[mesh$graph$tt[i,j],i] + posflux( c(-1,0), v2) / TriList$Tri_Area[i]
        M4[mesh$graph$tt[i,j],i] = M4[mesh$graph$tt[i,j],i] + posflux( c(0,-1), v2) / TriList$Tri_Area[i]
      }
    }
    # Movement out of cell
    M1[i,i] = -1*sum( M1[,i] )
    M2[i,i] = -1*sum( M2[,i] )
    M3[i,i] = -1*sum( M3[,i] )
    M4[i,i] = -1*sum( M4[,i] )
  }
  # Rescale
  maxout = max(abs(diag(M1+M2+M3+M4)))
  Return = list("maxout"=maxout, "M1"=M1/maxout, "M2"=M2/maxout, "M3"=M3/maxout, "M4"=M4/maxout)
}
