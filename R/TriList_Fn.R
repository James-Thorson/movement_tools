TriList_Fn <-
function( mesh ){
  # Triangle to vertex indexing
  TV = mesh$graph$tv
  # V = vertices for each triangle
  V0 = mesh$loc[TV[,1],1:2]
  V1 = mesh$loc[TV[,2],1:2]
  V2 = mesh$loc[TV[,3],1:2]
  # E = edge for each triangle
  E0 = V2 - V1
  E1 = V0 - V2
  E2 = V1 - V0
  # Triangle areas
  Tri_Area = rep(NA, nrow(TV))
  for(i in 1:length(Tri_Area)) Tri_Area[i] = abs(crossprod_fn( E0[i,], E1[i,] ))/2   # T = area of each triangle
  # Return
  TriList = list( "TV"=TV, "V0"=V0, "V1"=V1, "V2"=V2, "E0"=E0, "E1"=E1, "E2"=E2, "Tri_Area"=Tri_Area )
  return( TriList )
}
