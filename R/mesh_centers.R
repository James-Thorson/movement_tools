mesh_centers <-
function(mesh){
  center_tri = matrix(NA, nrow=nrow(mesh$graph$tv), ncol=2)
  for(i in 1:nrow(center_tri)) center_tri[i,] = colMeans(mesh$loc[,1:2][mesh$graph$tv[i,],])
  return( center_tri )
}
