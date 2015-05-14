Diff_GridOperator <-
function(var="x", grid_dataframe, h=1){
  # Compute grid distances
  DeltaX = mean( diff(sort(unique(grid_dataframe[,'x']))) )
  DeltaY = mean( diff(sort(unique(grid_dataframe[,'y']))) )
  # Compute adjecancy matrix
  d <- approx_equals(as.matrix(dist(grid_dataframe)), c("x"=DeltaX,"y"=DeltaY)[var]) * 0.5*outer(grid_dataframe[[var]],grid_dataframe[[var]],"-")
  diag(d) <- -colSums(d) ## Mass conservation
  as(d/h,"dgCMatrix")    ## Sparse matrix
}
