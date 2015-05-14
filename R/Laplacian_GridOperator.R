Laplacian_GridOperator <-
function(var="x", grid_dataframe, h=1){
  # Compute grid distances
  DeltaX = mean( diff(sort(unique(grid_dataframe[,'x']))) )
  DeltaY = mean( diff(sort(unique(grid_dataframe[,'y']))) )
  # Compute adjecancy matrix
  d <- approx_equals(as.matrix(dist(grid_dataframe)),c("x"=DeltaX,"y"=DeltaY)[var]) * approx_equals(as.matrix(dist(grid_dataframe[[var]])),c("x"=DeltaX,"y"=DeltaY)[var])
  diag(d) <- -colSums(d) ## Mass conservation
  as(d/h^2,"dsCMatrix")  ## Symmetric sparse matrix
}
