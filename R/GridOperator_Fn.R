GridOperator_Fn <-
function( grid_dataframe ){
  ## Setup regular mesh
  ngrid <- nrow(grid_dataframe)
  h <- 1 / sqrt(ngrid)
  #grid <- igrid * h ## Unit square
  
  ## Laplacian operator - regular spacing h
  nablaxx <- Laplacian_GridOperator("x",grid_dataframe,h=h)
  nablayy <- Laplacian_GridOperator("y",grid_dataframe,h=h)
  
  ## Central difference - zero flux across boundary
  gradx <- Diff_GridOperator("x",grid_dataframe,h=h)
  grady <- Diff_GridOperator("y",grid_dataframe,h=h)
  nablaxy <- gradx %*% grady ## (nablayx == nablaxy)
  
  ## gradx -- left and right -- zero flux across boundary
  gradx_lwr = gradx_upr = gradx
  gradx_lwr[upper.tri(gradx,diag=TRUE)] = 0
  gradx_upr[lower.tri(gradx,diag=TRUE)] = 0
  diag(gradx_upr) = -colSums( as.matrix(gradx_upr) )
  diag(gradx_lwr) = -colSums( as.matrix(gradx_lwr) )
  
  ## grady -- left and right -- zero flux across boundary
  grady_lwr = grady_upr = grady
  grady_lwr[upper.tri(grady,diag=TRUE)] = 0
  grady_upr[lower.tri(grady,diag=TRUE)] = 0
  diag(grady_upr) = -colSums( as.matrix(grady_upr) )
  diag(grady_lwr) = -colSums( as.matrix(grady_lwr) )

  # Return objects
  Return = list("nablaxx"=nablaxx, "nablayy"=nablayy, "nablaxy"=nablaxy, "gradx"=gradx, "grady"=grady, "gradx_lwr"=gradx_lwr, "gradx_upr"=gradx_upr, "grady_lwr"=grady_lwr, "grady_upr"=grady_upr )
  return( Return )
}
