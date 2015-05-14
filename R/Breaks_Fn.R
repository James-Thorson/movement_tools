Breaks_Fn <-
function(Obj, breaks=c(-Inf,10^(-3:0))){
  Cut <- cut(Obj,breaks)
  if(is.array(Obj)) Return <- array( as.numeric(Cut), dim=dim(Obj))
  if(is.vector(Obj)) Return <- as.numeric(Cut)
  return( Return )
}
