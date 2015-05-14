PlotImage_Fn <-
function( Vec, breaks, Xset, Yset, DFexpand, ...){
  X = c(Xset[1]-mean(diff(Xset))/2,Xset+mean(diff(Xset))/2)
  Y = c(Yset[1]-mean(diff(Yset))/2,Yset+mean(diff(Yset))/2)
  Z = matrix(Breaks_Fn(Vec[DFexpand[,'r_loc']],breaks=breaks),ncol=length(Xset))
  image( z=Z, x=X, y=Y, breaks=1:length(breaks), col=Col(length(breaks)-1), ...)
}
