matpow <- function( mat, pow ){
  Return <- mat
  for( i in seq(1,pow-1,length=pow-1) ){
    Return <- Return %*% mat
  }
  return( Return )
}
