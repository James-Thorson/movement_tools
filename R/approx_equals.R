approx_equals <-
function(a,b) ifelse( abs(log(a/b))<1e-3, TRUE, FALSE )
