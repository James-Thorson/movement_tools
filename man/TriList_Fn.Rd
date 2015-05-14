\name{TriList_Fn}
\alias{TriList_Fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
TriList_Fn(mesh)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mesh}{
%%     ~~Describe \code{mesh} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (mesh) 
{
    TV = mesh$graph$tv
    V0 = mesh$loc[TV[, 1], 1:2]
    V1 = mesh$loc[TV[, 2], 1:2]
    V2 = mesh$loc[TV[, 3], 1:2]
    E0 = V2 - V1
    E1 = V0 - V2
    E2 = V1 - V0
    Tri_Area = rep(NA, n_r)
    for (i in 1:length(Tri_Area)) Tri_Area[i] = abs(crossprod_fn(E0[i, 
        ], E1[i, ]))/2
    TriList = list(TV = TV, V0 = V0, V1 = V1, V2 = V2, E0 = E0, 
        E1 = E1, E2 = E2, Tri_Area = Tri_Area)
    return(TriList)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
