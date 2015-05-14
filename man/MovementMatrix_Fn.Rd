\name{MovementMatrix_Fn}
\alias{MovementMatrix_Fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate movement matrices on triangulated mesh
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MovementMatrix_Fn(mesh, TriList)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mesh}{
%%     ~~Describe \code{mesh} here~~
}
  \item{TriList}{
%%     ~~Describe \code{TriList} here~~
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
function (mesh, TriList) 
{
    M1 = M2 = M3 = M4 = matrix(0, ncol = n_r, nrow = n_r)
    for (i in 1:n_r) {
        for (j in 1:3) {
            if (!is.na(mesh$graph$tt[i, j])) {
                v2 <- list(TriList$E0, TriList$E1, TriList$E2)[[j]][i, 
                  ]
                M1[mesh$graph$tt[i, j], i] = M1[mesh$graph$tt[i, 
                  j], i] + posflux(c(1, 0), v2)/TriList$Tri_Area[i]
                M2[mesh$graph$tt[i, j], i] = M2[mesh$graph$tt[i, 
                  j], i] + posflux(c(0, 1), v2)/TriList$Tri_Area[i]
                M3[mesh$graph$tt[i, j], i] = M3[mesh$graph$tt[i, 
                  j], i] + posflux(c(-1, 0), v2)/TriList$Tri_Area[i]
                M4[mesh$graph$tt[i, j], i] = M4[mesh$graph$tt[i, 
                  j], i] + posflux(c(0, -1), v2)/TriList$Tri_Area[i]
            }
        }
        M1[i, i] = -1 * sum(M1[, i])
        M2[i, i] = -1 * sum(M2[, i])
        M3[i, i] = -1 * sum(M3[, i])
        M4[i, i] = -1 * sum(M4[, i])
    }
    maxout = max(abs(diag(M1 + M2 + M3 + M4)))
    Return = list(maxout = maxout, M1 = M1/maxout, M2 = M2/maxout, 
        M3 = M3/maxout, M4 = M4/maxout)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
