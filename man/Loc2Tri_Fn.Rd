\name{Loc2Tri_Fn}
\alias{Loc2Tri_Fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Assign locations to a triangulated domain
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Loc2Tri_Fn(locmat, TriList, RemoveMissing = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{locmat}{
%%     ~~Describe \code{locmat} here~~
}
  \item{TriList}{
%%     ~~Describe \code{TriList} here~~
}
  \item{RemoveMissing}{
%%     ~~Describe \code{RemoveMissing} here~~
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
function (locmat, TriList, RemoveMissing = FALSE) 
{
    r_loc = rep(NA, nrow(locmat))
    for (li in 1:nrow(locmat)) {
        Which = NULL
        for (ri in 1:nrow(TriList$E0)) {
            v1 = rbind(TriList$E0[ri, ], TriList$E1[ri, ], TriList$E2[ri, 
                ])
            v2 = outer(rep(1, 3), locmat[li, ]) - rbind(TriList$V2[ri, 
                ], TriList$V0[ri, ], TriList$V1[ri, ])
            crossprod_vec = sapply(1:3, FUN = function(i) {
                crossprod_fn(v1[i, ], v2[i, ])
            })
            if (all(crossprod_vec > 0)) 
                Which = c(Which, ri)
        }
        if (length(Which) >= 2) 
            stop("location in 2 triangles")
        if (length(Which) == 0) {
            if (RemoveMissing == FALSE) 
                stop("location in 2 triangles")
            if (RemoveMissing == TRUE) 
                Which = NA
        }
        r_loc[li] = Which
    }
    return(r_loc)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
