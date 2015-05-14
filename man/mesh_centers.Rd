\name{mesh_centers}
\alias{mesh_centers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate centers for a set of triangles
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mesh_centers(mesh)
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
    center_tri = matrix(NA, nrow = nrow(mesh$graph$tt), ncol = 2)
    for (i in 1:nrow(center_tri)) center_tri[i, ] = colMeans(mesh$loc[, 
        1:2][mesh$graph$tv[i, ], ])
    return(center_tri)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
