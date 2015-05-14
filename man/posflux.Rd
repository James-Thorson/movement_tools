\name{posflux}
\alias{posflux}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate flux outward from triangle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
posflux(v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v1}{
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{
%%     ~~Describe \code{v2} here~~
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
function (v1, v2) 
{
    rotation = function(rad) matrix(c(cos(rad), sin(rad), sin(-rad), 
        cos(rad)), nrow = 2, ncol = 2)
    rotate90 = round(rotation(pi/2), digits = 1e-10)
    ifelse(dotprod(rotate90 \%*\% v1, v2) > 0, 1, 0) * abs(crossprod_fn(v1, 
        v2))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
