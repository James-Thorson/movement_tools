\name{Breaks_Fn}
\alias{Breaks_Fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Breaks_Fn(Obj, breaks = c(-Inf, 10^(-3:0)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Obj}{
%%     ~~Describe \code{Obj} here~~
}
  \item{breaks}{
%%     ~~Describe \code{breaks} here~~
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
function (Obj, breaks = c(-Inf, 10^(-3:0))) 
{
    Cut <- cut(Obj, breaks)
    if (is.array(Obj)) 
        Return <- array(as.numeric(Cut), dim = dim(Obj))
    if (is.vector(Obj)) 
        Return <- as.numeric(Cut)
    return(Return)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
