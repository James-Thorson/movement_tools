\name{PlotImage_Fn}
\alias{PlotImage_Fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot density surface on a triangulated mesh
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotImage_Fn(Vec, breaks, Xset, Yset, DFexpand, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Vec}{
%%     ~~Describe \code{Vec} here~~
}
  \item{breaks}{
%%     ~~Describe \code{breaks} here~~
}
  \item{Xset}{
%%     ~~Describe \code{Xset} here~~
}
  \item{Yset}{
%%     ~~Describe \code{Yset} here~~
}
  \item{DFexpand}{
%%     ~~Describe \code{DFexpand} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (Vec, breaks, Xset, Yset, DFexpand, ...) 
{
    X = c(Xset[1] - mean(diff(Xset))/2, Xset + mean(diff(Xset))/2)
    Y = c(Yset[1] - mean(diff(Yset))/2, Yset + mean(diff(Yset))/2)
    Z = matrix(Breaks_Fn(Vec[DFexpand[, "r_loc"]], breaks = breaks), 
        ncol = length(Xset))
    image(z = Z, x = X, y = Y, breaks = 1:length(breaks), col = Col(length(breaks) - 
        1), ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
