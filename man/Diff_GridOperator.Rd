\name{Diff_GridOperator}
\alias{Diff_GridOperator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Difference operator for a grid
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Diff_GridOperator(var = "x", grid_dataframe, h = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{grid_dataframe}{
%%     ~~Describe \code{grid_dataframe} here~~
}
  \item{h}{
%%     ~~Describe \code{h} here~~
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
function (var = "x", grid_dataframe, h = 1) 
{
    DeltaX = mean(diff(sort(unique(grid_dataframe[, "x"]))))
    DeltaY = mean(diff(sort(unique(grid_dataframe[, "y"]))))
    d <- approx_equals(as.matrix(dist(grid_dataframe)), c(x = DeltaX, 
        y = DeltaY)[var]) * 0.5 * outer(grid_dataframe[[var]], 
        grid_dataframe[[var]], "-")
    diag(d) <- -colSums(d)
    as(d/h, "dgCMatrix")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
