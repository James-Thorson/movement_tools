\name{GridOperator_Fn}
\alias{GridOperator_Fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate movement operators on a grid
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GridOperator_Fn(grid_dataframe)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid_dataframe}{
%%     ~~Describe \code{grid_dataframe} here~~
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
function (grid_dataframe) 
{
    ngrid <- nrow(grid_dataframe)
    h <- 1/sqrt(ngrid)
    nablaxx <- Laplacian_GridOperator("x", grid_dataframe, h = h)
    nablayy <- Laplacian_GridOperator("y", grid_dataframe, h = h)
    gradx <- Diff_GridOperator("x", grid_dataframe, h = h)
    grady <- Diff_GridOperator("y", grid_dataframe, h = h)
    nablaxy <- gradx \%*\% grady
    gradx_lwr = gradx_upr = gradx
    gradx_lwr[upper.tri(gradx, diag = TRUE)] = 0
    gradx_upr[lower.tri(gradx, diag = TRUE)] = 0
    diag(gradx_upr) = -colSums(gradx_upr)
    diag(gradx_lwr) = -colSums(gradx_lwr)
    grady_lwr = grady_upr = grady
    grady_lwr[upper.tri(grady, diag = TRUE)] = 0
    grady_upr[lower.tri(grady, diag = TRUE)] = 0
    diag(grady_upr) = -colSums(grady_upr)
    diag(grady_lwr) = -colSums(grady_lwr)
    Return = list(nablaxx = nablaxx, nablayy = nablayy, nablaxy = nablaxy, 
        gradx = gradx, grady = grady, gradx_lwr = gradx_lwr, 
        gradx_upr = gradx_upr, grady_lwr = grady_lwr, grady_upr = grady_upr)
    return(Return)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
