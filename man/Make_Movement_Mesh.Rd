\name{Make_Movement_Mesh}
\alias{Make_Movement_Mesh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make a triangulated mesh for the domain of a population, to account for movement among triangles
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Make_Movement_Mesh(loc_orig, MeshType = "Samples", Cutoff = 1e-12, Refine = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{loc_orig}{
%%     ~~Describe \code{loc_orig} here~~
}
  \item{MeshType}{
%%     ~~Describe \code{MeshType} here~~
}
  \item{Cutoff}{
%%     ~~Describe \code{Cutoff} here~~
}
  \item{Refine}{
%%     ~~Describe \code{Refine} here~~
}
  \item{\dots}{Optional, additional arguments passed to 'inla.mesh.create' when creating movement mesh}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{n_r }{Number of triangles in movement mesh}
  \item{mesh_domain }{Description of movement mesh in INLA mesh format}  
  \item{loc_r }{Location of centroids of triangles in movement mesh}
  \item{mesh_gmrf }{SPDE INLA mesh for centroids of movement mesh}    
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
function (loc_orig, MeshType = "Samples", Cutoff = 1e-12, Refine = FALSE, 
    ...) 
{
    mesh_discard = inla.mesh.create(loc_orig, plot.delay = NULL, 
        extend = list(n = 8, offset = -0.15), refine = FALSE)
    loc_discard = mesh_centers(mesh_discard)
    mesh_domain = inla.mesh.create(loc_discard, plot.delay = NULL, 
        extend = list(n = 8, offset = -0.15), refine = Refine, 
        cutoff = Cutoff, ...)
    loc_v = mesh_domain$loc[, 1:2]
    loc_r = mesh_centers(mesh_domain)
    n_v = nrow(loc_v)
    n_r = nrow(loc_r)
    colnames(loc_v) = colnames(loc_r) = colnames(loc_orig)
    if (MeshType == "Samples") 
        mesh_gmrf = inla.mesh.create(loc_r, plot.delay = NULL, 
            extend = list(n = 8, offset = -0.15), refine = FALSE)
    if (MeshType == "Refined") 
        mesh_gmrf = inla.mesh.create(loc_r, plot.delay = NULL, 
            extend = list(n = 8, offset = -0.15), refine = list(min.angle = 26))
    spde_gmrf = inla.spde2.matern(mesh_gmrf, alpha = 2)
    n_g = mesh_gmrf$n
    Return = list(n_g = n_g, n_v = n_v, n_r = n_r, loc_v = loc_v, 
        loc_r = loc_r, mesh_domain = mesh_domain, spde_gmrf = spde_gmrf, 
        mesh_gmrf = mesh_gmrf)
    return(Return)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
