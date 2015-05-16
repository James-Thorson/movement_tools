
Make_Movement_Mesh <- function( loc_orig, MeshType="Samples", ... ){
  # 1st mesh
    # for determining locations to track as vertices, and then discarded
  mesh_discard = inla.mesh.create( loc_orig, plot.delay=NULL, extend=list(n=8,offset=-0.15), refine=FALSE )  # loc_samp
  loc_discard = mesh_centers( mesh_discard )

  # 2nd mesh
    # for generating triangles to track movement                                                           #
  mesh_domain = inla.mesh.create( loc_discard, plot.delay=NULL, extend=list(n=8,offset=-0.15), refine=FALSE, ... )  # loc_samp
  loc_v = mesh_domain$loc[,1:2]
  loc_r = mesh_centers( mesh_domain )

  # Number of triangles (r) and vertices (v)
  n_v = nrow( loc_v )
  n_r = nrow( loc_r )

  # 3rd mesh
    # for SPDE approximation to RF based on center of triangles for movement
  if(MeshType=="Samples") mesh_gmrf = inla.mesh.create( loc_r, plot.delay=NULL, extend=list(n=8,offset=-0.15), refine=FALSE )  # loc_samp
  if(MeshType=="Refined") mesh_gmrf = inla.mesh.create( loc_r, plot.delay=NULL, extend=list(n=8,offset=-0.15), refine=list(min.angle=26) )  # loc_samp  ;  ,max.edge.data=0.08,max.edge.extra=0.2
  spde_gmrf = inla.spde2.matern(mesh_gmrf, alpha=2)
  n_g = mesh_gmrf$n

  # Return list
  Return = list("n_g"=n_g, "spde_gmrf"=spde_gmrf, "n_v"=n_v, "loc_v"=loc_v, "n_r"=n_r, "mesh_domain"=mesh_domain)
  return( Return )
}

