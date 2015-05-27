
#  MoveMat, SD_omega=1, SD_epsilon=1, SD_effort=1, effort_par=c(0.2,0.5), sizepar=c(1,0.5), Scale, Dynamical_Model, n_s, n_t, r_s, n_r, loc_r, alpha, beta, km2_r 
SpatProdSimulator_Fn = function( SettingsList ){
  # Attach settings
  attach( SettingsList )
  on.exit( detach(SettingsList) )
  
  # Load library
  require( RandomFields )

  # Simulate
  RF_omega = RMgauss(var=SD_omega^2, scale=Scale)
  RF_epsilon = RMgauss(var=SD_epsilon^2, scale=Scale)
  RF_effort = RMgauss(var=SD_effort^2, scale=Scale)

  # Simulate effort
  effortdens_t = rlnorm( n_t, meanlog=log(effort_par[1])-effort_par[2]^2/2, sdlog=effort_par[2] )
  effortdens_r = exp( RFsimulate(model=RF_effort, x=loc_r[,1], y=loc_r[,2])@data[,1] - SD_effort^2/2 )
  effortdens_rt = outer(effortdens_r, rep(1,n_t)) * outer(rep(1,n_r), effortdens_t)
  
  # Simulate density for each triangle
  # Gompertz: u(t+1) = u(t) * exp( alpha - beta*log(u(t)) )
  # Moran-Ricker: u(t+1) = u(t) * exp( alpha - beta*u(t) )
  catch_rt = upred_rt = u_rt = Epsilon_rt = matrix(NA, ncol=n_t, nrow=n_r)
  Omega_r = RFsimulate(model=RF_omega, x=loc_r[,1], y=loc_r[,2])@data[,1]
  for(t in 1:n_t){
    Epsilon_rt[,t] = RFsimulate(model=RF_epsilon, x=loc_r[,1], y=loc_r[,2])@data[,1]
    if(t==1){
      u_rt[,t] = km2_r * exp( logmeanu0 + Omega_r + Epsilon_rt[,t] )
      catch_rt[,t] = (1 - exp(-effortdens_rt[,t]) ) * u_rt[,t]
      u_rt[,t] = exp(-effortdens_rt[,t]) * u_rt[,t] 
    }
    if(t>=2){
      # Fishing effort
      catch_rt[,t] = (1 - exp(-effortdens_rt[,t]) ) * u_rt[,t-1]
      upred_rt[,t] = exp(-effortdens_rt[,t]) * u_rt[,t-1] 
      # Movement
      upred_rt[,t] = as.vector( MoveMat %*% upred_rt[,t] )
      # Production
      if( Dynamical_Model=="Gompertz" ) u_rt[,t] = upred_rt[,t] * exp(alpha + Omega_r - beta*log(upred_rt[,t]/km2_r) + Epsilon_rt[,t])
      if( Dynamical_Model=="Ricker" ) u_rt[,t] = upred_rt[,t] * exp(alpha + Omega_r - beta*(upred_rt[,t]/km2_r) + Epsilon_rt[,t])
    }
  }

  # Simulate samples for each site and year
  DF = expand.grid("s_i"=1:n_s, "t_i"=1:n_t)
  DF = cbind( DF, "r_i"=r_s[DF[,'s_i']], "km2_i"=1 )
  DF = cbind( DF, "cexp_i"=u_rt[ as.matrix(DF[,c('r_i','t_i')]) ] / km2_r[DF[,'r_i']] * DF[,'km2_i'] )
  DF = cbind( DF, "c_i"=rpois(n_s*n_t, lambda=DF[,'cexp_i']) )
  #DF = cbind( DF, "cpue_i"=NA)
  #for(i in 1:nrow(DF)) DF[i,'cpue_i'] = sum(rlnorm(DF[,'c_i'], meanlog=log(sizepar[1])-sizepar[2]^2/2, sdlog=sizepar[2]))
  #cpue_mean_i = sizepar[1] * DF[,'c_i']
  #cpue_CV_i = sizepar[2]/sizepar[1] * DF[,'c_i']^(-0.5)
  #DF = cbind( DF, "cpue_i"=rgamma(nrow(DF), shape=cpue_CV_i^(-2), scale=cpue_mean_i*cpue_CV_i^2) )      # ~ gamma( shape= CV^(-2), scale=mean*CV^2 )
  DF = cbind( DF, "zinflognorm_i"=ifelse(DF[,'c_i']>0,1,0) * rlnorm(nrow(DF), meanlog=log(DF[,'cexp_i']), sdlog=1) )
  DF = cbind( DF, "zinfgamma_i"=ifelse(DF[,'c_i']>0,1,0) * rgamma(nrow(DF), shape=1^(-2), scale=DF[,'cexp_i']*1^2) )

  # Total catches
  catch_t = colSums( catch_rt )

  # Return stuff
  Return = list("SettingsList"=SettingsList, "DF"=DF, "catch_t"=catch_t, "catch_rt"=catch_rt, "effortdens_rt"=effortdens_rt, "MoveMat"=MoveMat, "upred_rt"=upred_rt, "u_rt"=u_rt, "Epsilon_rt"=Epsilon_rt, "Omega_r"=Omega_r)
  return( Return )
}

