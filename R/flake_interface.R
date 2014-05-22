## modified from flake_interface_ver2.1.R

## dyn.load('lib/flake_interface.so')

#######################
## time series input ##
#######################
# dMsnowdt_in  The rate of snow accumulation [kg m^{-2} s^{-1}]
# I_atm_in     Solar radiation flux at the surface [W m^{-2}]
# Q_atm_lw_in  Long-wave radiation flux from the atmosphere [W m^{-2}]
# height_u_in  Height above the lake surface where the wind speed is
#                measured [m]
# height_tq_in Height where temperature and humidity are measured [m]
# U_a_in       Wind speed at z=height_u_in [m s^{-1}]
# T_a_in       Air temperature at z=height_tq_in [K]
# q_a_in       Air specific humidity at z=height_tq_in
# P_a_in       Surface air pressure [N m^{-2} = kg m^{-1} s^{-2}]

##############################
## lake specific parameters ##
##############################
# depth_w      The lake depth [m]
# fetch        Typical wind fetch [m]
# depth_bs     Depth of the thermally active layer of the bottom sediments [m]
# T_bs         Temperature at the outer edge of 
#                the thermally active layer of the bottom sediments [K]
# par_Coriolis The Coriolis parameter [s^{-1}]
# del_time     The model time step [s]

##############################
## previous state variables ##
##############################
# T_snow_in   Temperature at the air-snow interface [K] 
# T_ice_in    Temperature at the snow-ice or air-ice interface [K]
# T_mnw_in    Mean temperature of the water column [K]
# T_wML_in    Mixed-layer temperature [K]
# T_bot_in    Temperature at the water-bottom sediment interface [K]
# T_B1_in     Temperature at the bottom of the upper layer of the sediments [K]
# C_T_in      Shape factor (thermocline)
# h_snow_in   Snow thickness [m]
# h_ice_in    Ice thickness [m]
# h_ML_in     Thickness of the mixed-layer [m]
# H_B1_in     Thickness of the upper layer of bottom sediments [m]
# T_sfc_p     Surface temperature at the previous time step [K]  

##############################################
## not sure if this is used - need to check ## -- wrap in parameters
##############################################
# albedo_water Water surface albedo with respect to the solar radiation
# albedo_ice   Ice surface albedo with respect to the solar radiation
# albedo_snow  Snow surface albedo with respect to the solar radiation

########################
## optical parameters ##
########################
# opticpar_nband_water[1]        Number of wave-length bands for water       
# opticpar_frac_water[1:10]      Fractions of total radiation flux for water 
# opticpar_extincoef_water[1:10] Extinction coefficients for water           
# opticpar_nband_ice[1]          Number of wave-length bands for ice         
# opticpar_frac_ice[1:10]        Fractions of total radiation flux for ice   
# opticpar_extincoef_ice[1:10]   Extinction coefficients for ice             
# opticpar_nband_snow[1]         Number of wave-length bands for snow        
# opticpar_frac_snow[1:10]       Fractions of total radiation flux for snow  
# opticpar_extincoef_snow[1:10]  Extinction coefficients for snow            

##########################
## next state variables ##
##########################
# T_snow_out  Temperature at the air-snow interface [K] 
# T_ice_out   Temperature at the snow-ice or air-ice interface [K]
# T_mnw_out   Mean temperature of the water column [K]
# T_wML_out   Mixed-layer temperature [K]
# T_bot_out   Temperature at the water-bottom sediment interface [K]
# T_B1_out    Temperature at the bottom of the upper layer of the sediments [K]
# C_T_out     Shape factor (thermocline)
# h_snow_out  Snow thickness [m]
# h_ice_out   Ice thickness [m]
# h_ML_out    Thickness of the mixed-layer [m]
# H_B1_out    Thickness of the upper layer of bottom sediments [m]
# T_sfc_n     Updated surface temperature [K]
###########################
## extra state variables ##
###########################
## Momentum flux [N m^{-2}]               
## Sensible heat flux [W m^{-2}]          
## Latent heat flux [W m^{-2}]            
## Flux of water vapour [kg m^{-2} s^{-1}]
## Heat flux through the air-snow interface [W m^{-2}]                     
## Heat flux through the snow-ice or air-ice interface [W m^{-2}]          
## Heat flux through the ice-water or air-water interface [W m^{-2}] (net)      
## Heat flux through the water-bottom sediment interface [W m^{-2}]        
## Radiation flux at the lower boundary of the atmosphere [W m^{-2}],      
##   i.e. the incident radiation flux with no regard for the surface albedo  
## Radiation flux through the air-snow interface [W m^{-2}]                
## Radiation flux through the snow-ice or air-ice interface [W m^{-2}]     
## Radiation flux through the ice-water or air-water interface [W m^{-2}]  
## Radiation flux through the mixed-layer-thermocline interface [W m^{-2}] 
## Radiation flux through the water-bottom sediment interface [W m^{-2}]   
## Heat flux through the ice-water or air-water interface [W m^{-2}] (from water)      
## used albedo values for snow, ice, and water (only used when they are surface)






runflake <- function(ts.input, parameters, init.state,
                     optic, simpleoptic = TRUE) {
  ts.input.names <- c('dMsnowdt_in', 'I_atm_in', 'Q_atm_lw_in', 'height_u_in',
                      'height_tq_in', 'U_a_in', 'T_a_in', 'q_a_in', 'P_a_in')
  ## ts.input is expected to be:
  ## 1) a data.frame with these names
  ## 2) no missing values
  ## 3) nrow > 2
  ## 1)
  if (!is.data.frame(ts.input)) {
    stop(cat(c('ts.input must be a data.frame with names:', ts.input.names)))
  } else if (!all(ts.input.names %in% names(ts.input))) {
    stop(cat(c('ts.input must be a data.frame with names:', ts.input.names)))
  }
  ## 2)
  flags <- unlist(lapply(ts.input[ts.input.names],
                         function(x) return(all(is.finite(x)))
                         )
                  )
  if (!all(flags)) {
    stop(cat('missing values found in the following variables in ts.input:',
               ts.input.names[!flags]
               )
         )
  }
  flags1 <- unlist(lapply(ts.input[ts.input.names], is.numeric))
  if (!all(flags1)) {
    stop(cat('missing values found in the following variables in ts.input:',
               ts.input.names[!flags1]
               )
         )
  }
  ## 3)
  if (nrow(ts.input) == 1) stop('supply at least more than 1 time step')


  ## parameters must be a named vector with the following names
  parameter.names <- c('depth_w', 'fetch', 'depth_bs', 'T_bs', 'par_Coriolis',
                       'del_time',
                       'albedo_water', 'albedo_ice', 'albedo_snow')
  if (!is.vector(parameters, mode = 'numeric')) {
    stop(cat(c('parameters must be a numeric vector with names:',
                 parameter.names)))
  }
  flags2 <- names(parameters) %in% parameter.names
  if (!all(flags2)) {
    stop(cat(c('parameters must be a numeric vector with names:',
                 parameter.names)))
  }

  if (simpleoptic) {
    ## if simpleoptic is TRUE, optic must be a named vector with the names:
    optic.names <- c(paste('extincoef_', c('water', 'ice', 'snow'), sep = ''))
    if (!is.vector(optic, mode = 'numeric')) {
      stop(cat(c('if simpleoptic is TRUE, optic must be a numeric vector',
                   'with names:', optic.names)))
    }
    flags3 <- names(optic) %in% optic.names
    if (!all(flags3)) {
      stop(cat(c('if simpleoptic is TRUE, optic must be a numeric vector',
                   'with names:', optic.names)))
    }
    opticpar <- list()
    opticpar[['opticpar_nband_water']] <- c(1)
    opticpar[['opticpar_nband_ice']]   <- c(1)
    opticpar[['opticpar_nband_snow']]  <- c(1)
    opticpar[['opticpar_frac_water']]  <- c(1, rep(0, times = 9))
    opticpar[['opticpar_frac_ice']]    <- c(1, rep(0, times = 9))
    opticpar[['opticpar_frac_snow']]   <- c(1, rep(0, times = 9))
    opticpar[['opticpar_extincoef_water']] <- c(optic[['extincoef_water']],
                                                rep(1e10, times = 9))
    opticpar[['opticpar_extincoef_ice']] <- c(optic[['extincoef_ice']],
                                                rep(1e10, times = 9))
    opticpar[['opticpar_extincoef_snow']] <- c(optic[['extincoef_snow']],
                                                rep(1e10, times = 9))
  } else {
    ## stop('simpleoptic = FALSE is not implemented yet')
    ## if simpleoptic is FALSE, optic must be a named list with the names:
    optic.names <- paste('opticpar',
                         c('nband', 'frac', 'extincoef'),
                         rep(c('water', 'ice', 'snow'), each = 3),
                         sep = '_'
                         )
    if (!is.list(optic)) {
      stop(cat(c('if simpleoptic is FALSE, optic must be a list',
                   'with names:', optic.names)))
    }
    flags4 <- names(optic) %in% optic.names
    if (!all(flags4)) {
      stop(cat(c('if simpleoptic is FALSE, optic must be a list',
                   'with names:', optic.names)))
    }
    flags5 <- unlist(lapply(optic[optic.names], is.vector, mode = 'numeric'))
    if (!all(flags5)) {
      stop(cat(c('if simpleoptic is FALSE, optic must be list, with each',
                   'element being a numeric vector of lengths:',
                   as.character(optic.length),
                   'but there were problems with:',
                   optic.names[!flags5]
                   )
                 )
           )
    }
    givenlengths <- unlist(lapply(optic[optic.names], length))
    optic.lengths <- rep(c(1, 10, 10), times = 3) 
    flags6 <- givenlengths == optic.lengths
    if (!all(flags6)) {
      stop(cat(c('if simpleoptic is FALSE, optic must be list, with each',
                   'element being a numeric vector of lengths:',
                   as.character(optic.length),
                   'but there were problems with:',
                   optic.names[!flags6]
                   )
                 )
           )
    }
    opticpar <- optic
    extw <- opticpar[['opticpar_extincoef_water']]
    frcw <- opticpar[['opticpar_frac_water']]
    extwtypical <- sum(extw * frcw) / sum(frcw)
    cat('WARNING: typical extinction coefficient needs to be reviewed\n')
  }

  ## init.state must be a named vector with the following names
  init.state.names <- c('T_snow', 'T_ice', 'T_mnw', 'T_wML', 'T_bot', 'T_B1',
                        'C_T', 'h_snow', 'h_ice', 'h_ML', 'H_B1', 'T_sfc')
  if (!(is.vector(init.state, mode = 'numeric'))) {
    stop(paste(c('init.state must be a numeric vector with names:',
                 init.state.names)))
  }
  flags7 <- names(init.state) %in% init.state.names
  if (!all(flags7)) {
    stop(paste(c('init.state must be a numeric vector with names:',
                 init.state.names)))
  }

  ## all numbers now should be well behaving

  n <- nrow(ts.input)
  
  depth_w      <- parameters[['depth_w']]     
  fetch        <- parameters[['fetch']]            
  depth_bs     <- parameters[['depth_bs']]         
  T_bs         <- parameters[['T_bs']]             
  par_Coriolis <- parameters[['par_Coriolis']]     
  del_time     <- parameters[['del_time']]

  albedo_water <- parameters[['albedo_water']]
  albedo_ice   <- parameters[['albedo_ice']]
  albedo_snow  <- parameters[['albedo_snow']]
  
  opticpar_nband_water     <- opticpar[['opticpar_nbound_water']]   
  opticpar_frac_water      <- opticpar[['opticpar_frac_water']]     
  opticpar_extincoef_water <- opticpar[['opticpar_extincoef_water']]
  opticpar_nband_ice       <- opticpar[['opticpar_nbound_ice']]     
  opticpar_frac_ice        <- opticpar[['opticpar_frac_ice']]       
  opticpar_extincoef_ice   <- opticpar[['opticpar_extincoef_ice']]  
  opticpar_nband_snow      <- opticpar[['opticpar_nbound_snow']]    
  opticpar_frac_snow       <- opticpar[['opticpar_frac_snow']]      
  opticpar_extincoef_snow  <- opticpar[['opticpar_extincoef_snow']]

  opticpar_extincoef_water_single <- opticpar[['opticpar_extincoef_water']][1]
  opticpar_extincoef_ice_single <- opticpar[['opticpar_extincoef_ice']][1]
  opticpar_extincoef_snow_single <- opticpar[['opticpar_extincoef_snow']][1]

  state <- matrix(NA, nrow = n, ncol = 30)
  ts.input1 <- ts.input[['dMsnowdt_in']]
  ts.input2 <- ts.input[['I_atm_in']]
  ts.input3 <- ts.input[['Q_atm_lw_in']]
  ts.input4 <- ts.input[['height_u_in']]
  ts.input5 <- ts.input[['height_tq_in']]
  ts.input6 <- ts.input[['U_a_in']]
  ts.input7 <- ts.input[['T_a_in']]
  ts.input8 <- ts.input[['q_a_in']]
  ts.input9 <- ts.input[['P_a_in']]
  
  ## n == 1
  if (simpleoptic) {
    temp <-
      .Fortran('flake_interface_flaker',
               ts.input1[1], ts.input2[1], ts.input3[1],
               ts.input4[1], ts.input5[1], ts.input6[1],
               ts.input7[1], ts.input8[1], ts.input9[1],
               depth_w, fetch, depth_bs, T_bs, par_Coriolis, del_time, # 15
               init.state[['T_snow']], init.state[['T_ice']],
               init.state[['T_mnw']], init.state[['T_wML']],
               init.state[['T_bot']], init.state[['T_B1']],
               init.state[['C_T']], init.state[['h_snow']],
               init.state[['h_ice']], init.state[['h_ML']], # 25
               init.state[['H_B1']], init.state[['T_sfc']],
               albedo_water, albedo_ice, albedo_snow,  # 30
               opticpar_extincoef_water_single,
               opticpar_extincoef_ice_single,
               opticpar_extincoef_snow_single, # 39 (+ 54) - 6
               as.double(numeric(30))
               )
    state[1, ] <- temp[[length(temp)]]
  } else {
    temp <-
      .Fortran('flake_interface_flaker_10band',
               ts.input1[1], ts.input2[1], ts.input3[1],
               ts.input4[1], ts.input5[1], ts.input6[1],
               ts.input7[1], ts.input8[1], ts.input9[1],
               depth_w, fetch, depth_bs, T_bs, par_Coriolis, del_time, # 15
               init.state[['T_snow']], init.state[['T_ice']],
               init.state[['T_mnw']], init.state[['T_wML']],
               init.state[['T_bot']], init.state[['T_B1']],
               init.state[['C_T']], init.state[['h_snow']],
               init.state[['h_ice']], init.state[['h_ML']], # 25
               init.state[['H_B1']], init.state[['T_sfc']],
               albedo_water, albedo_ice, albedo_snow,  # 30
               extw[1], extw[2], extw[3], extw[4], extw[5],
               extw[6], extw[7], extw[8], extw[9], extw[10],
               extwtypical,
               frcw[1], frcw[2], frcw[3], frcw[4], frcw[5],
               frcw[6], frcw[7], frcw[8], frcw[9], frcw[10],
               opticpar_extincoef_ice_single,
               opticpar_extincoef_snow_single, # 39 (+ 54) - 6 + 20 
               as.double(numeric(30))
               )
    state[1, ] <- temp[[length(temp)]]
  }
  ## for i in 2:n
  for (i in 2:n) {
    if (i %% 10000 == 0) {
      cat(paste('... iteration', i, ':', round(i / n * 100), 'percent done\n'))
    }
    if (simpleoptic) {
      temp <- 
        .Fortran('flake_interface_flaker',
                 ts.input1[i], ts.input2[i], ts.input3[i],
                 ts.input4[i], ts.input5[i], ts.input6[i],
                 ts.input7[i], ts.input8[i], ts.input9[i],
                 depth_w, fetch, depth_bs, T_bs, par_Coriolis, del_time, # 15
                 state[i - 1, 1], state[i - 1, 2], state[i - 1, 3], 
                 state[i - 1, 4], state[i - 1, 5], state[i - 1, 6], 
                 state[i - 1, 7], state[i - 1, 8], state[i - 1, 9], 
                 state[i - 1, 10], state[i - 1, 11], state[i - 1, 12],
                 albedo_water, albedo_ice, albedo_snow,  # 30
                 opticpar_extincoef_water_single,
                 opticpar_extincoef_ice_single,
                 opticpar_extincoef_snow_single, # 39 (+ 54) - 6
                 as.double(numeric(30))
                 )
      state[i, ] <- temp[[length(temp)]]
    } else {
      temp <- 
        .Fortran('flake_interface_flaker_10band',
                 ts.input1[i], ts.input2[i], ts.input3[i],
                 ts.input4[i], ts.input5[i], ts.input6[i],
                 ts.input7[i], ts.input8[i], ts.input9[i],
                 depth_w, fetch, depth_bs, T_bs, par_Coriolis, del_time, # 15
                 state[i - 1, 1], state[i - 1, 2], state[i - 1, 3], 
                 state[i - 1, 4], state[i - 1, 5], state[i - 1, 6], 
                 state[i - 1, 7], state[i - 1, 8], state[i - 1, 9], 
                 state[i - 1, 10], state[i - 1, 11], state[i - 1, 12],
                 albedo_water, albedo_ice, albedo_snow,  # 30
                 extw[1], extw[2], extw[3], extw[4], extw[5],
                 extw[6], extw[7], extw[8], extw[9], extw[10],
                 extwtypical,
                 frcw[1], frcw[2], frcw[3], frcw[4], frcw[5],
                 frcw[6], frcw[7], frcw[8], frcw[9], frcw[10],
                 opticpar_extincoef_ice_single,
                 opticpar_extincoef_snow_single, # 39 (+ 54) - 6 + 20 
                 as.double(numeric(30))
                 )
      state[i, ] <- temp[[length(temp)]]
    }
  }
  st <- as.data.frame(state)
  names(st) <- c('T_snow', 'T_ice', 'T_mnw', 'T_wML', 'T_bot', 'T_B1', 'C_T',
                 'h_snow', 'h_ice', 'h_ML', 'H_B1', 'T_sfc',
                 'Q_momentum_out', 'Q_sensible_out', 'Q_latent_out', 
                 'Q_watvap_out', 'Q_snow_flk_out', 'Q_ice_flk_out', 
                 'Q_w_flk_out', 'Q_bot_flk_out', 
                 'I_atm_flk_out', 'I_snow_flk_out', 'I_ice_flk_out', 
                 'I_w_flk_out', 'I_h_flk_out', 'I_bot_flk_out', 'Q_lw_out',
                 'albedo_snow', 'albedo_ice', 'albedo_water'
                 )
  st[['I_refl_snow']] <- ifelse(st[['h_snow']] > 0,
                                st[['I_atm_flk_out']] * st[['albedo_snow']],
                                0)
  st[['I_refl_ice']] <- ifelse((!(st[['h_snow']] > 0)) & (st[['h_ice']] > 0),
                              st[['I_atm_flk_out']] * st[['albedo_ice']],
                              0)
  st[['I_refl_w']] <- ifelse((!(st[['h_snow']] > 0)) & (!(st[['h_ice']] > 0)),
                            st[['I_atm_flk_out']] * st[['albedo_water']],
                            0)
  st[['albedo_snow']] <- pmax(st[['albedo_snow']], 0)
  st[['albedo_ice']] <- pmax(st[['albedo_ice']], 0)
  
  return(st)
}

  
  



  
