\name{runflake}
\alias{runflake}
\title{Run the FLake model}
\description{Run the 1-d FLake given a time series and parameters,
  returns the state variable time series.}
\usage{
runflake(ts.input, parameters, init.state,
         optic, simpleoptic = TRUE)
}
\arguments{
  \item{ts.input}{
    a named \code{data.frame} with the following names:
    \code{dMsnowdt_in}, \code{I_atm_in}, \code{Q_atm_lw_in},
    \code{height_u_in}, 
    \code{height_tq_in}, \code{U_a_in}, \code{T_a_in},
    \code{q_a_in}, \code{P_a_in}
  }
  \item{parameters}{
    a named \code{vector} or \code{data.frame} with the following names:
    \code{depth_w}, \code{fetch}, \code{depth_bs}, \code{T_bs},
    \code{par_Coriolis}, \code{del_time}, 
    \code{albedo_water}, \code{albedo_ice}, \code{albedo_snow}
  }
  \item{init.state}{
    a named \code{vector} with the following names:
    \code{T_snow}, \code{T_ice}, \code{T_mnw}, \code{T_wML},
    \code{T_bot}, \code{T_B1}, \code{C_T}, \code{h_snow},
    \code{h_ice}, \code{h_ML}, \code{H_B1}, \code{T_srf}
  }
  \item{simpleoptic}{
    see \code{optic}
  }
  \item{optic}{
    if \code{simpleoptic} is \code{TRUE}, a vector with names
    (\code{extincoef_water},
    \code{extincoef_ice},
    \code{extincoef_snow}), otherwise a list with names
    (\code{opticpar_nband_water},
    \code{opticpar_nband_ice},
    \code{opticpar_nband_snow},
    \code{opticpar_frac_water},
    \code{opticpar_frac_ice},
    \code{opticpar_frac_snow},
    \code{opticpar_extincoef_water},
    \code{opticpar_extincoef_ice},
    \code{opticpar_extincoef_snow}) with
    \code{*_nband_*} being an integer,
    \code{*_frac_*} being a numeric vector of length 10,
    \code{*_extincoef_*} being a numeric vector of length 10
  }
}
\value{a \code{data.frame} with variables 
  (\code{T_snow}, \code{T_ice}, \code{T_mnw}, \code{T_wML},
  \code{T_bot}, \code{T_B1}, \code{C_T}, \code{h_snow},
  \code{h_ice}, \code{h_ML}, \code{H_B1}, \code{T_srf}) with
  the same \code{nrow} as \code{ts.input}
}
\details{
  \tabular{llll}{
    Time series input \tab \tab \tab \cr 
    \tab \code{dMsnowdt_in}
    \tab The rate of snow accumulation
    \tab [kg m^{-2} s^{-1}] \cr
    \tab \code{I_atm_in}
    \tab Solar radiation flux at the surface
    \tab [W m^{-2}] \cr
    \tab \code{Q_atm_lw_in}
    \tab Long-wave radiation flux from the atmosphere
    \tab [W m^{-2}] \cr
    \tab \code{height_u_in}
    \tab Height above the lake surface where the wind speeed measured
    \tab [m] \cr
    \tab \code{height_tq_in}
    \tab Height where temperature and humidity are measured
    \tab [m] \cr
    \tab \code{U_a_in}
    \tab Wind speed at z = \code{height_u_in}
    \tab [m s^{-1}] \cr
    \tab \code{T_a_in}
    \tab Air temperature at z = \code{height_tq_in}
    \tab [K] \cr
    \tab \code{q_a_in}
    \tab Air specific humidity at z = \code{height_tq_in}
    \tab \cr
    \tab \code{P_a_in}
    \tab Surface air pressure
    \tab [N m^{-2} = kg m^{-1} s^{-2}] \cr
    Lake specific parameters \tab \tab \tab \cr 
    \tab \code{depth_w}
    \tab The lake depth
    \tab [m] \cr
    \tab \code{fetch}
    \tab Typical wind fetch
    \tab [m] \cr
    \tab \code{depth_bs}
    \tab Depth of the thermally active layer of the bottom sediments
    \tab [m] \cr
    \tab \code{T_bs}
    \tab Temperature at the outer edge of the thermally active layer of the
    bottom sediments
    \tab [K] \cr
    \tab \code{par_Coriolis}
    \tab The Coriolis parameter
    \tab [s^{-1}] \cr
    \tab \code{del_time}
    \tab The model time step
    \tab [s] \cr
  }
  State variables 
  \itemize{
    \item T_snow   Temperature at the air-snow interface [K] 
    \item T_ice    Temperature at the snow-ice or air-ice interface [K]
    \item T_mnw    Mean temperature of the water column [K]
    \item T_wML    Mixed-layer temperature [K]
    \item T_bot    Temperature at the water-bottom sediment interface [K]
    \item T_B1     Temperature at the bottom of the upper layer of the sediments [K]
    \item C_T      Shape factor (thermocline)
    \item h_snow   Snow thickness [m]
    \item h_ice    Ice thickness [m]
    \item h_ML     Thickness of the mixed-layer [m]
    \item H_B1     Thickness of the upper layer of bottom sediments [m]
    \item T_sfc    Surface temperature at the previous time step [K]  
  }
  Not sure if this is used - need to check -- wrap in parameters
  \itemize{
    \item albedo_water Water surface albedo with respect to the solar radiation
    \item albedo_ice   Ice surface albedo with respect to the solar radiation
    \item albedo_snow  Snow surface albedo with respect to the solar radiation
  }
  Optical parameters
  \itemize{
    \item opticpar_nband_water[1]        Number of wave-length bands for water       
    \item opticpar_frac_water[1:10]      Fractions of total radiation flux for water 
    \item opticpar_extincoef_water[1:10] Extinction coefficients for water           
    \item opticpar_nband_ice[1]          Number of wave-length bands for ice         
    \item opticpar_frac_ice[1:10]        Fractions of total radiation flux for ice   
    \item opticpar_extincoef_ice[1:10]   Extinction coefficients for ice             
    \item opticpar_nband_snow[1]         Number of wave-length bands for snow        
    \item opticpar_frac_snow[1:10]       Fractions of total radiation flux for snow  
    \item opticpar_extincoef_snow[1:10]  Extinction coefficients for snow            
  }
}



    