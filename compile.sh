R CMD SHLIB -o flake_interface.so data_parameters.f90 flake_configure.f90 flake_parameters.f90 flake_paramoptic_ref.f90 flake_albedo_ref.f90 flake_derivedtypes.f90 flake.f90 SfcFlx.f90 src_flake_interface_1D.f90 

R CMD SHLIB -o flake_interface_10band.so data_parameters.f90 flake_configure.f90 flake_parameters.f90 flake_paramoptic_ref.f90 flake_albedo_ref.f90 flake_derivedtypes.f90 flake.f90 SfcFlx.f90 src_flake_interface_1D_10band.f90 
