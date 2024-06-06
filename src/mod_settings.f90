! /***************************************************************************
!  cropcoeff
!  A executable/library to use to produce inputs for the IdrAgra model
! -------------------
!         begin                : 2022-01-01
!         copyright            : (C) 2022 by Enrico A. Chiaradia
!         email                    : enrico.chiaradia@unimi.it
!  ***************************************************************************/

! /***************************************************************************
!  *                                                                           *
!  *   This program is free software; you can redistribute it and/or modify  *
!  *   it under the terms of the GNU General Public License as published by  *
!  *   the Free Software Foundation; either version 2 of the License, or       *
!  *   (at your option) any later version.                                   *
!  *                                                                           *
!  ***************************************************************************/
!
!  /***************************************************************************
! credits:
! Part of this work derives from unsigned codes. If have rights about some parts,
! please contact me at enrico.chiaradia@unimi.it
!   ***************************************************************************/

module mod_settings
    Use mod_utilities
    
    implicit none
    
    type Settings
        character(len=maxlength) :: meteo_path                 ! path to meteo timeseries tables
        character(len=maxlength) :: ws_filename                 ! filename of wheater stations list
        character(len=maxlength) :: CO2_filename            ! filename of CO2 concentration by year
        character(len=maxlength) :: soiluses_folder            ! path to the soiluses files
        character(len=maxlength) :: soiluses_filename        ! filename of soil uses list
        character(len=maxlength) :: crop_folder                ! path to the crop parameters files
        character(len=maxlength) :: pheno_outpath             ! path where output phenophases files are stored
        character(len=maxlength) :: pheno_root                 ! a common string to use as prefix in output folder
        character(len=maxlength) :: canres_filename            ! filename for the canopy resistance filename
        
        Integer :: CanopyResMod                                            ! value of canopy resistance printed in output [0 = def value (70 s/m), 1 = calculated as function of CO2]
        
        LOGICAL :: checkFutureTemp                                        ! check if following time period has enough degree to permit crop growing
        REAL(dp) :: tollerance                                                ! compare sum of GDD with maximum crop GDD * tollerance
        REAL(dp) :: vfactor                                                    ! minimum vernalization fraction to activate vernalization
        logical :: debug                                                        ! activate/deactivate debug mode (e.g. print all outputs)
        integer :: window                                                    ! the number of elements to replicate as tails
        integer :: movMeanNum                                            ! the number of elements to consider after and before target element to calculate movable mean
    end type
    
    contains
        
        subroutine makeDefault(aSimSettings)
            type(Settings), intent(inout) :: aSimSettings
            
            aSimSettings%meteo_path = 'meteodata'
            aSimSettings%ws_filename = 'weather_stations.dat'
            aSimSettings%CO2_filename = 'CO2_conc.dat'
            aSimSettings%soiluses_folder = 'landuses'
            aSimSettings%soiluses_filename = 'soil_uses.txt'
            aSimSettings%crop_folder = 'landuses/crop_parameters'
            aSimSettings%pheno_outpath = 'pheno'
            aSimSettings%pheno_root = 'pheno_'
            aSimSettings%canres_filename = 'CanopyRes.dat'
            aSimSettings%CanopyResMod = 1
            aSimSettings%checkFutureTemp = .true.
            aSimSettings%tollerance = 0.9
            aSimSettings%vfactor = 0.2
            aSimSettings%debug = .false.
            aSimSettings%window = 366
            aSimSettings%movMeanNum = 2
            
        end subroutine
    
end module