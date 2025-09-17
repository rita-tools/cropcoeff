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

module mod_crop
    use mod_utilities
    implicit none
    
    type Crop
        character(LEN=maxlength) :: cropName ! Crop denomination
        character(LEN=maxlength) :: fileName ! Name of the data-base file that contains crop characteristics
        integer :: cropId = -1 ! unique id identification
        integer :: SowingDate_min ! minimum sowing date (1-366)
        integer :: SowingDelay_max ! maximum number of days allowed for sowing after SowingDate_min (1-366)
        integer :: HarvestDate_max ! maximum harvest date (1-366)
        integer :: HarvNum_max ! maximum number of harvest/cuts per the year
        integer :: CropsOverlap ! minimum number of days between two subsequent crops in case of double cropping
        
        real(dp) :: Tsowing  ! minimum sowing temperature [�C]
        real(dp) :: Tdaybase  ! minimum temperature for crop growth [�C]
        real(dp) :: Tcutoff  ! maximum temperature for crop growth [�C]
        
        logical :: Vern !  response to vernalization [1=Yes, 0=No]
        
        real(dp) :: Tv_min  ! minimum temperature for optimal vernalization [�C]
        real(dp) :: Tv_max  ! maximum temperature for optimal vernalization [�C]
        real(dp) :: VFmin  ! vernalization factor at the beginning of the vernalization process [-]
        
        integer :: Vstart ! number of days required for vernalization to start
        integer :: Vend ! number of days required for vernalization to end
        integer :: Vslope ! vernalization curve parameter
        
        integer :: ph_r ! photoperiod impact [0=Day-neutral plants, 1=Long-day plants, 2=Short-day plants]
        real(dp) :: daylength_if ! day length threshold below (above) which no accumulation of physiological time occurs for long-day (short-day) crops
        real(dp) :: daylength_ins ! day length threshold above (below) which maximum accumulation of physiological time occurs for long-day (short-day) crops
        real(dp) :: WP ! biomass water productivity [t/ha] (C4 crops = 0.30 � 0.35, C3 crops = 0.15 � 0.20, some leguminous crops < 0.15 t/ha)
        real(dp) :: fsink ! crop sink strength coefficient
        real(dp) :: Tcrit_HS ! critical temperature threshold for heat stress [�C] 
        real(dp) :: Tlim_HS ! limit temperature threshold for heat stress[�C] 
        real(dp) :: HI ! harvest index
        
        real(dp) :: kyT ! water stress coefficient for the overall crop growth cycle 
        real(dp) :: ky1 ! water stress coefficient for the ini stage 
        real(dp) :: ky2 ! water stress coefficient for the dev stage 
        real(dp) :: ky3 ! water stress coefficient for the mid stage
        real(dp) :: ky4 ! water stress coefficient for the end stage
        real(dp) :: pRAW ! parameter to compute RAW
        real(dp) :: aInterception ! parameter to calculate interception
        
        integer :: cl_CN ! CN class
        logical :: Irrigation ! irrigation (1 = Yes, 0 = No)
        
        LOGICAL :: adj_flag = .true.! compute kcb correction
        
        real(dp) :: Ke = 0. ! fraction of root in the evaporative layer [obsolete]
        real(dp) :: Kt = 1. ! fraction of root in the transpirative layer [obsolete]
        real(dp) :: RFt = 1. ! fraction of root in the transpirative layer
        
         ! table of GDD [°C], Kcb [-], LAI [-], crop height Hc [m], root depth Sr [m], water stress coefficient Ky (-); hydr condition CNvalue (-), cover fraction fc (-), resistance to stress r_stress (-) .
         ! Missing values are entered as * 
        real(dp), dimension(:), pointer :: GDD, Kcb, LAI, Hc, Sr, Ky, CNvalue, fc, r_stress
    end type Crop
    
end module mod_crop
