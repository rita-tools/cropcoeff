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

module mod_weather_station
    use mod_utilities
    USE mod_datetime
    
    implicit none
    
    type WeatherStation
        character(LEN=maxlength) :: wsName ! Station denomination
        character(LEN=maxlength) :: fileName ! Name of the data-base file that contains sweather characteristics
        integer :: wsId ! unique id identification
        real(dp) :: wsAlt ! altitude
        real(dp) :: wsLat ! latitude (WGS84, decimal degree)
        real(dp) :: x ! longitude, projected coordinates 
        real(dp) :: y ! latitude, projected coordinates
        integer :: numOfDays ! number of expected values
        type(date) :: startDate ! the first day in the serie
        type(date) :: endDate ! the last day in the serie
        
        integer, dimension(:), pointer :: nOfDay ! number of days starting from the first selected day
        
        real(dp), dimension(:), pointer :: Tmin, Tmax, Ptot, Umin,Umax,Vmed,RGCorr ! minimum and maximum temperature
        
    end type WeatherStation
    
    Contains
    
        subroutine initVarArray(aWeatherStation)
            type(WeatherStation) :: aWeatherStation
            
            allocate(aWeatherStation%Tmin(aWeatherStation%numOfDays), &
                        & aWeatherStation%Tmax(aWeatherStation%numOfDays), &
                        & aWeatherStation%Ptot(aWeatherStation%numOfDays), &
                        & aWeatherStation%Umin(aWeatherStation%numOfDays), & 
                        & aWeatherStation%Umax(aWeatherStation%numOfDays), & 
                        & aWeatherStation%Vmed(aWeatherStation%numOfDays), & 
                        & aWeatherStation%RGCorr(aWeatherStation%numOfDays))
            
        end subroutine initVarArray
            
        function addWeatherStation(aWsList, aWS) result(nOfWS)
            type(WeatherStation), intent(inout), dimension(:), allocatable :: aWsList
            Type(WeatherStation), intent(in) :: aWS
            type(WeatherStation), dimension(:), allocatable :: newWsList
            integer :: i,nOfWS
            nOfWS = 0
            if (ALLOCATED(aWsList) .eqv. .true.) then
                nOfWS = ubound(aWsList,1)
                !print*, 'following allocate', nOfCropSeq
                ! copy the list
                allocate(newWsList(nOfWS))
                do i=1, nOfWS
                    newWsList(i) = aWsList(i)
                end do
                ! clear original
                deallocate(aWsList)
                allocate(aWsList(nOfWS+1))
                do i=1, nOfWS
                    aWsList(i) = newWsList(i)
                end do
                
                aWsList(nOfWS+1) = aWS
            else
                !print*, 'first allocation', nOfCropSeq
                allocate(aWsList(1))
                aWsList(1) = aWS
            end if
            nOfWS = nOfWS + 1
        end function
    
end module mod_weather_station
