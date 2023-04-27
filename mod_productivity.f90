! /***************************************************************************
!  cropcoeff
!  A executable/library to use to produce inputs for the IdrAgra model
! -------------------
! 		begin				: 2022-01-01
! 		copyright			: (C) 2022 by Enrico A. Chiaradia
! 		email				    : enrico.chiaradia@unimi.it
!  ***************************************************************************/

! /***************************************************************************
!  *																		   *
!  *   This program is free software; you can redistribute it and/or modify  *
!  *   it under the terms of the GNU General Public License as published by  *
!  *   the Free Software Foundation; either version 2 of the License, or	   *
!  *   (at your option) any later version.								   *
!  *																		   *
!  ***************************************************************************/
!
!  /***************************************************************************
! credits:
! Part of this work derives from unsigned codes. If have rights about some parts,
! please contact me at enrico.chiaradia@unimi.it
!   ***************************************************************************/

module mod_productivity
	use mod_utilities
	implicit none
	
	contains
	
	function adjustWaterProductivity(origWP,CO2conc,fsink) result(adjWP)
			REAL(dp), intent(in) :: origWP,CO2conc,fsink
			REAL(dp) :: adjWP
			real(dp) :: f_type,w,fCO2
			
			!TODO: keep from matlab source
			f_type = ((40.-(origWP)*100.)/(40.-20.))
			f_type = max(0.,f_type)
			f_type = min(1.,f_type)
			! calculate w
            w = 1-(550.-CO2conc)/(550.-369.41)
			w = max(0.,w)
			w = min(1.,w)
			! calculate fCO2
            fCO2 = (CO2conc/369.41)/(1+(CO2conc-369.41)*((1-w)*0.000138 + w*(fsink*0.000138+(1-fsink)*0.001165)))
			adjWP = (1+f_type*(fCO2-1))*origWP
			!print*,'origWP = ',origWP,'adjWP = ',adjWP
		end function
	
		function computeCanopyResistance(CO2conc) result (CanRes)
			REAL(dp), intent(in) :: CO2conc
			REAL(dp) :: CanRes
			REAL(dp) :: LeafCond, LeafRes
			! Calculate canopy resistance values
			! Easterling WE et al. (1992)
			! Preparing the erosion productivity impact calculator (EPIC) model
			! to simulate crop response to climate change and the direct effects of CO2.
			! Agricultural and Forest Meteorology 59 (1), 17 U34.
			LeafCond = 1 / 100. * (1.4 - 0.4 * (CO2conc / 330.))  
			LeafRes = 1. / LeafCond
			CanRes = LeafRes / (0.5 * 24. * 0.12)  ! FAO56 pag. 22
			
		end function
		
end module

!~ program testadjustWaterProductivity
	!~ use mod_productivity
	!~ implicit none
	
	!~ !real(dp), dimension(8) :: a = (/1.,2.,3.,-9999.,-9999.,-9999.,10.,11./)
	!~ real(dp) :: origWP, CO2conc, fsink, adjWP
	!~ integer :: i
	
	!~ origWP = 0.149
	!~ CO2conc = 337
	!~ fsink = 0.1
	
	!~ adjWP = adjustWaterProductivity(origWP,CO2conc,fsink)
	
	!~ print*, 'adjWP: ', adjWP 
	
		
!~ end program