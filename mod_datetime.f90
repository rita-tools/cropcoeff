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

module mod_datetime
	use mod_utilities
	
	implicit none
	
	type date
		integer::day
		integer::month
		integer::year
	end type date
	
	contains
	
		function dayRank(aDate, fromEnd) RESULT(numOfDay)
			! calculate the rank in the year of the input date
			type(date) :: aDate
			integer :: numOfDay
			integer :: i
			integer,dimension(12) :: daysInMonths
			logical :: fromEnd
			
			numOfDay = 0
			
			call getDaysInMonths(aDate%year,daysInMonths)
			do i=1,aDate%month-1
				numOfDay = numOfDay + daysInMonths(i)
			end do
			
			numOfDay = numOfDay + aDate%day
			
			if (fromEnd .eqv. .true.) then
				numOfDay = 365-numOfDay
				if (isleap(aDate%year) .eqv. .true.) then
					numOfDay = numOfDay+1
				end if
			end if
		end function dayRank
		
		function isLeap(year) result(flag)
			integer,intent(in)::year!
			logical :: flag!
			
			if(mod(year,400)==0 .or. (mod(year,4)==0 .and. (.not.(mod(year,100)==0))))then  !  leap year
				flag = .true.
			else    ! regular year
				flag = .false.
			end if!
		end function isLeap
		
		subroutine getDaysInMonths(year,daysInMonths)!
			! Dato l'anno restituisce un vettore col numero di giorno per ogni mese!
			implicit none!
			integer,intent(in)::year!
			integer,dimension(:),intent(out)::daysInMonths!
			!
			if (isLeap(year) .eqv. .true.) then  !  leap year
				daysInMonths=(/31,29,31,30,31,30,31,31,30,31,30,31/)
			else    ! regular year
				daysInMonths=(/31,28,31,30,31,30,31,31,30,31,30,31/)
			end if!
		end subroutine getDaysInMonths
			
		function calcNumDays(startDate, endDate) RESULT(numDays)
			! calculate the number of days between two days
			type(date) :: startDate, endDate
			integer :: numDays
			integer :: startDayOfYear = 0
			integer :: endDayOfYear = 0
			integer :: y
			! get the number of days of the first year
			numDays = 0
			!print *, 'start: ',dayRank(startDate,.true.)
			numDays = numDays + dayRank(startDate,.true.)+1
			
			! get the number of days in the years between
			do y=(startDate%year+1),(endDate%year-1)
				numDays = numDays + 365
				!print *, 'in loop'
				if (isLeap(y) .eqv. .true.) then
					numDays = numDays + 1
				end if
			end do
			
			! get the number of days of the last year
			! TODO: test it better
			if (startDate%year /= endDate%year ) then
				!print *, 'end: ',dayRank(endDate,.false.)
				numDays = numDays + dayRank(endDate,.false.)
			else
				if ((startDate%month == 12) .and. (startDate%day == 31))  then
					numDays = numDays
				else
					numDays = numDays - dayRank(endDate,.true.)
				end if
			end if
			
		end function calcNumDays
			
		subroutine strToDate(dateStr, dateTo)
			character(len=*), intent(in) :: dateStr
			character(len=maxlength) :: dummy
			Type(date), Intent(out) :: dateTo
			dummy = dateStr
			call replaceChar(dummy, '/',' ')
			READ(dummy,*) dateTo%day,dateTo%month,dateTo%year
		end subroutine
		
		function dateToStr(aDate)result(dateStr)
		type(date), intent(in) :: aDate
			character(len=10) :: dateStr
			dateStr = trim(adjustl(intToStr(aDate%year)))//'-'//&
						   trim(adjustl(intToStr(aDate%month)))//'-'//&
						   trim(adjustl(intToStr(aDate%day)))
		end Function
		
		SUBROUTINE calculateDoY(startDate,nOfDays, nOfDay)
			Type(date), Intent(IN) :: startDate
			INTEGER, INTENT(IN) :: nOfDays
			integer, dimension(:), ALLOCATABLE :: nOfDay
			INTEGER :: i, nOfFirstDay, year
			
			allocate(nOfDay(nOfDays))
			
			nOfFirstDay = dayRank(startDate, .false.)
			year = startDate%year
			
			!print*, 'date = ', dateToStr(startDate), 'nOfFirstDay = ',nOfFirstDay

			do i=1,  nOfDays
				nOfDay(i) = nOfFirstDay
				nOfFirstDay = nOfFirstDay+1
				if (((isLeap(year) .eqv. .true.) .and. (nOfFirstDay>366)) .or. ((isLeap(year) .eqv. .false.) .and. (nOfFirstDay>365))) then
					! end of year, reset counter
					nOfFirstDay = 1
					year = year+1
				end if
			END DO
		end subroutine
		
		function doyToDate(doy,year)result(aDate)
			integer::doy,year,span,i,days
			type(date)::aDate
			integer,dimension(12) :: daysInMonths
			
			call getDaysInMonths(year,daysInMonths)
			span=0
			i=0
			do while (span<doy)
				i=i+1
				span = span+daysInMonths(i)
			end do
			! go back of one
			span = span-daysInMonths(i)
			i=i-1
			
			days = doy-span
			if (days ==0) then
				! get max days in latest month
				days =daysInMonths(i)
			else
				! get following month for residual days
				i=i+1
			end if
			! update output date
			aDate%year = year
			aDate%month = i
			aDate%day = days
		end function
		
		function daysInYear(year)result(nOfDays)
			integer:: year,nOfDays
			if (isLeap(year).eqv. .true.) then
				nOfDays = 366
			else
				nOfDays = 365
			end if
		end function
		
		function addDays(aDate,nOfDays) result(newDate)
			type(date) :: aDate,newDate
			integer :: nOfDays, doy, span, endYear,days
			
			!print*, 'nOfDays = ',nOfDays
			
			if (nOfDays>=0) then
				doy = dayRank(aDate, .false.)
				endYear = aDate%year
				span = daysInYear(endYear)-doy
				! add number
				do while (span <nOfDays)
					endYear = endYear+1
					span = span+daysInYear(endYear)
				end do
				span = span-daysInYear(endYear)
				
				days = nOfDays-span ! it's the rank in the endYear
				
				if (days ==0) then
					endYear = endYear-1
					days = daysInYear(endYear)
				end if
				
				newDate = doyToDate(days,endYear)
			else
				newDate = removeDays(aDate,nOfDays)
			end if
		end function
		
		function removeDays(aDate,nOfDays) result(newDate)
			type(date) :: aDate,newDate
			integer :: nOfDays, doy, span, endYear,days
		
			doy = dayRank(aDate, .false.)
			endYear = aDate%year
			span = -doy
			! add number
			do while (span >nOfDays)
				endYear = endYear-1
				span = span-daysInYear(endYear)
			end do
			span = span+daysInYear(endYear)
			
			days = daysInYear(endYear)-(span-nOfDays) ! it's the rank in the endYear
			
			!print*,'check: ',daysInYear(endYear), span, nOfDays,' = ',days
			
			if (days ==0) then
				endYear = endYear-1
				days = daysInYear(endYear)
			end if
			
			newDate = doyToDate(days,endYear)
			
		end function
	
	
end module mod_datetime	
	
!~ program testDateFun
	!~ use mod_datetime
	!~ implicit none
	
	!~ character(LEN=250) :: startDateStr
	!~ character(LEN=250) :: endDateStr
	
	!~ type(date) :: startDate,endDate
	!~ integer :: startRank, endRank, numOfDays
	
	!~ ! first test
	!~ print *, 'first test'
	!~ startDateStr = '01/01/2018'
	!~ endDateStr = '31/12/2018'
	
	!~ call strToDate(startDateStr, startDate)
	!~ call strToDate(endDateStr, endDate)
	
	!~ startRank = dayRank(startDate, .false.)
	!~ endRank = dayRank(endDate, .false.)
	
	!~ print *, trim(startDateStr),': ',startRank
	!~ print *, trim(endDateStr),': ',endRank

	!~ numOfDays = calcNumDays(startDate, endDate)
	
	!~ print *, 'n. of days between: ',numOfDays, ' (expected 365)'
		
	!~ ! second test
	!~ print *, 'second test'
	!~ startDateStr = '01/01/2018'
	!~ endDateStr = '31/12/2019'
	
	!~ call strToDate(startDateStr, startDate)
	!~ call strToDate(endDateStr, endDate)
	
	!~ startRank = dayRank(startDate, .false.)
	!~ endRank = dayRank(endDate, .false.)
	
	!~ print *, trim(startDateStr),': ',startRank
	!~ print *, trim(endDateStr),': ',endRank

	!~ numOfDays = calcNumDays(startDate, endDate)
	
	!~ print *, 'n. of days between: ',numOfDays, ' (expected 730)'
	
	!~ ! second test
	!~ print *, 'third test'
	!~ startDateStr = '06/01/2018'
	!~ endDateStr = '10/01/2018'
	
	!~ call strToDate(startDateStr, startDate)
	!~ call strToDate(endDateStr, endDate)
	
	!~ startRank = dayRank(startDate, .false.)
	!~ endRank = dayRank(endDate, .false.)
	
	!~ print *, trim(startDateStr),': ',startRank
	!~ print *, trim(endDateStr),': ',endRank

	!~ numOfDays = calcNumDays(startDate, endDate)
	
	!~ print *, 'n. of days between: ',numOfDays, ' (expected 5)'
	
	
	!~ !print *, " <enter> to continue"
	!~ !read *	
!~ end program testDateFun

!~ program testDateFun
	!~ use mod_datetime
	!~ implicit none
	
	!~ character(LEN=250) :: startDateStr = '01/02/2020'
		
	!~ type(date) :: startDate
	!~ integer :: nOfDays = 400
	!~ INteger :: i
	!~ INTEGER, allocatable, dimension(:) :: nOfDay

	!~ call strToDate(startDateStr, startDate)
	
	!~ call calculateDoY(startDate,nOfDays, nOfDay)
	
	!~ do i=1,  nOfDays
		   !~ print *, i, nOfDay(i)
	!~ end do
		
!~ end program

!~ program testDateSum
	!~ use mod_datetime
	!~ implicit none
	
	!~ character(LEN=250) :: startDateStr = '01/01/2000'
		
	!~ type(date) :: startDate, newDate
	!~ integer :: nOfDays = -600
	!~ INTEGER, allocatable, dimension(:) :: nOfDay

	!~ call strToDate(startDateStr, startDate)
	
	!~ newDate= addDays(startDate,nOfDays)
	
	!~ print *, startDate%year, startDate%month, startDate%day
	
	!~ print *, newDate%year, newDate%month, newDate%day
		
!~ end program