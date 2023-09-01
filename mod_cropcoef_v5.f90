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

module mod_cropcoef_v5

	use mod_utilities
	use mod_crop
	USE mod_datetime
	USE mod_cropseq
	USE mod_weather_station
	uSE mod_settings
	USe mod_cropcoef_v4
	
	implicit none
	
	contains
	
	
	FUNCTION findSowingDate_v5(Tave,DoY,currentDayIndex,timeSpan,minSowingDate,sowingDelay,T_sowing,isWinterCrop) result(sowingDateIdx)
		REAL(dp), dimension(:), intent(in) :: Tave
		INTEGER, dimension(:), intent(in) :: DoY
		INTEGER, intent(in) :: currentDayIndex, timeSpan,minSowingDate,sowingDelay
		REAL(dp), intent(in) :: T_sowing
		Logical, intent(in) :: isWinterCrop
		integer :: sowingDate, sowingDateIdx
		INTEGER, dimension(:), allocatable :: testSpan
		INTEGER, dimension(:), allocatable :: flag
		INTEGER, dimension(:), allocatable :: rows
		! return the index of the first available day for seeding
		sowingDateIdx=-1
		
		! test current days window
		allocate(testSpan(size(Tave)))
		allocate(flag(size(Tave)))
		flag = 0
		
		testSpan = 0
		testSpan(currentDayIndex:currentDayIndex+timeSpan) = 1

		! test sowing window
		flag =0
		
		where ((DoY>=minSowingDate) .and. (DoY<=minSowingDate+sowingDelay)  .and. (testSpan==1))
			flag = 1
		end where
		
		if (sum(flag) == 0) then
			! no available dates in serie
			! FIXED: force sowing
			if (isWinterCrop .eqv. .true.) then
				sowingDate = minSowingDate+sowingDelay ! winter crop
			else
				sowingDate = minSowingDate ! summer crop
			end if

			rows = findloc(DoY,sowingDate)
			if (size(rows)>0) then
				sowingDateIdx = rows(1)
			end if
			!~ print*, 'minSowingDate = ',minSowingDate
			!~ print*, 'sowingDelay = ',sowingDelay
			!~ print*, 'force sowing at ',sowingDate
			
			return
		end if
		
		flag = 0
		! test temperature Tave > T_sowing and sowing window
		where ((DoY>=minSowingDate) .and. &
				(DoY<=minSowingDate+sowingDelay) .and. &
				(testSpan==1) .and. &
				(Tave>T_sowing))
			flag = 1
		end where
				
		! adjust sowing date
		if (sum(flag)==0) then
			if (isWinterCrop .eqv. .true.) then
				sowingDate = minSowingDate+sowingDelay ! winter crop
			else
				sowingDate = minSowingDate ! summer crop
			end if

			rows = findloc(DoY,sowingDate)
			if (size(rows)>0) then
				sowingDateIdx = rows(1)
			end if
		else
			! find the first element in array
			rows =  findloc(flag,1)
			if (size(rows)>0) then
				sowingDateIdx = rows(1)
			end if
		end if
		
		return
	end FUNCTION
	
	
	subroutine computeParamsDistro_v5(cropsOverYears,T_GDD_corr,cropId,GDDList,pValueList,parValues)
		integer, dimension(:), intent(in) :: cropsOverYears
		real(dp), dimension(:), intent(in) :: T_GDD_corr
		integer, intent(in) :: cropId
		real(dp), dimension(:), intent(in) :: GDDList,pValueList
		real(dp), dimension(:), intent(INOUT) :: parValues
		
		real(dp), dimension(:), allocatable :: T_GDD_masked
		integer, dimension(:), allocatable :: gddTest,infPoints,idx,maxGDDIdx
		real(dp) :: gdd, pValue,minGDD, pAtZero
		INTEGER i,j,n
		
		n = size(T_GDD_corr)
		allocate(T_GDD_masked(n),gddTest(n),infPoints(n))
	
		pAtZero = 0.
		! get start and end index of the selected crop along the serie
		T_GDD_masked = T_GDD_corr
		where  (cropsOverYears/=cropId)
			T_GDD_masked = nodatar
		end where
				
		!parValues = 0. ! mark as zeros the non crop period
		where  (cropsOverYears==cropId)
			parValues = nodatar
		end where
		
		do i=1, size(GDDList)
			gdd = GDDList(i)
			pValue = pValueList(i)
			! mark param inflection points
			! this method forces to the first T_GDD that meets gdd
			!print*,i,gdd,pvalue
			if (gdd==0) then
				! set parValues with zero GDD at the end, after the loop
				pAtZero = pValue
			else
				gddTest = 0
				where (T_GDD_masked >= gdd)
					gddTest = 1
				end where
				! print *,'Test: ',gdd,'',pValue,' --> sum GDD test = ',sum(gddTest)
				
				infPoints = 0
				infPoints(2:n) = gddTest(2:n)-gddTest(1:n-1)
				infPoints(n)=gddTest(n)
				! get ascendet points
				idx = pack([(i,i=1,n)],infPoints==1.,[(0,i=1,n)])
				do j=1,size(idx)
					!print *,'asc gdd = ',gdd,' idx = ',idx(j),' pval = ',pValue
					if (idx(j)>0) then
						parValues(idx(j)) = pValue
					else
						exit
					end if
				end do
				
				! get descendet points
				idx = 0
				idx = pack([(i,i=1,n)],infPoints==-1.,[(0,i=1,n)])
				do j=1,size(idx)
					!print *,'desc gdd = ',gdd,' idx = ',idx(j)-1,' pval = ',pValue
					! FIXED: get previous point
					if (idx(j)-1>0) then
						parValues(idx(j)-1) = pValue
					else
						exit
					end if
				end do
			
			end if
		
		end do
		
			
		! set value when GDD = 0 and it is not a valid number
		where ((T_GDD_masked == 0.) .and. (parValues==nodatar))
			parValues = pAtZero
		end where
		
	end subroutine

subroutine computeCropSeq_v5(wsLat, startDay, Tmax, Tmin, movMeanNum, &
														cropList,tollerance,minGDDForVern,checkFutureTemp,&
														DoY,cropsOverYear,T_GDD_corr, printFun)
												
		Real(dp), intent(in) :: wsLat, tollerance,minGDDForVern
		type(date), intent(in) :: startDay
		Real(dp), intent(in), dimension(:) :: Tmax, Tmin
		integer, intent(in) :: movMeanNum
		type(Crop), Intent(in), dimension(:) :: cropList
		logical, intent(in) :: checkFutureTemp
		
		integer, intent(out), dimension(:), allocatable :: DoY,cropsOverYear
		real(dp), intent(out), dimension(:), allocatable :: T_GDD_corr
				
		integer :: nOfDays, currentDayIndex, harvestIndex, timeSpan, ccIdx, sowIndex, i, minPFIdx,minVFIdx, maxGDDIdx, lowTempIdx
		integer :: maxHarvestIndex
		real(dp) :: minPF, minVF, maxGDD
		real(dp), dimension(:), allocatable :: Tave, DLH,Tmax_sub,Tmin_sub,Tave_sub,DLH_sub
		real(dp), dimension(:), allocatable :: T_GDD_sub,GDD_cum_sub, T_GDD_corr_sub,VF_sub, PF_sub
		integer, dimension(:), allocatable ::  rows
		logical :: changeCrop
		
		interface
			subroutine printFun(text)
				character(len=*):: text
			end subroutine
		end interface


		nOfDays = size(Tmax, DIM=1)
		call calculateDoY(startDay,nOfDays,DoY)
		
		! make a list of crops in field
		allocate(cropsOverYear(nOfDays),T_GDD_corr(nOfDays))
		cropsOverYear = 0
		T_GDD_corr = 0.

		Tave = 0.5*(Tmax+Tmin)
		
		! TODO: make movable mean
		Tave = movMean(Tave, movMeanNum)

		call calculateDLH(DoY, wsLat, DLH)
		!print('DLH',DLH)

		currentDayIndex = 1
		harvestIndex = 1
		timeSpan = 366

		ccIdx = 1 ! current crop index
		changeCrop = .true.
		
		call printFun('Index		[DoY]		Message')

		do while (currentDayIndex<=nOfDays)
			! leave last crop in field
			!print*, '[',currentDayIndex,']','last crop is ', cropList(ccIdx)%cropId
			cropsOverYear(currentDayIndex)=cropList(ccIdx)%cropId
			
			! get the next crop in the list
			if (changeCrop .eqv. .true.) then
				ccIdx = ccIdx+1
				if (ccIdx>size(cropList, DIM=1)) then
					ccIdx = 1 ! loop back
				end if
			end if
			
			if (size(cropList(ccIdx)%GDD)<1) then
				call printFun(trim(intToStr(currentDayIndex))//' ['//trim(intToStr(DoY(currentDayIndex)))//&
					'] No data for crop  '//trim(cropList(ccIdx)%cropName))
				!currentDayIndex = currentDayIndex+1
				changeCrop = .true.
				cycle
			end if
			
			! update timeSpan
			if (nOfDays-currentDayIndex<timeSpan) then
				timeSpan = nOfDays-currentDayIndex
			end if
			
			! get maximum thermal days
			maxGDD = cropList(ccIdx)%HarvNum_max * maxval(cropList(ccIdx)%GDD, DIM = 1)
			
			!print*, 'cropId=',cropList(ccIdx)%cropId
			!Print*, 'timeSpan=',timeSpan

			! find sowing index
			sowIndex = findSowingDate_v5(Tave, DoY, currentDayIndex,timeSpan, &
											cropList(ccIdx)%SowingDate_min, cropList(ccIdx)%SowingDelay_max, &
											cropList(ccIdx)%Tsowing, cropList(ccIdx)%Vern)
			
			call printFun(trim(intToStr(currentDayIndex))//' ['//trim(intToStr(DoY(currentDayIndex)))//&
									'] Suggested day for sowing  '//trim(cropList(ccIdx)%cropName)//' is '//&
									trim(intToStr(sowIndex))//' ['//trim(intToStr(DoY(sowIndex)))//']')
			!print*,sowIndex, ' = sowIndex'
			
			if (sowIndex<=0) then
				call printFun(trim(intToStr(currentDayIndex))//' ['//trim(intToStr(DoY(currentDayIndex)))//&
									'] No condition to sow crop  '//trim(cropList(ccIdx)%cropName))
					
				currentDayIndex = currentDayIndex+1 !cr['CropsOverlap']
				changeCrop = .false.
				cycle
			else
				call printFun(trim(intToStr(currentDayIndex))//' ['//trim(intToStr(DoY(currentDayIndex)))//&
									'] Try sow crop  '//trim(cropList(ccIdx)%cropName))
			end if
			
			! leave last crop in field
			cropsOverYear(currentDayIndex:sowIndex)=cropList(ccIdx)%cropId
			
			! update timeSpan if necessary
			if (sowIndex+timeSpan-1>nOfDays) then
				timeSpan=nOfDays-sowIndex+1
			end if
			
			! make a subset of variable
			if (ALLOCATED(Tmax_sub) .eqv. .true.) then
				! clear arrays
				deallocate(Tmax_sub,Tmin_sub,Tave_sub,DLH_sub, PF_sub,VF_sub)
			end if 
			
			allocate(Tmax_sub(timeSpan),Tmin_sub(timeSpan),Tave_sub(timeSpan),DLH_sub(timeSpan),&
							VF_sub(timeSpan),PF_sub(timeSpan))
			
			Tmax_sub = Tmax(sowIndex:sowIndex+timeSpan-1)
			Tmin_sub = Tmin(sowIndex:sowIndex + timeSpan-1)
			Tave_sub = Tave(sowIndex:sowIndex + timeSpan-1)
			DLH_sub = DLH(sowIndex:sowIndex + timeSpan-1)
			
			VF_sub = 1.
			PF_sub =1
			

			!print*, 'len(Tmax_sub)', size(Tmax_sub)
			
			call calculateGDD(Tmax_sub, Tmin_sub, &
										cropList(ccIdx)%Tdaybase, cropList(ccIdx)%Tcutoff, &
										T_GDD_sub)

			!print*, 'len(T_GDD_sub)', size(T_GDD_sub)
			
			! get first index with temperature lower than required (only for summer crops)
			if ((cropList(ccIdx)%Vern .eqv. .false.) .and. (checkFutureTemp .eqv. .true.)) then
				lowTempIdx = findloc(T_GDD_sub, 0, DIM=1)
				if (lowTempIdx>0) then
					T_GDD_sub(lowTempIdx:timeSpan)=0 ! set all following thermal day to zero, plant will die ...
				end if
			end if
			
			if (cropList(ccIdx)%Vern .eqv. .true.) then
				CALL vernalization(Tave_sub, cropList(ccIdx)%Tv_min, cropList(ccIdx)%Tv_max,&
											cropList(ccIdx)%Vslope, cropList(ccIdx)%Vstart, &
											cropList(ccIdx)%Vend, cropList(ccIdx)%VFmin, &
											VF_sub)
				! get minimum vernalization factor
				minVF = minval(VF_sub, DIM = 1)
				minVFIdx = findloc(VF_sub, minVF, DIM=1)
				if (minVF>=1) then
					call printFun(trim(intToStr(currentDayIndex))//' ['//trim(intToStr(DoY(currentDayIndex)))//&
									'] Not enough cool days for complete vernalization of  '//trim(cropList(ccIdx)%cropName))
					currentDayIndex = currentDayIndex+1 !cr['CropsOverlap']
					changeCrop = .false.
					cycle
				end if
			end if
			!print*, 'len(VF_sub)', size(VF_sub)
			

			if (cropList(ccIdx)%ph_r >0) then
				CALL photoperiod(DLH_sub, &
											cropList(ccIdx)%ph_r, cropList(ccIdx)%daylength_if, cropList(ccIdx)%daylength_ins,&
											PF_sub)
				minPF = minval(PF_sub, DIM = 1)
				minPFIdx = findloc(PF_sub,minPF, DIM=1)

				if (minPF>=1) then
					call printFun(trim(intToStr(currentDayIndex))//' ['//trim(intToStr(DoY(currentDayIndex)))//&
									'] Not enough shirt/long days for complete photoperiod of  '//trim(cropList(ccIdx)%cropName))
					currentDayIndex = currentDayIndex+ 1 !cr['CropsOverlap']
					changeCrop = .false.
					cycle
				end if
			end if
			
			
			!print*, 'len(PF_sub)', size(PF_sub)

			T_GDD_corr_sub = T_GDD_sub * min(VF_sub, PF_sub)
			!print('PF', PF)
			! Computes GDD considering both VF and PF
			GDD_cum_sub = cumsum(T_GDD_corr_sub)  ! PhD Thesis Anna Borghi: eq i-85 page 173
			if ((cropList(ccIdx)%Vern .eqv. .true.).and.(minGDDForVern>0)) then
				! check if vernalization minimum is at 1/10 of the maximum required GDD, crop has enought time to grow before vernalization
				if (GDD_cum_sub(minVFIdx)>(minGDDForVern*maxval(cropList(ccIdx)%GDD, DIM = 1))) then
					call printFun(trim(intToStr(currentDayIndex))//' ['//trim(intToStr(DoY(currentDayIndex)))//&
									'] Not enough growing days before vernalization of  '//trim(cropList(ccIdx)%cropName))

					currentDayIndex = currentDayIndex+ 1 !cr['CropsOverlap']
					changeCrop = .false.
					cycle
				end if
			end if

			if (maxval(GDD_cum_sub, DIM=1)>tollerance*maxGDD) then
				! enough thermal resources to finish the crop
				rows = pack([(i,i=1,timeSpan)],GDD_cum_sub > maxGDD,[(0,i=1,timeSpan)])
				! try with full thermal condition
				if (maxGDD<0) then
					! undetermined number of crops
					maxGDDIdx = maxval(rows, DIM=1)
				else
					if (maxval(rows, DIM=1)>0) then
						maxGDDIdx = rows(1)
					else
						! this condition is already verified
						rows = pack([(i,i=1,timeSpan)],GDD_cum_sub > tollerance*maxGDD,[(0,i=1,timeSpan)])
						maxGDDIdx = rows(1)
					end if
				end if
				harvestIndex = sowIndex +maxGDDIdx-1 ! TODO: check correction
				! TODO: limit harvest time to the maximum set by the user
				!~ if (cropList(ccIdx)%HarvestDate_max > cropList(ccIdx)%SowingDate_min) then
					!~ ! summer crops above equator. winter crop otherwise
					!~ maxHarvestIndex = sowIndex + (cropList(ccIdx)%HarvestDate_max-DoY(sowIndex))
				!~ else
					!~ maxHarvestIndex = sowIndex + (DoY(sowIndex)-cropList(ccIdx)%HarvestDate_max)
				!~ end if
				
				!~ harvestIndex = min(harvestIndex,maxHarvestIndex)
				
				!~ if (harvestIndex<sowIndex) then
					!~ call printFun(trim(intToStr(sowIndex))//' ['//trim(intToStr(DoY(sowIndex)))//&
									!~ '] Predicted harvest overcomes expected for '//trim(cropList(ccIdx)%cropName))
					!~ currentDayIndex = currentDayIndex+1 !cr['CropsOverlap']
					!~ changeCrop = .false.
					!~ cycle
				!~ end if
				
				! always check crop overlaps
				cropsOverYear(sowIndex-cropList(ccIdx)%CropsOverlap:sowIndex) = 0
				T_GDD_corr(sowIndex-cropList(ccIdx)%CropsOverlap:sowIndex) = 0.0
				! set the period to crop
				cropsOverYear(sowIndex:harvestIndex)=cropList(ccIdx)%cropId
				
				!~ PRINT*,'harvestIndex=',harvestIndex
				!~ PRINT*,'maxGDDIdx=',maxGDDIdx
				
				
				T_GDD_corr(sowIndex:harvestIndex)=GDD_cum_sub(1:maxGDDIdx)
				call printFun(trim(intToStr(sowIndex))//' ['//trim(intToStr(DoY(sowIndex)))//&
									'] Set crop '//trim(cropList(ccIdx)%cropName))
				call printFun(trim(intToStr(harvestIndex))//' ['//trim(intToStr(DoY(harvestIndex)))//&
									'] harvest crop '//trim(cropList(ccIdx)%cropName))
				! update currentDayIndex
				!print*,harvestIndex, ' = harvestIndex'
				currentDayIndex = harvestIndex+1 !cr['CropsOverlap']
				changeCrop = .true.
			else
				call printFun(trim(intToStr(currentDayIndex))//' ['//trim(intToStr(DoY(currentDayIndex)))//&
									'] Not enough thermal days to grow '//trim(cropList(ccIdx)%cropName))
				currentDayIndex = currentDayIndex+ 1 !cr['CropsOverlap']
				changeCrop = .false.
			end if
			
		end do

	end subroutine
	

end module