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

module mod_cropcoef_v4

    use mod_utilities
    use mod_crop
    USE mod_datetime
    USE mod_cropseq
    USE mod_weather_station
    uSE mod_settings
    use mod_productivity
    
    implicit none
    
    contains
    
    subroutine addTails_v4(origArray,newArray,window)
        REAL(dp), dimension(:), intent(in) :: origArray
        REAL(dp), dimension(:), ALLOCATABLE, intent(INout) :: newArray
        Integer, intent(inout) :: window
        INTEGER :: origLen, newLen
        
        ! init new array
        origLen = ubound(origArray,1)
        if (origLen<window) then
            window = origLen
        end if
        
        newLen = window+origLen+window
        allocate(newArray(newLen))
        
        ! copy values
        newArray(1:window) = origArray(1:window) ! repeat the first part
        newArray(newLen-window+1:newLen) = origArray(origLen-window+1:origLen) ! repeat the last part
        newArray(window+1:newLen-window) = origArray(:)
        
    end subroutine

    FUNCTION findSowingDate_v4(Tave,DoY,minSowingDate,sowingDelay,T_sowing,isWinterCrop) result(sowingDateIdx)
        REAL(dp), dimension(:), intent(in) :: Tave
        INTEGER, dimension(:), intent(in) :: DoY
        INTEGER, intent(in) :: minSowingDate,sowingDelay
        REAL(dp), intent(in) :: T_sowing
        Logical, intent(in) :: isWinterCrop
        integer :: sowingDate, s,e,i, j, n
        INTEGER, dimension(:), allocatable :: testTemp
        INTEGER, dimension(:), allocatable :: flag
        INTEGER, dimension(:), allocatable :: rows
        INTEGER, dimension(:), allocatable :: sowingDateIdx
        
        ! select all potential sowing days (the number is the same that respects the condition)
        rows = pack([(i,i=1,size(DoY))], DoY==minSowingDate,[(0,i=1,size(DoY))])
        
        n = size(rows)
        
        allocate(sowingDateIdx(n))
        sowingDateIdx = 0
        
        do j=1,n
            ! select a window for sowing
            s = rows(j)
            if (s ==0) exit
            e = s+sowingDelay
            !print*, 'e = ',e
            ! check temperature
            testTemp = pack([(i,i=1,sowingDelay)], Tave(s:e)>=T_sowing,[(0,i=1,sowingDelay)])
            
            ! add negative control to prevent not realistic results
            if (sum(testTemp)<=0) then
                ! no temperature condition to sow, assign default
                if (isWinterCrop .eqv. .true.) then
                    sowingDateIdx(j) = s
                else
                    sowingDateIdx(j) = e
                end if
            else
                ! get the first useful day
                sowingDateIdx(j) = s+testTemp(1)-1
            end if

            ! deallocate pack array to clear assigned space
            deallocate(testTemp)
            
        end do
        
    end FUNCTION
    
    function findharvestDate_v4(DoY, maxHarvestDate, sowIndex) result(maxHarvestIndex)
        ! find max harvest date in doy
        INTEGER, dimension(:), intent(in) :: DoY, sowIndex
        INTEGER, intent(in) :: maxHarvestDate
        INTEGER, dimension(:), allocatable :: rows
        INTEGER, dimension(:), allocatable :: maxHarvestIndex
        integer :: i,j, n 
        ! select all potential harvest days (the number is the same that respects the condition)
        rows = pack([(i,i=1,size(DoY))], DoY==maxHarvestDate,[(0,i=1,size(DoY))])
        
        n = size(rows)
        allocate(maxHarvestIndex(size(sowIndex)))
        maxHarvestIndex = size(DoY)
        
        !print*, 'n = ', n, ' size(sowIndex) = ', size(sowIndex)
        
        i = 1
        do j=1, n
            ! check index
            if (rows(j)>sowIndex(i)) then
                maxHarvestIndex(i) = rows(j)
                i=i+1
            end if
        end do
        
    end function

    Subroutine calculateDLH(DoY, wsLat,DLH)
        INTEGER, dimension(:), intent(in) :: DoY
        real(dp):: wsLat, phi
        real(dp), dimension(:), allocatable :: delta, omega_s
        real(dp), dimension(:), allocatable, intent(OUT) :: DLH
        
        allocate(DLH(size(DoY)))
        !Computes daylight hours
        phi = pi / 180 * wsLat  ! latitude [rad], FAO56: eq. 22 pag 46
        delta = 0.409 * sin(2 * pi / 365 * DoY - 1.39)  ! solar declination, FAO56: eq. 24 pag 46
        omega_s = acos(-tan(phi) * tan(delta))  ! sunset hour angle [rad], FAO56: eq. 25 pag 46
        DLH = 24 / pi * omega_s  ! daylight hours,  FAO56: eq. 34 pag 48
    end subroutine
    
    subroutine calculateGDD(Tmax, Tmin, Tdaybase, Tcutoff,T_GDD)
        real(dp), dimension(:), Intent(in) :: Tmax, Tmin
        real(dp), intent(in) :: Tdaybase, Tcutoff
        real(dp), dimension(:), allocatable, Intent(OUT) ::T_GDD
        real(dp), dimension(lbound(Tmax,dim=1):ubound(Tmax,dim=1)) :: Tave,T_GDD_low,T_GDD_up, W,theta2,phi3,theta4,phi4
        logical, dimension(lbound(Tmax,dim=1):ubound(Tmax,dim=1)) :: i1,i2,i3,i4,i5
        
        
        ! New version: sine wave
        Tave = 0.5*(Tmax+Tmin)
        T_GDD_low=0
        T_GDD_up = 0
        W = 0.5*(Tmax - Tmin)
        i1 = (Tmin >= Tdaybase).and.(Tmax <= Tcutoff)
        i2 = (Tmin < Tdaybase).and.(Tmax <= Tcutoff).and.(Tmax > Tdaybase)
        i3 = (Tmin >= Tdaybase).and.(Tmax > Tcutoff).and.(Tmin < Tcutoff)
        i4 = (Tmin < Tdaybase).and.(Tmax > Tcutoff)
        i5 = (Tmin >= Tcutoff)
        ! i6      both Tmax and T min are < Tcutoff -> no heat units accumulation (matrix already initialized to zero)
        
        ! case 1
        where (i1)
            T_GDD_low = Tave - Tdaybase
        end where

        ! case 2
        where (i2)
            theta2 = asin((Tdaybase - Tave)/ W)
            T_GDD_low = ((Tave - Tdaybase)* (pi / 2 - theta2) + W* cos(theta2))/ pi
        end where
        
        ! case 3
        where(i3)
            phi3 = asin((Tcutoff - Tave) / W)
            T_GDD_low = Tave - Tdaybase
            T_GDD_up = ((Tave - Tcutoff) * (pi / 2 - phi3) + W* cos(phi3))/ pi
        end where
        
        ! case 4
        where(i4)
            theta4 = asin((Tdaybase - Tave)/ W)
            phi4 = asin((Tcutoff - Tave)/ W)
            T_GDD_low = ((Tave - Tdaybase)* (pi / 2 - theta4) + W* cos(theta4))/ pi
            T_GDD_up = ((Tave - Tcutoff) * (pi / 2 - phi4) + W* cos(phi4))/ pi
        end where
        ! case 5
        where(i5)
            T_GDD_low = Tcutoff - Tdaybase
        end where
        
        ! calcolate GDD
        T_GDD = T_GDD_low - T_GDD_up
        !print*,'Tmax',minval(Tmax),maxval(Tmax)
        !print*,'Tmin',minval(Tmin),maxval(Tmin)
        !print*,'W',minval(W),maxval(W)
        !print*,'T_GDD_low',minval(T_GDD_low),maxval(T_GDD_low)
        !print*,'T_GDD_up',minval(T_GDD_up),maxval(T_GDD_up)
        !print*,'T_GDD',minval(T_GDD),maxval(T_GDD)
    end subroutine
    
    
    subroutine vernalization(Tave, Tv_min, Tv_max, Vslope, Vstart, Vend, VFmin,VF)
        real(dp), dimension(:), Intent(in) :: Tave
        real(dp) :: Tv_min, Tv_max, VFmin
        integer ::Vslope, Vstart, Vend
        real(dp), dimension(:), allocatable ::VDsum
        !~ real(dp), dimension(lbound(Tave,dim=1):ubound(Tave,dim=1)), Intent(OUT) :: VF
        !~ real(dp), dimension(lbound(Tave,dim=1):ubound(Tave,dim=1)) :: Veff
        real(dp), dimension(size(Tave,dim=1)), Intent(OUT) :: VF
        real(dp), dimension(size(Tave,dim=1)) :: Veff
        
        ! Vernalization
        Veff = 0.  ! vernalization contribution of the day
        ! Calculate vernalization contribution of the day
        ! PhD Thesis Anna Borghi: eq i-81 page 172
        where ((Tave >= (Tv_min - Vslope)).and.(Tave < Tv_min))  
            Veff = 1 - (Tv_min - Tave) / Vslope
        end where
        
        where ((Tave >= Tv_min).and.(Tave < Tv_max))
            Veff = 1.
        end where
        
        where ((Tave >= Tv_max) .and. (Tave < (Tv_max + Vslope))) 
            Veff = 1 - (Tave - Tv_max) / Vslope
        end where
        
        ! Calculate  sum of accumulated vernalization days
        VDsum = cumsum(Veff)  ! sum of the currently accumulated vernalization days
        ! Calculate  vernalization factor
        VF = VFmin + ((1 - VFmin) * (VDsum - Vstart)) / (Vend - Vstart)
        where (VDsum < Vstart)
            VF = 1
        end where
        
        where (VDsum > Vend)
            VF = 1
        end where
        
    end subroutine
    
    subroutine photoperiod(DLH, ph_r, daylength_if, daylength_ins,PF)
        real(dp), dimension(:), Intent(in) :: DLH
        real(dp),intent(in):: daylength_if,daylength_ins
        integer, intent(in) :: ph_r
        !~ real(dp), dimension(lbound(DLH,dim=1):ubound(DLH,dim=1)), intent(out) :: PF
        real(dp), dimension(size(DLH,dim=1)), intent(out) :: PF
        
        ! Photoperiod
        PF = 1. ! photoperiod factor
        if (ph_r == 1) then  ! Long-day plants, PhD Thesis Anna Borghi: eq i-82 page 172
            where (DLH < daylength_if)
                PF = 0.
            elsewhere (DLH > daylength_ins)
                PF = 1.
            elsewhere ((DLH >= daylength_if).and.(DLH <= daylength_ins))
                PF = (DLH - daylength_if) / (daylength_ins - daylength_if)
            end where
        end if
        
        if (ph_r == 2) then  ! Short-day plants, PhD Thesis Anna Borghi: eq i-83 page 172 (Corrected!!! see http://modeling.bsyse.wsu.edu/CS_Suite/cropsyst/manual/simulation/crop/photoperiod.htm)
            where (DLH > daylength_if)
                PF = 0
            elsewhere (DLH < daylength_ins)
                PF = 1 !TODO: questa condizione annulla la precedente se daylength_ins > daylength_if. Aggiugere check?
            elsewhere ((DLH >= daylength_ins).and.(DLH < daylength_if))
                PF = (daylength_if - DLH) / (daylength_if - daylength_ins)
            end where
        end if

    end subroutine

    function calcKcbCorrFact(RHmin, Wind, hc) result(kcbCorrFact)
        real(dp) :: RHmin, Wind, hc
        real(dp) :: kcbCorrFact
        
        if (RHmin < 20) then
            RHmin = 20
        else    if (RHmin > 80) then
            RHmin = 80
        end if
        
        if (Wind < 1) then
            Wind = 1
        else if (Wind > 6) then 
            Wind = 6
        end if

        if (hc < 0.1) then
            hc = 0.1
        else if (hc > 10) then
            hc = 10
        end if
        
        kcbCorrFact = (0.04 * (Wind - 2) - 0.004 * (RHmin - 45)) * (hc / 3) ** 0.3
    end function
    
    
    function adjustKcb(Kcb,RHmin, Wind, hc) result(adjKcb)
        ! why not daily correction
        real(dp), intent(in) :: Kcb(:)
        real(dp), intent(in) :: RHmin(:)
        real(dp), intent(in) :: Wind(:)
        real(dp), intent(in) :: hc(:)
        real(dp), allocatable :: adjKcb(:)
        real(dp), allocatable :: r(:)
        real(dp) :: CF, d
        
        integer :: i,s,e,n
        n = size(Kcb)
        
        Allocate(adjKcb(n), r(n))
        
        adjKcb = Kcb
        r = 0
        r = pack([(i,i=1,n)],Kcb/=nodatar,[(0,i=1,n)])
        
        !~ print*, 'len(r) = ', size(r), ' n = ', n
        !~ print*, 'i    s    e'
        do i =1, size(r)-1
            s = r(i)
            e = r(i+1)
            !e = e+1
            !~ print*,i,s,e
            if ((s == 0) .or. (e == 0)) exit
            
            if ((Kcb(e)>0.45).and.(Kcb(s)>0.45)) then
                ! calculate difference and check phase
                d = Kcb(e)-Kcb(s)
                if (d<=0.) then
                    CF = calcKcbCorrFact(mean(RHmin(s:e)), mean(Wind(s:e)), mean(hc(s:e)))
                    if (d==0.) adjKcb(s) = adjKcb(s)+CF ! adjust the first only for flat phase
                    adjKcb(e) = adjKcb(e)+CF
                end if
            end if
        end do
        
    end function

    function computeParamsDistro_v4(T_GDD_corr,GDDList,pValueList,printFun)result(parValues)
        real(dp), dimension(:), intent(in) :: T_GDD_corr
        real(dp), dimension(:), intent(in) :: GDDList,pValueList
        real(dp), dimension(:), allocatable :: parValues
                
        interface
            subroutine printFun(text)
                character(len=*):: text
            end subroutine
        end interface
        
        integer, dimension(:), allocatable :: gddTest,infPoints,idx
        real(dp) :: gdd, pValue, maxGDD
        INTEGER i,j,n,maxGDDIdx
        
        n = size(T_GDD_corr)
        allocate(gddTest(n),infPoints(n),parValues(n))
    
        parValues = nodatar
        
        do i=1, size(GDDList)
            gdd = GDDList(i)
            pValue = pValueList(i)
        
            ! mark param inflection points
            ! this method forces to the first T_GDD that meets gdd
            !print*,i,gdd,pvalue
            gddTest = 0
            where (T_GDD_corr >= gdd)
                gddTest = 1
            end where
            
            infPoints = 0
            infPoints(2:n) = gddTest(2:n)-gddTest(1:n-1)
            ! manually set first value
            if (T_GDD_corr(1)>=gdd) infPoints(1) = 1.
            
            !infPoints(n)=gddTest(n)
            ! get ascendet points
            idx = pack([(i,i=1,n)],infPoints==1.,[(0,i=1,n)])
            do j=1,size(idx)
                !~ print *,'asc gdd = ',gdd,' idx = ',idx(j),' pval = ',pValue
                if (idx(j) == 0) then
                    exit
                end if
                if (parValues(idx(j))==nodatar) then
                    parValues(idx(j)) = pValue
                else
                    ! assign param to the following value
                    if (idx(j)<n) parValues(idx(j)+1) = pValue
                end if
            end do
        
        end do
        
        maxGDD = maxval(T_GDD_corr)
        idx = pack([(i,i=1,n)],T_GDD_corr==maxGDD,[(0,i=1,n)])
        
        if (size(idx)==0) then
            call printFun('Idx is zero')
            !print*,'Max GDD value: ',maxGDD
            !print*,'Max GDD value recalc: ',maxval(T_GDD_corr)
        end if
        
        maxGDDIdx = idx(1)
        call printFun('Max GDD index: '//trim(intToStr(maxGDDIdx)))
        call printFun('Value at max GDD: '//trim(realToStr(parValues(maxGDDIdx))))
        
        ! assign interpolated value to the last -1
        if (parValues(maxGDDIdx)==nodatar) then
            parValues(maxGDDIdx) =getValue(GDDList,pValueList,maxGDD)
            call printFun('Assigned interpolated value: '//trim(realToStr(parValues(maxGDDIdx))))
        end if
        
        ! Set first value (sowing) required for interpolation
        ! value set to the minimum value of the parameter
        if (parValues(1) == nodatar) parValues(1) =  minval(pValueList)
        ! Set value after harvest (value need to be at the minimum the day after harvest)
        ! value set to the minimum value of the parameter
        if (maxGDDIdx < n) then
            parValues(maxGDDIdx+1) = minval(pValueList)
            !~ if (parValues(maxGDDIdx+1) == nodatar) then
                !~ parValues(maxGDDIdx+1) = minval(pValueList)
            !~ else
                !~ call printFun('Value at max GDD+1: '//trim(realToStr(parValues(maxGDDIdx+1))))
            !~ end if
        end if
        ! Set last value of the series
        ! value set to the minimum value of the parameter
        if (parValues(n) == nodatar) parValues(n) =  minval(pValueList) 
        !~ else
            !~ ! replace for best interpolation
            !~ parValues(n-1) =  getValue(GDDList,pValueList,T_GDD_corr(n-1))
            !~ parValues(n) =  minval(pValueList)
        !~ end if
        
    end function
    

    subroutine computeCropSeq_v4(wsLat, startDay, Tmax, Tmin, movMeanNum, &
                                                        RHmin, Wind, &
                                                        cropList, &
                                                        DoY,cropsOverYear,T_GDD_corr, cropInField, &
                                                        lai, hc,kcb, adjKcb, sr, ky, cn, fc, &
                                                        r_stress, &
                                                        printFun)
                                                
        Real(dp), intent(in) :: wsLat
        type(date), intent(in) :: startDay
        Real(dp), intent(in), dimension(:) :: Tmax, Tmin, RHmin, Wind
        integer, intent(in) :: movMeanNum
        type(Crop), Intent(in), dimension(:) :: cropList
        
        integer, intent(out), dimension(:), allocatable :: DoY,cropsOverYear,cropInField
        real(dp), intent(out), dimension(:), allocatable :: T_GDD_corr, lai, hc,kcb,adjKcb, sr, ky, cn, fc, r_stress
                
        integer :: nOfDays, i,  c, s, e, timeSpan, harvestGDDIdx
        integer, dimension(:), allocatable :: sowIndex, maxHarvestIndex
        real(dp) :: minPF, minVF, maxGDD
        real(dp), dimension(:), allocatable :: Tave, DLH,Tmax_sub,Tmin_sub,Tave_sub,DLH_sub
        real(dp), dimension(:), allocatable :: T_GDD_sub,GDD_cum_sub, T_GDD_corr_sub,VF_sub, PF_sub
        integer, dimension(:), allocatable ::  rows
        
        real(dp), dimension(:), allocatable :: parGDD, parKcb, parLAI, parHc, parSr, parKy, parCNvalue, parFc, par_r_stress
        
        interface
            subroutine printFun(text)
                character(len=*):: text
            end subroutine
        end interface


        nOfDays = size(Tmax, DIM=1)
        call calculateDoY(startDay,nOfDays,DoY)
        
        ! make a list of crops in field
        allocate(cropsOverYear(nOfDays),T_GDD_corr(nOfDays), cropInField(nOfDays))
        allocate(lai(nOfDays), hc(nOfDays),kcb(nOfDays),adjKcb(nOfDays), sr(nOfDays), ky(nOfDays), cn(nOfDays), fc(nOfDays))
        allocate(r_stress(nOfDays))
        cropInField = 0. ! FIX 1.
        cropsOverYear = 0 ! crops always has id from 1
        T_GDD_corr = 0.
        lai = nodatar
        hc = nodatar
        kcb = nodatar
        adjKcb = nodatar 
        sr = nodatar
        ky = nodatar
        cn = nodatar
        fc = nodatar
        
        r_stress = nodatar

        !print*, 'min lai after allocation: ', minval(lai)
                
        
        Tave = 0.5*(Tmax+Tmin)
        
        ! make movable mean
        Tave = movMean(Tave, movMeanNum)
        
        ! calculate daylength
        call calculateDLH(DoY, wsLat, DLH)
        
        ! for each crop in cropsequence
        do c=1, size(cropList)
             CALL printFun('Processing crop '//trim(cropList(c)%cropName))
                
            ! make the params with multiple harvest
            parGDD = repeatPars(cropList(c)%GDD, cropList(c)%HarvNum_max, .true.)
            parKcb = repeatPars(cropList(c)%Kcb, cropList(c)%HarvNum_max, .false.)
            parLAI = repeatPars(cropList(c)%LAI, cropList(c)%HarvNum_max, .false.)
            parHc = repeatPars(cropList(c)%Hc, cropList(c)%HarvNum_max, .false.)
            parSr = repeatPars(cropList(c)%Sr, cropList(c)%HarvNum_max, .false.)
            parKy = repeatPars(cropList(c)%Ky, cropList(c)%HarvNum_max, .false.)
            parCNvalue = repeatPars(cropList(c)%CNvalue, cropList(c)%HarvNum_max, .false.)
            parFc = repeatPars(cropList(c)%fc, cropList(c)%HarvNum_max, .false.)
            
            par_r_stress = repeatPars(cropList(c)%r_stress, cropList(c)%HarvNum_max, .false.)
            
            
            maxGDD = maxval(parGDD)
            
            ! select the period for sowing
            sowIndex = findSowingDate_v4(Tave,DoY,&
                                                            cropList(c)%SowingDate_min, &
                                                            cropList(c)%SowingDelay_max,&
                                                            cropList(c)%Tsowing,&
                                                            cropList(c)%Vern)
            ! select period for harvesting
            maxHarvestIndex = findharvestDate_v4(DoY,&
                                                cropList(c)%HarvestDate_max, &
                                                sowIndex)
            
            do i=1,size(sowIndex)
                ! "sow" and assign crop id
                s = sowIndex(i)
                e = maxHarvestIndex(i)
                
                if ((s == 0) .or. (e == 0)) then
                    exit
                end if
                
                
                if (e>nOfDays) then
                    print*,'maxHarvestIndex greater than maximum number of days'
                    e=nOfDays
                end if 
                ! TODO: check when s<e
                
                if (s>e) exit
                
                CALL printFun('Crop sowed at '//trim(intToStr(s)))
                CALL printFun('Crop harvest at '//trim(intToStr(e)))
                
                
                timeSpan = e-s+1
                !print*,'timeSpan = ', timeSpan
                
                ! set crop
                !print*,'s = ', s, ' e = ', e
                cropsOverYear(s:e) = cropList(c)%cropId
                cropInField(s:e) = 1
                
                ! mark previous day to make field free
                if (cropList(c)%CropsOverlap>0) then
                    cropInField(s-cropList(c)%CropsOverlap:s-1) = 0
                end if
                
                ! check if there is enough time from the previous crop
                if ((cropList(c)%CropsOverlap>0).and.(cropList(c)%CropsOverlap<s)) then
                    ! check id s is one
                    cropsOverYear(s-cropList(c)%CropsOverlap:s-1) = 0.
                end if
                
                ! make a subset of variable
                if (ALLOCATED(Tmax_sub) .eqv. .true.) then
                    ! clear arrays
                    deallocate(Tmax_sub,Tmin_sub,Tave_sub,DLH_sub, PF_sub,VF_sub)
                end if 
                
                allocate(Tmax_sub(timeSpan),Tmin_sub(timeSpan),Tave_sub(timeSpan),DLH_sub(timeSpan),&
                                VF_sub(timeSpan),PF_sub(timeSpan))

                Tmax_sub = Tmax(s:e)
                Tmin_sub = Tmin(s:e)
                Tave_sub = Tave(s:e)
                DLH_sub = DLH(s:e)
                
                VF_sub = 1.
                PF_sub =1
                
                call calculateGDD(Tmax_sub, Tmin_sub, &
                                            cropList(c)%Tdaybase, cropList(c)%Tcutoff, &
                                            T_GDD_sub)
                
                CALL printFun('Max GDD '//trim(realToStr(maxval(T_GDD_sub))))
            
                if (cropList(c)%Vern .eqv. .true.) then
                    CALL vernalization(Tave_sub, cropList(c)%Tv_min, cropList(c)%Tv_max,&
                                                cropList(c)%Vslope, cropList(c)%Vstart, &
                                                cropList(c)%Vend, cropList(c)%VFmin, &
                                                VF_sub)
                end if
                !print*, 'len(VF_sub) after', size(VF_sub)
            

                if (cropList(c)%ph_r >0) then
                    CALL photoperiod(DLH_sub, &
                                                cropList(c)%ph_r, cropList(c)%daylength_if, cropList(c)%daylength_ins,&
                                                PF_sub)
                end if
            
                !print*, 'len(PF_sub after)', size(PF_sub)
                
                ! Computes GDD considering both VF and PF
                T_GDD_corr_sub = T_GDD_sub * min(VF_sub, PF_sub)
                CALL printFun('Max GDD after VF, PF '//trim(realToStr(maxval(T_GDD_corr_sub))))
                
                GDD_cum_sub = cumsum(T_GDD_corr_sub)  ! PhD Thesis Anna Borghi: eq i-85 page 173
                
                CALL printFun('Max cum GDD '//trim(realToStr(maxval(GDD_cum_sub))))
                
                ! limit cumulative GDD to the maximum just before harvesting
                where (GDD_cum_sub>maxGDD)
                    GDD_cum_sub =maxGDD
                end where
                
                rows =  findloc(GDD_cum_sub, maxGDD)
                if (size(rows)>0) then
                    harvestGDDIdx = rows(1)
                    if (harvestGDDIdx>0) GDD_cum_sub(harvestGDDIdx+1:) = 0. ! after harvest (maxGDD obtained), set GDD to zero
                end if
                                
                CALL printFun('Max cum GDD after harvest '//trim(realToStr(maxval(GDD_cum_sub))))
                                
                T_GDD_corr(s:e)=GDD_cum_sub
                
                ! assign pars lai, hc,kcb, sr, cn
                ! TODO: ADD KY
                call printFun('*** Assign LAI ***')
                lai(s:e) = computeParamsDistro_v4(GDD_cum_sub, parGDD, parLAI,printFun)
                lai(s:e) = fillMissingL(lai(s:e))
                if ((s-1)>0) lai(s-1) = minval(parLAI)
                if ((e+1)<nOfDays) lai(e+1) = minval(parLAI)
                !print*, 'min lai after compute: ', minval(lai)
                
                call printFun('*** Assign Hc ***')
                hc(s:e) = computeParamsDistro_v4(GDD_cum_sub, parGDD, parHc,printFun)
                
                if ((s-1)>0) hc(s-1) = minval(parHc)
                if ((e+1)<nOfDays) hc(e+1) = minval(parHc)
                
                call printFun('*** Assign Kcb ***')
                kcb(s:e) = computeParamsDistro_v4(GDD_cum_sub, parGDD, parKcb,printFun)
                
                call printFun('*** Assign Adjusted Kcb ***')
                if (cropList(c)%adj_flag .eqv. .true.) then
                    adjKcb(s:e) = adjustKcb(kcb(s:e),RHmin(s:e), Wind(s:e), hc(s:e))
                else
                    adjKcb(s:e) = kcb(s:e)
                end if
                
                kcb(s:e) = fillMissingL(kcb(s:e))
                adjKcb(s:e) = fillMissingL(adjKcb(s:e))
                hc(s:e) = fillMissingL(hc(s:e))
                
                if ((s-1)>0) kcb(s-1) = minval(parKcb)
                if ((e+1)<nOfDays) kcb(e+1) = minval(parKcb)
                if ((s-1)>0) adjKcb(s-1) = minval(parKcb)
                if ((e+1)<nOfDays) adjKcb(e+1) = minval(parKcb)
                
                
                call printFun('*** Assign Sr ***')
                sr(s:e) = computeParamsDistro_v4(GDD_cum_sub, parGDD, parSr,printFun)
                sr(s:e) = fillMissingL(sr(s:e))
                if ((s-1)>0) sr(s-1) = minval(parSr)
                if ((e+1)<nOfDays) sr(e+1) = minval(parSr)
                
                call printFun('*** Assign Ky ***')
                ky(s:e) = computeParamsDistro_v4(GDD_cum_sub, parGDD, parKy,printFun)
                ky(s:e) = fillMissingL(ky(s:e))
                if ((s-1)>0) ky(s-1) = minval(parKy)
                if ((e+1)<nOfDays) ky(e+1) = minval(parKy)
                
                call printFun('*** Assign CN ***')
                cn(s:e) = computeParamsDistro_v4(GDD_cum_sub, parGDD, parCNvalue,printFun)
                cn(s:e) = fillMissingK(cn(s:e))
                if ((s-1)>0) cn(s-1) = minval(parCNvalue)
                if ((e+1)<nOfDays) cn(e+1) = minval(parCNvalue)

                call printFun('*** Assign fc ***')
                fc(s:e) = computeParamsDistro_v4(GDD_cum_sub, parGDD, parFc,printFun)
                fc(s:e) = fillMissingL(fc(s:e))
                if ((s-1)>0) fc(s-1) = minval(parFc)
                if ((e+1)<nOfDays) fc(e+1) = minval(parFc)
                
                call printFun('*** Assign r_stress ***')
                r_stress(s:e) = computeParamsDistro_v4(GDD_cum_sub, parGDD, par_r_stress,printFun)
                r_stress(s:e) = fillMissingL(r_stress(s:e))
                if ((s-1)>0) r_stress(s-1) = minval(par_r_stress)
                if ((e+1)<nOfDays) r_stress(e+1) = minval(par_r_stress)
                

            end do
            
            call printFun('### end crop ###')
        end do
        
    end subroutine

    
    subroutine processWS_v4(aWeatherStation,aCropSeqList, window,movMeanNum, &
                                        gddDistro,doyDistro,cropIds,checkFutureTemp, tollerance, vfactor, &
                                        laiDistro,hcDistro,kcbDistro,adjKcbDistro,srDistro,&
                                        kyDistro,cnDistro,fcDistro,r_stressDistro,&
                                        printFun)
        implicit none
            
        type(CropSeq),dimension(:), intent(in) :: aCropSeqList
        type(WeatherStation), intent(in) :: aWeatherStation
        integer, intent(in) :: movMeanNum
        Real(dp), dimension(:,:), intent(inout),allocatable :: gddDistro,doyDistro,cropIds ! for debug
        Real(dp), dimension(:,:), intent(inout),allocatable :: laiDistro,hcDistro,kcbDistro,adjKcbDistro,srDistro
        Real(dp), dimension(:,:), intent(inout),allocatable :: kyDistro, cnDistro, fcDistro, r_stressDistro
        integer, intent(inOUT) :: window
        logical, intent(in) :: checkFutureTemp
        real(dp), Intent(in) :: tollerance, vfactor
        integer :: i,j,n, nOfCropSeq,nOfDays,errorFlag
        
        integer, dimension(:), allocatable :: DoY,cropsOverYear,cropInField
        REAL(dp), dimension(:), allocatable :: T_GDD_corr, TmaxExt, TminExt, RHminExt, WindExt
        REAL(dp), dimension(:), allocatable :: lai, hc,kcb, adjKcb, sr, ky, cn, fc, r_stress
        LOGICAL:: debug
        
        type(date) :: extStartDate
        
        character(len=55) :: int_to_char
        
        
        interface
            subroutine printFun(text)
                character(len=*) :: text
            end subroutine
        end interface

        debug = .false.
        
        ! extend timeseries
        !call printFun('add tails ...')
        !print*, 'window1 = ', window
        call addTails_v4(aWeatherStation%Tmax,TmaxExt, window)
        call addTails_v4(aWeatherStation%Tmin,TminExt,window)
        call addTails_v4(aWeatherStation%Umin,RHminExt,window)
        call addTails_v4(aWeatherStation%Vmed,WindExt,window)
        
        !print*, 'window2 = ', window
        extStartDate = addDays(aWeatherStation%startDate,-window)
        
        !print*,'extStartDate = ',dateToStr(extStartDate)
        
        
        nOfCropSeq = size(aCropSeqList)
        nOfDays = size(TmaxExt)
        
        ! prepare space for outputs
            
        allocate(gddDistro(nOfDays,nOfCropSeq),&
                        doyDistro(nOfDays,nOfCropSeq),&
                        cropIds(nOfDays,nOfCropSeq),&
                        laiDistro(nOfDays,nOfCropSeq),&
                        hcDistro(nOfDays,nOfCropSeq),&
                        kcbDistro(nOfDays,nOfCropSeq),&
                        adjKcbDistro(nOfDays,nOfCropSeq),&
                        srDistro(nOfDays,nOfCropSeq),&
                        kyDistro(nOfDays,nOfCropSeq),&
                        cnDistro(nOfDays,nOfCropSeq),&
                        fcDistro(nOfDays,nOfCropSeq),&
                        r_stressDistro(nOfDays,nOfCropSeq))
                        
        gddDistro = 0.
        doyDistro = 0.
        cropIds = 0.
        laiDistro = nodatar
        hcDistro = nodatar
        kcbDistro = nodatar
        adjKcbDistro = nodatar
        srDistro = nodatar
        kyDistro = nodatar
        cnDistro = nodatar
        fcDistro = nodatar
        r_stressDistro = nodatar
        
        !print *, 'Max val CN distro 0: ',maxval(cnDistro)
        
        do j=1,size(aCropSeqList)
        
            !call printFun('compute crop seq ...')
            !~ call computeCropSeq_v5(aWeatherStation%wsLat, extStartDate,&
                                            !~ TmaxExt, TminExt,movMeanNum, &
                                            !~ aCropSeqList(j)%cropList,tollerance,vfactor,checkFutureTemp,&
                                            !~ DoY,cropsOverYear,T_GDD_corr,printFun)
                                            
            CALL computeCropSeq_v4(aWeatherStation%wsLat, extStartDate,&
                                                TmaxExt, TminExt,movMeanNum, &
                                                RHminExt, WindExt, &
                                                aCropSeqList(j)%cropList, &
                                                DoY,cropsOverYear,T_GDD_corr, cropInField, &
                                                lai, hc,kcb, adjKcb, sr, ky, cn, fc, r_stress, &
                                                printFun)
                                                
                                            
            gddDistro(:,j) = T_GDD_corr
            doyDistro(:,j) = DoY
            cropIds(:,j) = cropsOverYear
            laiDistro(:,j) = lai
            hcDistro(:,j) = hc
            kcbDistro(:,j) = kcb
            adjKcbDistro(:,j) = adjKcb
            srDistro(:,j) = sr
            kyDistro(:,j) = ky
            cnDistro(:,j) = cn
            fcDistro(:,j) = fc
            r_stressDistro(:,j) = r_stress
            
            !print *, 'Max val CN distro 1: ',maxval(cnDistro(:,j))
            
            ! complete series filling nodata with linear interpolation
            laiDistro(:,j) = fillMissingL(laiDistro(:,j))
            hcDistro(:,j) = fillMissingL(hcDistro(:,j))
            kcbDistro(:,j) = fillMissingL(kcbDistro(:,j))
            adjKcbDistro(:,j) = fillMissingL(adjKcbDistro(:,j))
            srDistro(:,j) = fillMissingL(srDistro(:,j))
            kyDistro(:,j) = fillMissingL(kyDistro(:,j))
            cnDistro(:,j) = fillMissingK(cnDistro(:,j))
            fcDistro(:,j) = fillMissingL(fcDistro(:,j))
            r_stressDistro(:,j) = fillMissingK(r_stressDistro(:,j))
            
            ! set empty array to zeros
            !print *, 'Max val CN distro 2: ',maxval(cnDistro(:,j))
            if (maxval(cnDistro(:,j))>2.) cnDistro(:,j)=0.
            !print *, 'Max val CN distro 3: ',maxval(cnDistro(:,j))
                        
            ! clean from overlapping crop
            gddDistro(:,j) = gddDistro(:,j)*cropInField
            cropIds(:,j) = cropIds(:,j)*cropInField
            laiDistro(:,j) = laiDistro(:,j)*cropInField
            hcDistro(:,j) = hcDistro(:,j)*cropInField
            kcbDistro(:,j) = kcbDistro(:,j)*cropInField
            adjKcbDistro(:,j) = adjKcbDistro(:,j)*cropInField
            srDistro(:,j) = srDistro(:,j)*cropInField
            kyDistro(:,j) = kyDistro(:,j)*cropInField
            cnDistro(:,j) = cnDistro(:,j)*cropInField
            r_stressDistro(:,j) = r_stressDistro(:,j)*cropInField ! maintain praw also to no crop period
            r_stressDistro(:,j) = merge(r_stressDistro(:,j),0.0D0,cropInField==1.)
            if (maxval(fcDistro(:,j))/=nodatar) then
                ! clean only if there are valid values (not nodatar)
                fcDistro(:,j) = fcDistro(:,j)*cropInField
            end if

        end do
        
    end subroutine
    
    function repeatPars(a,n,makeCum) result(r)
        real(dp), intent(in) :: a(:)
        integer, intent(in) :: n
        logical, intent(in) :: makeCum
        real(dp), allocatable :: r(:)
        integer :: i,s,e, nOfElem
        real :: valToAdd
        
        nOfElem = size(a, DIM=1)
        allocate(r(nOfElem*n))
        
        r = 0.
        valToAdd = 0.
        
        do i=1,n
            if (makeCum .eqv. .true.) then
                valToAdd = maxval(r, DIM = 1)
            end if
            s = 1+(i-1)*nOfElem
            e = s+nOfElem-1
            r(s:e) = a(1:nOfElem)+valToAdd
        end do
    
    end function
    
    subroutine calculateAdjWP(aCropSeqList,yearList, co2List, cropSeqIdList, r )
        implicit none
        type(CropSeq), intent(in), dimension(:) :: aCropSeqList
        integer, dimension(:), intent(in) :: yearList
        real(dp), dimension(:), intent(in) :: co2List
        integer,dimension(:),intent(out), allocatable :: cropSeqIdList
        real(dp),dimension(:,:),intent(out), allocatable :: r
        
        type(CropSeq) :: aCropSeq
        Real(dp), dimension (:), allocatable :: adjWP
        integer :: i,j,y,c,z, nOfCrops, nOfCropSeq, maxNumCropList,nOfYear
        integer,dimension(:), allocatable :: tempCropSeqIdList
        
        nOfCropSeq = size(aCropSeqList,1) 
        maxNumCropList = 2
        nOfYear = size(yearList)
        
        allocate(adjWP(nOfCropSeq*maxNumCropList*nOfYear))
        allocate(tempCropSeqIdList(nOfCropSeq*maxNumCropList))
        
        c = 0
        z = 0
        do i=1, nOfCropSeq 
            aCropSeq = aCropSeqList(i)
            do j=1, size(aCropSeq%cropList,1)
                z = z+1
                tempCropSeqIdList(z) = aCropSeq%cropSeqId
                do y=1, nOfYear
                    c = c+1
                    !print*, aCropSeq%cropList(j)%WP,co2List(y),aCropSeq%cropList(j)%fsink
                    adjWP(c) = adjustWaterProductivity(aCropSeq%cropList(j)%WP,co2List(y),aCropSeq%cropList(j)%fsink)
                end do
            end do
        end do
        
        ! prepare outputs
        nOfCrops = int(c/nOfYear)
        !print*, 'nOfCrops: ', nOfCrops
        !print*, 'z: ', z
        
        allocate(r(nOfYear,nOfCrops))
        do i=1,nOfCrops
            r(:,i) = adjWP(1+(i*nOfYear)-nOfYear:(i*nOfYear))
        end do
        
        Allocate(cropSeqIdList(z))
        do i=1, z
            cropSeqIdList(i) = tempCropSeqIdList(i)
        end do

    end subroutine

    
end module

!~ program testadjustKcb
    !~ use mod_cropcoef
    !~ implicit none
    
    !~ !real(dp), dimension(8) :: a = (/1.,2.,3.,-9999.,-9999.,-9999.,10.,11./)
    !~ real(dp), dimension(10) :: Kcb = (/-9999.,0.2,-9999.,0.7,-9999.,-9999.,0.7,-9999.,-9999.,0.6/)
    !~ real(dp), dimension(10) :: RHmin = 40.
    !~ real(dp), dimension(10) :: Wind = 4.
    !~ real(dp), dimension(10) :: hc = 0.5
    
    !~ real(dp), dimension(:), allocatable :: r
    !~ integer :: i
    
    !~ r = adjustKcb(Kcb,RHmin, Wind, hc)
    
    !~ print*
    !~ print*, 'i    Kcb    RHmin    Wind    hc r'
    !~ do i=1,size(Kcb)
        !~ print*,i,Kcb(i),RHmin(i), Wind(i), hc(i),r(i)
    !~ end do
    
!~ end program

!~ program test_findSowingDate_v4
    !~ use mod_cropcoef
    !~ implicit none
    
    !~ real(dp), dimension(20) :: Tave = (/1.,2.,5.,7.,12.,15.,13.,7.,5.,1.,1.,2.,5.,7.,9.,9.,9.,7.,5.,1./)
    !~ integer, dimension(20) :: DoY = (/1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10/)
    !~ integer :: minSowingDate = 2
    !~ integer :: sowingDelay = 5
    !~ integer :: maxHarvestDate = 2
    !~ real(dp) :: T_sowing = 10
    !~ logical :: isWinterCrop = .false.
    
    !~ integer, dimension(:), allocatable :: s, h
    !~ integer :: i, j, sow, harvest
    
    
    !~ s  = findSowingDate_v4(Tave,DoY,minSowingDate,sowingDelay,T_sowing,isWinterCrop)
    
    !~ h = findharvestDate_v4(DoY, maxHarvestDate, s)
    
    !~ print*,'minSowingDate = ', minSowingDate
    !~ print*,'sowingDelay = ', sowingDelay
    !~ print*,'maxHarvestDate = ', maxHarvestDate
    !~ print*,'T_sowing = ', T_sowing
    !~ print*,'isWinterCrop = ', isWinterCrop
    !~ print*,'size s = ', size(s)
    !~ print*,'size h = ', size(h)
    
    !~ print*
    !~ print*, 'i    Tave    DoY    sow[0/1]    harvest[0/1]'
    !~ do i=1,size(Tave)
        !~ sow=0
        !~ harvest = 0
        !~ do j=1,size(s)
            !~ if (i == s(j)) sow = 1
        !~ end do
        !~ do j=1,size(h)
            !~ if (i == h(j)) harvest = 1
        !~ end do
        !~ print*,i,Tave(i),DoY(i),sow, harvest
    !~ end do
    
!~ end program


