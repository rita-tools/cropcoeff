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

module mod_io_file
    use mod_utilities
    USE mod_settings
    USE mod_crop
    USE mod_datetime
    use mod_weather_station
    USE mod_cropseq
    use mod_productivity
    implicit none
    
    integer::log_unit = -1
    

    contains
    
    ! read simulation file
    subroutine read_sim_file( fileName, aSimSettings, ErrorFlag, Debug)
        implicit none
        character(len=*), intent(in) :: fileName ! Name of the file containing useful information about simulation
        type(Settings), intent(inOUT) :: aSimSettings
        integer, intent(out) :: ErrorFlag
        logical, optional, intent(in) :: Debug

        integer :: i, j, l
        integer :: ios, free_unit, checkstat, eof
        
        character(LEN=maxlength) :: rubbish ! To read and (eventually) throw away char
        character(len=maxlength) :: buffer, label
        integer :: line = 0
        integer :: p
        
        ErrorFlag = 0 ! 0 if no error occours
        
        call SeekUN( ErrorFlag, free_unit) !Look for a free unit
        open( unit = free_unit, file = trim(fileName), status = 'old', action="read", iostat = ios )
        if( ios /= 0 ) then 
            CALL printMessage(0,"mod_io_file","read_sim_file","Error opening file:",trim(fileName))
        end if
        
        do while (ios == 0)
            read (free_unit, '(A)', iostat=ios) buffer
            if (ios == 0) then
                line = line + 1
                call replaceChar(buffer, achar(9),' ')
                buffer = removeRepChar(buffer,' ')
                CALL splitString(buffer,  '#', buffer, rubbish)
                CALL splitString(buffer,  '=', label, buffer)
                call lower_case(label)
                
                label = trim(label) ! clear whitespaces
                buffer = trim(buffer) ! clear whitespaces
                !print *,trim(label), ' len: ',len(label),' len_trim: ',len_trim(label)
                                
                select case (label)
                    case('weathstatfilename')
                        aSimSettings%ws_filename = trim(ADJUSTL(buffer))
                    case('meteodatafolder')
                        aSimSettings%meteo_path = trim(ADJUSTL(buffer))
                    CASE('cropinputsfolder')
                        aSimSettings%soiluses_folder = trim(ADJUSTL(buffer))
                    CASE('outputfolder')
                        aSimSettings%pheno_outpath = trim(ADJUSTL(buffer))
                    CASE('soilusesfilename')
                        aSimSettings%soiluses_filename = trim(ADJUSTL(buffer))
                    CASE('cropfolder')
                        aSimSettings%crop_folder = trim(ADJUSTL(buffer))
                    CASE('phenoroot')
                        aSimSettings%pheno_root = trim(ADJUSTL(buffer))
                    CASE('canopyresmod')
                        aSimSettings%CanopyResMod = strToInt(buffer)
                    CASE('checkfuturetemp')
                        aSimSettings%checkFutureTemp = strToBool(buffer)
                    CASE('tollerance')
                        aSimSettings%tollerance = strToReal(buffer)
                    CASE('vfactor')
                        aSimSettings%vfactor = strToReal(buffer)
                    CASE('debug')
                        aSimSettings%debug = strToBool(buffer)
                    CASE('window')
                        aSimSettings%window = strToInt(buffer)
                    CASE('movmeannum')
                        aSimSettings%movMeanNum = strToInt(buffer)
                    CASE('')
                        write (buffer, "(I2)") line
                    CASE DEFAULT
                        CALL printMessage(2,"mod_io_file","read_sim_file","Unrecognized variable:",label)
                end select
            end if
        end do
        
        close(free_unit)
    end subroutine
    
    subroutine printSim(aSimSettings)
        implicit none
        type(Settings), intent(in) :: aSimSettings
        
        print *
        print *, " *** SIMULATION SETTINGS ***"
        
        print *, 'Path to meteo timeseries tables: ',trim(aSimSettings%meteo_path)
        print *, 'Filename of weater stations list: ',trim(aSimSettings%ws_filename)
        print *, 'Path to the soiluses files: ',trim(aSimSettings%soiluses_folder)
        print *, 'Filename of soil uses list: ',trim(aSimSettings%soiluses_filename)
        print *, 'Path to the crop parameters files: ',trim(aSimSettings%crop_folder)
        print *, 'Path where output phenophases files are stored: ',trim(aSimSettings%pheno_outpath)
        print *, 'A common string to use as prefix in output folder: ',trim(aSimSettings%pheno_root)
        print *, 'Print values of canopy resistance (o/1): ',aSimSettings%CanopyResMod
        
        print *
        
    end subroutine
    
    ! read land use list file
    subroutine read_soil_uses( fileName, aCropSeqList, ErrorFlag, Debug)
        implicit none
        character(len=*), intent(in) :: fileName ! Name of the file containing useful information about simulation
        type(CropSeq), intent(inOUT), dimension(:), Allocatable :: aCropSeqList
        type(CropSeq) :: aCropSeq
        integer, intent(out) :: ErrorFlag
        logical, optional, intent(in) :: Debug

        integer :: i, j, l
        integer :: ios, free_unit, checkstat, eof
        type(Crop):: crop1,crop2
        
        character(LEN=maxlength) :: rubbish ! To read and (eventually) throw away char
        character(len=maxlength) :: buffer, label
        integer :: ncols
        integer :: line
        integer :: p
        Integer::landUseId
        character(len=maxlength):: firstCropFN, secondCropFN
        
        ncols = -1
        line = 0
        ErrorFlag = 0 ! 0 if no error occours
        
        call SeekUN( ErrorFlag, free_unit) !Look for a free unit
        open( unit = free_unit, file = trim(fileName), status = 'old', action="read", iostat = ios )
        if( ios /= 0 ) then
            CALL printMessage(0,"mod_io_file","read_soil_uses","Error opening file:",trim(fileName))
        end if
        
        do while (ios == 0)
            read (free_unit, '(A)', iostat=ios) buffer
            if (ios == 0) then
                line = line + 1
                call replaceChar(buffer, achar(9),' ')
                buffer = removeRepChar(buffer,' ')
                CALL splitString(buffer,  '#', buffer, rubbish)
                CALL splitString(buffer,  '=', label, buffer)
                call lower_case(label)
                
                label = trim(label) ! clear whitespaces
                buffer = trim(buffer) ! clear whitespaces
                ! check ent of table
                if  (label =='endtable') then
                    ! stop reading table
                    !print *,'stop table'
                    ncols=-1
                end if

                !print *,'after edit: ',buffer
                
                if (ncols==-1) then
                    !print *,trim(label), ' len: ',len(label),' len_trim: ',len_trim(label)
                    select case (label)
                        case('cr_id crop1 crop2')
                            ncols = 3
                            i=1
                        CASE('endtable')
                            ncols =-1
                        CASE('')
                            write (buffer, "(I2)") line
                        CASE DEFAULT
                            CALL printMessage(2,"mod_io_file","read_crop_par","Unrecognized variable:",label)
                    end select
                elseif (ncols == 3) then ! get cols
                    !print *,'read row 3xcol: ',trim(label)
                    buffer = trim(replaceText(label,'*','NaN'))
                    read(buffer,*,iostat=ios) landUseId, firstCropFN, secondCropFN
                    ! populate sequence
                    
                    aCropSeq%cropSeqId = landUseId
                    crop1%cropId = 1
                    crop1%fileName = firstCropFN
                    call addCrop(aCropSeq, crop1)
                    !print*,'OK add crop 1'
                    if (secondCropFN /='NaN') then
                        crop2%cropId = 2
                        crop2%fileName = secondCropFN
                        call addCrop(aCropSeq, crop2)
                        !print*,'OK add crop 2'
                    end if
                    i = addCropSeq(aCropSeqList, aCropSeq)
                    !print*,'OK add crop sequence'
                    CALL clearCropList(aCropSeq)
                    !print*, 'i =',i, size(aCropSeqList)
                end if
            end if
        end do
        
        close(free_unit)
        
    end subroutine read_soil_uses
    
    ! read_crop parameters file 
    subroutine read_crop_par(parFilePath, aCrop, errorFlag, debug)
        ! This subroutine reads from the file "cropname.tab" info to inizialize
        ! an element of the array "crops"
        ! IN: aCrop -> An element of type <crop>. Info read are putted here
        ! OUT: ErrorFlag -> 0 if no error occours
        ! IN: Debug -> Useful to print debug info
        implicit none
        CHARACTER(len=*) :: parFilePath
        type(Crop), intent(inout) :: aCrop
        integer, intent(out) :: errorFlag
        logical, optional, intent(in) :: debug

        integer :: i, free_unit, ios, ie, checkstat ! To iter anc  check for errors
        character(LEN=maxlength) :: rubbish ! To read and (eventually) throw away char
        character(len=maxlength) :: buffer, label
        integer :: ncols
        integer :: line
        integer :: p
        
        integer, dimension(:), allocatable :: maxIdx
        
        ncols = -1
        line = 0
        errorFlag = 0
    
        call SeekUN( ErrorFlag, free_unit) !Look for a free unit
        open ( unit = free_unit, file = trim(parFilePath), &
                & status = 'old', action="read", iostat = ios )
        if( ios /= 0 ) then ! Check for reading errors
            errorFlag = 1
            call printMessage(errorFlag,"mod_io_file","read_crop_par","Error opening file",&
                    & parFilePath)
        end if
        
        p = scan(parFilePath, '/',.true.)
        aCrop%cropName = parFilePath(p+1:)
        p = scan(aCrop%cropName, '.',.true.)
        aCrop%cropName = aCrop%cropName(1:p-1)
        aCrop%fileName = parFilePath
        
        allocate(aCrop%GDD(maxlength), aCrop%Kcb(maxlength), aCrop%LAI(maxlength), aCrop%Hc(maxlength), &
                        aCrop%Sr(maxlength),aCrop%Ky(maxlength), aCrop%CNvalue(maxlength), aCrop%fc(maxlength),&
                        aCrop%r_stress(maxlength))
                        
        ! init variable
        aCrop%r_stress = nodatar
        
        do while (ios == 0)
            read (free_unit, '(A)', iostat=ios) buffer
            if (ios == 0) then
                line = line + 1
                call replaceChar(buffer, achar(9),' ')
                buffer = removeRepChar(buffer,' ')
                CALL splitString(buffer,  '#', buffer, rubbish)
                CALL splitString(buffer,  '=', label, buffer)
                call lower_case(label)
                
                label = trim(label) ! clear whitespaces
                buffer = trim(buffer) ! clear whitespaces
                ! check ent of table
                if  (label =='endtable') then
                    ! stop reading table
                    !print *,'stop table'
                    ncols=-1
                end if

                !print *,'after edit: ',buffer
                
                if (ncols==-1) then
                    !print *,trim(label), ' len: ',len(label),' len_trim: ',len_trim(label)
                    select case (label)
                        case('sowingdate_min')
                            aCrop%SowingDate_min = strToInt(buffer)
                        case('sowingdelay_max')
                            aCrop%SowingDelay_max = strToInt(buffer)
                        case('harvestdate_max')
                            aCrop%HarvestDate_max = strToInt(buffer)
                        case('harvnum_max')
                            aCrop%HarvNum_max = strToInt(buffer)
                        case('cropsoverlap')
                            aCrop%CropsOverlap = strToInt(buffer)
                        case('tsowing')
                            aCrop%Tsowing = strToReal(buffer)
                        case('tdaybase')
                            aCrop%Tdaybase = strToReal(buffer)
                        case('tcutoff')
                            aCrop%Tcutoff = strToReal(buffer)
                        case('vern')
                            aCrop%Vern = strToBool(buffer)
                        case('tv_min')
                            aCrop%Tv_min = strToReal(buffer)
                        case('tv_max')
                            aCrop%Tv_max = strToReal(buffer)
                        case('vfmin')
                            aCrop%VFmin = strToReal(buffer)
                        case('vstart')
                            aCrop%Vstart = strToInt(buffer)
                        case('vend')
                            aCrop%Vend = strToInt(buffer)
                        case('vslope')
                            aCrop%Vslope = strToReal(buffer)
                        case('ph_r')
                            aCrop%ph_r = strToInt(buffer)
                        case('daylength_if')
                            aCrop%daylength_if = strToInt(buffer)
                        case('daylength_ins')
                            aCrop%daylength_ins = strToInt(buffer)
                        case('wp')
                            aCrop%WP = strToReal(buffer)
                        case('fsink')
                            aCrop%fsink = strToReal(buffer)
                        case('tcrit_hs')
                            aCrop%Tcrit_HS = strToReal(buffer)
                        case('tlim_hs')
                            aCrop%Tlim_HS = strToReal(buffer)
                        case('hi')
                            aCrop%HI = strToReal(buffer)
                        case('kyt')
                            aCrop%kyT = strToReal(buffer)
                        case('ky1')
                            aCrop%ky1 = strToReal(buffer)
                        case('ky2')
                            aCrop%ky2 = strToReal(buffer)
                        case('ky3')
                            aCrop%ky3 = strToReal(buffer)
                        case('ky4')
                            aCrop%ky4 = strToReal(buffer)
                        case('praw')
                            buffer = trim(replaceText(buffer,'*',trim(realToStr(nodatar))))
                            read(buffer,*,iostat=ie) aCrop%pRaw
                        case('ainterception')
                            aCrop%aInterception = strToReal(buffer)
                        case('cl_cn')
                            aCrop%cl_CN = strToInt(buffer)
                        case('irrigation')
                            aCrop%Irrigation = strToBool(buffer)
                        case('adj_flag')
                            aCrop%adj_flag = strToBool(buffer)
                        !case('ke') ![obsolete]
                        !    aCrop%Ke = strToReal(buffer)
                        !case('kt') ![obsolete]
                        !    aCrop%Kt = strToReal(buffer)
                        case('rft')
                            aCrop%RFt = strToReal(buffer)
                        case('gdd kcb lai hc sr')
                            ! start reading 5xcols table
                            !print *,'init table 5'
                            ncols = 5
                            i=1
                        case('gdd kcb lai hc sr cn')
                            ! start reading 6xcols table
                            !print *,'init table 6'
                            ncols = 6
                            i=1
                        case('gdd kcb lai hc sr cn fc')
                            ! start reading 7xcols table
                            !print *,'init table 7'
                            ncols = 7
                            i=1
                        case('gdd kcb lai hc sr cn fc r_stress')
                            ! start reading 8xcols table
                            !print *,'init table 8'
                            ncols = 8
                            i=1
                        case('gdd kcb lai hc sr cn fc r_stress ky')
                            ! start reading 9xcols table
                            ncols = 9
                            i=1
                        CASE('endtable')
                            i=i-1 ! update row counter
                            ncols = -1
                        CASE('')
                            write (buffer, "(I2)") line
                        CASE DEFAULT
                            CALL printMessage(2,"mod_io_file","read_crop_par","Unrecognized variable:",label)
                    end select
                elseif (ncols == 5) then
                    !print *,'read row 5',trim(label)
                    buffer = trim(replaceText(label,'*',trim(realToStr(nodatar))))
                    read(buffer,*,iostat=ie) aCrop%GDD(i), aCrop%Kcb(i), aCrop%LAI(i), aCrop%Hc(i), aCrop%Sr(i)
                    aCrop%CNvalue(i) = nodatar
                    aCrop%fc(i) = nodatar
                    aCrop%r_stress(i) = 0.0D0
                    aCrop%Ky(i) = nodatar
                    i=i+1
                ELSEif (ncols == 6) then
                    !print *,'read row 6'
                    buffer = trim(replaceText(label,'*',trim(realToStr(nodatar))))
                    read(buffer,*) aCrop%GDD(i), aCrop%Kcb(i), aCrop%LAI(i), aCrop%Hc(i), aCrop%Sr(i),aCrop%CNvalue(i)
                    aCrop%fc(i) = nodatar
                    aCrop%r_stress(i) = 0.0D0
                    aCrop%Ky(i) = nodatar
                    i=i+1
                ELSEif (ncols == 7) then
                    !print *,'read row 7'
                    buffer = trim(replaceText(label,'*',trim(realToStr(nodatar))))
                    read(buffer,*) aCrop%GDD(i), aCrop%Kcb(i), aCrop%LAI(i), aCrop%Hc(i), aCrop%Sr(i),aCrop%CNvalue(i),aCrop%fc(i)
                    aCrop%r_stress(i) = 0.0D0
                    aCrop%Ky(i) = nodatar
                    i=i+1
                ELSEif (ncols == 8) then
                    !print *,'read row 8'
                    buffer = trim(replaceText(label,'*',trim(realToStr(nodatar))))
                    read(buffer,*) aCrop%GDD(i), aCrop%Kcb(i), aCrop%LAI(i), aCrop%Hc(i), aCrop%Sr(i),&
                                            aCrop%CNvalue(i),&
                                            aCrop%fc(i),&
                                            aCrop%r_stress(i)
                    aCrop%Ky(i) = nodatar
                    i=i+1
                ELSEif (ncols == 9) then
                        !print *,'read row 8'
                        buffer = trim(replaceText(label,'*',trim(realToStr(nodatar))))
                        read(buffer,*) aCrop%GDD(i), aCrop%Kcb(i), aCrop%LAI(i), aCrop%Hc(i), aCrop%Sr(i),&
                                                aCrop%CNvalue(i),&
                                                aCrop%fc(i),&
                                                aCrop%r_stress(i),&
                                                aCrop%Ky(i)
                        i=i+1 
                end if
            end if
        end do
        
        close(free_unit)
                
        ! resize parameters array
        call resizeArray(aCrop%GDD,i)
        call resizeArray(aCrop%Kcb,i)
        call resizeArray(aCrop%LAI,i)
        call resizeArray(aCrop%Hc,i)
        call resizeArray(aCrop%Sr,i)
        call resizeArray(aCrop%Ky,i)
        call resizeArray(aCrop%CNvalue,i)
        call resizeArray(aCrop%fc,i)
        
        CALL resizeArray(aCrop%r_stress,i)
        
        ! replace nodata with interpolated values
        aCrop%Kcb = fillMissingL(aCrop%Kcb, aCrop%GDD)
        aCrop%LAI = fillMissingL(aCrop%LAI, aCrop%GDD)
        aCrop%Hc = fillMissingL(aCrop%Hc, aCrop%GDD)
        aCrop%Sr = fillMissingL(aCrop%Sr, aCrop%GDD)
        aCrop%Ky = fillMissingL(aCrop%Ky, aCrop%GDD)
        aCrop%fc = fillMissingL(aCrop%fc, aCrop%GDD)
        
        aCrop%r_stress = fillMissingL(aCrop%r_stress) ! use linear because plant stress resistance can gradually change according to the maturity
        
        ! adjust CN
        do i=1, size(aCrop%Kcb)
            if (aCrop%CNvalue(i) == nodatar) then
                aCrop%CNvalue(i) = 1.
                if (aCrop%Kcb(i) == 0.) aCrop%CNvalue(i) = 0.
                if (aCrop%Kcb(i) >= 0.45) aCrop%CNvalue(i) = 2.
            end if
        end do
        
        ! TODO: check if this method is correct
        aCrop%Ky(1) = aCrop%ky1 ! start phase
        aCrop%Ky(size(aCrop%Ky)) = aCrop%ky4  ! ending phase
        do i=1, size(aCrop%Kcb)
            if (aCrop%Ky(i) == nodatar) then
                if (aCrop%Kcb(i) < 0.45) aCrop%Ky(i) = aCrop%ky2 ! in the low-middle development
                if (aCrop%Kcb(i) >= 0.45) aCrop%Ky(i) = aCrop%ky3 ! in the high-middle development
            end if
        end do
        
        ! DON'T adjust cf: negative value says to idragra to calculate it according to the formula
                
    end subroutine read_crop_par


    subroutine print_crop_par(aCrop)
        ! This subroutine prints on stdout useful debug info 
        ! about subroutine "read_cultivation"
        ! IN: cult -> Elem. of the array cultivations. Contais info to be printed.
        ! OUT: ErrorFlag -> 0 if no error occours
        implicit none
        type(crop), intent(in) :: aCrop
        
        integer :: i

        print *
        print *, " *** CROP PARAMETERS ***"
        print *, 'Crop denomination: ', aCrop%cropName
        print *, 'Name of the data-base file that contains crop characteristics: ', aCrop%fileName
        print *, 'unique id identification ', aCrop%cropId
        print *, 'minimum sowing date (1-366)', aCrop%SowingDate_min
        print *, 'maximum number of days allowed for sowing after SowingDate_min (1-366)', aCrop%SowingDelay_max
        print *, 'maximum harvest date (1-366)', aCrop%HarvestDate_max
        print *, 'maximum number of harvest/cuts per the year', aCrop%HarvNum_max
        print *, 'minimum number of days between two subsequent crops in case of double cropping', aCrop%CropsOverlap
        print *
        print *, 'minimum sowing temperature [�C]', aCrop%Tsowing 
        print *, 'minimum temperature for crop growth [�C]', aCrop%Tdaybase 
        print *, 'maximum temperature for crop growth [�C]', aCrop%Tcutoff 
        print *
        print *, ' response to vernalization [1=Yes, 0=No]', aCrop%Vern
        print *
        print *, 'minimum temperature for optimal vernalization [�C]', aCrop%Tv_min 
        print *, 'maximum temperature for optimal vernalization [�C]', aCrop%Tv_max 
        print *, 'vernalization factor at the beginning of the vernalization process [-]', aCrop%VFmin 
        print *
        print *, 'number of days required for vernalization to start', aCrop%Vstart
        print *, 'number of days required for vernalization to end', aCrop%Vend
        print *, 'vernalization curve parameter', aCrop%Vslope
        print *
        print *, 'photoperiod impact [0=Day-neutral plants, 1=Long-day plants, 2=Short-day plants]', aCrop%ph_r
        print *, 'day length threshold below (above) for long-day (short-day) crops', aCrop%daylength_if
        print *, 'day length threshold above (below)  for long-day (short-day) crops', aCrop%daylength_ins
        print *, 'biomass water productivity [t/ha]', aCrop%WP
        print *, 'crop sink strength coefficient', aCrop%fsink
        print *, 'critical temperature threshold for heat stress [�C] ', aCrop%Tcrit_HS
        print *, 'limit temperature threshold for heat stress[�C] ', aCrop%Tlim_HS
        print *, 'harvest index', aCrop%HI
        print *
        print *, 'water stress coefficient for the overall crop growth cycle ', aCrop%kyT
        print *, 'water stress coefficient for the ini stage ', aCrop%ky1
        print *, 'water stress coefficient for the dev stage ', aCrop%ky2
        print *, 'water stress coefficient for the mid stage', aCrop%ky3
        print *, 'water stress coefficient for the end stage', aCrop%ky4
        print *, 'parameter to compute RAW', aCrop%pRAW
        print *, 'parameter to calculate interception', aCrop%aInterception
        print *
        print *, 'CN class', aCrop%cl_CN
        print *, 'irrigation (T = Yes, F = No)', aCrop%Irrigation
        print *, 'compute kcb correction flag (T = Yes, F = No)', aCrop%adj_flag
        !print *, 'fraction of root in the evaporative layer', aCrop%Ke ![obsolete]
        !print *, 'fraction of root in the transpirative layer', aCrop%Kt ![obsolete]
        print *, 'fraction of root in the transpirative layer', aCrop%RFt

        print *
        print *, "i GDD  Kcb LAI Hc Sr Ky cn fc r_stress"
        do i=1, size(aCrop%GDD)
           print *, i, aCrop%GDD(i), aCrop%Kcb(i), aCrop%LAI(i), aCrop%Hc(i), aCrop%Sr(i),&
                            aCrop%Ky(i),aCrop%CNvalue(i),aCrop%fc(i),aCrop%r_stress(i)
        end do
        
        print*
        
    end subroutine print_crop_par

    
    ! read weather station list
    subroutine read_weather_station_list(filename, aWsList, ErrorFlag, Debug)
        implicit none
        CHARACTER(len=*) :: filename
        type(WeatherStation), intent(inout), dimension(:), allocatable :: aWsList
        
        integer, intent(out) :: errorFlag
        logical, optional, intent(in) :: debug
        type(WeatherStation) :: aWS
        
        integer :: i, r, free_unit, ios, ie, checkstat,nOfWs ! To iter anc  check for errors
        character(LEN=maxlength) :: rubbish ! To read and (eventually) throw away char
        character(len=maxlength) :: buffer, label,firstPart, secondPart
        integer :: ncols
        integer :: line
        integer :: p
        
        ncols = -1
        line = 0
        errorFlag = 0
        
        call SeekUN( ErrorFlag, free_unit) !Look for a free unit
        open ( unit = free_unit, file = trim(filename), &
                & status = 'old', action="read", iostat = ios )
        if( ios /= 0 ) then ! Check for reading errors
            errorFlag = 1
            call printMessage(errorFlag,"mod_io_file","read_crop_par","Error opening file",&
                    & filename)
        end if
        
        do while (ios == 0)
            read (free_unit, '(A)', iostat=ios) buffer
            if (ios == 0) then
                line = line + 1
                call replaceChar(buffer, achar(9),' ')
                buffer = removeRepChar(buffer,' ')
                CALL splitString(buffer,  '#', buffer, rubbish)
                CALL splitString(buffer,  '=', label, buffer)
                call lower_case(label)
                
                label = trim(label) ! clear whitespaces
                buffer = trim(buffer) ! clear whitespaces
                ! check ent of table
                if  (label =='endtable') then
                    ! stop reading table
                    !print *,'stop table'
                    ncols=-1
                end if

                !print *,'after edit: ',buffer
                
                if (ncols==-1) then
                    !print *,trim(label), ' len: ',len(label),' len_trim: ',len_trim(label)
                    select case (label)
                        case('statnum')
                            nOfWs = strToInt(buffer)
                        case('table')
                            ncols = 3
                            i = 1
                        CASE('')
                            write (buffer, "(I2)") line
                            !CALL printMessage(2,"mod_io_file","read_crop_par","Empty variable at line:",buffer)
                        CASE DEFAULT
                            CALL printMessage(2,"mod_io_file","read_crop_par","Unrecognized variable:",label)
                    end select
                elseif (ncols == 3) then ! get cols
                    if (i>1) then
                        !print *,'read row 5',trim(label)
                        CALL splitString(label,  ' ',firstPart, secondPart)
                        aWS%fileName = trim(firstPart)
                        ! append weather stations
                        r = addWeatherStation(aWsList, aWS)
                        if (r==nOfWs) then
                            ncols = -1
                        end if
                    end if
                    i=i+1
                    
                ELSE !
                    !print *,'read row 6'
                end if
            end if
        end do
        
        close(free_unit)
                
    end subroutine
        
    ! read temp data
    subroutine read_meteo_data(aWeatherStation, ErrorFlag, Debug)
        ! This subroutine reads metereological informations from the N_Station files
        ! with names stored in File_station_names, compute mean values of Tmin and Tmax
        ! and returns them in the array SowingYears
        ! IN: N_stations -> Number of the metereological stations considered for the simulation(topoieti)
        !     File_station_names -> Array containing names of the files containing meteo info
        !     Debug -> Optional value, useful to print various debug info...
        ! OUT: SowingYears -> Here will be stored computed mean temperature max and min
        !      ErrorFlag -> 0 if no error occours
        type(WeatherStation), intent(inOUT) :: aWeatherStation ! Info about simulation are stored here
        integer, intent(out) :: ErrorFlag
        logical, optional, intent(in) :: Debug
        character(LEN=maxlength) :: rubbish, rubbish2 ! To read and (eventually) throw away char
        character(len=maxlength) :: firstPart, secondPart
        type(date) :: startDate, endDate
        integer :: totNumDays
        
        integer :: i, j ! for iter
        integer :: ios, ReadError ! To check reading errors
        integer :: free_unit ! To read from files
        
        ErrorFlag = 0 ! 0 if no error occours
                
        ! Read all metereological data
        call SeekUN( ErrorFlag, free_unit) ! Look for a free unit
        
        ! Open the file containing climatic data
        open ( unit = free_unit, file = trim(aWeatherStation%fileName), status = 'old', &
            & action="read", iostat = ios )
        if( ios /= 0 ) then 
            CALL printMessage(1,"mod_io_file","read_meteo_data","Unable to open file:", trim(aWeatherStation%fileName))
        end if
        
        ! Read Id and weather station name
        read( free_unit, '(A)') rubbish
        call splitString(rubbish,  ',', firstPart, secondPart)
        rubbish = firstPart
        rubbish2 = secondPart
        call splitString(rubbish,  ':', firstPart, secondPart)
        aWeatherStation%wsId = strToInt(secondPart)
        call splitString(rubbish2,  ':', firstPart, secondPart)
        aWeatherStation%wsName = secondPart
        
        ! Read latitude and altitude
        read( free_unit, '(A)') rubbish
        !print*, '0: ', rubbish
        read( rubbish,*) aWeatherStation%wsLat, aWeatherStation%wsAlt
        !print*, '0b: ', aWeatherStation%wsLat, aWeatherStation%wsAlt
        
        ! read the line with start and end dates
        read( free_unit, '(A)') rubbish
        
        call splitString(rubbish,  '->', firstPart, secondPart)
        !print*, '2: ', firstPart
        !print*, '3: ', secondPart
        
        CALL strToDate(firstPart, startDate)
        CALL strToDate(secondPart, endDate)
        aWeatherStation%startDate = startDate
        aWeatherStation%endDate = endDate
        aWeatherStation%numOfDays = calcNumDays(startDate, endDate)
        
        ! read and throw away the table header
        ! T_max   T_min   P_tot   U_max   U_min   V_med   RG_CORR
        read( free_unit,*)
        
        ! init weather parameters
        CALL initVarArray(aWeatherStation)
        
        ! start reading weather data rows
        do i = 1, aWeatherStation%numOfDays
            read( unit=free_unit, FMT=*, iostat=ReadError) aWeatherStation%Tmax(i), aWeatherStation%Tmin(i), &
                                                                                        & aWeatherStation%Ptot(i), &
                                                                                        & aWeatherStation%Umax(i), aWeatherStation%Umin(i), &
                                                                                        & aWeatherStation%Vmed(i), aWeatherStation%RGCorr(i)
            if (ReadError /= 0) exit ! Error o end of file
        end do
        
        close(free_unit)
        
    end subroutine read_meteo_data
    
    subroutine print_meteo_data(aWeatherStation)
        ! This subroutine prints on stdout useful debug info 
        ! about subroutine "read_cultivation"
        ! IN: cult -> Elem. of the array cultivations. Contais info to be printed.
        ! OUT: ErrorFlag -> 0 if no error occours
        implicit none
        type(WeatherStation), intent(in) :: aWeatherStation
        
        integer :: i
        
        print *
        print *, " *** WEATHER STATION ***"
        print *, 'WS denomination: ', aWeatherStation%wsName
        print *, 'Input filename: ', aWeatherStation%fileName
        print *, 'WS unique id: ', aWeatherStation%wsId
        print *, 'Altitude: ', aWeatherStation%wsAlt
        print *, 'Longitude: ', aWeatherStation%wsLat
        print *, 'numOfDays: ', aWeatherStation%numOfDays
        print *, 'from: ', dateToStr(aWeatherStation%startDate)
        print *, 'to: ', dateToStr(aWeatherStation%endDate)
        PRINT *
        print *, 'variable',achar(9),'min',achar(9),'mean',achar(9),'max'
        print *, 'Tmax',achar(9),trim(adjustl(realToStr(minval(aWeatherStation%Tmax)))),&
                    achar(9),trim(adjustl(realToStr(mean(aWeatherStation%Tmax)))),&
                    achar(9),trim(adjustl(realToStr(maxval(aWeatherStation%Tmax))))
                    
        print *, 'Tmin',achar(9),trim(adjustl(realToStr(minval(aWeatherStation%Tmin)))),&
                    achar(9),trim(adjustl(realToStr(mean(aWeatherStation%Tmin)))),&
                    achar(9),trim(adjustl(realToStr(maxval(aWeatherStation%Tmin))))

        print *, 'Ptot',achar(9),trim(adjustl(realToStr(minval(aWeatherStation%Ptot)))),&
                    achar(9),trim(adjustl(realToStr(mean(aWeatherStation%Ptot)))),&
                    achar(9),trim(adjustl(realToStr(maxval(aWeatherStation%Ptot))))
        
        print *, 'Umax',achar(9),trim(adjustl(realToStr(minval(aWeatherStation%Umax)))),&
                    achar(9),trim(adjustl(realToStr(mean(aWeatherStation%Umax)))),&
                    achar(9),trim(adjustl(realToStr(maxval(aWeatherStation%Umax))))
        
        print *, 'Umin',achar(9),trim(adjustl(realToStr(minval(aWeatherStation%Umin)))),&
                    achar(9),trim(adjustl(realToStr(mean(aWeatherStation%Umin)))),&
                    achar(9),trim(adjustl(realToStr(maxval(aWeatherStation%Umin))))

        print *, 'Vmed',achar(9),trim(adjustl(realToStr(minval(aWeatherStation%Vmed)))),&
                    achar(9),trim(adjustl(realToStr(mean(aWeatherStation%Vmed)))),&
                    achar(9),trim(adjustl(realToStr(maxval(aWeatherStation%Vmed))))
                    
        print *, 'RGCorr',achar(9),trim(adjustl(realToStr(minval(aWeatherStation%RGCorr)))),&
                    achar(9),trim(adjustl(realToStr(mean(aWeatherStation%RGCorr)))),&
                    achar(9),trim(adjustl(realToStr(maxval(aWeatherStation%RGCorr))))
        
        print *

    end subroutine print_meteo_data

    
    subroutine printMessage(errorFlag,moduleName,funName,label,description)
        ! Print messages in standard output.
        ! errorFlag: error number for which message must be printed 0=debug, 1=warning, 2=error
        ! moduleName: the name of module that generated the message
        ! funName: the name of function/subroutine that generated the message
        ! label: shirt name of the message
        ! description: long description of the message

        implicit none

        integer  :: errorFlag
        character(LEN=*) :: moduleName
        character(LEN=*) :: funName
        character(LEN=*) :: label
        character(LEN=*) :: description
        character(LEN=maxlength) :: msgType = "Debug"

        select case (errorFlag)
            case  (0) ! blocking error
                msgType = "Error"
            case  (1) ! warning
                msgType = "Warning"
        end select

        print *
        print *, trim(msgType), " in ",trim(moduleName)," - ",trim(funName)
        print *, trim(label)
        print *, trim(description)

    end subroutine


    subroutine printToConsole(text)
        ! Print messages in standard output.

        implicit none
        character(len=*) :: text 
        print *, text

    end subroutine
    
    subroutine openLogFile(filename)
        implicit none
        character(len=*), intent(in) :: filename
        integer :: ErrorFlag, free_unit, ios, i
        
        call SeekUN( ErrorFlag, free_unit) !Look for a free unit
        log_unit = free_unit
        
        open( unit = free_unit, file = trim(filename), status = 'replace', action="write", iostat = ios )
        if( ios /= 0 ) then 
            call printMessage(0,'mod_cropcoef','writeResults',' Error opening file',trim(filename))
            log_unit = -1
        end if
    
    end subroutine
    
    subroutine closeLogFile()
        if (log_unit>0) then
            close(log_unit)
        end if
    end subroutine
    
    subroutine addLog(text)
        ! Print messages in standard output.

        implicit none
        character(len=*) :: text 
        if (log_unit>0) then
            write(log_unit, "(a)", advance="YES") trim(adjustl(text))
        end if
        
    end subroutine
        
    subroutine writeRealResults( filename,resArray2d, fromRow, toRow, printFormat, ErrorFlag)
        implicit none
        character(len=*), intent(in) :: filename
        real(dp), dimension(:,:), intent(in) :: resArray2d
        character(len=*), intent(in) :: printFormat
        integer :: fromRow, toRow,ErrorFlag
        
        integer :: free_unit, ios, i
        character(len=55) :: int_to_char
        !printFormat = '(*(f10.3))'
        
        fromRow = max(1,fromRow)
        toRow = min(toRow,size(resArray2d,1) )
        
        ErrorFlag = 0

        call SeekUN( ErrorFlag, free_unit) !Look for a free unit
        
        !print*, 'filename= ',filename

        open( unit = free_unit, file = trim(filename), status = 'replace', action="write", iostat = ios )
        if( ios /= 0 ) then 
            call printMessage(0,'mod_cropcoef','writeResults',' Error opening file',trim(filename))
        end if

        do i=1, size(resArray2d,2)
            write(int_to_char,*) i
            write(free_unit, "(a)", advance="no") " CrID_"//trim(adjustl(int_to_char))
        end do
        write(free_unit,*)

        do i=fromRow,toRow
            write(free_unit,printFormat) resArray2d(i,:)
        end do

        close(free_unit)

    end subroutine
    
    subroutine writeIntResults( filename,resArray2d, fromRow, toRow, printFormat, ErrorFlag)
        implicit none
        character(len=*), intent(in) :: filename
        integer, dimension(:,:), intent(in) :: resArray2d
        character(len=*), intent(in) :: printFormat
        integer :: fromRow, toRow,ErrorFlag
        
        integer :: free_unit, ios, i
        character(len=55) :: int_to_char
        !printFormat = '(*(f10.3))'
        
        fromRow = max(1,fromRow)
        toRow = min(toRow,size(resArray2d,1) )
        
        ErrorFlag = 0

        call SeekUN( ErrorFlag, free_unit) !Look for a free unit
        
        !print*, 'filename= ',filename

        open( unit = free_unit, file = trim(filename), status = 'replace', action="write", iostat = ios )
        if( ios /= 0 ) then 
            call printMessage(0,'mod_cropcoef','writeResults',' Error opening file',trim(filename))
        end if

        do i=1, size(resArray2d,2)
            write(int_to_char,*) i
            write(free_unit, "(a)", advance="no") " CrID_"//trim(adjustl(int_to_char))
        end do
        write(free_unit,*)

        do i=fromRow,toRow 
            write(free_unit,*) resArray2d(i,:)
        end do

        close(free_unit)

    end subroutine
    
    subroutine writeCropPars(filename, aCropSeqList)
        implicit none
        character(len=*), intent(in) :: filename
        type(CropSeq), intent(in), dimension(:) :: aCropSeqList
        type(CropSeq) :: aCropSeq
        integer :: ErrorFlag
        
        integer :: free_unit, ios, i,j
        
        character(len=max_plen) :: varStr
        character(len=max_plen) :: irrigStr
        character(len=max_plen) :: CNclassStr
        character(len=max_plen) :: pRAWStr
        character(len=max_plen) :: aIntStr
        character(len=max_plen) :: TlimStr
        character(len=max_plen) :: TcritStr
        character(len=max_plen) :: HIStr
        character(len=max_plen) :: kyTStr
        character(len=max_plen) :: ky1Str
        character(len=max_plen) :: ky2Str
        character(len=max_plen) :: ky3Str
        character(len=max_plen) :: ky4Str
        !character(len=max_plen) :: keStr ![obsolete]
        !character(len=max_plen) :: ktStr ![obsolete]
        character(len=max_plen) :: rftStr
        
        ErrorFlag = 0

        call SeekUN( ErrorFlag, free_unit) !Look for a free unit
        
        !print*, 'filename= ',filename

        open( unit = free_unit, file = trim(filename), status = 'replace', action="write", iostat = ios )
        if( ios /= 0 ) then 
            call printMessage(0,'mod_cropcoef','writeResults',' Error opening file',trim(filename))
        end if
        
        varStr = 'Var'
        irrigStr = 'Irrig'
        CNclassStr = 'CNclass'
        pRAWStr = 'pRAW'
        aIntStr = 'aInt'
        TlimStr = 'Tlim'
        TcritStr = 'Tcrit'
        HIStr = 'HI'
        kyTStr = 'kyT'
        ky1Str = 'ky1'
        ky2Str = 'ky2'
        ky3Str = 'ky3'
        ky4Str = 'ky4'
        !keStr = 'Ke' ![obsolete]
        !ktStr = 'Kt' ![obsolete]
        rftStr = 'RFt'
        
        do i=1, size(aCropSeqList,1)
            aCropSeq = aCropSeqList(i)
            do j=1, size(aCropSeq%cropList,1)
                varStr = trim(adjustl(varStr))//achar(9)//'CrID_'//trim(adjustl(intToStr(aCropSeq%cropSeqId)))
                irrigStr = trim(adjustl(irrigStr))//achar(9)//trim(adjustl(boolToStr(aCropSeq%cropList(j)%Irrigation)))
                CNclassStr = trim(adjustl(CNclassStr))//achar(9)//trim(adjustl(intToStr(aCropSeq%cropList(j)%cl_CN)))
                pRAWStr = trim(adjustl(pRAWStr))//achar(9)//trim(adjustl(realToStr(aCropSeq%cropList(j)%pRAW)))
                aIntStr = trim(adjustl(aIntStr))//achar(9)//trim(adjustl(realToStr(aCropSeq%cropList(j)%aInterception)))
                TlimStr = trim(adjustl(TlimStr))//achar(9)//trim(adjustl(realToStr(aCropSeq%cropList(j)%Tlim_HS)))
                TcritStr = trim(adjustl(TcritStr))//achar(9)//trim(adjustl(realToStr(aCropSeq%cropList(j)%Tcrit_HS)))
                HIStr = trim(adjustl(HIStr))//achar(9)//trim(adjustl(realToStr(aCropSeq%cropList(j)%HI)))
                kyTStr = trim(adjustl(kyTStr))//achar(9)//trim(adjustl(realToStr(aCropSeq%cropList(j)%kyT)))
                ky1Str = trim(adjustl(ky1Str))//achar(9)//trim(adjustl(realToStr(aCropSeq%cropList(j)%ky1)))
                ky2Str = trim(adjustl(ky2Str))//achar(9)//trim(adjustl(realToStr(aCropSeq%cropList(j)%ky2)))
                ky3Str = trim(adjustl(ky3Str))//achar(9)//trim(adjustl(realToStr(aCropSeq%cropList(j)%ky3)))
                ky4Str = trim(adjustl(ky4Str))//achar(9)//trim(adjustl(realToStr(aCropSeq%cropList(j)%ky4)))
                !keStr = trim(adjustl(keStr))//achar(9)//trim(adjustl(realToStr(aCropSeq%cropList(j)%Ke))) ![obsolete]
                !ktStr = trim(adjustl(ktStr))//achar(9)//trim(adjustl(realToStr(aCropSeq%cropList(j)%Kt))) ![obsolete]
                rftStr = trim(adjustl(rftStr))//achar(9)//trim(adjustl(realToStr(aCropSeq%cropList(j)%RFt)))
            end do
        end do
        
        write(free_unit,'(a)') trim(adjustl(varStr))//achar(32)
        write(free_unit,'(a)') trim(adjustl(irrigStr))//achar(9)
        write(free_unit,'(a)') trim(adjustl(CNclassStr))//achar(9)
        write(free_unit,'(a)') trim(adjustl(pRAWStr))//achar(9)
        write(free_unit,'(a)') trim(adjustl(aIntStr))//achar(9)
        write(free_unit,'(a)') trim(adjustl(TlimStr))//achar(9)
        write(free_unit,'(a)') trim(adjustl(TcritStr))//achar(9)
        write(free_unit,'(a)') trim(adjustl(HIStr))//achar(9)
        write(free_unit,'(a)') trim(adjustl(kyTStr))//achar(9)
        write(free_unit,'(a)') trim(adjustl(ky1Str))//achar(9)
        write(free_unit,'(a)') trim(adjustl(ky2Str))//achar(9)
        write(free_unit,'(a)') trim(adjustl(ky3Str))//achar(9)
        write(free_unit,'(a)') trim(adjustl(ky4Str))//achar(9)
        !write(free_unit,'(a)') trim(adjustl(keStr))//achar(9)
        !write(free_unit,'(a)') trim(adjustl(ktStr))//achar(9)
        write(free_unit,'(a)') trim(adjustl(rftStr))//achar(9)

        close(free_unit)
        
    end subroutine
    
    subroutine read_CO2(filename,yearList, co2List, errorFlag, debug )
        implicit none
        CHARACTER(len=*) :: filename
        integer, intent(inout), dimension(:), allocatable :: yearList
        real(DP), intent(inout), dimension(:), allocatable :: co2List
        
        integer, intent(out) :: errorFlag
        logical, optional, intent(in) :: debug
        
        integer :: i, r, free_unit, ios, ie, checkstat,nOfWs ! To iter anc  check for errors
        character(LEN=maxlength) :: rubbish ! To read and (eventually) throw away char
        character(len=maxlength) :: buffer, label,firstPart, secondPart
        integer :: ncols
        integer :: line
        integer :: p
        
        ncols = -1
        line = 0
        
        errorFlag = 0
        
        call SeekUN( ErrorFlag, free_unit) !Look for a free unit
        open ( unit = free_unit, file = trim(filename), &
                & status = 'old', action="read", iostat = ios )
        if( ios /= 0 ) then ! Check for reading errors
            errorFlag = 1
            call printMessage(errorFlag,"mod_io_file","read_CO2","Error opening file",&
                    & filename)
        end if
                
        do while (ios == 0)
            read (free_unit, '(A)', iostat=ios) buffer
            if (ios == 0) then
                line = line + 1
                !print *,'orig. buffer: ',buffer
                call replaceChar(buffer, achar(9),' ')
                buffer = removeRepChar(buffer,' ')
                CALL splitString(buffer,  '#', buffer, rubbish)
                CALL splitString(buffer,  '=', label, buffer)
                call lower_case(label)
                
                label = trim(adjustl(label)) ! clear whitespaces
                buffer = trim(adjustl(buffer)) ! clear whitespaces
                ! check ent of table
                if  (label =='endtable') then
                    ! stop reading table
                    ncols=-1
                end if
                
                if (ncols==-1) then
                    !print *,trim(label), ' len: ',len(label),' len_trim: ',len_trim(label)
                    select case (label)
                        case('year co2')
                            !print*, 'year co2', ncols
                            ncols = 2
                        CASE('')
                            write (buffer, "(I2)") line
                            !CALL printMessage(2,"mod_io_file","read_crop_par","Empty variable at line:",buffer)
                        CASE DEFAULT
                            CALL printMessage(2,"mod_io_file","read_CO2","Unrecognized variable:",label)
                    end select
                elseif (ncols == 2) then ! get cols
                        CALL splitString(label,  ' ', firstPart, secondPart)
                        ! append to output array
                        i = addInt(yearList, strToInt(firstPart))
                        i = addReal(co2List, strToReal(secondPart))
                ELSE !
                    !print *,'read row 6'
                end if
            end if
        end do
        
        close(free_unit)
                
    end subroutine
    
    
    subroutine writeAdjWP(filename, yearList, cropSeqIdList,  adjWP)
        implicit none
        character(len=*), intent(in) :: filename
        integer, dimension(:), intent(in) :: yearList, cropSeqIdList
        real(dp), dimension(:,:), intent(in) :: adjWP
        integer :: ErrorFlag
        integer :: free_unit, ios, i,j,y
        
        character(len=max_plen) :: varStr, adjWpStr

        ErrorFlag = 0

        call SeekUN( ErrorFlag, free_unit) !Look for a free unit
        
        !print*, 'filename= ',filename

        open( unit = free_unit, file = trim(filename), status = 'replace', action="write", iostat = ios )
        if( ios /= 0 ) then 
            call printMessage(0,'mod_io_file','writeAdjWP',' Error opening file',trim(filename))
        end if
        
        adjWpStr = ''
        varStr = 'Year'
        
        do y=0, size(yearList)
            if (y > 0) then
                adjWpStr = intToStr(yearList(y))
                !print*, y, 'adjWpStr: ',adjWpStr
            end if
            do j=1, size(cropSeqIdList)
                if (y>0) then
                    adjWpStr = trim(adjustl(adjWpStr))//achar(9)//trim(adjustl(realToStr(adjWP(y,j))))                        
                else
                    varStr = trim(adjustl(varStr))//achar(9)//'CrID_'//trim(adjustl(intToStr(cropSeqIdList(j))))
                end if
            end do
            
            if (y>0) then
                write(free_unit,'(a)') trim(adjustl(adjWpStr))//achar(32)
            else
                write(free_unit,'(a)') trim(adjustl(varStr))//achar(9)
            end if
        end do

        close(free_unit)
        
    end subroutine
    
    subroutine writeCanResistance(filename, yearList, canResList)
        implicit none
        character(len=*), intent(in) :: filename
        integer, dimension(:), intent(in) :: yearList
        real(dp), dimension(:), intent(in) :: canResList
        integer :: ErrorFlag
        
        integer :: free_unit, ios, y
        character(len=max_plen):: str
        
        ErrorFlag = 0

        call SeekUN( ErrorFlag, free_unit) !Look for a free unit
        
        open( unit = free_unit, file = trim(filename), status = 'replace', action="write", iostat = ios )
        if( ios /= 0 ) then 
            call printMessage(0,'mod_io_file','writeCanResistance',' Error opening file',trim(filename))
        end if
        
        str = 'Year'//achar(32)//'CanRes'
        write(free_unit,'(a)') trim(adjustl(str))
        
        do y=1, size(yearList)
            !print*, 'canResList(y) = ',realToStr(canResList(y))
            str = trim(adjustl(intToStr(yearList(y))))//achar(32)//trim(adjustl(realToStr(canResList(y))))
            write(free_unit,'(a)') trim(adjustl(str))
        end do

        close(free_unit)
        
    end subroutine


end module mod_io_file

!~ program test1
    
    !~ use mod_io_file ! Include module containing useful subroutines
    
    !~ implicit none
    
    !~ type(WeatherStation) :: aWeatherStation
    !~ integer :: errorFlag = -1
    !~ logical :: debug = .false.
    
    !~ aWeatherStation%fileName = './example/100.dat'
    !~ call read_meteo_data(aWeatherStation, errorFlag, debug)
    !~ call print_meteo_data(aWeatherStation, errorFlag)
    

    !~ print *, " <enter> to continue"
    !~ read *    
!~ end program test1

!~ program test2
    
    !~ use mod_io_file ! Include module containing useful subroutines
    
    !~ implicit none
    !~ character(len=maxlength) :: filename
    !~ integer,dimension(:),allocatable :: yearList
    !~ real(dp),dimension(:),allocatable :: co2List
    !~ integer :: errorFlag = -1
    !~ logical :: debug = .false.
    !~ integer :: i
    
    !~ filename = './CO2_conc.dat'
    !~ CALL read_CO2(trim(adjustl(filename)),yearList, co2List, errorFlag, debug )
    
    !~ do i=1, size(yearList)
        !~ print*,i,yearList(i),co2List(i)
    !~ end do
    
!~ end program test2
