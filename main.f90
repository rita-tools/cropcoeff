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

program main
	USE mod_settings
	use mod_io_file
	USE mod_cropcoef_v4
	USE mod_crop
	USE mod_utilities

	implicit none
	
	!logical, parameter :: Debug = .false. ! If true all debug messages are printed
	logical :: verbose ! If true all debug messages are printed

	integer ::  ErrorFlag! To check for errors
	
	type(Settings) :: Sim 
	TYPE(WeatherStation), dimension(:), allocatable :: aWsList
	TYPE(CropSeq), dimension(:), allocatable :: aCropSeqList
	Real(dp), dimension(:,:), allocatable :: doyDistro, gddDistro,cropIds, errorCodes
	Real(dp), dimension(:,:), allocatable :: laiDistro,hcDistro,kcbDistro,adjKcbDistro,srDistro
	Real(dp), dimension(:,:), allocatable :: kyDistro, cnDistro, fcDistro
	Real(dp), dimension(:,:), allocatable :: prawDistro
	
	integer, dimension(:,:), allocatable :: cropIdsInt, cnDistroInt, doyDistroInt
	
	integer,dimension(:),allocatable :: yearList, cropSeqIdList
	real(dp),dimension(:),allocatable :: co2List,canResList
	real(dp),dimension(:,:),allocatable :: adjWP
		
	CHARACTER(len=maxlength) :: arg, settingsFileName, outPath
	character(len=55) :: int_to_char
	integer :: i, j, r, c, nOfCropSeq, window, nOfWS
	Integer :: selStart, selEnd
	character :: delimiter
	character(len=10) :: makedir
	logical :: dir_exists
		
	! Inizialize 
	ErrorFlag = 0
	verbose = .false.
	settingsFileName = 'cropcoef.txt'

	! without indentation
	
	delimiter = '\' ! for win, update this if compile under other OS
	makedir = 'md'

!~ #ifdef _WIN32
!~ delimiter = '\' ! for win, update this if compile under other OS
!~ makedir = 'md'
!~ #elif __unix__
!~ delimiter = '/'
!~ makedir = 'mkdir'
!~ #endif

	! print*,'Using delimeter:',delimiter
	! print*,'Using make dir command:',makedir
	! Upload default values for directories and files
	call makeDefault(Sim)
	
	! get option
	DO i = 1, iargc()
		CALL getarg(i, arg)
		if (arg(1:1)=='-') then
			call lower_case(arg)
			! is an option !
			select case (arg)
				case ('-verbose', '-v')
					! set debug mode
					verbose = .true.
				case ('-file', '-f')
					CALL getarg(i+1, arg)
					settingsFileName = arg
				case ('-help', '-h')
					! show help string
					call show_help()
				case default ! all other cases ... !
					print *, 'Not supported option <',trim(arg),'>'
					call show_help()
			end select
		end if
	END DO

	
	! Read Settings
	CALL read_sim_file( settingsFileName, Sim, ErrorFlag, verbose)
	if (verbose .eqv. .true.) CALL printSim(Sim)
	
	! Fix crop parameters path
	! check if the dir exists or make a new dir
	inquire(file=trim(Sim%crop_folder), exist=dir_exists)   ! dir_exists will be TRUE if the directory exists
	if (dir_exists .eqv. .false.) then
		! replace with custom directory 
		Sim%crop_folder = trim(Sim%soiluses_folder)//delimiter//'crop_parameters'
	end if

	
	! Make outdir if doesn't exist
	
	! check if the dir exists or make a new dir
	inquire(file=trim(Sim%pheno_outpath), exist=dir_exists)   ! dir_exists will be TRUE if the directory exists
	if (dir_exists .eqv. .true.) then
		print *,'The directory ', trim(Sim%pheno_outpath), ' already exists and will be updated'
	else
		print*,'Create folder ',trim(adjustl(Sim%pheno_outpath))
		call system(trim(makedir)//" "//trim(Sim%pheno_outpath))!
	end if
	
	
	! Read meteo data
	CALL read_weather_station_list(sim%ws_filename, aWsList, ErrorFlag, verbose)
	
	! read co2 values
	CALL read_CO2(sim%CO2_filename,yearList, co2List, errorFlag, verbose )
		
	! Read landuses
	CALL read_soil_uses(trim(Sim%soiluses_folder)//delimiter//trim(Sim%soiluses_filename),&
									aCropSeqList, ErrorFlag, verbose)
	
	nOfCropSeq = size(aCropSeqList)
	
	!update max printing length
	! 10 = n. of  digits, 2 = max n. of crops, nOfCropseq + 1 = n. of cropseq + labels
	max_plen = 10*2*(nOfCropSeq+1)
	
	!print*, 'n of crop seqs ', nOfCropSeq
	! read crop parameters
	do i=1, nOfCropSeq
		do j=1, size(aCropSeqList(i)%cropList)
			call read_crop_par(trim(Sim%crop_folder) //delimiter &
									 //trim(adjustl(aCropSeqList(i)%cropList(j)%fileName)),&
											aCropSeqList(i)%cropList(j), errorFlag, verbose)
											
			if (verbose .eqv. .true.) CALL print_crop_par(aCropSeqList(i)%cropList(j))
			
			end do
	end do
	
	! save canopy resistance
	allocate(canResList(size(co2List)))
	canResList = 70.0 ! default value
	do i=1, size(co2List)
		canResList(i) = computeCanopyResistance(co2List(i))
		!print*,'canResList(i) = ' , canResList(i)
	end do
	
	CALL writeCanResistance(trim(sim%pheno_outpath)//delimiter//trim(sim%canres_filename),yearList, canResList)
	
	nOfWS = size(aWsList,dim=1)
	! start loop to run crop coef for each station
	do i=1,nOfWS
		print*, 'processing ', trim(adjustl(aWsList(i)%fileName))
		aWsList(i)%fileName=trim(Sim%meteo_path)//delimiter//trim(aWsList(i)%fileName)
		! read weather station
		call read_meteo_data(aWsList(i), errorFlag, verbose)
		
		if (verbose .eqv. .true.) CALL print_meteo_data(aWsList(i))
	
		! Make outdir if doesn't exist
		write(int_to_char,*) aWsList(i)%wsId
		
		
		
		outPath = trim(Sim%pheno_outpath)//delimiter//trim(Sim%pheno_root)//trim(adjustl(int_to_char))
		
		
		! check if the dir exists or make a new dir
		inquire(file=trim(outPath), exist=dir_exists)   ! dir_exists will be TRUE if the directory exists
		if (dir_exists .eqv. .true.) then
			print *,'The directory ', trim(outPath), ' already exists and will be updated'
		else
			print*,'Create folder ',trim(adjustl(outPath))
			!print *,trim(makedir)//" "//trim(outPath)
			call system(trim(makedir)//" "//trim(outPath))!
		end if
		
		! open log file
		if (Sim%debug .eqv. .true.) then
			call openLogFile(trim(adjustl(outPath))//delimiter//'cropcoef.log')
		end if
		
		CALL processWS_v4(aWsList(i), aCropSeqList, Sim%window, Sim%movMeanNum, &
									gddDistro,doyDistro,cropIds, Sim%checkFutureTemp, Sim%tollerance, Sim%vfactor, &
									laiDistro,hcDistro,kcbDistro,adjKcbDistro,srDistro,kyDistro, cnDistro, fcDistro, prawDistro, &
									addLog)
								
		selStart =  Sim%window+1 !lbound(doyDistro, DIM = 1)
		selEnd =  aWsList(i)%numOfDays+Sim%window !ubound(doyDistro, DIM = 1)
		
		! write outputs
		print*, 'write outputs'
		allocate(cropIdsInt(size(gddDistro, 1),size(gddDistro, 2)), &
						cnDistroInt(size(gddDistro, 1),size(gddDistro, 2)), &
						doyDistroInt(size(gddDistro, 1),size(gddDistro, 2)))
		! cast array
		do r=1, size(gddDistro, 1)
			do c = 1, size(gddDistro, 2)
				cropIdsInt(r,c) = int(cropIds(r,c))
				cnDistroInt(r,c) = int(cnDistro(r,c))
				doyDistroInt(r,c) = int(doyDistro(r,c))
			end do
		end do
		
		! for debug
		if (Sim%debug .eqv. .true.) then
			call writeRealResults(trim(adjustl(outPath))//delimiter//'GDD.dat',&
										gddDistro, selStart,selEnd, realFormat, ErrorFlag)
			call writeIntResults(trim(adjustl(outPath))//delimiter//'doy.dat',&
										doyDistroInt, selStart,selEnd, intFormat, ErrorFlag)
			call writeIntResults(trim(adjustl(outPath))//delimiter//'cropId.dat',&
										cropIdsInt, selStart,selEnd, intFormat, ErrorFlag)
			call writeRealResults(trim(adjustl(outPath))//delimiter//'Kcb_plain.dat',&
										kcbDistro, selStart,selEnd, realFormat, ErrorFlag)
		end if
		
		! useful outputs
		call writeRealResults(trim(adjustl(outPath))//delimiter//'LAI.dat',&
									laiDistro, selStart,selEnd, realFormat, ErrorFlag)
		call writeRealResults(trim(adjustl(outPath))//delimiter//'H.dat',&
									hcDistro, selStart,selEnd, realFormat, ErrorFlag)
		call writeRealResults(trim(adjustl(outPath))//delimiter//'Kcb.dat',&
									adjKcbDistro, selStart,selEnd, realFormat, ErrorFlag)
		call writeRealResults(trim(adjustl(outPath))//delimiter//'Sr.dat',&
									srDistro, selStart,selEnd, realFormat, ErrorFlag)
		call writeIntResults(trim(adjustl(outPath))//delimiter//'CNvalue.dat',&
									cnDistroInt, selStart,selEnd, intFormat, ErrorFlag)
		call writeRealResults(trim(adjustl(outPath))//delimiter//'Ky.dat',&
									kyDistro, selStart,selEnd, realFormat, ErrorFlag)
		call writeRealResults(trim(adjustl(outPath))//delimiter//'fc.dat',&
							fcDistro, selStart,selEnd, realFormat, ErrorFlag)
		call writeRealResults(trim(adjustl(outPath))//delimiter//'praw.dat',&
							prawDistro, selStart,selEnd, realFormat, ErrorFlag)
	
		deallocate(gddDistro,&
							doyDistro, &
							cropIds, &
							laiDistro,&
							hcDistro,&
							kcbDistro,&
							adjKcbDistro,&
							srDistro,&
							kyDistro,&
							fcDistro,&
							cnDistro,&
							cnDistroInt, &
							doyDistroInt, &
							cropIdsInt, &
							prawDistro)
		
		! write crop parameters
		CALL writeCropPars(trim(adjustl(outPath))//delimiter//'CropParam.dat', aCropSeqList)
		
		! write adjusted wp
		call calculateAdjWP(aCropSeqList,yearList, co2List, cropSeqIdList,adjWP)
		CALL writeAdjWP(trim(adjustl(outPath))//delimiter//'WPadj.dat',yearList, cropSeqIdList, adjWP)
		
		CALL closeLogFile()
		
		print*, 'progress ', trim(adjustl(intToStr(int(100*i/nOfWS))))
									
	end do
	
	
end program

subroutine show_help()
	print *, '===== Crop_coeff v. 5.0 ====='
	print *, 'List of options:'
	print *, '-h, -help: return some helpfull information'
	print *, '-v, -verbose: print all outputs'
	print *, '-f, -file: set the filename of the simulation parameters'
	print *, '====================='
	! stop execution to prevent errors !
	STOP
end subroutine
