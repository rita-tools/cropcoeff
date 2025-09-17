module mod_et0
    use mod_utilities, only: sp, dp, pi
    use mod_weather_station
    use mod_cropseq
    
    implicit none
    
    contains

    subroutine calc_et0(aWeatherStation,aCropSeqList,can_resistance,et0Distro)
        type(WeatherStation), intent(in) :: aWeatherStation
        type(CropSeq),dimension(:), intent(in) :: aCropSeqList
        real(dp),intent(in) :: can_resistance

        Real(dp), dimension(:,:), intent(inout), allocatable :: et0Distro ! for debug

        Integer, dimension(:), allocatable :: nOfDay
        
        integer :: nOfCropSeq, nOfDays, j

        nOfCropSeq = size(aCropSeqList)
        nOfDays = size(aWeatherStation%Tmax)

        allocate(et0Distro(nOfDays,nOfCropSeq))
        et0Distro = nodatar

        call calculateDoY(aWeatherStation%startDate, nOfDays, nOfDay)

        do j=1,nOfCropSeq
            et0Distro(:,j) = ET_reference_mat(aWeatherStation%Tmax,aWeatherStation%Tmin,&
                                              & aWeatherStation%Umax,aWeatherStation%Umin,&
                                              & aWeatherStation%Vmed,aWeatherStation%RGCorr,&
                                              & aWeatherStation%wsLat,aWeatherStation%wsAlt,&
                                              & can_resistance,nOfDay,&
                                              & nOfDays)
        end do
    
    end subroutine

    function ET_reference_mat(T_max, T_min, HUM_max, HUM_min, Wind_vel, Rad_sol, lat_ws, alt_ws, res_surf, doy, n_day)!
        ! calculate refeterence evapotranspiration considering the elevation
        ! see FAO-56
        ! Note: matrix mode
        integer,intent(in):: n_day ! day of the year
        real(dp),dimension(:),intent(in)::T_max,T_min,HUM_max,HUM_min,Wind_vel,Rad_sol
        real(dp), intent(in)::lat_ws,alt_ws
        real(dp), intent(in)::res_surf
        integer, dimension(:), intent(in) :: doy
        ! real(dp),parameter::pi=3.141592653589793238462643383279502884197_sp!
        real(dp),parameter::cost_solare=0.0820 ! solar contant [MJ m-2 min-1]
        real(dp),parameter::as=0.25, bs=0.50   ! Other constants
        real(dp),parameter::alpha=0.23  ! albedo [-] = 0.23 for grass reference crop!
        real(dp),parameter::sigma=4.903E-9  ! Stefan-Boltzmann constant [MJ K-4 m-2 day-1]
        !!
        real(dp),dimension(n_day)::T_ave,press_atm,gamma
        real(dp),dimension(n_day)::SVP_max,SVP_min,SVP_ave  ! saturation vapour pressure at T_max, T_min and average
        real(dp),dimension(n_day)::delta_T,delta
        real(dp),dimension(n_day)::VP_act ! actual vapour pressure
        real(dp),dimension(n_day)::alat,DR,DL,omegaS,RA_T,RA,RSO,RNS,RNL_T1,RNL_T2,RNL,RN
        real(dp),dimension(n_day)::W1,W2,W3,ET0_T
        !!
        real(dp),dimension(n_day)::ET_reference_mat!
        !!
        ! (a) Calculate atmospheric parameters
        T_ave=(T_max+T_min)/2.!
        press_atm=101.3*((293-0.0065*alt_ws)/293)**5.26 ! atmospheric pressure at alt_ws m a.s.l. [kPa] !
        gamma=0.665E-3*press_atm ! psycometric const [kPa °C-1]!
        SVP_max=0.6108*EXP(17.27*T_max/(T_max+237.3)) ! saturation vapour pressure at T_max [kPa]!
        SVP_min=0.6108*EXP(17.27*T_min/(T_min+237.3)) ! saturation vapour pressure at T_min [kPa]!
        SVP_ave=(SVP_max+SVP_min)/2. ! daily average saturation vapour pressure [kPa]!
        delta_T=4098*(0.6108*EXP(17.27*T_ave/(T_ave+237.3)))  ! numerator of slope of saturation vapour pressure curve
        delta=delta_T/(T_ave+237.3)**2 ! slope of saturation vapour pressure curve [kPa °C-1]!
        VP_act=(SVP_min*(HUM_max/100)+SVP_max*(HUM_min/100))/2. ! actual vapour pression [kPa]!
        !!
        ! (b) Calculate net solar radiation
        alat=pi/180*lat_ws ! latitude in radians
        DR=1.+0.033*COS(2*pi*doy/365) ! inverse distance between Earth and Sun
        DL=0.409*SIN((2*pi*doy/365)-1.39) ! solar declination
        omegaS=ACOS(-TAN(alat)*TAN(DL)) ! sunset hour angle
        RA_T=24.0*60.0/pi*cost_solare*DR !
        RA=RA_T*(omegaS*SIN(alat)*SIN(DL)+COS(alat)*COS(DL)*SIN(omegaS)) ! extraterrestrial radiation [MJ m-2 day-1]!
        RSO=((as+bs)+2E-5*alt_ws)*RA ! clear-sky solar radiation at lat_ws [MJ m-2 h-1]
        RNS=(1-alpha)*Rad_sol ! net solar (shortwave) radiation [MJ m-2 day-1]!
        RNL_T1=sigma*((T_max+273.16)**4+(T_min+273.16)**4)/2.!
        RNL_T2=RNL_T1*(0.34-0.14*SQRT(VP_act))!
        RNL=RNL_T2*(1.35*Rad_sol/RSO-0.35) ! net longwave radiation [MJ m-2 day-1]!
        RN=RNS-RNL ! net radiation [MJ m-2 day-1]
        !!
        ! (c) Calculate reference evapotranspiration
        W1=0.408*delta*RN ! G is small at daily scale 
        W2=gamma*900/(T_ave+273.)*Wind_vel*(SVP_ave-VP_act)
        W3=(delta+gamma*(1.+res_surf*Wind_vel/208))
        ET0_T=(W1+W2)/W3
        !!
        ! check for positive ET0 
        where (ET0_T>=0)
            ET_reference_mat=ET0_T
        ELSE where
            ET_reference_mat=0.
        END where

        return
    end function ET_reference_mat!

    function ET_reference_sc(T_max, T_min, HUM_max, HUM_min, Wind_vel, Rad_sol, lat_ws, alt_ws, res_surf, doy)!
        ! calculate reference evapotranspiration considering the elevation
        ! see FAO-56
        ! Note: scalar mode
        implicit none!
        integer,intent(in)::doy ! day of the year
        real(dp),intent(in)::T_max, T_min, HUM_max, HUM_min, Wind_vel, Rad_sol, lat_ws, alt_ws
        real(dp),intent(in):: res_surf
        !real(sp),parameter::pi = 3.141592653589793238462643383279502884197_sp
        real(dp),parameter::cost_sol = 0.0820 ! solar contant [MJ m-2 min-1]
        real(dp),parameter::a_s = 0.25, b_s = 0.50    ! Other constants
        real(dp),parameter::albedo = 0.23  ! albedo [-] = 0.23 for grass reference crop!
        real(dp),parameter::sigma = 4.903E-9  ! Stefan-Boltzmann constant [MJ K-4 m-2 day-1]!
        !!
        real(dp)::T_ave, press_atm, gamma
        real(dp)::SVP_max, SVP_min, SVP_ave ! saturation vapour pressure at T_max, T_min and average
        real(dp)::delta_T, delta
        real(dp)::VP_act ! actual vapour pressure 
        real(dp)::alat, DR, DL, omegaS, RA_T, RA, RSO, RNS, RNL_T1, RNL_T2, RNL, RN
        real(dp)::W1, W2, W3, ET0_T
        !!
        real(dp)::ET_reference_sc!
        !!
        ! (a) Calculate atmospheric parameters
        T_ave=(T_max+T_min)/2.!
        press_atm=101.3*((293-0.0065*alt_ws)/293)**5.26 ! atmospheric pressure at alt_ws m a.s.l. [kPa] !
        gamma=0.665E-3*press_atm ! psycometric const [kPa °C-1]!
        SVP_max=0.6108*EXP(17.27*T_max/(T_max+237.3)) ! saturation vapour pressure at T_max [kPa]!
        SVP_min=0.6108*EXP(17.27*T_min/(T_min+237.3)) ! saturation vapour pressure at T_min [kPa]!
        SVP_ave=(SVP_max+SVP_min)/2. ! daily average saturation vapour pressure [kPa]!
        delta_T = 4098*(0.6108*EXP(17.27*T_ave/(T_ave+237.3))) ! numerator of slope of saturation vapour pressure curve
        delta = delta_T/(T_ave+237.3)**2 ! slope of saturation vapour pressure curve [kPa °C-1]!
        VP_act=(SVP_min*(HUM_max/100)+SVP_max*(HUM_min/100))/2. ! actual vapour pression [kPa]!
        !!
        ! (b) Calculate net solar radiation
        alat=pi/180*lat_ws ! latitude in radians
        DR=1.+0.033*COS(2*pi*doy/365) ! inverse distance between Earth and Sun
        DL=0.409*SIN((2*pi*doy/365)-1.39) ! solar declination
        omegaS=ACOS(-TAN(alat)*TAN(DL)) ! sunset hour angle
        RA_T=24.0*60.0/pi*cost_sol*DR !
        RA=RA_T*(omegaS*SIN(alat)*SIN(DL)+COS(alat)*COS(DL)*SIN(omegaS)) ! extraterrestrial radiation [MJ m-2 day-1]!
        RSO=((a_s+b_s)+2E-5*alt_ws)*RA ! clear-sky solar radiation at lat_ws [MJ m-2 h-1]
        RNS=(1-albedo)*Rad_sol ! net solar (shortwave) radiation [MJ m-2 day-1]!
        RNL_T1=sigma*((T_max+273.16)**4+(T_min+273.16)**4)/2.!
        RNL_T2=RNL_T1*(0.34-0.14*SQRT(VP_act))!
        RNL=RNL_T2*(1.35*Rad_sol/RSO-0.35) ! net longwave radiation [MJ m-2 day-1]!
        RN=RNS-RNL ! net radiation [MJ m-2 day-1]
        !!
        ! (c) Calculate reference evapotranspiration
        W1=0.408*delta*RN ! G is small at daily scale 
        W2=gamma*900/(T_ave+273.)*Wind_vel*(SVP_ave-VP_act)
        W3=(delta+gamma*(1.+res_surf*Wind_vel/208))
        ET0_T=(W1+W2)/W3
        !!
        ! check for positive ET0 
        IF (ET0_T>=0) THEN
            ET_reference_sc=ET0_T
        ELSE
            ET_reference_sc=0.
        END IF
        
        return
    end function ET_reference_sc
    
end module