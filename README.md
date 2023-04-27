# cropcoeff
the most simple way to create the executable is to execute the following instructions (order is mandatory):

<code>gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c mod_utilities.f90
gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c mod_settings.f90
gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c mod_crop.f90
gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c mod_cropseq.f90
gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c mod_datetime.f90
gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c mod_weather_station.f90
gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c mod_productivity.f90
gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c mod_io_file.f90
gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c mod_cropcoef_v4.f90
gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c main.f90
gfortran -o ./example/cropcoef.exe *.o -static
erase *.o *.mod
</code>
