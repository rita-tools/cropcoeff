SET PATH=%PATH%;C:\MinGW\bin

:: set the version code base on the latest commit
:: dirty means with local edits
FOR /F "delims=" %%i IN ('git describe --abbrev^=7 --dirty --always') DO set version=%%i

echo latest git commit=%version%

set year=%date:~-4%
echo year=%year%
set month=%date:~3,2%
echo month=%month%
set day=%date:~0,2%
echo day=%day%

set datetimef=%year%%month%%day%

echo datetimef=%datetimef%

gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c mod_utilities.f90
gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c mod_settings.f90
gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c mod_crop.f90
gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c mod_cropseq.f90
gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c mod_datetime.f90
gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c mod_weather_station.f90
gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c mod_productivity.f90
gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c mod_io_file.f90
gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c mod_cropcoef_v4.f90
gfortran -fbounds-check -march=prescott -ffast-math -funroll-loops -O3 -c main.f90

gfortran -o ./example/cropcoef_%version%_%datetimef%.exe *.o -static


erase *.o *.mod

cd ./example/

cropcoef_%version%_%datetimef%.exe -f cropcoef.txt -v