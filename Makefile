# commit code/number for versioning
COMMIT := $(shell git rev-parse --short HEAD)
CURRENTDATE := $(shell date --iso=seconds)

# Windows OS variables & settings
DEL = rm
EXE = .exe
WIN = 1

# Compiler settings
# -cpp: activates compiler pre processing
# -DGIT_VERSION: sets the macro GIT_VERSION in the code (actually used only in main.f90)
# -g: enables debug with breakpoints 

CC = gfortran
CPP = gfortran -cpp
# -g for gdb, -O0 zero optimization or -Og
### for debug ###
GFFLAGS = -cpp -DGIT_VERSION=\"$(COMMIT)\" -DCOMP_DATE=\"$(CURRENTDATE)\" -DWIN=$(WIN) -g -Wall  -Wconversion -fimplicit-none -fbacktrace -ffree-line-length-0 -fcheck=all -ffpe-trap=zero,overflow,underflow -finit-real=nan -c
#GFFLAGS = -g -O0 -Wall -Wextra -Wshadow -pedantic -static -c
#GFFLAGS =  -cpp -DMY_VERSION=\"$(COMMIT)\" -g -Wall -c
### for release ###
# -ffree-line-length-512 manage long commands in the code
#GFFLAGS = -cpp -DGIT_VERSION=\"$(COMMIT)\" -DCOMP_DATE=\"$(CURRENTDATE)\" -DWIN=$(WIN) -ffast-math  -O3 -ffree-line-length-512 -c
LDFLAGS = 

APPNAME = cropcoef
EXT = .f90
SRCDIR = src
OBJDIR = obj
RELDIR = release

# List of file names, without extention separated by space
# Check the list sequence according to compile order

FILES = mod_utilities mod_settings mod_crop mod_cropseq\
		mod_datetime mod_weather_station mod_productivity \
		mod_io_file mod_cropcoef_v4 mod_system
		

#### User, don't touch the following line ####

# Builds the app
# force removing main.o in order to update program metadata
$(APPNAME): $(patsubst %, $(OBJDIR)/%.o, $(FILES))
	$(DEL) -f /$(OBJDIR)/main.o
	$(CC) -o $(OBJDIR)/main.o -J$(OBJDIR) $(GFFLAGS) $(SRCDIR)/main.f90
	$(CPP) -g -o $(RELDIR)/$@$(EXE) $^ $(OBJDIR)/main.o $(LDFLAGS) -static

# The following sets a function that creates makefile rules
# Additionally:
# -o: set path to output
# -J: set path for *.mod
define make-o-rule
$(OBJDIR)/$1.o: $(SRCDIR)/$1.f90
	$(CC) -o $(OBJDIR)/$1.o -J$(OBJDIR) $(GFFLAGS) $(SRCDIR)/$1.f90
all: $1.o
endef

$(foreach element,$(FILES),$(eval $(call make-o-rule,$(element))))

all:
	$(APPNAME)

##### Set the obj folder empty ####
# Cleans complete project
# call as: make cleanall
.PHONY: cleanall
cleanall:
	$(DEL) $(wildcard ./$(OBJDIR)/*.mod)
	$(DEL) $(wildcard ./$(OBJDIR)/*.o)
	$(DEL) $(wildcard ./$(RELDIR)/*.exe)

.PHONY: cleanmain
cleanmain:
	@echo "hello from cleanmain"
	$(DEL) -f /$(OBJDIR)/main.o
	