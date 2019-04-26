# CC  = xlc
# F90 = xlf90
# F77 = xlf90 -qfixed
# FFLAGS = -O2
# CFLAGS = -O2
# LIB = -L./BUFRLIB_v10.2.3/ -lbufr

CC  = gcc
F90 = gfortran -ffree-form -ffree-line-length-none
F77 = gfortran
FFLAGS = -O2
CFLAGS = -O2 -DUNDERSCORE
LIB = -L/usr/lib -lbufr

TARGET =  read_h8_amv debufr change_h8amv_oberr
PACKAGE = read_h8_amv

default:	$(TARGET)

change_h8amv_oberr:	change_h8amv_oberr.o module_obs.o
	$(F90) -g -o $@ $@.o module_obs.o

change_h8amv_oberr.o:	module_obs.o

read_h8_amv:	read_h8_amv.f90
	$(F90) -o $@ $@.f90 $(LIB)

debufr:	module.o fdebufr.o debufr.o
	$(F90) -o debufr module.o debufr.o fdebufr.o $(LIB)

tar:
	tar -cvf $(PACKAGE).tar *.f90 Makefile
	mv $(PACKAGE).tar $(PACKAGE)-`date +%Y%m%d%H%M`.tar
	gzip $(PACKAGE)-*.tar

.SUFFIXES:
.SUFFIXES: .c .f90 .f .o

.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(F77) $(FFLAGS) -c $<

.f90.o:
	$(F90) $(FFLAGS) -c $<

clean:
	rm -f *.o *.mod $(TARGET)
