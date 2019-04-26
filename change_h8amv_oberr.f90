program change_h8amv_oberr
use obs
implicit none

integer :: nread, nout

character(len=195) :: arg1, filein, fileout, filename
character(len=231) :: line

integer :: iargc, narg, i, n, j, system, ierr
real :: oberr, uv_oberr, t_oberr, p_oberr

type(t_obs) :: obs1
logical :: keep

narg=iargc()

if(narg>=3)then
   call getarg(1,line)
   read(line,*) uv_oberr
   call getarg(2,filein)
   call getarg(3,fileout)
else
   write(*,*) "Usage: change_h8amv_oberr uv_oberr filein fileout"
   stop
endif

call reset_obs()
open (11, file=filein , status="old", iostat=ierr)
if(ierr==0) then
   write(*,*) "Read file:", trim(filein)
else
   write(*,*) "Error open file:", trim(filein)
   stop
endif
call read_header(11)

open (31, file=fileout, status="unknown",iostat=ierr)
if(ierr==0)then
   write(*,*) "Write file:", trim(fileout)
else
   write(*,*) "Error open file:", trim(fileout)
   stop
endif
! output header
!call write_header(31) 
do i=1, 21
   write(31,"(A)") trim(head_lines(i))
enddo

do i=1, ntotal
   call read_obs(11,obs1)
   call count_obs(obs1)      
   if(obs1%platform(7:12)=="SATOB".and.obs1%site_id(1:14)=="HIMAWARI-8 AMV")then
      call change_uv_oberr(obs1,uv_oberr)      
   endif
   call write_obs(31,obs1)
enddo


close(11)
close(31)

!ierr=system("rm -f fort.21")

end program
