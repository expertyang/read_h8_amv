module obs
implicit none

integer :: ntotal, nsynop,  nmetar, nship,  nbuoy,  nbogus, ntemp,  namdar, & 
           nairep, ntamdar, npilot, nsatem, nsatob, ngpspw, ngpszd, ngpsrf, &
           ngpsep, nssmt1,  nssmt2, ntovs,  nqscat, nprofl, nairsr, nother

character(len=195), dimension(21) :: head_lines

type t_obs
   character(len=12) :: platform
   character(len=20) :: site_id
   integer :: nlevel
   character(len=195) :: head_line
   character(len=231), dimension(:), allocatable :: lines
end type

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine count_obs(obs)
   implicit none
   type(t_obs), intent(in) :: obs

   select case (trim(obs%platform(7:12)))
   case("SYNOP")
      nsynop=nsynop+1
   case("METAR")
      nmetar=nmetar+1
   case("SHIP")
      nship =nship +1
   case("BUOY")
      nbuoy =nbuoy +1
   case("TEMP")
      ntemp =ntemp +1
   case("AMDAR")
      namdar=namdar+1
   case("AIREP")
      nairep=nairep+1
   case("SATEM")
      nsatem=nsatem+1
   case("SATOB")
      nsatob=nsatob+1
   case("PILOT")
      npilot=npilot+1
   case default
      nother=nother+1
   end select

   end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine reset_obs()
   implicit none

   nsynop =0
   nmetar =0
   nship  =0
   nbuoy  =0
   nbogus =0
   ntemp  =0
   namdar =0
   nairep =0
   ntamdar=0
   npilot =0
   nsatem =0
   nsatob =0
   ngpspw =0
   ngpszd =0
   ngpsrf =0
   ngpsep =0
   nssmt1 =0
   nssmt2 =0
   ntovs  =0
   nqscat =0
   nprofl =0
   nairsr =0
   nother =0

   end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine read_header(iunit)
   implicit none
      integer, intent(in) :: iunit
      integer :: i
      do i=1, 21
         read(iunit,"(A)") head_lines(i)
!        write(*,"(A)") trim(head_lines(i))
      enddo
      read(head_lines(1),"(7X,I7)") ntotal
   end subroutine
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine write_header(ounit)
   implicit none
   integer, intent(in) :: ounit
   integer :: i
   
      write(ounit,"(A7,I7,A18)") "TOTAL =",ntotal,", MISS. =-888888.,"
      write(ounit,"(6(A7,I7,', '))") "SYNOP =",nsynop,"METAR =",nmetar,"SHIP  =",nship  ,"BUOY  =",nbuoy ,"BOGUS =",nbogus,"TEMP  =",ntemp
      write(ounit,"(6(A7,I7,', '))") "AMDAR =",namdar,"AIREP =",nairep,"TAMDAR=",ntamdar,"PILOT =",npilot,"SATEM =",nsatem,"SATOB =",nsatob
      write(ounit,"(6(A7,I7,', '))") "GPSPW =",ngpspw,"GPSZD =",ngpszd,"GPSRF =",ngpsrf ,"GPSEP =",ngpsep,"SSMT1 =",nssmt1,"SSMT2 =",nssmt2
      write(ounit,"(5(A7,I7,', '))") "TOVS  =",ntovs ,"QSCAT =",nqscat,"PROFL =",nprofl ,"AIRSR =",nairsr,"OTHER =",nother
   
      do i=6, 21
         write(ounit,"(A)") trim(head_lines(i))
      enddo
   end subroutine
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine read_obs(iunit, obs)
   implicit none
   integer, intent(in) :: iunit
   type(t_obs), intent(inout) :: obs
   integer :: i

      read(iunit,"(A)") obs%head_line
      obs%platform=obs%head_line(1:12)
      obs%site_id=obs%head_line(34:53)
      read(obs%head_line,"(73X,I7)") obs%nlevel
      obs%nlevel=obs%nlevel+1
      if(allocated(obs%lines))then
         deallocate(obs%lines)
      endif
      allocate(obs%lines(obs%nlevel))
      do i=1, obs%nlevel
         read(iunit,"(A)") obs%lines(i)
      enddo 
   end subroutine
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine write_obs(ounit, obs)
   implicit none
   integer, intent(in) :: ounit
   type(t_obs), intent(in) :: obs
   integer :: i
   
      write(ounit,"(A)") obs%head_line
      do i=1, obs%nlevel
         write(ounit,"(A)") trim(obs%lines(i))
      enddo 
   end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine write_slp0(obs)
   implicit none
   type(t_obs), intent(in) :: obs
   integer :: i
   real :: slp

   read(obs%lines(1),*) slp
   if(slp==0)then
      write(*,"(A)") obs%head_line
      do i=1, obs%nlevel
         write(*,"(A)") trim(obs%lines(i))
      enddo
   endif
   end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine change_t_oberr(obs,oberr)
   implicit none
   type(t_obs), intent(inout) :: obs
   real, intent(in) :: oberr
   integer :: i, ierr
   integer, dimension(7) :: qc
   real   , dimension(7) :: dat, err
   real :: slp, ps, dir, spd, t, td, hgt
   real, parameter :: miss=-888888.

   read(obs%lines(1),*) slp
   !write(*,*) trim(obs%lines(2))
   if(obs%nlevel>1)then
      read(obs%lines(2),*,iostat=ierr) (dat(i),qc(i),err(i),i=1,7)
      if(ierr/=0) then
         write(*,*) "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
         call print_obs(obs)
         write(*,*) "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
      endif
      err(5)=oberr
      write(obs%lines(2),"(3(F12.3,I4,F7.2),11X,3(F12.3,I4,F7.2),11X,3(F12.3,I4,F7.2))") (dat(i),qc(i),err(i),i=1,7)
   endif
   end subroutine
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine change_td_oberr(obs,oberr)
   implicit none
   type(t_obs), intent(inout) :: obs
   real, intent(in) :: oberr
   integer :: i, ierr
   integer, dimension(7) :: qc
   real   , dimension(7) :: dat, err
   real :: slp, ps, dir, spd, t, td, hgt
   real, parameter :: miss=-888888.

   read(obs%lines(1),*) slp
   !write(*,*) trim(obs%lines(2))
   if(obs%nlevel>1)then
      ! p, spd, dir, hgt, t, td, rh
      read(obs%lines(2),*,iostat=ierr) (dat(i),qc(i),err(i),i=1,7)
      if(ierr/=0) then
         write(*,*) "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
         call print_obs(obs)
         write(*,*) "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
      endif
      err(6)=oberr
      write(obs%lines(2),"(3(F12.3,I4,F7.2),11X,3(F12.3,I4,F7.2),11X,3(F12.3,I4,F7.2))") (dat(i),qc(i),err(i),i=1,7)
   endif
   end subroutine
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine change_p_oberr(obs,oberr)
   implicit none
   type(t_obs), intent(inout) :: obs
   real, intent(in) :: oberr
   integer :: i, ierr
   integer, dimension(7) :: qc
   real   , dimension(7) :: dat, err
   real :: slp, ps, dir, spd, t, td, hgt
   real, parameter :: miss=-888888.

   read(obs%lines(1),*) slp
   !write(*,*) trim(obs%lines(2))
   if(obs%nlevel>1)then
      read(obs%lines(2),*,iostat=ierr) (dat(i),qc(i),err(i),i=1,7)
      if(ierr/=0) then
         write(*,*) "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
         call print_obs(obs)
         write(*,*) "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
      endif
      err(1)=oberr
      write(obs%lines(2),"(3(F12.3,I4,F7.2),11X,3(F12.3,I4,F7.2),11X,3(F12.3,I4,F7.2))") (dat(i),qc(i),err(i),i=1,7)
   endif
   end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine change_uv_oberr(obs,oberr)
   implicit none
   type(t_obs), intent(inout) :: obs
   real, intent(in) :: oberr
   integer :: i, ierr
   integer, dimension(7) :: qc
   real   , dimension(7) :: dat, err
   real :: slp, ps, dir, spd, t, td, hgt
   real, parameter :: miss=-888888.

   read(obs%lines(1),*) slp
   !write(*,*) trim(obs%lines(2))
   if(obs%nlevel>1)then
      read(obs%lines(2),*,iostat=ierr) (dat(i),qc(i),err(i),i=1,7)
      if(ierr/=0) then
         write(*,*) "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
         call print_obs(obs)
         write(*,*) "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
      endif
      err(2)=oberr
      write(obs%lines(2),"(3(F12.3,I4,F7.2),11X,3(F12.3,I4,F7.2),11X,3(F12.3,I4,F7.2))") (dat(i),qc(i),err(i),i=1,7)
   endif
   end subroutine
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   logical function check_synop_zero(obs)
   implicit none
   type(t_obs), intent(in) :: obs
   integer :: i, ierr
   integer, dimension(7) :: qc
   real   , dimension(7) :: dat, err
   real :: slp, ps, dir, spd, t, td, hgt
   real, parameter :: miss=-888888.

   check_synop_zero=.true.

   read(obs%lines(1),*) slp
   !write(*,*) trim(obs%lines(2))
   read(obs%lines(2),*,iostat=ierr) (dat(i),qc(i),err(i),i=1,7)
   if(ierr/=0) then
      write(*,*) "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
      call print_obs(obs)
      write(*,*) "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
   endif
   ps =dat(1)
   spd=dat(2)
   dir=dat(3)
   hgt=dat(4)
   t  =dat(5)
   td =dat(6)
   !   (hgt==0.or.hgt==miss).and. &
   if( (slp==0.or.slp==miss).and. &
       (ps ==0.or.ps ==miss).and. &
       (spd==0.or.spd==miss).and. &
       (dir==0.or.dir==miss).and. &
       (t  ==273.15.or.t  ==miss).and. &
       (td ==273.15.or.td ==miss) ) then
       check_synop_zero=.false.
   endif
   end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   character(len=40) function get_obs_id(obs)
   implicit none
   type(t_obs), intent(in) :: obs
  
   get_obs_id=obs%head_line(156:195)
   end function
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   real function get_obs_lat(obs)
   implicit none
   type(t_obs), intent(in) :: obs
  
   read(obs%head_line(104:115),*) get_obs_lat
   end function
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   real function get_obs_lon(obs)
   implicit none
   type(t_obs), intent(in) :: obs
  
   read(obs%head_line( 81: 92),*) get_obs_lon
   end function
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine print_obs(obs)
   implicit none
   type(t_obs), intent(in) :: obs
   integer :: i
   real :: slp

   write(*,*) "=============================="
   write(*,*) "PlatForm:",obs%head_line(  1: 12)
   write(*,*) "ObsDate :",obs%head_line( 14: 32)
   write(*,*) "SiteName:",obs%head_line( 34: 73)
   write(*,*) "nLevels :",obs%head_line( 75: 80)
   write(*,*) "SiteLon :",obs%head_line( 81: 92)
   write(*,*) "SiteLat :",obs%head_line(104:115)
   write(*,*) "SiteElv :",obs%head_line(127:138)
   write(*,*) "SiteID  :",obs%head_line(156:195)
   do i=1, obs%nlevel
      write(*,*) "-----------Level",i,"-----------"
      if(i==1)then
         write(*,*) "SeaLevelPressure :",obs%lines(i)( 1:12)
         write(*,*) "PrecipitableWater:",obs%lines(i)(24:35)
      else
         if(obs%head_line(4:6)=="115".or.obs%head_line(4:6)=="116")then
            write(*,*) "Pressure   :",obs%lines(i)(  1: 12)
            write(*,*) "U-Wind     :",obs%lines(i)( 24: 35)
            write(*,*) "V-Wind     :",obs%lines(i)( 47: 58)
            write(*,*) "Height     :",obs%lines(i)( 81: 92)
            write(*,*) "Temperature:",obs%lines(i)(104:115)
            write(*,*) "DewPoint   :",obs%lines(i)(127:138)
            write(*,*) "Direction  :",obs%lines(i)(161:172)
            write(*,*) "Speed      :",obs%lines(i)(184:195)
            write(*,*) "RH         :",obs%lines(i)(207:218)
         else
            write(*,*) "Pressure   :",obs%lines(i)(  1: 12)
            write(*,*) "Speed      :",obs%lines(i)( 24: 35)
            write(*,*) "Direction  :",obs%lines(i)( 47: 58)
            write(*,*) "Height     :",obs%lines(i)( 81: 92)
            write(*,*) "Temperature:",obs%lines(i)(104:115)
            write(*,*) "DewPoint   :",obs%lines(i)(127:138)
            write(*,*) "RH         :",obs%lines(i)(161:172)
         endif
      endif
   enddo
   end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end module
