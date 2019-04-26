module little_r
implicit none

type t_little_r_level
   real :: prs, hgt, tmp, dew, spd, dir, rh
end type

type t_little_r_obs
   character(len=40) :: id, name, platform, source
   real :: latitude, longitude, elevation
   real :: slp, ground_t, psfc, p_tend03, p_tend24, cloud_cvr, ceiling
   !real :: pressure, height, temperature, dew_point, speed, direction
   character(len=14) :: date_char
   integer :: nlevel
   type(t_little_r_level), dimension(:), allocatable :: level 
end type

integer, parameter :: lr_missing  = -888888
real   , parameter :: lr_end_data = -777777.0, lr_missing_r = lr_missing

contains


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine transfer_upar_to_little_r(upar_dat, lr_obs)
   use upar_data
   implicit none
   type(t_upar_data),   intent(in)  :: upar_dat
   type(t_little_r_obs),intent(out) :: lr_obs

   integer :: i

   write(lr_obs%date_char,"(I4.4,5I2.2)") upar_dat%level(1)%year, upar_dat%level(1)%month,  upar_dat%level(1)%day, &
                                          upar_dat%level(1)%hour, 0, 0
   lr_obs%id        = upar_dat%id
   lr_obs%name      = upar_dat%name
   lr_obs%platform  = "FM-35 TEMP"
   lr_obs%source     = "UPAR_WEA_CHN_MUL_FTM_SEC"
   lr_obs%latitude  = upar_dat%lat
   lr_obs%longitude = upar_dat%lon
   lr_obs%elevation = upar_dat%elv

   lr_obs%slp         = lr_missing_r
   lr_obs%ground_t    = lr_missing_r
   lr_obs%psfc        = lr_missing_r
   lr_obs%p_tend03    = lr_missing_r
   lr_obs%p_tend24    = lr_missing_r
   lr_obs%cloud_cvr   = lr_missing_r
   lr_obs%ceiling     = lr_missing_r

   lr_obs%nlevel      = upar_dat%nlevel
   if(allocated(lr_obs%level))then
      deallocate(lr_obs%level)
   endif
   allocate(lr_obs%level(lr_obs%nlevel))
   lr_obs%level(:)%dew =lr_missing

   do i=1, lr_obs%nlevel
      lr_obs%level(i)%tmp=lr_missing
      lr_obs%level(i)%prs=lr_missing
      lr_obs%level(i)%rh =lr_missing
      lr_obs%level(i)%dir=lr_missing
      lr_obs%level(i)%spd=lr_missing
      lr_obs%level(i)%hgt=lr_missing
      if(upar_dat%level(i)%tmp/=upar_missing_r) lr_obs%level(i)%tmp=upar_dat%level(i)%tmp+273.15
      if(upar_dat%level(i)%prs/=upar_missing_r) lr_obs%level(i)%prs=upar_dat%level(i)%prs*100.
      if(upar_dat%level(i)%rh /=upar_missing_r) lr_obs%level(i)%rh =upar_dat%level(i)%rh
      if(upar_dat%level(i)%dir/=upar_missing_r) lr_obs%level(i)%dir=upar_dat%level(i)%dir
      if(upar_dat%level(i)%spd/=upar_missing_r) lr_obs%level(i)%spd=upar_dat%level(i)%spd
      if(upar_dat%level(i)%hgt/=upar_missing_r) lr_obs%level(i)%hgt=upar_dat%level(i)%hgt
   enddo

   end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine transfer_upar_to_little_r_n(upar_dat, lr_obs)
   use upar_data
   use date_pack
   implicit none
   type(t_upar_data),   intent(in)  :: upar_dat
   type(t_little_r_obs), dimension(:), allocatable, intent(out) :: lr_obs
   
   integer :: i
   type(date) :: odate, ndate
 
   if(allocated(lr_obs))then
      deallocate(lr_obs)
   endif
   allocate(lr_obs(upar_dat%nlevel))

   do i=1, upar_dat%nlevel

      odate=init_date(upar_dat%level(i)%year, upar_dat%level(i)%month,  upar_dat%level(i)%day, upar_dat%level(i)%hour)
      ndate=get_new_date(odate, upar_dat%level(i)%stime*one_second)
      write(lr_obs(i)%date_char,"(I4.4,5I2.2)") ndate%year, ndate%month,  ndate%day, ndate%hour, ndate%minute, ndate%second

      lr_obs(i)%id        = upar_dat%id
      lr_obs(i)%name      = upar_dat%name
      lr_obs(i)%platform  = "FM-35 TEMP"
      lr_obs(i)%source    = "UPAR_WEA_CHN_MUL_FTM_SEC"
      lr_obs(i)%latitude  = lr_missing
      lr_obs(i)%longitude = lr_missing
      if(upar_dat%level(i)%dlon/=upar_missing_r) lr_obs(i)%longitude=upar_dat%lon+upar_dat%level(i)%dlon
      if(upar_dat%level(i)%dlat/=upar_missing_r) lr_obs(i)%latitude =upar_dat%lat+upar_dat%level(i)%dlat
      lr_obs(i)%elevation = upar_dat%elv

      lr_obs(i)%slp       = lr_missing_r
      lr_obs(i)%ground_t  = lr_missing_r
      lr_obs(i)%psfc      = lr_missing_r
      lr_obs(i)%p_tend03  = lr_missing_r
      lr_obs(i)%p_tend24  = lr_missing_r
      lr_obs(i)%cloud_cvr = lr_missing_r
      lr_obs(i)%ceiling   = lr_missing_r

      lr_obs(i)%nlevel      = 1 
      if(allocated(lr_obs(i)%level))then
         deallocate(lr_obs(i)%level)
      endif
      allocate(lr_obs(i)%level(lr_obs(i)%nlevel))
      lr_obs(i)%level(:)%dew =lr_missing

      lr_obs(i)%level(1)%tmp=lr_missing
      lr_obs(i)%level(1)%prs=lr_missing
      lr_obs(i)%level(1)%rh =lr_missing
      lr_obs(i)%level(1)%dir=lr_missing
      lr_obs(i)%level(1)%spd=lr_missing
      lr_obs(i)%level(1)%hgt=lr_missing
      if(upar_dat%level(i)%tmp/=upar_missing_r) lr_obs(i)%level(1)%tmp=upar_dat%level(i)%tmp+273.15
      if(upar_dat%level(i)%prs/=upar_missing_r) lr_obs(i)%level(1)%prs=upar_dat%level(i)%prs*100.
      if(upar_dat%level(i)%rh /=upar_missing_r) lr_obs(i)%level(1)%rh =upar_dat%level(i)%rh
      if(upar_dat%level(i)%dir/=upar_missing_r) lr_obs(i)%level(1)%dir=upar_dat%level(i)%dir
      if(upar_dat%level(i)%spd/=upar_missing_r) lr_obs(i)%level(1)%spd=upar_dat%level(i)%spd
      if(upar_dat%level(i)%hgt/=upar_missing_r) lr_obs(i)%level(1)%hgt=upar_dat%level(i)%hgt
   enddo

   end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine write_little_r_obs (ounit, lr_obs)
   implicit none 
       
   integer,           intent(in) :: ounit
   type(t_little_r_obs),intent(in) :: lr_obs

   integer :: i

   character(len=84) ::  rpt_format 
   character(len=22) ::  meas_format 
   character(len=14) ::  end_format

   rpt_format =  ' ( 2f20.5 , 2a40 , '                   &
                // ' 2a40 , 1f20.5 , 5i10 , 3L10 , '     &
                // ' 2i10 , a20 ,  13( f13.5 , i7 ) ) '
   meas_format =  ' ( 10( f13.5 , i7 ) ) '
   end_format = ' ( 3 ( i7 ) ) '     
   
   ! header:
   WRITE ( UNIT = ounit  , FMT = rpt_format )                                     &
           lr_obs%latitude  , lr_obs%longitude , lr_obs%id        , lr_obs%name , &
           lr_obs%platform  , lr_obs%source    , lr_obs%elevation , 6           , &
           0                , 0                , 1                , 0           , &
           .true.           , .false.          , .false.          , lr_missing  , &
           lr_missing       , lr_obs%date_char , lr_obs%slp       , 0           , &
           lr_missing_r     , 0                , lr_obs%ground_t  , 0           , &
           lr_missing_r     , 0                , lr_obs%psfc      , 0           , &
           lr_missing_r     , 0                , lr_missing_r     , 0           , &
           lr_missing_r     , 0                , lr_missing_r     , 0           , &
           lr_obs%p_tend03  , 0                , lr_obs%p_tend24  , 0           , &
           lr_obs%cloud_cvr , 0                , lr_obs%ceiling   , 0

   ! ! Surface Measurement
   ! WRITE ( UNIT = ounit  , FMT = meas_format )                                    &
   !         lr_missing_r        , 0 , lr_missing_r        , 0 , lr_missing_r , 0 , &
   !         lr_missing_r        , 0 , lr_missing_r        , 0 , lr_missing_r , 0 , &
   !         lr_missing_r        , 0 , lr_missing_r        , 0 , lr_missing_r , 0 , lr_missing_r , 0
   ! report:
   do i=1, lr_obs%nlevel
      WRITE ( UNIT = ounit  , FMT = meas_format )                                 &
              lr_obs%level(i)%prs , 0 , lr_obs%level(i)%hgt , 0 , lr_obs%level(i)%tmp , 0 , &
              lr_obs%level(i)%dew , 0 , lr_obs%level(i)%spd , 0 , lr_obs%level(i)%dir , 0 , &
              lr_missing_r        , 0 , lr_missing_r        , 0 , lr_obs%level(i)%rh  , 0 , lr_missing_r , 0
   enddo

   ! end of report line:
   WRITE ( UNIT = ounit, FMT = meas_format )                        &
           lr_end_data  , 0 , lr_end_data  , 0 , 6.           , 0 , &
           lr_missing_r , 0 , lr_missing_r , 0 , lr_missing_r , 0 , &
           lr_missing_r , 0 , lr_missing_r , 0 , lr_missing_r , 0 , &
           lr_missing_r , 0

   ! end of message line:
   WRITE ( UNIT = ounit  , FMT = end_format )  6*lr_obs%nlevel, 0, 0
      
   end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module
