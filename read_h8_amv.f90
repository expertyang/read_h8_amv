program read_h8_amv
implicit none
   integer, parameter :: lnbufr=11, lntabl=12, use_unit=21, rej_unit=22, csv_unit=98
   integer, parameter :: i_kind=4, r_kind=8
   integer, parameter :: maxinfo=12, maxchanl=100

   integer :: icount, tcount

   integer(i_kind) :: ireadmg,ireadsb
   integer(i_kind),parameter:: n1bhdr=10
   integer(i_kind),parameter:: n2bhdr=5
   real(r_kind),dimension(n1bhdr):: bfr1bhdr
   real(r_kind),dimension(n2bhdr):: bfr2bhdr
   character(80) hdr1b
   character(80) hdr2b

   integer(i_kind), parameter :: nvar=12
   real(r_kind), dimension(nvar) :: PCCF, NCTH

   integer(i_kind) iret,idate,nchan
   logical :: loutput
   character(8) subset,subfgn
   character(5)  band

   data hdr1b /'SAID YEAR MNTH DAYS HOUR MINU SECO SAZA LSQL SCCF'/
   data hdr2b /'CLATH CLONH PRLC WDIR WSPD'/

   nchan=15
   subfgn="FN005000"

   open(use_unit ,file="h8_amv.little_r",status="unknown")
   open(csv_unit ,file="h8_amv.csv",status="unknown")
   write(csv_unit,"(4(A5,','),11(A10,','),1X,A5,',',A3)") "year","month","day","hour","lat","lon","prs","dir","spd","qi1","th1","qc2","th2","qc3","th3","band","use"

   open(rej_unit ,file="h8_amv.rej.little_r",status="unknown")
   open(lnbufr,file="h8_amv.bufr",status="old",form="unformatted")
   open(lntabl,file="H8_AMV.DXT",status="old")
   write(*,*) "openbf..."
   call openbf(lnbufr,"IN",lntabl)
   write(*,*) "set datelen..."
   call datelen(10)
!  call readmg(lnbufr,subset,idate,iret)
!  write(*,*) ireadmg(lnbufr,subset,idate), subset==subfgn
   icount=0
   tcount=0
   obs: do while (ireadmg(lnbufr,subset,idate)==0 ) !.and. subset==subfgn)
      write(*,*) "subset,idate: ", subset,idate
      do while (ireadsb(lnbufr)==0)
         ! 1.0     Read header record and data record
         call ufbint(lnbufr,bfr1bhdr,n1bhdr,1,iret,hdr1b)
         call ufbint(lnbufr,bfr2bhdr,n2bhdr,1,iret,hdr2b)
         call ufbrep(lnbufr,PCCF,1,nvar,iret,'PCCF')
         call ufbrep(lnbufr,NCTH,1,nvar,iret,'NCTH')
         !write(99,"(15F10.2)") bfr1bhdr(2), bfr1bhdr(3), bfr1bhdr(4), bfr1bhdr(5), &
         !                      bfr2bhdr(1), bfr2bhdr(2), bfr2bhdr(3), bfr2bhdr(4), bfr2bhdr(5), &
         !                      PCCF(1), NCTH(1), PCCF(5), NCTH(5), PCCF(9), NCTH(9)
         loutput=.true.
         ! Band
         if(bfr1bhdr(10)>48353600000000.)then ! Visible Band
             band="VIS"
             if(bfr2bhdr(3)>70000.)then ! Below 700hPa
                loutput=.false.
             endif
         elseif(bfr1bhdr(10)<41067400000000.)then ! IR Band
             band="IR"
             if(bfr2bhdr(3)>70000.)then ! Below 700hPa
                loutput=.false.
             endif
         else ! Water Vapor Band
             band="WV"
             loutput=.true.
         endif
         ! Confidence
         if(loutput)then
            if(pccf(1)<ncth(1).or.pccf(5)<ncth(5).or.pccf(9)<ncth(9))then
               loutput=.false.
            endif
         endif
         ! QI Thresholds 
         if(loutput)then
            select case(band)
            case("VIS")
               if((pccf(9)<88.and. bfr2bhdr(3)< 40000.).or. &
                  (pccf(9)<95.and.(bfr2bhdr(3)>=40000.and.bfr2bhdr(3)<70000.)).or.&
                  (pccf(9)<85.and. bfr2bhdr(3)>=70000.))then
                  loutput=.false.
               endif
            case("IR")
               if((pccf(9)<85.and. bfr2bhdr(3)< 40000.).or. &
                  (pccf(9)<94.and.(bfr2bhdr(3)>=40000.and.bfr2bhdr(3)<70000.)).or.&
                  (pccf(9)<88.and. bfr2bhdr(3)>=70000.))then
                  loutput=.false.
               endif
            case("WV")
               if((pccf(9)<85.and. bfr2bhdr(3)< 40000.).or. &
                  (pccf(9)<85.and.(bfr2bhdr(3)>=40000.and.bfr2bhdr(3)<70000.)))then
                  loutput=.false.
               endif
            case default
               loutput=.false.
            end select
         endif
         write(csv_unit,"(4(I5,','),11(F10.2,','),1X,A5,',',L3)") &
               int(bfr1bhdr(2)), int(bfr1bhdr(3)), int(bfr1bhdr(4)), int(bfr1bhdr(5)), &
               bfr2bhdr(1), bfr2bhdr(2), bfr2bhdr(3), bfr2bhdr(4), bfr2bhdr(5), &
               PCCF(1), NCTH(1), PCCF(5), NCTH(5), PCCF(9), NCTH(9), band, loutput
!        call ufbrep(lnbufr,data1b8,1,nchan,iret,'TMBRST')
!         write(*,*) "subset  =",subset
!         write(*,*) "idate   =",idate
!         write(*,*) "bfr1bhdr=",bfr1bhdr
!         write(*,*) "bfr2bhdr=",bfr2bhdr
         tcount=tcount+1
         if(loutput)then
            icount=icount+1
            call write_little_r_obs(use_unit, bfr1bhdr(2), bfr1bhdr(3), bfr1bhdr(4), bfr1bhdr(5), bfr1bhdr(6), bfr1bhdr(7), &
                                           bfr2bhdr(1), bfr2bhdr(2), bfr2bhdr(3), bfr2bhdr(4), bfr2bhdr(5), band)
         else
            call write_little_r_obs(rej_unit, bfr1bhdr(2), bfr1bhdr(3), bfr1bhdr(4), bfr1bhdr(5), bfr1bhdr(6), bfr1bhdr(7), &
                                           bfr2bhdr(1), bfr2bhdr(2), bfr2bhdr(3), bfr2bhdr(4), bfr2bhdr(5), band)
         endif
!        write(*,*) "data1b8 =",data1b8(1:nchan)
      enddo
   enddo obs
   write(*,*) "write ", icount, "/", tcount, " obs."
   call closbf(lnbufr)
   close(lnbufr)
   close(lntabl)
   close(use_unit)
   close(rej_unit)
end program

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine write_little_r_obs (ounit, ryear, rmonth, rday, rhour, rminute, rsecond, lat, lon, prs, dir, spd, BAND)
   implicit none

   integer, intent(in) :: ounit
   real(kind=8),    intent(in) :: ryear, rmonth, rday, rhour, rminute, rsecond, lat, lon, prs, dir, spd
   character(len=*), intent(in) :: band
   
   integer :: year, month, day, hour, minute, second
   integer :: i

   real, parameter   :: lr_missing = -888888. , lr_end_data=-777777.
   character(len=84) ::  rpt_format
   character(len=22) ::  meas_format
   character(len=14) ::  end_format
   character(len=40) :: id, platform, name, source

   character(len=20) :: date_char

   rpt_format =  ' ( 2f20.5 , 2a40 , '                   &
                // ' 2a40 , 1f20.5 , 5i10 , 3L10 , '     &
                // ' 2i10 , a20 ,  13( f13.5 , i7 ) ) '
   meas_format =  ' ( 10( f13.5 , i7 ) ) '
   end_format = ' ( 3 ( i7 ) ) '

   year  =ryear  
   month =rmonth 
   day   =rday   
   hour  =rhour  
   minute=rminute
   second=rsecond

   id="H8"//trim(band)//"AMV"
   name="HIMAWARI-8 AMV"
   platform="FM-88 SATOB"
   source="RJTD JMA HIMAWARI-8 AMV"
   write(date_char,"(6X,I4.4,5I2.2)" ) year, month, day, hour, minute, second
   ! header:
   WRITE ( UNIT = ounit    , FMT = rpt_format )                             &
           lat             , lon        , id           , name             , &
           platform        , source     , lr_missing   , 6                , &
           0               , 0          , 1            , 0                , &
           .true.          , .false.    , .false.      , int(lr_missing)  , &
           int(lr_missing) , date_char  , lr_missing   , 0                , &
           lr_missing      , 0          , lr_missing   , 0                , &
           lr_missing      , 0          , lr_missing   , 0                , &
           lr_missing      , 0          , lr_missing   , 0                , &
           lr_missing      , 0          , lr_missing   , 0                , &
           lr_missing      , 0          , lr_missing   , 0                , &
           lr_missing      , 0          , lr_missing   , 0

   ! report:
   WRITE ( UNIT = ounit  , FMT = meas_format )                &
           prs        , 0 , lr_missing , 0 , lr_missing , 0 , &
           lr_missing , 0 , spd        , 0 , dir        , 0 , &
           lr_missing , 0 , lr_missing , 0 , lr_missing , 0 , lr_missing , 0
   

   ! end of report line:
   WRITE ( UNIT = ounit, FMT = meas_format )                    &
           lr_end_data , 0 , lr_end_data , 0 , 6.         , 0 , &
           lr_missing  , 0 , lr_missing  , 0 , lr_missing , 0 , &
           lr_missing  , 0 , lr_missing  , 0 , lr_missing , 0 , &
           lr_missing  , 0

   ! end of message line:
   WRITE ( UNIT = ounit  , FMT = end_format )  6, 0, 0

   end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

