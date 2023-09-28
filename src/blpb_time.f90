MODULE BLPB_TIME

CONTAINS

    SUBROUTINE juldate(yyyymmdd,hhmmss,jdate)
        !-------------------------------------------------------------------------------
        ! subroutine juldate
        !
        ! DESCRIPTION :
        ! return julian date for current yyyymmdd and hhmmss
        !
        ! PARAMETERS :
        !    - yyyymmdd
        !    - hhmmss
        !
        ! RETURN :
        !    - jdate
        !
        ! HISTORY :
        ! 08/01/2014   | AFontaine (OMP-SEDOO)   |    creation
        !-------------------------------------------------------------------------------

        IMPLICIT NONE

        !In variable
        integer :: yyyymmdd,hhmmss
        !Out variable
        real*8 :: jdate
        !Local variables
        integer :: yyyy,mo,dd,hh,mi,ss,a,jy,jm,jdn

        yyyy=yyyymmdd/10000
        mo=(yyyymmdd-yyyy*10000)/100
        dd=yyyymmdd-yyyy*10000-mo*100
        hh=hhmmss/10000
        mi=(hhmmss-hh*10000)/100
        ss=hhmmss-hh*10000-mi*100

        a=(14-mo)/12
        jy=yyyy-a+4800
        jm=mo+12*a-3

        jdn=dd+(153*jm+2)/5+365*jy+jy/4-jy/100+jy/400-32045

        jdate=dble(jdn)+dble((hh-12)/24.)+dble(mi/1440.)+dble(ss/86400.)


    END SUBROUTINE juldate

    SUBROUTINE timelist(date,time,ndate,tlist)

        IMPLICIT NONE

        !In variable
        integer :: ndate
        integer,dimension(ndate) :: date,time
        !Out variable
        real,dimension(ndate) :: tlist
        !Local variables
        integer :: i
        real*8 :: jdate0,jdate

        call juldate(date(3),time(3),jdate0)

        do i=1,ndate
            call juldate(date(i),time(i),jdate)
            tlist(i)=real(jdate-jdate0)
        end do

    END SUBROUTINE timelist

    SUBROUTINE timestep(u,v,dx,dy,dt0,dtc)
        !-------------------------------------------------------------------------------
        ! subroutine timestep
        !
        ! DESCRIPTION :
        ! return julian date for current yyyymmdd and hhmmss
        !
        ! PARAMETERS :
        !    -
        !    -
        !
        ! RETURN :
        !    - jdate
        !
        ! HISTORY :
        ! 17/01/2014   | AFontaine (OMP-SEDOO)   |    creation
        !-------------------------------------------------------------------------------

        USE BLPB_PARAM

        IMPLICIT NONE

        !In variable
        real :: u,v,dx,dy,dt0
        !Out variable
        real :: dtc
        !Local variables
        real :: dtu,dtv

        dtu=dx/abs(u)
        dtv=dy/abs(v)

        dtc=min(dtv,dtu)

        if (dtc.lt.dt0) then
            write(*,'("WARNING: nominal timestep dt=",f12.8,"s bigger than cfl timestep: ",f12.8,"s")') dt0,dtc
        end if
        !dtc=min(dtc,dt0)


    END SUBROUTINE timestep


END MODULE BLPB_TIME
