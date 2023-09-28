MODULE BLPB_CHECK

CONTAINS

    SUBROUTINE checkflight(nbal,status,flight)
        !-------------------------------------------------------------------------------
        ! subroutine checkflight
        !
        ! DESCRIPTION :
        ! check field in grib file for bplc_trajectory
        !
        ! PARAMETERS :
        !    - nlev : number of level
        !    - llev : last level
        !    - dtfield : timestep in meteo field
        !    -
        !
        ! RETURN :
        !    -
        !    -
        !
        ! HISTORY :
        ! 16/12/2013   | AFontaine (OMP-SEDOO)   |    creation
        !-------------------------------------------------------------------------------

        IMPLICIT NONE

        !In var
        integer :: nbal
        integer,dimension(nbal) :: status
        !Out var
        logical :: flight
        !Local var
        integer :: k

        flight=.false.

        do k=1,nbal
            if (status(k).eq.1) then
                flight=.true.
            end if
        end do

    END SUBROUTINE checkflight

    SUBROUTINE checkhoriz(ibal,xmin,xmax,ymin,ymax,xb,yb,status)
        !-------------------------------------------------------------------------------
        ! subroutine checkhoriz
        !
        ! DESCRIPTION :
        ! check field in grib file for bplc_trajectory
        !
        ! PARAMETERS :
        !    - nlev : number of level
        !    - llev : last level
        !    - dtfield : timestep in meteo field
        !    -
        !
        ! RETURN :
        !    -
        !    -
        !
        ! HISTORY :
        ! 16/12/2013   | AFontaine (OMP-SEDOO)   |    creation
        !-------------------------------------------------------------------------------

        IMPLICIT NONE

        integer :: ibal
        real :: xmin,xmax,ymin,ymax,xb,yb
        integer :: status

        if (xb.lt.xmin) then
            !write(*,*) xb,' domain west-longitude: ',xmin
            status=0
        elseif (xb.gt.xmax) then
            !write(*,*) xb,' domain east-longitude: ',xmax
            status=0
        endif

        if (yb.lt.ymin) then
            !write(*,*) yb,' domain south-latitude: ',ymin
            status=0
        elseif (yb.gt.ymax) then
            !write(*,*) yb,' domain nort-latitude: ',ymax
            status=0
        endif


    END SUBROUTINE checkhoriz

    SUBROUTINE checklev(ibal,lev,top,down,status)
        !-------------------------------------------------------------------------------
        ! subroutine checklev
        !
        ! DESCRIPTION :
        ! check field in grib file for bplc_trajectory
        !
        ! PARAMETERS :
        !    - nlev : number of level
        !    - llev : last level
        !    - dtfield : timestep in meteo field
        !    -
        !
        ! RETURN :
        !    -
        !    -
        !
        ! HISTORY :
        ! 16/12/2013   | AFontaine (OMP-SEDOO)   |    creation
        !-------------------------------------------------------------------------------

        IMPLICIT NONE

        !In var
        integer :: ibal
        real :: lev,top,down
        !In/Out var
        integer :: status

        if (lev.gt.down) then
            !write(*,*) ' Exit throuht domain bottom-level: ',ibal,lev,down,status
            status=4
        elseif (lev.lt.top) then
            !write(*,*) ' Exit throuht domain top-level: ',ibal,lev,top,status
            status=5
        endif

    END SUBROUTINE checklev

    SUBROUTINE checkfield(nfield,path,fname,date,time,nlev,llev,dtfield)
        !-------------------------------------------------------------------------------
        ! subroutine checkfield
        !
        ! DESCRIPTION :
        ! check field in grib file for bplc_trajectory
        !
        ! PARAMETERS :
        !    - nlev : number of level
        !    - llev : last level
        !    - dtfield : timestep in meteo field
        !    -
        !
        ! RETURN :
        !    -
        !    -
        !
        ! HISTORY :
        ! 16/12/2013   | AFontaine (OMP-SEDOO)   |    creation
        !-------------------------------------------------------------------------------

        USE BLPB_TIME
        USE BLPB_GRIBF

        IMPLICIT NONE

        integer :: nfield,nlev,llev,dtfield
        integer :: i,klev
        integer :: ni,nj,nz,d,t,step,t0,dt
        real(8) :: cdate,idate

        character,parameter :: tab=char(9) 

        integer, dimension(nfield) :: date,time
        character*(*) :: path
        character*(*), dimension(nfield) :: fname

        t0=0
        do i=1,nfield
            call juldate(date(i),time(i),cdate)
            call checkgrib(path//fname(i),ni,nj,nz,klev,d,t)
            write(*,*) '*** ',ni,nj,nz,klev
            call juldate(d,t,idate)
            write(*,*) '*** ',date(i),time(i),cdate
            write(*,*) '*** ',d,t,idate
            !check date
            if (cdate.ne.idate) then
                write(*,*) tab,'Invalid date in ',fname(i)
                stop
            end if
            !check timestep
            dt=t-t0
            if (dt.lt.0) then
                dt=dt+240000
            end if
            if ((i.gt.1).and.(dt.ne.dtfield)) then
                write(*,*) tab,'Invalid time interval between ',fname(i-1),' ',fname(i),' ',dt
                stop
            end if
            t0=t
            !check size

        end do



        return

    END SUBROUTINE checkfield

END MODULE BLPB_CHECK
