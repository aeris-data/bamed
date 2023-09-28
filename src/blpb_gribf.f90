MODULE BLPB_GRIBF

CONTAINS

    SUBROUTINE pressure(ps,ni,nj,nz,coeff,p)
        !-------------------------------------------------------------------------------
        ! subroutine pressure
        !
        ! DESCRIPTION :
        ! Compute pressure from surface pressure
        !
        ! PARAMETERS :
        !    - ps
        !    - ni : number of longitude points
        !    - nj : number of latitude points
        !    - nz : number of level
        !
        ! RETURN :
        !    - p : pressure
        !
        ! HISTORY :
        ! 21/01/2014   | AFontaine (OMP-SEDOO)   |    creation
        !-------------------------------------------------------------------------------

        IMPLICIT NONE

        !In variables
        integer :: ni,nj,nz
        real, dimension(2*nz) :: coeff
        real,dimension(ni,nj) :: ps
        !Out variables
        real,dimension(ni,nj,nz) :: p
        !Local variables
        integer :: k

        do k=1,nz
            p(:,:,k)=coeff(k)+coeff(nz+k)*ps
        end do

    END SUBROUTINE pressure


    SUBROUTINE density(p,t,q,ni,nj,nz,rho)
        !-------------------------------------------------------------------------------
        ! subroutine density
        !
        ! DESCRIPTION :
        ! Compute density from pressure
        !
        ! PARAMETERS :
        !    - ni : number of longitude points
        !    - nj : number of latitude points
        !    - nz : number of level
        !
        ! RETURN :
        !    - rho : density
        !
        ! HISTORY :
        ! 21/01/2014   | AFontaine (OMP-SEDOO)   |    creation
        !-------------------------------------------------------------------------------

        USE BLPB_PARAM

        IMPLICIT NONE

        !In variables
        integer :: ni,nj,nz
        real,dimension(ni,nj,nz) :: p,t,q
        !Out variables
        real,dimension(ni,nj,nz) :: rho

        rho=p/(r_air*t*(1.+hum*q))


    END SUBROUTINE density

    SUBROUTINE height(p,t,q,ni,nj,nz,h)
        !-------------------------------------------------------------------------------
        ! subroutine density
        !
        ! DESCRIPTION :
        ! Compute density from pressure
        !
        ! PARAMETERS :
        !    - ni : number of longitude points
        !    - nj : number of latitude points
        !    - nz : number of level
        !
        ! RETURN :
        !    - h : height
        !
        ! HISTORY :
        ! 21/01/2014   | AFontaine (OMP-SEDOO)   |    creation
        !-------------------------------------------------------------------------------

        USE BLPB_PARAM

        IMPLICIT NONE

        !In variables
        integer :: ni,nj,nz
        real,dimension(ni,nj,nz) :: p,t,q
        !Out variables
        real,dimension(ni,nj,nz) :: h
        !Local var
        integer :: k

        h(:,:,nz)=0.

        do k=nz-1,1,-1
            !dh=-r/g*Tv*dP/P
            !h(:,:,k)=h(:,:,k-1)-r_air/ga*log(p(:,:,k)/p(:,:,k-1))*0.5*(t(:,:,k-1)*(1.+hum*q(:,:,k-1))+t(:,:,k)*(1.+hum*q(:,:,k)))
            h(:,:,k)=h(:,:,k+1)-r_air/ga*log(p(:,:,k)/p(:,:,k+1))*0.5*(t(:,:,k+1)*(1.+hum*q(:,:,k+1))+t(:,:,k)*(1.+hum*q(:,:,k)))
        end do


    END SUBROUTINE height

    SUBROUTINE checkgrib(filename,ni,nj,nlev,llev,date,time)
        !-------------------------------------------------------------------------------
        ! subroutine checkgrib
        !
        ! DESCRIPTION :
        ! Get parameters in a grib file in simple grid format and return values in field
        !
        ! PARAMETERS :
        !    - filename: name of the grib file
        !
        ! RETURN :
        !    - ni : number of point along meridian
        !    - nj : number of point along parallel
        !    - nlev : number of level
        !
        ! HISTORY :
        ! 08/01/2014   | AFontaine (OMP-SEDOO)   |    creation
        !-------------------------------------------------------------------------------

        USE GRIB_API
        USE BLPB_PARAM

        IMPLICIT NONE

        !In variable
        character*(*) :: filename
        !Out variable
        integer :: ni,nj,nlev,llev,date,time,step
        !Local variables
        integer :: ifile,iret,n,i,igrib,lev0,k,error
        integer, dimension(parnum) :: pcount
        character*6 :: pname

        error=0

        do k=1,parnum
            pcount(k)=0
        end do

        !Open grib file
        write(*,*) 'check:',filename
        call grib_open_file(ifile,filename,'r')
        !Count size
        call grib_count_in_file(ifile,n)

        call grib_new_from_file(ifile,igrib,iret)
        !Get size of data from first message
        !get number of point along meridian
        call grib_get_int(igrib,'Ni',ni,iret)
        call grib_check(iret,'checkgrib','Cannot get Ni in grib file')
        !get number of point along parallel
        call grib_get_int(igrib,'Nj',nj,iret)
        call grib_check(iret,'checkgrib','Cannot get Nj in grib file')
        !get date
        call grib_get_int(igrib,'dataDate',date,iret)
        call grib_check(iret,'checkgrib','Cannot get date in grib file')
        !get time
        call grib_get_int(igrib,'dataTime',time,iret)
        call grib_check(iret,'checkgrib','Cannot get time in grib file')
        !get step
        call grib_get_int(igrib,'stepRange',step,iret)
        call grib_check(iret,'checkgrib','Cannot get step in grib file')
        !get current level
        call grib_get_int(igrib,'level',lev0,iret)
        call grib_check(iret,'checkgrib','Cannot get level in grib file')
        !get param
        call grib_get(igrib,'shortName',pname)
        call grib_check(iret,'checkgrib','Cannot get param')
        do k=1,parnum
            if (pname.eq.parlist(k)) then
                pcount(k)=pcount(k)+1
            end if
        end do

        ! release
        call grib_release(igrib)

        !Load message from file (parlist is defined in blpb_param.f90)
        do i=2,n
            call grib_new_from_file(ifile,igrib,iret)
            call grib_get(igrib,'shortName',pname)
            call grib_check(iret,'checkgrib','Cannot get param')
            do k=1,parnum
                if (pname.eq.parlist(k)) then
                    pcount(k)=pcount(k)+1
                end if
            end do

            !release grib
            call grib_release(igrib)
        end do

        !Close file
        call grib_close_file(ifile)

        !check the existence of the parameter
        do k=1,parnum
            if (pcount(k).eq.0) then
                write(*,*) 'Missing parameter:',parlist(k)
                error=1
            end if
        end do

        !check the level count of the 3D parameters
        if ((pcount(1).ne.pcount(2)).or.(pcount(1).ne.pcount(3)).or.(pcount(1).ne.pcount(4)).or.(pcount(1).ne.pcount(6))) then
            write(*,*) 'Inconsistant parameters size in file'
            write(*,*) pcount
            error=1
        end if

        if (error.eq.1) then
            write(*,*) 'check file:',filename
            stop
        end if

        nlev=pcount(1)-1
        llev=lev0+nlev

        time=time*100+step*10000

        return

    END SUBROUTINE checkgrib

    SUBROUTINE readfield(filename,field,nv,nlev,param)
        !-------------------------------------------------------------------------------
        ! subroutine readfield
        !
        ! DESCRIPTION :
        ! Get a parameters in a simple grid format and return a in 2d array
        !
        ! PARAMETERS :
        !    - filename: name of the grib file
        !    - nv : number of values
        !    - nlev : number of level
        !    - param: short name of parameter to get
        !
        ! RETURN :
        !    - field(nv*nlev) : values of param
        !
        ! HISTORY :
        ! 08/01/2014   | AFontaine (OMP-SEDOO)   |    creation
        !-------------------------------------------------------------------------------

        USE GRIB_API

        IMPLICIT NONE

        !In variables
        integer :: nv,nlev
        character*(*) :: param
        character*(*) :: filename
        !Out variables
        real,dimension(nv,nlev) :: field
        !Local variables
        integer :: ifile,n,i,igrib,j,iret,nval,ilev
        character*10 :: pname

        !Open grib file
        call grib_open_file(ifile,trim(filename),'r')
        !Count size
        call grib_count_in_file(ifile,n)
        write(*,*)'Open grib file: ',filename,n
        write(*,*)'readfield get param: ',param

        igrib=-1
        !Load parameter's message from file
        j=1
        do i=1,n
            call grib_new_from_file(ifile,igrib,iret)
            call grib_get(igrib,'shortName',pname)
            call grib_check(iret,'readfield','Cannot get param')
            if (pname.eq.param) then
                !get current level
                call grib_get_int(igrib,'level',ilev)
                !get number of values nval
                call grib_get_size(igrib,'values',nval)
                if (nval.ne.nv) then
                    write(*,*) nval,nv
                    stop '*** ERROR: inconsistent size in grib file'
                end if
                !get values
                call grib_get(igrib,'values',field(:,j))
                j=j+1
            end if
            call grib_release(igrib)
        end do

        !Close file
        call grib_close_file(ifile)


        if (j.lt.nlev) then
            write(*,*) j,' expecting: ',nlev !debug
            stop '*** ERROR: inconsistent number of level in grib file'
        end if

    END SUBROUTINE readfield

    SUBROUTINE readpv(gribfile,nlev,llev,coeff)
        !-------------------------------------------------------------------------------
        ! subroutine readpv
        !
        ! DESCRIPTION :
        ! Load meteorological fields from grib file
        !
        ! PARAMETERS :
        !    - gribfile : complete path to grib file
        !    - nlev : number of level in file
        !    - llev : number of maximum level
        !
        ! RETURN :
        !    - coeff : pressure coefficient
        !
        ! HISTORY :
        ! 21/01/2014   | AFontaine (OMP-SEDOO)   |    creation
        !-------------------------------------------------------------------------------

        USE GRIB_API

        IMPLICIT NONE

        !In variables
        integer :: nlev,llev
        character*(*) :: gribfile
        !Out variables
        real, dimension(2*nlev) :: coeff
        !Local variables
        integer :: igrib,iret,k,kl,flev,ifile
        real,dimension(:),allocatable :: pv

        allocate(pv(2*llev+2))

        flev=llev-nlev+1

        !Open grib file
        call grib_open_file(ifile,trim(gribfile),'r')
        !Count size
        !write(*,*),'read pv values in: ',gribfile

        !Load pv from first message
        call grib_new_from_file(ifile,igrib,iret)
        call grib_get(igrib,'pv',pv,iret)
        call grib_check(iret,'readpv','Cannot read pressure coefficient in file')
        call grib_release(igrib)


        do k=1,nlev
            kl=flev+k-1
            coeff(k)=0.5*(pv(kl)+pv(kl+1))
            coeff(k+nlev)=0.5*(pv(kl+llev+1)+pv(kl+llev+2))
            !write(*,*) 'coeff: ',kl,coeff(k),coeff(k+nlev)
        end do

        deallocate(pv)

    END SUBROUTINE readpv

    SUBROUTINE loadfield(gribfile,nlon,nlat,nlev,llev,u,v,t,w,tp,ps,q)
 
        USE GRIB_API
        USE BLPB_PARAM

        IMPLICIT NONE

        !In variables
        integer :: nlon,nlat,nlev,llev
        character*(*) :: gribfile
        !Out variables
        real,dimension(nlon,nlat,nlev) :: u,v,t,q,w
        real,dimension(nlon,nlat) :: tp,ps
        !Local variables
        integer :: ifile,n,i,igrib,j,iret,nval,ilev,flev
        character*10 :: param
        real, dimension(:),allocatable :: values


        nval=nlon*nlat
        flev=llev-nlev+1

        allocate(values(nval))

        !Open grib file
        call grib_open_file(ifile,trim(gribfile),'r')
        !Count size
        call grib_count_in_file(ifile,n)
        !write(*,*),'Open grib file: ',gribfile,n
        !write(*,*) 'first lev:',flev

        igrib=-1
        !Load parameter's message from file
        do i=1,n
            call grib_new_from_file(ifile,igrib,iret)
            !get current level
            call grib_get_int(igrib,'level',ilev)
            j=ilev-flev+1
            !write(*,*) j,ilev
            call grib_get(igrib,'shortName',param)
            call grib_check(iret,'loadfield','Cannot get param')

            !get values
            if (param.eq.'u') then
            !write(*,*) 'u   ',ilev,j
                call grib_get(igrib,'values',values)
                u(:,:,j)=reshape(values,(/nlon,nlat/))
            end if
            if (param.eq.'v') then
            !write(*,*) 'v   ',ilev,j
                call grib_get(igrib,'values',values)
                v(:,:,j)=reshape(values,(/nlon,nlat/))
            end if
            if (param.eq.'t') then
            !write(*,*) 't   ',ilev,j
                call grib_get(igrib,'values',values)
                t(:,:,j)=reshape(values,(/nlon,nlat/))
            end if
            if (param.eq.'w') then
            !write(*,*) 'w   ',ilev,j
                call grib_get(igrib,'values',values)
                w(:,:,j)=reshape(values,(/nlon,nlat/))
            end if
            if (param.eq.'q') then
            !write(*,*) 'q   ',ilev,j
                call grib_get(igrib,'values',values)
                q(:,:,j)=reshape(values,(/nlon,nlat/))
            end if
            if (param.eq.'lnsp') then
            !write(*,*) 'lnsp'
                call grib_get(igrib,'values',values)
                ps(:,:)=reshape(values,(/nlon,nlat/))
            end if
            if (param.eq.'tp') then
            !write(*,*) 'tp'
                call grib_get(igrib,'values',values)
                tp(:,:)=reshape(values,(/nlon,nlat/))
            end if

            call grib_release(igrib)
        end do

        !Surface pressure is usually written in ln(ps) in ECMWF grib
        ps=exp(ps)
        !write(*,*) 'ps:',maxval(lnsp),minval(lnsp)

        deallocate(values)

    END SUBROUTINE loadfield



END MODULE BLPB_GRIBF
