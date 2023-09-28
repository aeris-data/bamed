MODULE BLPB_FIELD

CONTAINS

    SUBROUTINE initfield(path,gribfile,psize,llev,nt,coeff,u,v,t,p,rho,h,w,tp)
        USE BLPB_GRIBF
        USE BLPB_TIME

        IMPLICIT NONE

        !In variables
        integer :: llev,nt
        integer,dimension(3) :: psize
        real,dimension(2*psize(3)) :: coeff
        character*(*) :: path
        character*(*), dimension(nt) :: gribfile
        !Out variables
        real, dimension(psize(1),psize(2),psize(3),nt) :: u,v,t,p,rho,h,w
        real, dimension(psize(1),psize(2),nt) :: tp
        !Local variables
        integer :: nlon,nlat,nlev,k
        real,dimension(:,:),allocatable :: ps
        real,dimension(:,:,:),allocatable :: q

        nlon=psize(1)
        nlat=psize(2)
        nlev=psize(3)

        allocate(ps(nlon,nlat),q(nlon,nlat,nlev))

        do k=1,nt
            call loadfield(path//gribfile(k),nlon,nlat,nlev,llev,u(:,:,:,k),v(:,:,:,k),t(:,:,:,k),w(:,:,:,k),tp(:,:,k),ps,q)
            call pressure(ps,nlon,nlat,nlev,coeff,p(:,:,:,k))
            call density(p(:,:,:,k),t(:,:,:,k),q,nlon,nlat,nlev,rho(:,:,:,k))
            call height(p(:,:,:,k),t(:,:,:,k),q,nlon,nlat,nlev,h(:,:,:,k))
        end do

        deallocate(ps,q)

    END SUBROUTINE initfield

    SUBROUTINE updatefield(path,gribfile,psize,llev,nt,nl,coeff,u,v,t,p,rho,h,w,tp)

        USE BLPB_GRIBF

        IMPLICIT NONE
        !In variables
        integer :: llev,nt,nl
        integer,dimension(3) :: psize
        real,dimension(2*psize(3)) :: coeff
        character*(*) :: path
        character*(*), dimension(nl) :: gribfile
        !Out variables
        real, dimension(psize(1),psize(2),psize(3),nt) :: u,v,t,p,rho,h,w
        real, dimension(psize(1),psize(2),nt) :: tp
        !Local variables
        integer :: nlon,nlat,nlev,k
        real,dimension(:,:),allocatable :: ps
        real,dimension(:,:,:),allocatable :: q

        nlon=psize(1)
        nlat=psize(2)
        nlev=psize(3)

        do k=1,nl
            u(:,:,:,k)=u(:,:,:,k+nl)
            v(:,:,:,k)=v(:,:,:,k+nl)
            t(:,:,:,k)=t(:,:,:,k+nl)
            p(:,:,:,k)=p(:,:,:,k+nl)
            rho(:,:,:,k)=rho(:,:,:,k+nl)
            h(:,:,:,k)=h(:,:,:,k+nl)
            w(:,:,:,k)=w(:,:,:,k+nl)
            tp(:,:,k)=tp(:,:,k+nl)
        end do

        allocate(ps(nlon,nlat),q(nlon,nlat,nlev))
        do k=1,nl
            call loadfield(path//gribfile(k),nlon,nlat,nlev,llev,u(:,:,:,k+nl),v(:,:,:,k+nl),t(:,:,:,k+nl),w(:,:,:,k+nl),&
                    &tp(:,:,k+nl),ps,q)
            call pressure(ps,nlon,nlat,nlev,coeff,p(:,:,:,k+nl))
            call density(p(:,:,:,k+nl),t(:,:,:,k+nl),q,nlon,nlat,nlev,rho(:,:,:,k+nl))
            call height(p(:,:,:,k+nl),t(:,:,:,k+nl),q,nlon,nlat,nlev,h(:,:,:,k+nl))
        end do

        deallocate(ps,q)

    END SUBROUTINE updatefield

    !Added for tp interpolation (2D parameter)
    SUBROUTINE localfield2D(field,x,y,t,psize,ipos,pos,lux,luy,lut,fp)
        USE BLPB_SPLINE
        USE BLPB_PARAM

        IMPLICIT NONE
        !In variables
        integer,dimension(3) :: psize
        integer,dimension(2) :: ipos
        real,dimension(3) :: pos
        real,dimension(psize(1),psize(2),nload) :: field
        real,dimension(nload) :: t
        real,dimension(3,nload) :: lut
        real,dimension(nxi) :: x
        real,dimension(3,nxi) :: lux
        real,dimension(nyi) :: y
        real,dimension(3,nyi) :: luy
        !Out 2D variable interpolated value
        real fp
        !Local variables
        integer :: nlon,nlat,il,jl
        real :: xp,yp,tp
        integer :: i,j,p,if,jf
        real,dimension(:), allocatable :: fx,fy,ft,d2fx,d2fy,d2ft

        nlon=psize(1)
        nlat=psize(2)
        il=ipos(1)
        jl=ipos(2)
        xp=pos(1)
        yp=pos(2)
        tp=pos(3)

        allocate(ft(nload),d2ft(nload))
        allocate(fx(nxi),d2fx(nxi))
        allocate(fy(nyi),d2fy(nyi))

        do j=1,nyi !loop over latitudes
            jf=(j+jl)
            do i=1,nxi !loop over longitudes
                if=(i+il)
                !Time interpolation
                do p=1,nload
                    ft(p)=field(if,jf,p)
                end do
                call d2spline(nload,t,ft,lut,d2ft)
                call cubic(nload,t,ft,d2ft,1,tp,fx(i))
            end do
            !Longitude interpolation
            call d2spline(nxi,x,fx,lux,d2fx)
            call cubic(nxi,x,fx,d2fx,nxm,xp,fy(j))
        end do
        !Latitude interpolation
        call d2spline(nyi,y,fy,luy,d2fy)
        call cubic(nyi,y,fy,d2fy,nym,yp,fp)

        deallocate(ft,fx,fy,d2ft,d2fx,d2fy)

    END SUBROUTINE localfield2D


    SUBROUTINE localfield(field,x,y,t,psize,ipos,pos,lux,luy,lut,fp)
        USE BLPB_SPLINE
        USE BLPB_PARAM

        IMPLICIT NONE
        !In variables
        integer,dimension(3) :: psize
        integer,dimension(2) :: ipos
        real,dimension(3) :: pos
        real,dimension(psize(1),psize(2),psize(3),nload) :: field
        real,dimension(nload) :: t
        real,dimension(3,nload) :: lut
        real,dimension(nxi) :: x
        real,dimension(3,nxi) :: lux
        real,dimension(nyi) :: y
        real,dimension(3,nyi) :: luy
        !Out variables
        real, dimension(psize(3)) :: fp
        !Local variables
        integer :: nlon,nlat,nlev,il,jl
        real :: xp,yp,tp
        integer :: i,j,k,p,if,jf
        real,dimension(:), allocatable :: fx,fy,ft,d2fx,d2fy,d2ft

        nlon=psize(1)
        nlat=psize(2)
        nlev=psize(3)
        il=ipos(1)
        jl=ipos(2)
        xp=pos(1)
        yp=pos(2)
        tp=pos(3)

        allocate(ft(nload),d2ft(nload))
        allocate(fx(nxi),d2fx(nxi))
        allocate(fy(nyi),d2fy(nyi))

        do k=1,nlev !loop over levels
            do j=1,nyi !loop over latitudes
                jf=(j+jl)
                do i=1,nxi !loop over longitudes
                    if=(i+il)
                    !Time interpolation
                    do p=1,nload
                        ft(p)=field(if,jf,k,p)
                    end do
                    call d2spline(nload,t,ft,lut,d2ft)
                    !call cubic(nload,t,ft,d2ft,tp,fxyp(i,j,k))
                    call cubic(nload,t,ft,d2ft,1,tp,fx(i))
                end do
                !Longitude interpolation
                call d2spline(nxi,x,fx,lux,d2fx)
                call cubic(nxi,x,fx,d2fx,nxm,xp,fy(j))
            end do
            !Latitude interpolation
            call d2spline(nyi,y,fy,luy,d2fy)
            call cubic(nyi,y,fy,d2fy,nym,yp,fp(k))
        end do

        deallocate(ft,fx,fy,d2ft,d2fx,d2fy)

    END SUBROUTINE localfield

    SUBROUTINE ballfield(field,z,nz,luz,zi,fzi)
        USE BLPB_SPLINE
        USE BLPB_PARAM

        IMPLICIT NONE
        !In variables
        integer :: nz
        real :: zi
        real,dimension(nz) :: z,field
        real,dimension(3,nz) :: luz
        !Out variables
        real :: fzi
        !Local variables
        integer :: nz2
        real,dimension(nz) :: d2fz

        nz2=nz/2

        !if ((z(nz)-z(nz-1)).eq.(0.)) then
        !    write(*,*) '### ERROR in spline interpolation ',z
        !    stop
        !end if

        call d2spline(nz,z,field,luz,d2fz)
        call cubic(nz,z,field,d2fz,nz2,zi,fzi)

    END SUBROUTINE ballfield

END MODULE BLPB_FIELD
