MODULE BLPB_GRID

CONTAINS

    SUBROUTINE bpclgrid(lon0,lat0,nlon,nlat,grid,xlon,ylat)
        !-------------------------------------------------------------------------------
        ! subroutine bpclgrid
        !
        ! DESCRIPTION :
        !    Compute grid for bpcl balloon advection
        !
        ! PARAMETERS :
        !    -
        !
        ! RETURN :
        !    -
        !
        ! HISTORY :
        ! 11/12/2013   | AFontaine (OMP-SEDOO)   |    creation
        !-------------------------------------------------------------------------------
        USE BLPB_PARAM

        IMPLICIT NONE

        real :: lon0,lat0,grid
        integer :: nlon,nlat
        real,dimension(nlon) :: xlon
        real,dimension(nlat) :: ylat
        integer :: i
        real :: dl,x0,y0

        dl=grid*pi180
        x0=lon0*pi180
        y0=lat0*pi180

        do i=1,nlon
            xlon(i)=x0+real(i-1)*dl
        enddo

        do i=1,nlat
            ylat(i)=y0+real(i-1)*dl
        enddo


        return

    END SUBROUTINE bpclgrid

    SUBROUTINE locate(xlon,ylat,nlon,nlat,xb,yb,i,j,ii,jj)
        !-------------------------------------------------------------------------------
        ! subroutine locate
        !
        ! DESCRIPTION :
        !    Find balloon position on grid
        !
        ! PARAMETERS :
        !    - x
        !    - y
        !
        ! RETURN :
        !    - i
        !    - j
        !
        ! HISTORY :
        ! 13/01/2014   | AFontaine (OMP-SEDOO)   |    creation
        !-------------------------------------------------------------------------------

        USE BLPB_PARAM

        IMPLICIT NONE

        !In variables
        integer :: nlon,nlat
        real :: xb,yb
        real,dimension(nlon) :: xlon
        real,dimension(nlat) :: ylat
        !Out variables
        integer :: i,j,ii,jj
        !Local variables
        real :: x,y

        i=1
        x=xlon(1)
        do while (x.lt.xb)
            x=xlon(i)
            i=i+1
        end do

        j=1
        y=ylat(1)
        do while (y.lt.yb)
            y=ylat(j)
            j=j+1
        end do

        i=i-1
        j=j-1

        !Subgrid first indices for spline interpolation
        ii=max(i-nxm1,1)
        ii=min(ii,nlon-nxi)

        jj=max(j-nym1,1)
        jj=min(jj,nlat-nyi)


    END SUBROUTINE locate

END MODULE BLPB_GRID
