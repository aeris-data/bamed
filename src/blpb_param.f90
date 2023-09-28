!-------------------------------------------------------------------------------
! parameters file for bpcl trajectory
!
! PARAMETERS :
!   -
!   -
!   -
!
! HISTORY :
! 11/12/2013   | AFontaine (OMP-SEDOO)   |    creation
!-------------------------------------------------------------------------------

MODULE BLPB_PARAM

    IMPLICIT NONE

    ! pi            number "pi"
    ! r_earth       radius of earth [m]
    ! r_air         individual gas constant for dry air [J/kg/K]
    real,parameter :: pi=3.14159265, r_earth=6.371e6, ga=9.81
    real,parameter :: m_air=28.9644, m_vap=18.016, r_gp=8.31446, p0=101315.
    real,parameter :: r_air=r_gp/(m_air*0.001), mmr=m_vap/m_air, immr=m_air/m_vap, hum=immr-1

    ! pi180         pi/180.
    ! ipi180        180/pi
    ! twopi         2*pi
    real,parameter :: pi180=pi/180., ipi180=180./pi, twopi=2.*pi


    ! d2s           to convert day to second
    ! s2d           to convert second to day
    real,parameter :: d2s=24.*3600., s2d=1./d2s


    ! parnum        number of fields
    ! griblist      list of grib filenames
    ! parlist       list of field names
    integer,parameter :: parnum=7
    character*6, dimension(parnum) :: parlist=(/'t   ','u   ','v   ','q   ','lnsp','w   ','tp  '/)

    ! Spline interpolation
    ! nxi           spline interpolation subgrid x size
    ! nyi           spline interpolation subgrid y size
    integer,parameter :: nxi=8, nyi=8
    integer,parameter :: nxm=nxi/2, nym=nyi/2, nxm1=nxm-1, nym1=nym-1
    real,parameter :: R3=1./3., R6=1./6.

    !Time parameter
    integer,parameter :: nload=6
    integer,parameter :: nupd=nload/2

    !Runge Kutta parameter
    integer,parameter :: nrk=2
    real,dimension(nrk) :: ark=(/0.5,1./)



END MODULE BLPB_PARAM




