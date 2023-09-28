MODULE BLPB_SPLINE

CONTAINS

    SUBROUTINE luspline(n,x,lu)
        !-------------------------------------------------------------------------------
        ! subroutine lusplit
        !
        ! DESCRIPTION :
        !    Compute LU decomposition matrix for spline
        !
        ! PARAMETERS :
        !    -
        !    -
        !
        ! RETURN :
        !    -
        !    -
        !
        ! HISTORY :
        ! 13/01/2014   | AFontaine (OMP-SEDOO)   |    creation
        !-------------------------------------------------------------------------------

        USE BLPB_PARAM

        IMPLICIT NONE

        !In variables
        integer :: n
        real,dimension(n) :: x
        !Out variables
        real,dimension(3,n) :: lu
        !Local variables
        integer :: i

        !write(*,*) 'input:',n,'--',x
        !write(*,*) 'size:',shape(x),'-',shape(lu)

        !write(*,*) 1./x(3)-x(2)
        lu(1,1)=1./(x(3)-x(2))
        lu(3,1)=x(1)-x(3)
        lu(3,n)=x(2)-x(1)
        lu(2,2)=(x(2)-x(1))*R6*lu(1,1)
        lu(3,2)=(x(3)-x(2))*R6-lu(2,2)*lu(3,n)
        lu(1,2)=1./((x(3)-x(1))*R3-lu(2,2)*lu(3,1))

        do i=3,n-1
            lu(3,i)=(x(i+1)-x(i))*R6
            lu(2,i)=(x(i)-x(i-1))*R6*lu(1,i-1)
            !write(*,*) '***',(x(i+1)-x(i-1))*R3,lu(2,i)*lu(3,i-1)
            lu(1,i)=1./((x(i+1)-x(i-1))*R3-lu(2,i)*lu(3,i-1))
        end do
        lu(2,1)=(x(n)-x(n-1))*lu(1,n-2)
        lu(2,n)=(x(n-2)-x(n)-lu(2,1)*lu(3,n-2))*lu(1,n-1)
        lu(1,n)=1./(x(n-1)-x(n-2)-lu(2,n)*lu(3,n-1))

    END SUBROUTINE luspline

    SUBROUTINE d2spline(n,x,y,lu,z)
        !-------------------------------------------------------------------------------
        ! subroutine d2spline
        !
        ! DESCRIPTION :
        !    Compute spline 2nd derivates
        !
        ! PARAMETERS :
        !    -
        !    -
        !
        ! RETURN :
        !    -
        !    -
        !
        ! HISTORY :
        ! 14/01/2014   | AFontaine (OMP-SEDOO)   |    creation
        !-------------------------------------------------------------------------------

        IMPLICIT NONE

        !In variables
        integer :: n
        real,dimension(n) :: x,y
        real,dimension(3,n) :: lu
        !Out variables
        real,dimension(n) :: z
        !Local variables
        integer :: i,j
        real :: r,s

        !LZ=D
        z(1)=0.
        r=(y(2)-y(1))/(x(2)-x(1))
        do i=2,n-1
            s=(y(i+1)-y(i))/(x(i+1)-x(i))
            z(i)=s-r-lu(2,i)*z(i-1)
            r=s
        end do
        z(n)=-lu(2,n)*z(n-1)-lu(2,1)*z(n-2)

        !UZ=Z
        z(n)=z(n)*lu(1,n)
        do i=1,n-2
            j=n-i
            z(j)=(z(j)-lu(3,j)*z(j+1))*lu(1,j)
        end do
        z(1)=(z(1)-lu(3,1)*z(2)-lu(3,n)*z(3))*lu(1,1)

    END SUBROUTINE d2spline

    SUBROUTINE cubic(n,x,y,z,i,xi,fi)
        !-------------------------------------------------------------------------------
        ! subroutine d2spline
        !
        ! DESCRIPTION :
        !    Compute spline
        !
        ! PARAMETERS :
        !    -
        !    -
        !
        ! RETURN :
        !    -
        !    -
        !
        ! HISTORY :
        ! 14/01/2014   | AFontaine (OMP-SEDOO)   |    creation
        !-------------------------------------------------------------------------------

        USE BLPB_PARAM

        IMPLICIT NONE

        !In variables
        integer :: n,i
        real :: xi
        real,dimension(n) :: x,y,z
        !Out variables
        real :: fi
        !Local variables
        integer :: ka,kb,k
        real :: h,a,b

        ka=i
        kb=i+1
        if (xi.lt.x(ka)) then
            ka=1
        end if
        if (xi.gt.x(kb)) then
            kb=n
        end if

        do while ((kb-ka).gt.1)
            k=ka+(kb-ka)/2
            if (xi.lt.x(k)) then
                kb=k
            else
                ka=k
            end if
        end do

        h=x(kb)-x(ka)
        a=(x(kb)-xi)/h
        b=1.-a

        fi=a*y(ka)+b*y(kb)-a*b*((a+1.)*z(ka)+(b+1.)*z(kb))*h*h*R6

    END SUBROUTINE cubic




END MODULE BLPB_SPLINE

