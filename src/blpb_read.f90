MODULE BLPB_READ

CONTAINS

    SUBROUTINE gridinfo(lonw,lats,nlon,nlat,grid,nlev,llev,ngrib,path,np,start,dtn)
        IMPLICIT NONE

        !Out variable
        real :: lonw,lone,lats,latn,grid,dtn
        integer :: nlon,nlat,nlev,llev
        integer,dimension(2) :: start
        !Local var
        integer :: fid,line
        integer :: date,time
        integer :: nday,dtt,ngrib,np
        character*255 :: path
        character,parameter :: tab=char(9) 


        open(fid,file='blbp.dat',status='old',err=999)

        do line=1,4
            read(fid,*)
        end do

        !Read domain size and discretization parameters
        read(fid,*) lonw
        read(fid,*) lone
        read(fid,*) lats
        read(fid,*) latn
        read(fid,*) grid
        read(fid,*) nlev
        read(fid,*) llev

        do line=12,13
            read(fid,*)
        end do

        !Time info
        read(fid,'(i8)') date
        read(fid,'(I6)') time
        read(fid,*) nday
        read(fid,*) dtt
        read(fid,*) dtn

        dtn=dtn/24. !convert dtn from hours to days

        do line=22,23
            read(fid,*)
        end do

        !Grib location
        read(fid,'(a)') path

        close(fid)

        nlon=int((lone-lonw)/grid)+1
        nlat=int((latn-lats)/grid)+1

        ! write(*,'("From ",i8.8," at ",i6.6," during ",i2," days every ",i2,"h")') date,time,nday,dtt
        start(1)=date
        start(2)=time

        ngrib=nday*24/dtt+6
        ! write(*,*) 'Number of grib required: ',ngrib

        np=index(path,' ')-1
        ! write(*,*) path(1:np)

        return

! 999     write(*,*) '### BPCL TRAJ ERROR: cannot open blbp.dat ###'
!         write(*,*) 'check file in current directory'
999     write(*,'(a,"### BPCL TRAJ ERROR: cannot open blbp.dat ###")') tab
        write(*,'(a,"check file in current directory")') tab
        stop

    END SUBROUTINE gridinfo



    SUBROUTINE ballinfo(lonb,latb,nbal,rhob0,drhob)
        IMPLICIT NONE

        !Out var
        integer :: nbal
        real :: lonb,latb,rhob0,drhob
        !Local var:
        integer :: fid,line

        character,parameter :: tab=char(9) 

        open(fid,file='blbp.dat',status='old',err=998)

        do line=1,26
            read(fid,*)
        end do

        read(fid,*) lonb
        read(fid,*) latb
        read(fid,*) nbal
        read(fid,'(f6.4)') rhob0
        read(fid,'(f6.4)') drhob

        close(fid)

        return

! 998     write(*,*) '### BPCL TRAJ ERROR: cannot open blbp.dat ###'
!         write(*,*) 'check file in current directory'
998     write(*,'(a,"### BPCL TRAJ ERROR: cannot open blbp.dat ###")') tab
        write(*,'(a,"check file in current directory")') tab
        stop

    END SUBROUTINE ballinfo



    SUBROUTINE gribnb(ngrib,lpath)
        IMPLICIT NONE

        integer :: fid
        integer :: ngrib,lpath
        character*255 :: path
        character,parameter :: tab=char(9) 


        open(fid,file='gribinfo.dat',status='old',err=997)
        read(fid,'(a)') path
        read(fid,*) ngrib
        close(fid)

        lpath=index(path,' ')-1
        return

! 997     write(*,*) '### BLPB TRAJ ERROR: cannot open gribinfo.dat ###'
!         write(*,*) 'check file in current directory'
997     write(*,'(a,"### BLPB TRAJ ERROR: cannot open gribinfo.dat ###")') tab
        write(*,'(a,"check file in current directory")') tab
        stop

    END SUBROUTINE gribnb

    SUBROUTINE gribinfo(ngrib,fname,start,date,time)
        IMPLICIT NONE

        integer :: fid
        integer :: ngrib,k
        integer, dimension(2) :: start
        integer, dimension(ngrib) :: date,time
        character*80, dimension(ngrib):: fname
        integer :: d,t,k0,stat
        character*80 :: f
        character,parameter :: tab=char(9) 

        d=0
        t=0
        k0=1
		stat=0

        ! write(*,*) ""
        ! write(*,*) "Read list.dat"
        open(fid,file='list.dat',status='old',err=996)
	
        ! write(*,*) start(1),start(2)
            !read(fid,'(i8,2x,i6,2x,a80)',iostat=stat) d,t,f
            !write(*,*) start(1),d,start(2),t,f,k0
            !write(*,*) d,t,f,stat
        !Find starting date
        !write(*,*) 'Starting date : ',start(1)
        !write(*,*) 'Starting time : ',start(2)
        do while (((d.ne.start(1)).or.(t.ne.start(2))).and.(stat.eq.0))
            !write(*,*) d," <--> ",start(1)," = ",d.ne.start(1)
            !write(*,*) t," <--> ",start(2)," = ",t.ne.start(2)
            read(fid,'(i8,2x,i6,2x,a80)',iostat=stat) d,t,f
            !write(*,*) start(1),d,start(2),t,k0
	    !write(*,*) d,t,f,stat
            k0=k0+1
        end do

        if (stat.ne.0) then
            ! write(*,*) '### BLPB TRAJ ERROR: cannot find starting date in grib files list ###'
            ! write(*,*) stat,k0
            ! write(*,*) 'check file list.dat'
            write(*,'(a,"### BLPB TRAJ ERROR: cannot find starting date in grib files list ###")') tab
            write(*,'(a,i2,x,i2)') tab,stat,k0
            write(*,'(a,"check file list.dat")') tab
            stop
        end if

        rewind(fid)
        !
        do k=1,k0-4
            read(fid,*)
        end do
        !write(*,*) 'get date list'
        write(*,'("Need ",i3.3," grib files")') ngrib
        do k=1,ngrib
            read(fid,'(i8,2x,i6,2x,a80)') date(k),time(k),fname(k)
            !write(*,*) date(k),time(k),fname(k)
        end do
        close(fid)
        write(*,'(a,"First date for interpolation: ",i8.8,x,i6.6)') tab,date(1),time(1)
        return

996     write(*,'(a,"### BPCL TRAJ ERROR: cannot open list.dat ###")') tab
        write(*,'(a,"check file in current directory")') tab
        stop

    END SUBROUTINE gribinfo


END MODULE BLPB_READ
