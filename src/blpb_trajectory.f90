!-------------------------------------------------------------------------------
! program BLPBTRAJ
!
! DESCRIPTION :
!   Compute BLPB (Boundary Layer Pressurized Balloon) advection on isopycne layer.
!
!
! PARAMETERS :
!   - grib files with fields of wind, surface pressure and temperature from ECMWF
!   - file with computational domain parameters
!   - file with balloon information
!
! RETURN :
!    - file with balloon trajectory
!
! COMMENTS :
!   From fortran program Previ3BAMED:
!   - C. Basdevant (LMD/IPSL)
!   - A. Doerenbecher (CNRM)
!   - F. Bernard (CNRM)
!
! HISTORY :
! 11/12/2013   | AFontaine (OMP-SEDOO)   |    creation
!-------------------------------------------------------------------------------

PROGRAM BLPBTRAJ

    USE BLPB_PARAM
    USE BLPB_READ
    USE BLPB_GRID
    USE BLPB_CHECK
    USE BLPB_GRIBF
    USE BLPB_TIME
    USE BLPB_FIELD
    USE BLPB_SPLINE

    IMPLICIT NONE

    !Environment
    integer :: nlon,nlat,nlev,llev,nbal,ibal,nfield,kload
    integer :: ib,jb,ii,jj,iii
    real :: lonc,latc,grid,lonb,latb,rhob0,drhob,cosl
    real :: xmin,xmax,ymin,ymax

    character,parameter :: tab=char(9) 

    !Balloon variables
    real,dimension(:),allocatable :: xb,yb,rhob,hb,wb,tpb
    character*40 :: ofile
    integer :: kt
    real :: blon,blat
    integer,dimension(:),allocatable :: fout

    real,dimension(:),allocatable :: xlon,ylat,coeff
    !2D parameters
    real,dimension(:,:,:),allocatable :: tp
    !3D parameters
    real,dimension(:,:,:,:),allocatable :: u,v,t,p,rho,h,w
    logical :: flight

    !output var
    ! character*39,parameter :: fmt_file="('blpb_',i3.3,'_',i8.8,'_',i6.6,'.dat')"
    !                                     blbp_  date_    time_   density_  numero.dat
    character*48,parameter :: fmt_file="('blpb_',i8.8,'_',i6.6,'_',f4.2,'_',i3.3,'.dat')" ! blbp_20230106_060000_1.05_001.dat

    !Time management
    real*8 :: t0,tn
    real :: tf,tps,dtn,tload,dt1,dx0,dx,dtc
    integer,dimension(2) :: start
    integer,dimension(:),allocatable :: status,date,time
    real,dimension(:),allocatable :: tlist,tlistl

    !RK
    integer :: rk
    real :: xrk,yrk,trk
    real :: ub,vb,tb,pb

    !Spline interpolation environment
    integer,dimension(3) :: psize
    integer,dimension(2) :: indb
    real,dimension(3) :: posb
    real,dimension(:,:),allocatable :: lut,lux,luy,luz
    real,dimension(:),allocatable :: subx,suby,subu,subv,subt,subp,subz,subh,subw

    !grib environment
    integer :: lpath
    character*255 :: gribpath
    character*80,dimension(:),allocatable :: gribfield

    !Read grid information
    ! write(*,*)
    ! write(*,*) 'Read configuration....'
    call gridinfo(lonc,latc,nlon,nlat,grid,nlev,llev,nfield,gribpath,lpath,start,dtn)
    ! *****************************************************************************************
    ! lonc     --> west longitude
    ! latc     --> south latitude
    ! nlon     --> number of cells on longitude
    ! nlat     --> number of cells on latitude
    ! grid     --> grid resolution
    ! nlev     --> number of available levels
    ! llev     --> number of maximum levels (137 as in the ECMWF data)
    ! nfield   --> number of required GRIB files (take the 00h file of the newt day too)
    ! gribpath --> path to the grib files
    ! lpath    --> this integer represents the length of the string with GRIB path
    ! start    --> start(1) = start date, start(2) = start time
    ! dtn      --> timestep of the simulation/output (in days)
    ! *****************************************************************************************
    !dtn=0.1/24.
    dt1=dtn*24.*3600/r_earth ! r_earth = radius of the Earth in meters

    ! write(*,*) "lonc,latc,nlon,nlat,grid,nlev,llev,starti,dtni,dt1:"
    ! write(*,*) lonc,latc,nlon,nlat,grid,nlev,llev,start,dtn,dt1
    ! write(*,*)

    !Read balloon information
    ! write(*,*) 'Read ball info'
    call ballinfo(lonb,latb,nbal,rhob0,drhob)
    ! ****************************************************
    ! lonb  --> longitude of the launch site
    ! latb  --> latitude of the launch site
    ! nbal  --> number of baloons launched from this same site
    ! rhob0 --> intitial baloon density
    ! drhob --> delta of the density for next baloons
    ! ****************************************************
    
    ! write(*,*) "lonb,latb,nbal,rhob0,drhob..."
    ! write(*,*) lonb,latb,nbal,rhob0,drhob
    ! write(*,*)
 
    allocate(status(nbal),rhob(nbal),xb(nbal),yb(nbal),hb(nbal),wb(nbal),tpb(nbal))

    dx0=grid*pi180 ! grid resolution in radians
    do ibal=1,nbal ! for each baloon
        status(ibal)=1       ! status ok
        xb(ibal)=lonb*pi180  ! launch site longitude in radians
        yb(ibal)=latb*pi180  ! launch site latitude in radians
        rhob(ibal)=rhob0-(real(ibal)-1.0)*drhob ! compute density of each baloon (from initial density and delta)
        ! write(*,'("BLPB",i2," lon:",f6.2,"°E lat:",f6.2,"°N density:",f6.3," xb:",f8.5," yb:",f8.5)')ibal,lonb,latb,&
                ! &rhob(ibal),xb(ibal),yb(ibal)
    end do

    !Initialise grid in radians
    allocate(xlon(nlon),ylat(nlat))
    call bpclgrid(lonc,latc,nlon,nlat,grid,xlon,ylat)
    ! xlon and ylat are vectors and filled in the function with values
    ! of the grid in radians (the grid is regular)

    xmin=xlon(1)
    ymin=ylat(1)
    xmax=xlon(nlon)
    ymax=ylat(nlat)

    ! write(*,*)
    ! write(*,*) 'Grid initialized'
    ! write(*,*) 'lonc,latc,nlon,nlat,grid,xmin,xmax,ymin,ymax'
    ! write(*,*) lonc,latc,nlon,nlat,grid,xmin,xmax,ymin,ymax    
    ! write(*,*)

        
    !Check parameters
    !write(*,*) '*** check balloon:'
    do ibal=1,nbal ! for each baloon verify if the launch site is in the simulation domain
        call checkhoriz(ibal,xmin,xmax,ymin,ymax,xb(ibal),yb(ibal),status(ibal))
        ! status(ibal) is changed to 0 if the launch site is outside of the domain
    enddo

    call checkflight(nbal,status,flight) ! flight==True if at least one baloon is inside the simulation domain

    allocate(gribfield(nfield),date(nfield),time(nfield),tlist(nfield))

    ! Check if the data for simualation dates is available (based on list.dat)
    call gribinfo(nfield,gribfield,start,date,time)

    !check grib
    !call checkfield(nfield,gribpath(1:lpath),gribfield,date,time,nlev,llev,30000)

    !Allocate fields
    psize(1)=nlon
    psize(2)=nlat
    psize(3)=nlev

    allocate(coeff(2*nlev))
    allocate(u(nlon,nlat,nlev,nload),v(nlon,nlat,nlev,nload),t(nlon,nlat,nlev,nload))
    allocate(p(nlon,nlat,nlev,nload),rho(nlon,nlat,nlev,nload),h(nlon,nlat,nlev,nload))
    allocate(w(nlon,nlat,nlev,nload))
    allocate(tp(nlon,nlat,nload))
    allocate(tlistl(nload))
    allocate(subx(nxi),suby(nyi))
    allocate(subu(nlev),subv(nlev),subt(nlev),subp(nlev),subz(nlev),subh(nlev),subw(nlev))
    allocate(lut(3,nload),lux(3,nxi),luy(3,nyi),luz(3,nlev))

    !Initialise fields

    call readpv(gribpath(1:lpath)//gribfield(1),nlev,llev,coeff)
    ! write(*,'("Load fields from",x,i8,x,i6.6,x,"to",x,i8,x,i6.6)') date(1),time(1),date(nload),time(nload)
    call initfield(gribpath(1:lpath),gribfield(1:nload),psize,llev,nload,coeff,u,v,t,p,rho,h,w,tp)
    kload=nload+1

    !Manage time
    kt=0
    call juldate(date(3),time(3),t0)
    call juldate(date(nfield-3),time(nfield-3),tn)

    call timelist(date,time,nfield,tlist)
    tlistl=tlist(1:nload)
    call luspline(nload,tlistl,lut)
    tload=tlistl(nupd+1)
    tps=0.
    tf=tlist(nfield-3)
    !write(*,*) 'timelenght:',tf,' days'
    !write(*,*) 'initialize:',kt,kload,t0,tps,tload,tf
    !write(*,*) 'loaded:',tlistl
    !write(*,*)    

    !Create data file
    allocate(fout(nbal))

    ! write(*,*) "Open data files"    
    do ibal=1,nbal
        write(ofile,fmt_file) start(1),start(2),rhob(ibal),ibal
        !write(*,*) ofile,ibal,rhob(ibal),status(ibal),xb(ibal)*ipi180,yb(ibal)*ipi180
        write(*,'(a,"BLPB",i2," data store in: ",a40)') tab,ibal,ofile
        fout(ibal)=10+ibal
        open(unit=fout(ibal),file=trim(ofile))
        write(fout(ibal),'("launch_lat;",f0.10)') yb(ibal)/pi180
        write(fout(ibal),'("launch_lon;",f0.10)') xb(ibal)/pi180
        write(fout(ibal),'("launch_date;",i0)') start(1)
        write(fout(ibal),'("launch_time;",i0.6)') start(2)
        write(fout(ibal),'("density;",g0.3)') rhob(ibal)
        write(fout(ibal),'(A)') "Variables:;time;longitude;latitude;altitude;vertical_velocity;total_precipitation"
        write(fout(ibal),'(A)') "Units:;seconds;degrees_east;degrees_north;meters;Pa/s;meters"
        !write(fout(ibal),*)'time_units;seconds'
        !write(fout(ibal),*)'longitude_units;degrees_east'
        !write(fout(ibal),*)'latitude_units;degrees_north'
        !write(fout(ibal),*)'altitude_units;meters'
        !write(fout(ibal),*)'vertical_velocity_units;Pa/s'
        !write(fout(ibal),*)'total_precipitation_units;meters'
        !write(fout(ibal),*)'time;longitude;latitude;altitude;vertical_velocity;total_precipitation'
    end do
    ! write(*,*)F    

    !LOOP time
    !write(*,*) 'end time = ',tf
    do while ((tps.lt.tf).and.(flight))
        !do while (kt.lt.100)
        !timestep
        tps=tps+dtn
        kt=kt+1
        !write(*,*) 'current step = ',tps
        !Update fields
        if ((tps.ge.tload).and.(kload.lt.nfield-3)) then
            tlistl=tlist(kload-nupd:kload-nupd+nload)
            ! write(*,*)
            ! write(*,*)
            write(*,'(a,"Load fields from",x,i8,x,i6.6,x,"to",x,i8,x,i6.6)') tab,date(kload),time(kload)&
                &,date(kload-nupd+nload-1),time(kload-nupd+nload-1)

            !write(*,*) tload,tps
            call luspline(nload,tlistl,lut)
            call updatefield(gribpath(1:lpath),gribfield(kload:kload+nupd),psize,llev,nload,nupd,coeff,u,v,t,p,rho,h,w,tp)
            tload=tlistl(nupd+1)
            kload=kload+nupd
        end if !end update

        !LOOP balloon
        do ibal=1,nbal
            xrk=xb(ibal)
            yrk=yb(ibal)
            rk=1
            call checkhoriz(ibal,xmin,xmax,ymin,ymax,xrk,yrk,status(ibal))
            if (status(ibal).eq.1) then
                !statut ok
                !LOOP Runge-Kutta
                do while ((rk.le.nrk).and.(status(ibal).eq.1))
                    !CHECK balloon statut
                    trk=tps+ark(rk)*dtn
                    !locate balloon
                    call locate(xlon,ylat,nlon,nlat,xrk,yrk,ib,jb,ii,jj)
                    indb(1)=ii
                    indb(2)=jj
                    posb(1)=xrk
                    posb(2)=yrk
                    posb(3)=trk
                    subx=xlon(ii:ii+nxi-1)
                    suby=ylat(jj:jj+nyi-1)
                    call luspline(nxi,subx,lux)
                    call luspline(nyi,suby,luy)
                    !local interpolation
                    call localfield(rho,subx,suby,tlistl,psize,indb,posb,lux,luy,lut,subz)
                    !check level
                    call checklev(ibal,rhob(ibal),subz(1),subz(nlev),status(ibal))

                    if ((subz(nlev)-subz(nlev-1)).eq.(0.)) then
                        write(*,*) tab,'### WARNING density ###'
                        do iii=1,nlev
                            write(*,*) iii,subz(iii)
                        end do
                        write(*,*) tab,'xgrid: ',ii,ii+nxi-1
                        write(*,*) tab,'ygrid: ',jj,jj+nyi-1
                        write(*,*) tab,'pos: ',xrk,yrk,trk
                        write(*,*) tab,'blpb: ',ibal,kt,rk,tps
                        status(ibal)=-9
                    end if

                    if (status(ibal).eq.1) then
                        !2D parameter
                        call localfield2D(tp,subx,suby,tlistl,psize,indb,posb,lux,luy,lut,tpb(ibal))

                        !3D parameters
                        call localfield(u,subx,suby,tlistl,psize,indb,posb,lux,luy,lut,subu)
                        call localfield(v,subx,suby,tlistl,psize,indb,posb,lux,luy,lut,subv)
                        call localfield(t,subx,suby,tlistl,psize,indb,posb,lux,luy,lut,subt)
                        call localfield(p,subx,suby,tlistl,psize,indb,posb,lux,luy,lut,subp)
                        call localfield(h,subx,suby,tlistl,psize,indb,posb,lux,luy,lut,subh)
                        call localfield(w,subx,suby,tlistl,psize,indb,posb,lux,luy,lut,subw)

                        call luspline(nlev,subz,luz)
                        call ballfield(subu,subz,nlev,luz,rhob(ibal),ub)
                        call ballfield(subv,subz,nlev,luz,rhob(ibal),vb)
                        call ballfield(subt,subz,nlev,luz,rhob(ibal),tb)
                        call ballfield(subp,subz,nlev,luz,rhob(ibal),pb)
                        call ballfield(subh,subz,nlev,luz,rhob(ibal),hb(ibal))
                        call ballfield(subw,subz,nlev,luz,rhob(ibal),wb(ibal))

                        !Check timestep
                        cosl=cos(yrk)
                        dx=dx0*cosl
                        call timestep(ub,vb,dx,dx0,dt1,dtc)
                        !advection
                        xrk=xb(ibal)+ark(rk)*dt1*ub/cosl
                        yrk=yb(ibal)+ark(rk)*dt1*vb
                        rk=rk+1
                        call checkhoriz(ibal,xmin,xmax,ymin,ymax,xrk,yrk,status(ibal))
                    end if !end check level
                !END loop Runge-Kutta
                end do
                xb(ibal)=xrk
                yb(ibal)=yrk
                if (status(ibal).ne.1) then
                    blon=xrk*ipi180
                    blat=yrk*ipi180
                    write(*,'(a,"=> BLPB",i2," end with status ",i2," at ",f5.2," days lon:",f6.2,"°E lat:",f6.2,"°N alt:",f8.2,&
                            &"m  w:",f10.6," tp:",f11.8)') tab,ibal,status(ibal),tps,blon,blat,hb(ibal),wb(ibal),tpb(ibal)
                else if (mod(kt,100).eq.0) then
                    blon=xrk*ipi180
                    blat=yrk*ipi180
                    ! write(*,'("BLPB",i2," at ",f5.2," days,  kt:",i4," lon:",f6.2,"°E lat:",f6.2,"°N alt:",f8.2,"m  w:",f11.8,&
                    !         &" tp:",f10.6)')ibal,tps,kt,blon,blat,hb(ibal),wb(ibal),tpb(ibal)
                end if
                !save info in data file
                blon=xb(ibal)*ipi180
                blat=yb(ibal)*ipi180
                write(fout(ibal),'(";",f0.2,";",f0.8,";",f0.8,";",f0.2,";",f0.8,";",f0.8)')&
                    &tps*d2s,blon,blat,hb(ibal),wb(ibal),tpb(ibal)
            !END check statut
            end if
        !END loop balloon
        end do

        call checkflight(nbal,status,flight)
    !END loop time
    end do
    ! write(*,*)
    ! write(*,*)
    ! write(*,*) "End for time step:",kt,tps*d2s

    !Write end status then close data file
    do ibal=1,nbal
        write(fout(ibal),'("end_status;",i2)')status(ibal)
        close(fout(ibal))
    end do
    ! write(*,*) "Data files closed"

    !Deallocate variable
    deallocate(xlon,ylat)
    deallocate(gribfield,date,time,tlist,tlistl)
    deallocate(u,v,t,p,rho,h,coeff,w,tp)
    deallocate(status,xb,yb,rhob,hb,wb,tpb)
    deallocate(subx,suby)
    deallocate(subu,subv,subt,subp,subz,subh,subw)
    deallocate(lut,lux,luy,luz)
    deallocate(fout)

    ! write(*,*) "PROGRAM END"

END PROGRAM BLPBTRAJ
