! 2020.08.10  Keebler
! From an input location timeseries, output interpolated values
! along the trajectory.
!

program interpolate_output
  
  ! Read a *.outs IDL file of type ascii/real4/real8 and a satellite file,
  ! then interpolate the *.outs to the location/cadence from the satellite file.

  use ModIoUnit,     ONLY: io_unit_new
  use ModKind,       ONLY: Real4_, Real8_
  use ModPlotFile,   ONLY: save_plot_file, read_plot_file
  use ModUtilities,  ONLY: split_string
  use ModTimeConvert,ONLY: time_int_to_real
  use ModInterpolate
  
  implicit none

  ! Trajectory File
  character(len=100)   :: line ! single line from trajectory file
  integer,allocatable  :: TrajTime_DI(:,:) ! integer times from trajectory file
  integer,allocatable  :: TrajTime_I(:) ! simulation time from trajectory file
  real,allocatable     :: Xyz_DI(:,:) ! positions at each timestamp
  integer :: LineNumber ! line number in trajectory file
  character(len=100) :: NameTrajFile ! name of desired trajectory file
  integer :: nPoints ! number of timestamps in trajectory file
  real    :: StartTime ! simulation start time
  real    :: DateTime ! timestamp in simulation time
  
  ! I/O
  integer :: UnitTraj   ! iounit of trajectory file
  integer :: UnitIn     ! iounit of .outs file
  integer :: UnitInterp ! iounit of interpolated file
  integer :: iError     ! used for all files

  ! .outs File
  character(len=100) :: NameFileIn
  character(len=10)  :: TypeFileIn  ! ascii/real4/real8
  character(len=500) :: StringHeader
  character(len=500) :: NameVar
  integer            :: nStep, nDim, nParam, nVar
  real(Real8_)              :: Time,PrevTime ! snapshot times
  real(Real8_)              :: Param_I(100) ! parameters from .outs file
  real(Real8_), allocatable :: Coord_DII(:,:), Var_VII(:,:)
  real(Real8_), allocatable :: PrevCoord_DII(:,:), PrevVar_VII(:,:)
  integer :: n_D(0:3), n1, n2, n3
  integer :: i, TrajCounter
  real(Real8_), allocatable :: InterpData_VI(:,:)
  real(Real8_) :: dist

  ! Interpolated File
  character(len=100) :: NameInterpFile ! name of desired output file

  character(len=*), parameter:: NameSub = 'interpolate_output'
  !-------------------------------------------------------------------------

  ! READ TRAJECTORY FILE OF POSITIONS AND TIMES
  !  Should be in the same format as satellite files.
  !  All commands prior to #START will be ignored.
  !  Coordinate system is assumed to be the same as the .outs file.
  ! Example:
  !             year mo dy hr mn sc msc x y z
  !             #START
  !              1997 5 17 3 35 00 000 -0.368 5.275 0.0

  ! Get name/type of trajectory file.
!  write(*,'(a)') 'Input Filename (*.outs):'
!  read(*,'(a)') NameFileIn
!  write(*,'(a)') 'Input File Type (ascii/real4/real8):'
!  read(*,'(a)') TypeFileIn
!  write(*,'(a)') 'Trajectory Filename:'
!  read(*,'(a)') NameTrajFile
  NameFileIn = 'test_mov.outs'
  TypeFileIn = 'ascii'
  NameTrajFile = 'test_traj.dat'
  NameInterpFile = 'test_output.dat'
  UnitTraj = io_unit_new()
  UnitIn = io_unit_new()
  UnitInterp = io_unit_new()
  StartTime = 1640995200.0 ! Can this be read from the plot file?
  ! Note: all input will be from a file eventually.

  
  !---------------------------------------------------------------------------
  !                   READ TRAJECTORY
  
  ! Open the trajectory file.
  open(unit=UnitTraj, file=NameTrajFile, iostat=iError, status="old") 

  ! Find the number of points in the trajectory file.
  nPoints = 0
  COUNTPOINTS: do
     read(UnitTraj,'(a)',iostat=iError) line
     if(index(line, '#START')>0)then
        do
           read(UnitTraj, '(a)', iostat=iError) line
           if(iError/=0)EXIT COUNTPOINTS
           nPoints = nPoints + 1
        enddo
     endif
  enddo COUNTPOINTS

  ! Allocate arrays to hold times/positions.
  allocate(TrajTime_DI(7,nPoints))
  allocate(TrajTime_I(nPoints))
  allocate(Xyz_DI(3,nPoints))
  
  ! Rewind to start of file for reading times/positions.
  rewind(unit=UnitTraj) 

  ! Read in the trajectory times/positions.
  READFILE: do
     read(UnitTraj,'(a)',iostat=iError) line
     if(index(line, '#START')>0)then
        do LineNumber=1, nPoints
           read(UnitTraj, *, iostat=iError) TrajTime_DI(:,LineNumber), Xyz_DI(:,LineNumber)
           if(iError/=0)EXIT READFILE

           ! Convert integer time to simulation time.
           call time_int_to_real(TrajTime_DI(:,LineNumber), DateTime)
           TrajTime_I(LineNumber) = DateTime - StartTime
        enddo
     endif
  enddo READFILE
  
  ! Close the trajectory file.
  close(unit=UnitTraj) 

  write(*,*)'nPoints = ',nPoints
  !write(*,*)'TrajTime_I(1) = ',TrajTime_I(1)
  !write(*,*)'TrajTime_I(nPoints) = ',TrajTime_I(nPoints)


  
  !-------------------------------------------------------------------------------------
  !                            INTERPOLATE
  TrajCounter = 1
  do i=1,100
     ! Read header info from .outs file.
     call read_plot_file(NameFile = NameFileIn, iUnitIn = UnitIn, &
          TypeFileIn = TypeFileIn, &
          StringHeaderOut = StringHeader, &
          NameVarOut = NameVar, &
          nStepOut = nStep, &
          TimeOut = Time, &       ! simulation time
          nDimOut = nDim, &       ! number of dimensions, negative for curvilinear
          nParamOut = nParam, &   ! number of parameters
          nVarOut = nVar, &       ! number of variables
          n1Out = n1, &           ! grid sizes
          n2Out = n2, &
          n3Out = n3, &
          nOut_D = n_D, &         ! nOut_D grid size array
          ParamOut_I = Param_I)   ! parameters

     if(Time < TrajTime_I(1)) CYCLE  ! if before start of trajectory file
     if(TrajCounter > nPoints) EXIT  ! if after end of trajectory file

     ! Create array to hold interpolated data.
     if(.not.allocated(InterpData_VI))then
        allocate(InterpData_VI(nPoints,nVar))
        !write(*,*)'nDim =',nDim
        !write(*,*)'n1, n2, n3 =',n1,n2,n3
        !write(*,*)'nOut_D =',n_D
     endif
     
     ! Determine the shape of arrays from the header.
     !   Note: assumes no AMR/constant grid
     if(.not. allocated(Coord_DII)) &
          allocate(Coord_DII(nDim, n1), Var_VII(nVar, n1), &
          PrevVar_VII(nVar, n1))

     ! Read the data and interpolate in time if necessary.
     if(Time < TrajTime_I(TrajCounter))then
        ! Remember the previous snapshot.
        PrevTime = Time
        call read_plot_file(NameFile=NameFileIn, iUnitIn=UnitIn, &
             TypeFileIn = TypeFileIn, &
             CoordOut_DI = Coord_DII, &
             VarOut_VI = PrevVar_VII)
        !write(*,*)'Time < TrajTime_I',Time,'<',TrajTime_I(TrajCounter)
        CYCLE
     elseif(Time > TrajTime_I(TrajCounter))then
        ! Read the subsequent snapshot.
        call read_plot_file(NameFile=NameFileIn, iUnitIn=UnitIn, &
             TypeFileIn = TypeFileIn, &
             CoordOut_DI = Coord_DII, &
             VarOut_VI = Var_VII)

        ! Interpolate the data in time.
        !write(*,*)'Success... Time > TrajTime_I',Time,'>',TrajTime_I(TrajCounter)
        dist = (Time - TrajTime_I(TrajCounter))/(Time - PrevTime)
        Var_VII = dist*PrevVar_VII + (1-dist)*Var_VII   
        TrajCounter = TrajCounter + 1
     else
        ! Matching snapshot: no temporal interpolation needed.
        call read_plot_file(NameFile=NameFileIn, iUnitIn=UnitIn, &
             TypeFileIn = TypeFileIn, &
             CoordOut_DI = Coord_DII, &
             VarOut_VI = Var_VII)
        !write(*,*)'Success... Time = TrajTime_I',Time,'=',TrajTime_I(TrajCounter)
        TrajCounter = TrajCounter + 1
     endif

     ! Interpolate the data in space.
     !Var_VII = interpolate_vector( &
     !     a_VC = [PrevVar_VII, Var_VII], &
     !     nVar = nVar, &
     !     nDim = 1, &
     !     Min_D = [1], &
     !     Max_D = [2], &
     !     x_D = [real(TrajTime_I(TrajCounter))], &
     !     !x1_I = TrajTime_I(TrajCounter), &
     !     DoExtrapolate = .false., &
     !     iCell_D = [1], &
     !     Dist_D = [dist])
     
  enddo
     
  close(UnitIn)
  deallocate(Coord_DII, Var_VII)
  

  

  !-------------------------------------------------------------------------------------
  !                           WRITE OUTPUT
  
  ! Open file for interpolated output.
  open(unit=UnitInterp, file=NameInterpFile, iostat=iError, status="replace") 

  ! Include header.
  !   Eventually, will need labels for time and variables.
  write(UnitInterp,*)'Header'
  write(UnitInterp,*)NameVar
  
  do LineNumber=1,nPoints
     write(UnitInterp,'(1X,I4,6I2)')TrajTime_DI(:,LineNumber)
  enddo
  
  close(UnitInterp)
  
end program interpolate_output





