! 2020.08.10  Keebler
! From an input location timeseries, output interpolated values
! along the trajectory.
!

program interpolate_output
  
  ! Read a *.outs IDL file of type ascii/real4/real8 and a satellite file,
  ! then interpolate the *.outs to the trajectory from the satellite file.

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
  real,allocatable  :: TrajTime_I(:) ! simulation time from trajectory file
  real,allocatable     :: Xy_DI(:,:) ! positions at each timestamp
  character(len=100) :: NameTrajFile ! name of desired trajectory file
  integer :: LineNumber ! line number in trajectory file
  integer :: nPoints ! number of timestamps in trajectory file
  real    :: StartTime ! simulation start time
  real    :: DateTime ! timestamp in simulation time
  
  ! I/O
  integer :: UnitTraj   ! iounit of trajectory file
  integer :: UnitIn     ! iounit of .outs file
  integer :: UnitInterp ! iounit of interpolated file
  integer :: UnitQuery  ! iounit of query file
  integer :: iError     ! used for all files

  ! .outs File
  character(len=100) :: NameFileIn
  character(len=10)  :: TypeFileIn  ! ascii/real4/real8
  character(len=500) :: StringHeader
  character(len=500) :: NameVar
  integer            :: nStep, nDim, nParam, nVar
  logical            :: IsCartesian
  real(Real8_)              :: Time ! snapshot time
  real(Real8_)              :: Param_I(100) ! parameters
  real(Real8_), allocatable :: Coord_DII(:,:,:), Var_VII(:,:,:) ! coordinate and data matrices
  integer :: n_D(0:3), n1, n2, n3
  integer :: i, TrajTimestamp ! loop indices
  real(Real8_), allocatable :: InterpData_VI(:,:) ! fully interpolated output
  real(Real8_) :: InterpCoord(2) ! interpolated trajectory coordinates
  integer :: MaxSnapshot ! limit of snapshots in .outs file
  integer :: nSnapshots ! number of snapshots contained in .outs file

  ! Interpolated File
  character(len=100) :: NameInterpFile ! name of desired output file
  real(Real8_),allocatable :: TimeOut_D(:)
  real(Real8_),allocatable :: InterpCoord_DI(:,:)
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

  UnitQuery = io_unit_new()
  UnitTraj = io_unit_new()
  UnitIn = io_unit_new()
  UnitInterp = io_unit_new()

  open(unit=UnitQuery,file='query.txt',iostat=iError,status='old')
  read(UnitQuery,'(a)',iostat=iError) NameFileIn
  read(UnitQuery,'(a)',iostat=iError) TypeFileIn
  read(UnitQuery,'(a)',iostat=iError) NameTrajFile
  read(UnitQuery,'(a)',iostat=iError) NameInterpFile
  read(UnitQuery,*,iostat=iError) StartTime
  close(UnitQuery)
  
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
  allocate(Xy_DI(2,nPoints))
  
  ! Rewind to start of file for reading times/positions.
  rewind(unit=UnitTraj) 

  ! Read in the trajectory times/positions.
  READFILE: do
     read(UnitTraj,'(a)',iostat=iError) line
     if(index(line, '#START')>0)then
        do LineNumber=1, nPoints
           read(UnitTraj, *, iostat=iError) TrajTime_DI(:,LineNumber), Xy_DI(:,LineNumber)
           if(iError/=0)EXIT READFILE

           ! Convert integer time to simulation time.
           call time_int_to_real(TrajTime_DI(:,LineNumber), DateTime)
           TrajTime_I(LineNumber) = DateTime - StartTime
        enddo
     endif
  enddo READFILE
  
  ! Close the trajectory file.
  close(unit=UnitTraj) 

  
  !-------------------------------------------------------------------------------------
  !                            INTERPOLATE
  MaxSnapshot = 1000
  nSnapshots = 0
  do i=1,MaxSnapshot
     ! Read header info from .outs file.
     call read_plot_file(NameFile = NameFileIn, iUnitIn = UnitIn, &
          TypeFileIn = TypeFileIn, &
          StringHeaderOut = StringHeader, &
          NameVarOut = NameVar, &
          nStepOut = nStep, &
          TimeOut = Time, &       ! simulation time
          nDimOut = nDim, &       ! number of dimensions
          IsCartesianOut = IsCartesian, &
          nParamOut = nParam, &   ! number of parameters
          nVarOut = nVar, &       ! number of variables
          n1Out = n1, &           ! grid sizes
          n2Out = n2, &
          n3Out = n3, &
          nOut_D = n_D, &         ! nOut_D grid size array
          ParamOut_I = Param_I)   ! parameters

     !write(*,*)'n1,n2,n3',n1,n2,n3
     
     if(Time < TrajTime_I(1)) CYCLE  ! if before start of trajectory file
     if(Time > TrajTime_I(nPoints)) EXIT  ! if after end of trajectory file

     ! Create array to hold interpolated data.
     if(.not.allocated(InterpData_VI))then
        allocate(TimeOut_D(MaxSnapshot), InterpData_VI(nVar,MaxSnapshot), &
             InterpCoord_DI(nDim,MaxSnapshot))
     endif

     ! Determine the shape of arrays from the header.
     !   Note: assumes no AMR/constant grid
     if(.not. allocated(Coord_DII)) &
          allocate(Coord_DII(nDim, n1, n2), Var_VII(nVar, n1, n2))

     ! Read the data at the snapshot.
     call read_plot_file(NameFile=NameFileIn, iUnitIn=UnitIn, &
          TypeFileIn = TypeFileIn, &
          CoordOut_DII = Coord_DII, &
          VarOut_VII = Var_VII)
     
     ! Interpolate location of trajectory file in time to snapshot.
     TrajTimestamp = 1
     do while(Time > TrajTime_I(TrajTimestamp))
        TrajTimestamp = TrajTimestamp + 1
     enddo
     if(Time == TrajTime_I(TrajTimestamp))then
        InterpCoord = Xy_DI(:,TrajTimestamp)
     else
        InterpCoord = interpolate_vector( &
             a_VC = Xy_DI(:,TrajTimestamp-1:TrajTimestamp), &
             nVar = nDim, &
             nDim = 1, &
             Min_D = [1], &
             Max_D = [2], &
             x_D = [Time], &
             x1_I = TrajTime_I(TrajTimestamp-1:TrajTimestamp), &
             DoExtrapolate = .false.)
     endif

     ! Convert coordinates to curvilinear.
     if(.not.IsCartesian)then
        InterpCoord = [sqrt(InterpCoord(1)**2 + InterpCoord(2)**2), &
             atan2(InterpCoord(2), InterpCoord(1))]
     endif
     
     ! Interpolate snapshot to trajectory location.
     InterpData_VI(:,i) = interpolate_vector( &
          a_VC = Var_VII, &
          nVar = nVar, &
          nDim = 2, &
          Min_D = [1,1], &
          Max_D = [n1,n2], &
          x_D = InterpCoord, &
          x1_I = Coord_DII(1,:,1), & ! Incorrect indices?
          x2_I = Coord_DII(2,:,1), & ! Incorrect indices?
          DoExtrapolate = .false.)

     !if(i == 10)then
     !   write(*,*)'InterpCoord',InterpCoord
     !   write(*,*)'R:  '
     !   write(*,*)minval(Coord_DII(1,:,1)),maxval(Coord_DII(1,:,1))
     !   write(*,*)'Phi:  '
     !   write(*,*)minval(Coord_DII(2,1,:)),maxval(Coord_DII(2,1,:))
     !   !write(*,*)Coord_DII(2,:,1)
     !   write(*,*)minval(Coord_DII(1,1,:)),maxval(Coord_DII(1,1,:))
     !   write(*,*)Coord_DII(1,1,:)
     !endif

     InterpCoord_DI(:,i) = InterpCoord ! possibly incorporate into above statements
     TimeOut_D(i) = Time

     nSnapshots = i ! Update number of snapshots in .outs file
  enddo
  

  close(UnitIn)
  deallocate(Coord_DII, Var_VII)
  
  !-------------------------------------------------------------------------------------
  !                              OUTPUT FILE
  ! Open file for interpolated output.
  open(unit=UnitInterp, file=NameInterpFile, iostat=iError, status="replace") 

  ! Include header.
  write(UnitInterp,*)'t ',NameVar

  ! Write data. For now, time is in simulation time, but can be easily converted.
  do LineNumber=1,nSnapshots
     write(UnitInterp,*)TimeOut_D(LineNumber),InterpCoord_DI(:,LineNumber),InterpData_VI(:,LineNumber)
  enddo
  
  close(UnitInterp)



  
end program interpolate_output





