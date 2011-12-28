! test spawn
module spawn
  !use mpi
  implicit none
  private
  !
  public :: spawn_main
  integer, public :: MPI_COMM_CITY,MPI_COMM_PREF,MPI_COMM_AIRPLANE
  integer, parameter, public :: CLEN=200
  !
  type, private:: comm_x
    integer :: rank,nprocs, ierr
    integer :: istatus
  end type comm_x
  !
  type, private :: info_comm
    integer :: index_of_city
    integer :: index_of_city_member
    integer :: index_of_pref
    integer :: local_md
  end type info_comm
  !
  type, public :: input
    integer :: max_procs,half(1:2)
    integer :: option,ncommand
    character(LEN=CLEN):: command(1:2), cargv(1:2,1:2)
    integer :: info(1:2), root, comm, ierr,xerr(1:50)
  end type input
  !
  contains

  subroutine spawn_main(cinput)
    use mpi
    implicit none
    type(input),intent(OUT)   :: cinput
    type(comm_x)              :: comm_city, comm_world
    !
    integer   :: ncore
    !
    ncore = 1
    call parallel     (comm_city,comm_world,ncore)
    call setup        (cinput)
    call spawn_single (cinput)
    !
    return
  end subroutine spawn_main

  subroutine setup(cinput)
    use mpi
    implicit none
    type(input),target,intent(OUT)::cinput
    !
    cinput%ncommand   = 2
    cinput%max_procs  = 50
    cinput%root       = 0
    !
    cinput%command(1) = "./namd2"
    cinput%half(1)    = int(cinput%max_procs * 0.5)
    cinput%info(1)    = MPI_INFO_NULL
    cinput%cargv(1,1) = "apoa1.namd"
    cinput%cargv(2,1) = " "
    !
    cinput%command(2) = "./namd2"
    cinput%half(2)    = int(cinput%max_procs * 0.5)
    cinput%info(2)    = MPI_INFO_NULL
    cinput%cargv(1,2) = "apoa2.namd"
    cinput%cargv(2,2) = " "
   ! cinput%cargv(2)  = "out.1.d"
   ! cinput%cargv(2)  = " > out.1.d"
    !
    return
  end subroutine setup

  subroutine spawn_single(cinput)
    use mpi
    implicit none
    type(input),target,intent(IN):: cinput
    !
    integer,pointer :: half(:),ncommand
    !
    character(LEN=CLEN),pointer :: command(:),cargv(:,:)
    integer,            pointer :: max_procs,root,ierr,xerr(:),info(:)
    character(LEN=CLEN) :: cc
    integer :: comm_air
    integer :: i
    integer :: MPI_INFO
    !
    ncommand => cinput%ncommand
    command  => cinput%command
    cargv    => cinput%cargv
    max_procs=> cinput%max_procs
    root     => cinput%root
    ierr     => cinput%ierr
    xerr     => cinput%xerr
    half     => cinput%half
    info     => cinput%info
    !
    !half=int(max_procs*0.5d0)
    !
    do i = 1,2,1
     write(6,*)'chk command=',trim(adjustl(command(i)))
     write(6,*)'cargv  1   =',trim(adjustl(cargv(i,1)))
     write(6,*)'max_proces =',max_procs
     write(6,*)'half       =',half(i)
     write(6,*)'root       =',root
     write(6,*)'ierr       =',ierr
     write(6,*)'xerr       =',xerr(i)
     write(6,*)'info       =',info(i)
     write(6,*)'============start'
    enddo
    !
    ncommand = 1
    call MPI_INFO_CREATE(MPI_INFO, ierr)
    call MPI_INFO_SET(MPI_INFO, "soft", "25", ierr)
    info(1)=MPI_INFO
    info(2)=MPI_INFO
    call MPI_COMM_SPAWN_MULTIPLE(            &
                      & ncommand,command,    &
                      & cargv,               &
                      & half,                &
                      & info,       &
                      & root,                &
                      & MPI_COMM_SELF,      &
                      & MPI_COMM_AIRPLANE,   &
                      & xerr,                &
                      & ierr                 &
                      &)
    !call MPI_Comm_spawn(command, cargv,      &
    !                  & half,                &
    !                  & MPI_INFO_NULL,       &
    !                  & root,                &
    !                  & MPI_COMM_WORLD,      &
    !                  & MPI_COMM_AIRPLANE,   &
    !                  & MPI_ERRCODES_IGNORE, &
    !                  & ierr                 &
    !                  &)
    !call MPI_Recv (cc, 200, MPI_CHARACTER, MPI_ANY_SOURCE, 200, &
    !                  &         MPI_COMM_AIRPLANE, MPI_STATUS_IGNORE,ierr)
    !write(6,*)'chk=',cc
    !call MPI_INFO_DELETE(MPI_INFO, "soft", ierr)
    call MPI_INFO_FREE(MPI_INFO)
    write(6,*) 'ierr=',ierr
    
    !write(6,*)'======xxxxxxx 2 ======'
    !cargv(1)="apoa2.namd"
    !write(6,*)'chk command=',trim(adjustl(command))
    !write(6,*)'cargv  1   =',trim(adjustl(cargv(1)))
    !!write(6,*)'cargv  2   =',trim(adjustl(cargv(2)))
    !write(6,*)'max_proces =',max_procs
    !write(6,*)'root       =',root
    !write(6,*)'ierr       =',ierr
    !write(6,*)'============start 2'
    !call MPI_Comm_spawn(command, cargv, half,  &
    !      &        MPI_INFO_NULL, root, MPI_COMM_WORLD, MPI_COMM_AIRPLANE,  &
    !      &        MPI_ERRCODES_IGNORE, ierr)
          !&        xerr, ierr)
                             !   MPI_ERRCODES_IGNORE); 
    write(6,*)'===========finish'
    !
    return
  end subroutine spawn_single

  subroutine parallel(comm_city,comm_world,ncore)
    use mpi
    implicit none
    type(comm_x),intent(OUT),target :: comm_city
    type(comm_x),intent(OUT),target :: comm_world
    integer     ,intent(IN)         :: ncore
    !
    integer :: icity, mcity
    !
    integer, pointer  :: world_rank, world_nprocs, world_ierr
    integer, pointer  :: city_rank, city_nprocs, city_ierr
    !
    world_rank   => comm_world%rank
    world_nprocs => comm_world%nprocs
    world_ierr   => comm_world%ierr
    city_rank    => comm_city%rank
    city_nprocs  => comm_city%nprocs
    city_ierr    => comm_city%ierr
    !
    call get_size_rank(MPI_COMM_WORLD, comm_world)
    !
    icity = int(world_rank/ncore) + 1
    mcity = mod(world_rank,ncore) + 1
    write(6,*)' icity,mcity,world_rank=',icity,mcity,world_rank
    !
    !
    ! CITY
    call MPI_COMM_SPLIT( &
                      & MPI_COMM_WORLD, &
                      & icity,mcity,    &
                      & MPI_COMM_CITY,  &
                      & city_ierr       &
                      &)
    call get_size_rank(MPI_COMM_CITY, comm_city)
    write(6,*)' city_rank,city_nprocs,world_rank=',city_rank,city_nprocs,world_rank
    !
    return
  end subroutine parallel

  subroutine get_size_rank(comm, comm_xx)
    use mpi
    implicit none
    type(comm_x),intent(INOUT),target :: comm_xx
    integer, intent(IN) :: comm
    !
    integer, pointer :: rank,nprocs, ierr
    !
    rank    => comm_xx%rank
    nprocs  => comm_xx%nprocs
    ierr    => comm_xx%ierr
    !
    call MPI_COMM_RANK(comm, rank,ierr)
    call MPI_COMM_SIZE(comm, nprocs,ierr)
    !
    return
  end subroutine get_size_rank

end module spawn

program main
  use mpi
  use spawn
  implicit none
  !
  type(input) :: cinput
  integer     :: ierr
  !
  call MPI_INIT(ierr)
  !
  call spawn_main(cinput)
  write(6,*)'chk=',trim(adjustl(cinput%command(1)))
  !
  call MPI_FINALIZE(ierr)
  !
  stop
end program main
