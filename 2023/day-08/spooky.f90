PROGRAM spooky
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END 
  
  IMPLICIT NONE

  INTEGER(KIND=int32)          :: input_file
  INTEGER(KIND=int32), &
    PARAMETER                  :: linelen=500, len_nodenames=3
  INTEGER(KIND=int32), &
    ALLOCATABLE                :: directions(:), nodemap(:,:)
  CHARACTER(LEN=len_nodenames), &
    ALLOCATABLE                :: nodenames(:)
  INTEGER(KIND=int32)          :: nodestart, nodeend, nodecurrent, steps

  ! Read puzzle
  OPEN(NEWUNIT=input_file, FILE="input.txt")
  CALL read_directions(input_file, directions)
  CALL read_nodes(input_file, nodenames, nodemap, nodestart, nodeend)
  CLOSE(UNIT=input_file)

  steps = 0
  nodecurrent = nodestart
  DO
    nodecurrent = nodemap(nodecurrent, directions(MOD(steps,SIZE(directions))+1))
    steps = steps + 1
    IF (nodecurrent == nodeend) THEN
      EXIT
    END IF
  END DO

  WRITE(*, "(A,I0)") "Total Steps Taken: ", steps

  DEALLOCATE(nodemap)
  DEALLOCATE(nodenames)
  DEALLOCATE(directions)

CONTAINS
  SUBROUTINE read_directions(funit, directions)
    IMPLICIT NONE
    INTEGER(KIND=int32), &
      INTENT(IN)            :: funit
    INTEGER(KIND=int32), &
      ALLOCATABLE, &
      INTENT(INOUT)         :: directions(:)
    INTEGER(KIND=int32)     :: i, n_directions
    CHARACTER(LEN=linelen)  :: line

    line = ""
    READ(UNIT=funit, FMT="(A)") line
    n_directions = VERIFY(line, " ", BACK=.TRUE.)
    ALLOCATE(directions(n_directions))
    DO i=1,n_directions
      IF (line(i:i) == "L") THEN
        directions(i) = 1
      ELSE IF (line(i:i) == "R") THEN
        directions(i) = 2
      ELSE
        WRITE(*, "(A,I0)") "Weird direction '"//line(i:i)//"' at: ", i
        CALL ABORT()
      END IF
    END DO
  END SUBROUTINE read_directions

  SUBROUTINE read_nodes(funit, nodenames, nodemap, nodestart, nodeend)
    IMPLICIT NONE
    INTEGER(KIND=int32), &
      INTENT(IN)            :: funit
    CHARACTER(LEN=len_nodenames), &
      ALLOCATABLE, &
      INTENT(INOUT)         :: nodenames(:)
    INTEGER(KIND=int32), &
      ALLOCATABLE, &
      INTENT(INOUT)         :: nodemap(:,:)
    INTEGER(KIND=int32), &
      INTENT(OUT)            :: nodestart, nodeend
    INTEGER(KIND=int32)     :: i, ios, n_nodes
    CHARACTER(LEN=len_nodenames) :: nodename_l, nodename_r

    ! Find number of nodes
    CALL seek_to_node_entries_in_file(funit)
    n_nodes = 0
    DO
      READ(UNIT=funit, FMT="(A)", IOSTAT=ios)
      IF (ios == IOSTAT_END) THEN
        EXIT
      END IF
      n_nodes = n_nodes + 1
    END DO
    
    ALLOCATE(nodenames(n_nodes))
    ALLOCATE(nodemap(n_nodes, 2))

    ! Create indices for nodes
    nodestart = 0
    nodeend = 0
    CALL seek_to_node_entries_in_file(funit)
    DO i=1,n_nodes
      READ(UNIT=funit, FMT="(A3)") &
        nodenames(i)
      IF (nodenames(i) == "AAA") THEN
        nodestart = i
      END IF
      IF (nodenames(i) == "ZZZ") THEN
        nodeend = i
      END IF
    END DO

    ! Create node map
    CALL seek_to_node_entries_in_file(funit)
    DO i=1,n_nodes
      READ(UNIT=funit, FMT="(7X,A3,2X,A3)") &
        nodename_l, nodename_r
      nodemap(i, 1) = FINDLOC(nodenames, nodename_l, DIM=1)
      nodemap(i, 2) = FINDLOC(nodenames, nodename_r, DIM=1)
    END DO

  END SUBROUTINE read_nodes

  SUBROUTINE seek_to_node_entries_in_file(funit)
    IMPLICIT NONE
    INTEGER(KIND=int32), &
      INTENT(IN)            :: funit
    CHARACTER(LEN=linelen)  :: line
    REWIND(UNIT=funit)
    DO
      READ(UNIT=funit, FMT="(A)") line
      IF (INDEX(line, "=") > 0) THEN
        BACKSPACE(UNIT=funit)
        EXIT
      END IF
    END DO
  END SUBROUTINE seek_to_node_entries_in_file

END PROGRAM spooky
