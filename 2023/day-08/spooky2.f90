PROGRAM spooky
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, int64, IOSTAT_END 
  
  IMPLICIT NONE

  INTEGER(KIND=int32)          :: input_file
  INTEGER(KIND=int32), &
    PARAMETER                  :: linelen=500, len_nodenames=3
  INTEGER(KIND=int32), &
    ALLOCATABLE                :: directions(:), nodemap(:,:), &
                                  nodestarts(:), nodeends(:), nodecurrents(:), &
                                  seen(:,:), loop(:,:)
  INTEGER(KIND=int64), &
    ALLOCATABLE                :: period(:), start(:)
  INTEGER(KIND=int64)          :: current_period, &
                                  combined_period, combined_phase
  CHARACTER(LEN=len_nodenames), &
    ALLOCATABLE                :: nodenames(:)
  INTEGER(KIND=int32)          :: steps, i, j, check

  ! Read puzzle
  OPEN(NEWUNIT=input_file, FILE="input.txt")
  CALL read_directions(input_file, directions)
  CALL read_nodes(input_file, nodenames, nodemap, nodestarts, nodeends)
  CLOSE(UNIT=input_file)

  ! Work out the points at which each starting route loops past an exit node
  ! for the first time and the number of steps it takes to pass it again
  steps = 0
  ALLOCATE(nodecurrents(SIZE(nodestarts)))
  ALLOCATE(seen(SIZE(nodestarts),SIZE(nodeends)))
  ALLOCATE(loop(SIZE(nodestarts),SIZE(nodeends)))
  nodecurrents(:) = nodestarts(:)
  seen(:,:) = 0
  loop(:,:) = 0
  outer: DO
    nodecurrents = nodemap(nodecurrents, directions(MOD(steps,SIZE(directions))+1))
    steps = steps + 1
    ! After moving to the next node, check each node to see if it has hit
    ! an exit node
    check = 0
    DO i=1,SIZE(nodestarts)
      DO j=1,SIZE(nodeends)
        IF (nodecurrents(i) == nodeends(j)) THEN
          ! If it has hit one for the first time, store the step it is on.
          ! The next time this same node hits the same exit point take the
          ! difference between the two to find the loop length
          IF (loop(i,j) == 0) THEN
            IF (seen(i,j) /= 0) THEN
              loop(i,j) = steps - seen(i,j)
              EXIT
            END IF
            seen(i,j) = steps
          END IF
        END IF
      END DO
      ! Register as we find each loop and break from the main loop once
      ! we have identified loops for every node
      IF (ANY(loop(i,:) > 0)) THEN
        check = check + 1
        IF (check == SIZE(nodestarts)) THEN
          EXIT outer
        END IF
      END IF
    END DO
  END DO outer

  ! Mercifully, the puzzle seems to only ever have 1 loop per
  ! ghost.  If it had multiple then the steps below would need
  ! to be calculated for *each possible combination* of loops
  ! from the different routes to find the smallest
  DO i=1,SIZE(nodestarts)
    IF (COUNT(loop(i,:) > 0) > 1) THEN
      WRITE(*, "(A)") "Multiple loops present for a given start"
      WRITE(*, "(A)") "This case is not handled"
      CALL ABORT()
    END IF
  END DO

  ! Gather the 2d array into a simpler 1d array of the periods and
  ! phases involved since we know we only have 1 per ghost
  ALLOCATE(period(SIZE(nodestarts)))
  ALLOCATE(start(SIZE(nodestarts)))
  DO i=1,SIZE(nodestarts)
    period(i) = MAXVAL(loop(i,:))
    start(i) = seen(i, MAXLOC(loop(i,:),DIM=1))
  END DO

  ! Another special case - it looks like the starting time (i.e. the
  ! time from step 1 to the first time each ghost encounters its Z node)
  ! are equal to the periods.  This means the calculation of when the
  ! loops will land on a Z together is significantly simpler.  I had
  ! setup to account for the differeng offset in terms of when each
  ! ghost started to loop but since they are looping from the start
  ! the result is just the combined periods

  IF (ALL(period == start)) THEN
    current_period = period(1)
    DO i=2,SIZE(nodestarts)
      CALL calc_combined_period( &
        current_period, period(i), &
        0_int64, 0_int64, combined_period, combined_phase)
      current_period = combined_period
    END DO
  ELSE
    WRITE(*, "(A)") "Loop start times were not aligned"
    WRITE(*, "(A)") "This case is not handled"
    CALL ABORT()
  END IF

  WRITE(*, "(A,I0)") "Total Steps Taken: ", current_period

  DEALLOCATE(start)
  DEALLOCATE(period)
  DEALLOCATE(loop)
  DEALLOCATE(seen)
  DEALLOCATE(nodecurrents)
  DEALLOCATE(nodestarts)
  DEALLOCATE(nodeends)
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

  SUBROUTINE read_nodes(funit, nodenames, nodemap, nodestarts, nodeends)
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
      ALLOCATABLE, &
      INTENT(INOUT)            :: nodestarts(:), nodeends(:)
    INTEGER(KIND=int32)     :: i, ios, n_nodes
    INTEGER(KIND=int32)     :: n_starts, n_ends, count_start, count_end
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
    CALL seek_to_node_entries_in_file(funit)
    n_starts = 0
    n_ends = 0
    DO i=1,n_nodes
      READ(UNIT=funit, FMT="(A3)") &
        nodenames(i)
      IF (SCAN(nodenames(i), "A", BACK=.TRUE.) == 3) THEN
        n_starts = n_starts + 1
      END IF
      IF (SCAN(nodenames(i), "Z", BACK=.TRUE.) == 3) THEN
        n_ends = n_ends + 1
      END IF
    END DO

    ! Create node map
    ALLOCATE(nodestarts(n_starts))
    ALLOCATE(nodeends(n_ends))
    count_start = 1
    count_end = 1
    CALL seek_to_node_entries_in_file(funit)
    DO i=1,n_nodes
      READ(UNIT=funit, FMT="(7X,A3,2X,A3)") &
        nodename_l, nodename_r
      nodemap(i, 1) = FINDLOC(nodenames, nodename_l, DIM=1)
      nodemap(i, 2) = FINDLOC(nodenames, nodename_r, DIM=1)

      IF (SCAN(nodenames(i), "A", BACK=.TRUE.) == 3) THEN
        nodestarts(count_start) = i
        count_start = count_start + 1
      END IF
      IF (SCAN(nodenames(i), "Z", BACK=.TRUE.) == 3) THEN
        nodeends(count_end) = i
        count_end = count_end + 1
      END IF

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

  ! Shameless-ly ripped from a post on Math StackExchange;
  ! In my defence I was familiar with the theory but not smart
  ! enough to derive the equations to use
  SUBROUTINE calc_combined_period( &
      period_a, period_b, phase_a, phase_b, combined_period, combined_phase)
    IMPLICIT NONE
    INTEGER(KIND=int64), &
      INTENT(IN)           :: period_a, period_b, phase_a, phase_b 
    INTEGER(KIND=int64), &
      INTENT(OUT)          :: combined_period, combined_phase 
    INTEGER(KIND=int64)    :: gcd, s, t, phase_diff, &
                              phase_diff_multi

    CALL gcdx(period_a, period_b, gcd, s, t)

    phase_diff = phase_a - phase_b 
    phase_diff_multi = -phase_diff/gcd 
    IF (MOD(-phase_diff, gcd) /= 0) THEN
      WRITE(*,"(A)") "Will never converge"
      CALL ABORT()
    END IF

    combined_period = period_a / gcd * period_b
    combined_phase = MOD(phase_a -s*phase_diff_multi*period_a, combined_period)

  END SUBROUTINE calc_combined_period

  ! Shameless-ly ripped from Wikipedia this is the
  ! Extended Euclidean Algorithm
  SUBROUTINE gcdx(a, b, old_r, old_s, old_t)
    IMPLICIT NONE
    INTEGER(KIND=int64), &
      INTENT(IN)          :: a, b
    INTEGER(KIND=int64), &
      INTENT(OUT)         :: old_s, old_t, old_r
    INTEGER(KIND=int64)   :: r, s, t, quotient, prov

    old_r = a
    r     = b
    old_s = 1
    s     = 0
    old_t = 0
    t     = 1
    DO WHILE (r /= 0)
      quotient = old_r / r

      prov = r
      r = old_r - quotient*prov
      old_r = prov
      
      prov = s
      s = old_s - quotient*prov
      old_s = prov

      prov = t
      t = old_t - quotient*prov
      old_t = prov

    END DO

  END SUBROUTINE gcdx

END PROGRAM spooky
