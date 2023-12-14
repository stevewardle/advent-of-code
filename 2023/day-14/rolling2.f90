PROGRAM rolling
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  IMPLICIT NONE
  INTEGER(KIND=int32)       :: input_file, total, i, j, start
  INTEGER(KIND=int32)       :: offset, last_seen, seen, period
  INTEGER(KIND=int32), &
    PARAMETER               :: line_len=500
  INTEGER(KIND=int32), &
    ALLOCATABLE             :: grid(:,:)
  INTEGER(KIND=int32), &
    PARAMETER               :: empty=0, cube_rock=1, round_rock=2
  INTEGER(KIND=int32), &
    PARAMETER               :: cycles = 1000000000
  INTEGER(KIND=int32), &
    PARAMETER               :: cycles_loop_check = 500
  CHARACTER(LEN=:), &
    ALLOCATABLE             :: hash(:) 
  CHARACTER(LEN=1), &
    PARAMETER               :: symbols(0:2) = &
                                 [".", "#", "O"]

  OPEN(NEWUNIT=input_file, FILE="input.txt", ACCESS="stream", &
    FORM="formatted")
  CALL read_input(input_file, grid)
  CLOSE(UNIT=input_file)

  ALLOCATE(&
    CHARACTER(LEN=SIZE(grid,1)*SIZE(grid,2)) &
      :: hash(cycles_loop_check))
  ! Find the point where the grid starts to repeat
  ! itself each cycle
  offset = 0
  period = 0
  last_seen = 0
  DO i=1,cycles_loop_check
    ! Hash the grid to make comparison easier then just look
    ! for seeing the same grid pass by twice and hope that
    ! the period between them is consistent
    CALL hash_grid(grid, hash(i))
    IF (i > 1) THEN
      seen = FINDLOC(hash(i) == hash(1:i-1), .TRUE., DIM=1)
      IF (seen > 0) THEN
        ! Capture the first time we see the same grid for
        ! the first time
        IF (offset == 0) THEN
          offset = i 
          last_seen = seen 
        ELSE IF (seen == last_seen) THEN
          ! When we see that same grid again we can calculate
          ! the period and offset of the loop
          period = i - offset
          offset = offset - period
          EXIT
        END IF
      END IF
    END IF
    DO j=1,4
      CALL roll_rocks(grid)
      CALL rotate_grid_minus_90(grid, 1)
    END DO
  END DO

  ! Now calculate which cycle would hit the target 
  start = &
    MOD(-offset, period) + period + MOD(cycles, period) + offset

  ! Easiest to adjust this to keep going from the above until
  ! we hit the right place - we will have already gone past
  ! the computed start point by 2 cycles, so we will next
  ! hit the right cycle after this many more cycles
  start = (start + period*2) - i + 1

  ! Keep rolling and cycling until we are at the desired spot 
  total = 0
  DO i=1,start 
    DO j=1,4
      CALL roll_rocks(grid)
      CALL rotate_grid_minus_90(grid, 1)
    END DO
  END DO
  CALL calc_load(grid, total)

  WRITE(*, "(A, I0)") "Total: ", total
  DEALLOCATE(hash)
  DEALLOCATE(grid)

  CONTAINS
    SUBROUTINE hash_grid(grid, hash)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)           :: grid(:,:)
      CHARACTER(LEN=SIZE(grid,1)*SIZE(grid,2)) &
                             :: hash 
      INTEGER(KIND=int32)    :: i, j, k

      k = 0
      DO i=1,SIZE(grid,1)
        DO j=1,SIZE(grid,2)
          k = k+1
          hash(k:k) = symbols(grid(i,j))
        END DO
      END DO

    END SUBROUTINE hash_grid

    SUBROUTINE rotate_grid_minus_90(grid, times)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(INOUT)        :: grid(:,:)
      INTEGER(KIND=int32), &
        INTENT(IN)           :: times
      INTEGER(KIND=int32)    :: i, j, k
      INTEGER(KIND=int32)    :: rotated(SIZE(grid,1), SIZE(grid,2))

      DO k=1,times
        grid = TRANSPOSE(grid)
        DO i=1,SIZE(grid,1)
          DO j=1,SIZE(grid,2)
            rotated(i,j) = grid(i,SIZE(grid,2)+1-j)
          END DO
        END DO
        grid = rotated
      END DO

    END SUBROUTINE rotate_grid_minus_90

    SUBROUTINE calc_load(grid, load)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(INOUT)        :: grid(:,:)
      INTEGER(KIND=int32), &
        INTENT(OUT)          :: load
      INTEGER(KIND=int32)    :: i

      load = 0
      DO i=SIZE(grid,1),1,-1
        load = load + COUNT(grid(SIZE(grid,1)+1-i,:) == round_rock)*i
      END DO

    END SUBROUTINE calc_load

    SUBROUTINE roll_rocks(grid)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(INOUT)        :: grid(:,:)
      INTEGER(KIND=int32)    :: fill(SIZE(grid,2))
      INTEGER(KIND=int32)    :: i, j

      fill(:) = 0
      DO i=1,SIZE(grid,1)
        DO j=1,SIZE(grid,2)
          IF (grid(i,j) == round_rock) THEN
            grid(i,j) = empty
            fill(j) = fill(j) + 1
            grid(fill(j), j) = round_rock
          ELSE IF (grid(i,j) == cube_rock) THEN
            fill(j) = i
          END IF
        END DO
      END DO

    END SUBROUTINE

    SUBROUTINE print_grid(grid)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)           :: grid(:,:)
      CHARACTER(LEN=1)       :: cgrid(SIZE(grid,2))
      INTEGER(KIND=int32)    :: i, j
      CHARACTER(LEN=50)      :: fmt_string
      DO i=1,SIZE(grid,1)
        DO j=1,SIZE(grid,2)
          cgrid(j) = symbols(grid(i,j))
        END DO
        WRITE(fmt_string, "(A,I0,A)") "(",SIZE(grid,2),"A)"
        WRITE(*,TRIM(fmt_string)) cgrid
      END DO

    END SUBROUTINE print_grid

    SUBROUTINE read_input(funit, grid)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      INTEGER(KIND=int32), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: grid(:,:)
      INTEGER(KIND=int32)       :: n_rows, n_columns

      IF (ALLOCATED(grid)) DEALLOCATE(grid)
      CALL calc_gridsize(funit, n_rows, n_columns)
      ALLOCATE(grid(n_rows, n_columns))
      CALL read_grid(funit, grid)

    END SUBROUTINE read_input

    SUBROUTINE calc_gridsize(funit, rows, columns)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: columns, rows
      INTEGER(KIND=int32)       :: ios
      CHARACTER(LEN=line_len)   :: line

      columns = 0
      rows = 0
      DO 
        READ(UNIT=funit, FMT="(A)", IOSTAT=ios) line
        IF ((TRIM(line) == "") .OR. (ios == IOSTAT_END)) THEN
          EXIT
        END IF
        rows = rows + 1
        IF (rows == 1) THEN
          columns = LEN_TRIM(line)
        END IF
      END DO
      REWIND(funit)

    END SUBROUTINE calc_gridsize

    SUBROUTINE read_grid(funit, grid)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: grid(:,:)
      CHARACTER(LEN=line_len)   :: line
      INTEGER(KIND=int32)       :: i, j, k

      grid(:,:) = -1
      DO i=1,SIZE(grid,1)
        READ(UNIT=funit, FMT="(A)") line
        DO j=1,SIZE(grid,2)
          DO k=0,UBOUND(symbols,DIM=1)
            IF (line(j:j) == symbols(k)) THEN
              grid(i,j) = k
            END IF
          END DO
        END DO
      END DO
      IF (MINVAL(grid) == -1) THEN
        WRITE(*, "(A)") "Grid read error"
        CALL ABORT()
      END IF

    END SUBROUTINE read_grid

END PROGRAM rolling
