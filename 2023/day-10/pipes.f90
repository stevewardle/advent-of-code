PROGRAM pipes
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  
  IMPLICIT NONE

  INTEGER(KIND=int32)       :: input_file, i

  INTEGER(KIND=int32), &
    PARAMETER               :: line_len=500
  CHARACTER(LEN=line_len)   :: line

  INTEGER(KIND=int32)       :: n_columns, n_rows, &
                               start_row, start_column, &
                               row, column, &
                               fwd_row, fwd_column, &
                               bck_row, bck_column, &
                               steps
  LOGICAL, &
    ALLOCATABLE             :: grid(:,:,:)

  ! These mappings are key - using the same key index range
  ! we have the characters from the puzzle input and a set
  ! of logical arrays storing whether that symbol has a
  ! connection in that compass direction N-E-S-W
  CHARACTER(LEN=1), &
    PARAMETER               :: symbol_map(8) = &
    ["|","-","L","J","7","F",".","S"]
  LOGICAL, &
    PARAMETER               :: connection_map(4,8) = &
    RESHAPE([ &
      .TRUE., .FALSE.,.TRUE., .FALSE., & ! | : North + South
      .FALSE.,.TRUE., .FALSE.,.TRUE.,  & ! - : East  + West
      .TRUE., .TRUE., .FALSE.,.FALSE., & ! L : North + East
      .TRUE., .FALSE.,.FALSE.,.TRUE.,  & ! J : North + West
      .FALSE.,.FALSE.,.TRUE., .TRUE.,  & ! 7 : South + West
      .FALSE.,.TRUE., .TRUE., .FALSE., & ! F : South + East
      .FALSE.,.FALSE.,.FALSE.,.FALSE., & ! . : Nothing!
      .TRUE. ,.TRUE., .TRUE., .TRUE.   & ! S : Everything (Start)
    ], [4,8])

  LOGICAL :: direction(4), fwd_direction(4), bck_direction(4) 

  OPEN(NEWUNIT=input_file, FILE="input.txt")

  CALL calc_gridsize(input_file, n_rows, n_columns)

  ! The grid object is the size of the grid, but is a
  ! logical array with a 3rd dimension of direction
  ! Using the same NESW designation as the maps
  ALLOCATE(grid(n_rows, n_columns, 4))
  CALL read_grid(input_file, grid, start_row, start_column)
  CLOSE(input_file)

  ! Figure out the forward and backward points plus
  ! their starting directions from the start node
  fwd_direction(:) = .FALSE.
  bck_direction(:) = .FALSE.
  DO i=1,4
    row = start_row
    column = start_column
    direction(:) = .FALSE.
    direction(i) = .TRUE.
    CALL check_next_position(grid, row, column, direction)
    IF (.NOT. ALL(direction)) THEN
      IF (.NOT. ANY(fwd_direction)) THEN
        fwd_direction(i) = .TRUE.
      ELSE IF (.NOT. ANY(bck_direction)) THEN
        bck_direction(i) = .TRUE.
      END IF
    END IF
  END DO

  ! Follow the paths until they converge
  fwd_row = start_row
  fwd_column = start_column
  bck_row = start_row
  bck_column = start_column
  steps = 0
  DO
    CALL check_next_position(grid, fwd_row, fwd_column, fwd_direction)
    CALL check_next_position(grid, bck_row, bck_column, bck_direction)
    steps = steps + 1
    IF ((fwd_row == bck_row) .AND. (fwd_column == bck_column)) THEN
      EXIT
    END IF
  END DO

  WRITE(*, "(A, I0)") "Farthest step: ", steps

  DEALLOCATE(grid)

  CONTAINS
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
        IF (ios == IOSTAT_END) THEN
          EXIT
        END IF
        rows = rows + 1
        IF (rows == 1) THEN
          columns = LEN_TRIM(line)
        END IF
      END DO
      REWIND(funit)
    END SUBROUTINE calc_gridsize

    SUBROUTINE read_grid(funit, grid, start_row, start_col)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      LOGICAL, &
        INTENT(OUT)             :: grid(:,:,:)
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: start_row, start_col
      INTEGER(KIND=int32)       :: i, j, k, check
      check = 0
      start_row = 0
      start_col = 0
      ! Read the rows in reverse to make North positive
      DO i=n_rows,1,-1
        READ(UNIT=funit, FMT="(A)") line
        DO j=1,n_columns
          DO k=1,8
            IF (line(j:j) == symbol_map(k)) THEN
              grid(i,j,:) = connection_map(:,k)
              check = check + 1
              IF (line(j:j) == "S") THEN
                start_row = i
                start_col = j
              END IF
              EXIT
            END IF
          END DO
        END DO
      END DO
      IF (check /= SIZE(grid,DIM=1)*SIZE(grid,DIM=2)) THEN
        PRINT*, "Grid read not correct"
        CALL ABORT()
      END IF
    END SUBROUTINE read_grid

    SUBROUTINE check_next_position(grid, row, column, direction)
      IMPLICIT NONE
      LOGICAL, &
        INTENT(IN)              :: grid(:,:,:)
      INTEGER(KIND=int32), &
        INTENT(INOUT)           :: row, column
      LOGICAL, &
        INTENT(INOUT)           :: direction(4)
      INTEGER(KIND=int32)       :: new_row, new_column
      LOGICAL                   :: connection(4)

      ! Given a grid entry and a direction this returns a
      ! new direction if one exists (i.e. if navigating into)
      ! the given gridbox in that direction will work
      new_row = row
      new_column = column
      ! Find the point in the requested direction
      IF (direction(1)) THEN
        new_row = row + 1
      ELSE IF (direction(2)) THEN
        new_column = column + 1
      ELSE IF (direction(3)) THEN
        new_row = row - 1
      ELSE IF (direction(4)) THEN
        new_column = column - 1
      END IF
      ! Shifting the direction array by 2 positions will
      ! flip it because it swaps N/S and E/W positions
      ! this allows us to use to to test against the connections
      ! that the targetted point has
      connection = CSHIFT(direction, 2)
      connection = (grid(new_row, new_column,:) .NEQV. connection)
      ! If the above returns a single value it was a valid point
      ! we can navigate to; if not then don't move
      IF (COUNT(connection) == 1) THEN
        direction = connection 
        row = new_row
        column = new_column
      ELSE
        row = -1
        column = -1
        direction = [.FALSE.,.FALSE.,.FALSE.,.FALSE.]
      END IF

    END SUBROUTINE check_next_position

END PROGRAM pipes
