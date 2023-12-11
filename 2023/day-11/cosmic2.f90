PROGRAM cosmic
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, int64, IOSTAT_END
  IMPLICIT NONE
  INTEGER(KIND=int32)       :: input_file, i
  INTEGER(KIND=int64)       :: total
  INTEGER(KIND=int32), &
    PARAMETER               :: line_len=500
  INTEGER(KIND=int32)       :: n_columns, n_rows, n_galaxies
  INTEGER(KIND=int64), &
    ALLOCATABLE             :: galaxy_coords(:,:), &
                               x_diff(:), y_diff(:)
  CHARACTER(LEN=1), &
    PARAMETER               :: galaxy_symbol="#"
  INTEGER(KIND=int64), &
    PARAMETER               :: expansion_size=1000000

  OPEN(NEWUNIT=input_file, FILE="input.txt")

  CALL calc_sizes(input_file, n_rows, n_columns, n_galaxies)

  ! Read the input - this also deals with expansion and simply
  ! returns the co-ordinates of the galaxies on the expanded grid
  ! - no need to read the grid into memory
  ALLOCATE(galaxy_coords(n_galaxies,2))
  CALL read_input(input_file, n_rows, n_columns, galaxy_coords)
  CLOSE(UNIT=input_file)

  ! Now to calculate the distances - nothing fancy here really
  ! there's no need to do anything complicated as with discrete
  ! steps like this the shortest distance between two points is
  ! identical however you do it provided you are always moving
  ! towards the target in either axis - so the easiest way is
  ! to just sum the difference in the x and y directions
  ALLOCATE(x_diff(n_galaxies))
  ALLOCATE(y_diff(n_galaxies))
  total = 0
  DO i=1,n_galaxies-1
    ! Basically by shifting the coordinate array in each direction
    ! once per galaxy we eventually compare every galaxy to every
    ! other - the amount of terms to sum drops as we go through to
    ! avoid double counting anything
    x_diff = &
      ABS(galaxy_coords(:,1) - EOSHIFT(galaxy_coords(:,1), i))
    y_diff = &
      ABS(galaxy_coords(:,2) - EOSHIFT(galaxy_coords(:,2), i))
    total = total + SUM(x_diff(1:n_galaxies-i) + y_diff(1:n_galaxies-i))
  END DO

  WRITE(*,"(A,I0)") "Total distance: ", total

  DEALLOCATE(y_diff)
  DEALLOCATE(x_diff)
  DEALLOCATE(galaxy_coords)

  CONTAINS
    SUBROUTINE calc_sizes(funit, rows, columns, galaxies)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: columns, rows, galaxies
      INTEGER(KIND=int32)       :: ios, galaxy
      CHARACTER(LEN=line_len)   :: line
      columns = 0
      rows = 0
      galaxies = 0
      line = ""
      DO 
        READ(UNIT=funit, FMT="(A)", IOSTAT=ios) line
        IF (ios == IOSTAT_END) THEN
          EXIT
        END IF
        rows = rows + 1
        IF (rows == 1) THEN
          columns = LEN_TRIM(line)
        END IF
        DO
          galaxy = SCAN(line, galaxy_symbol)
          IF (galaxy == 0) THEN
            EXIT
          ELSE
            n_galaxies = n_galaxies + 1
            line(1:galaxy) = ""
          END IF
        END DO
      END DO
      REWIND(funit)
    END SUBROUTINE calc_sizes

    SUBROUTINE read_input(funit, n_rows, n_columns, coords) 
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit, n_rows, n_columns
      INTEGER(KIND=int64), &
        INTENT(OUT)             :: coords(:,:)
      INTEGER(KIND=int32)       :: i, j, galaxy
      INTEGER(KIND=int64)       :: offset_x
      CHARACTER(LEN=line_len)   :: line
      galaxy = 0
      offset_x = 0
      DO i=1,n_rows
        READ(UNIT=funit, FMT="(A)") line
        DO j=1,n_columns
          ! Find the galaxies and store their coords
          IF (line(j:j) == galaxy_symbol) THEN
            galaxy = galaxy + 1
            coords(galaxy,1) = i + offset_x
            coords(galaxy,2) = j
          END IF
        END DO
        ! Detect blank rows - each time we see
        ! one increase the offset so subsequent
        ! x coords
        IF (SCAN(line, galaxy_symbol) == 0) THEN
          offset_x = offset_x + expansion_size - 1
        END IF
      END DO
      ! Detect blank columns and shift the coords
      ! accordingly
      DO j=n_columns,1,-1
        IF (.NOT. ANY(coords(:,2) == j)) THEN
          DO i=1,galaxy
            IF (coords(i,2) > j) THEN
              coords(i,2) = coords(i,2) + expansion_size - 1
            END IF
          END DO
        END IF
      END DO

    END SUBROUTINE read_input

END PROGRAM cosmic
