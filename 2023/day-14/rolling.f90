PROGRAM rolling
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  IMPLICIT NONE
  INTEGER(KIND=int32)       :: input_file, total
  INTEGER(KIND=int32), &
    PARAMETER               :: line_len=500
  INTEGER(KIND=int32), &
    ALLOCATABLE             :: grid(:,:)
  INTEGER(KIND=int32), &
    PARAMETER               :: empty=0, cube_rock=1, round_rock=2
  CHARACTER(LEN=1), &
    PARAMETER               :: symbols(0:2) = &
                                 [".", "#", "O"]

  OPEN(NEWUNIT=input_file, FILE="input.txt", ACCESS="stream", &
    FORM="formatted")

  total = 0
  CALL read_input(input_file, grid)
  CLOSE(UNIT=input_file)

  CALL roll_rocks(grid)
  CALL calc_load(grid, total)

  WRITE(*, "(A, I0)") "Total: ", total
  DEALLOCATE(grid)

  CONTAINS

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
      INTEGER(KIND=int32)       :: n_rows, n_columns, fpos

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
      INTEGER(KIND=int32)       :: i, j, k, ios

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
