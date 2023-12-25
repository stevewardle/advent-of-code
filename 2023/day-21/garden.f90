PROGRAM garden
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  IMPLICIT NONE
  LOGICAL, &
    ALLOCATABLE             :: grid(:,:), step_grid(:,:)
  INTEGER(KIND=int32), &
    PARAMETER               :: steps=64
  INTEGER(KIND=int32), &
    PARAMETER               :: sym_rock=0, sym_plot=1, sym_start=2
  CHARACTER(LEN=1), &
    PARAMETER               :: symbol(0:2) = ["#", ".", "S"]
  INTEGER(KIND=int32)       :: start_row, start_col, i

  CALL read_input("input.txt", grid, start_row, start_col)
  
  ALLOCATE(step_grid(SIZE(grid,1), SIZE(grid,2)))
  step_grid(:,:) = .FALSE. 
  step_grid(start_row, start_col) = .TRUE.

  DO i=1,steps
    CALL take_step(grid, step_grid)
  END DO

  WRITE(*, "(2(A,I0))") "Total after ",steps," steps: ", COUNT(step_grid)

  DEALLOCATE(step_grid)
  DEALLOCATE(grid)

  CONTAINS

    SUBROUTINE take_step(grid, step_grid)
      IMPLICIT NONE
      LOGICAL, &
        INTENT(IN)              :: grid(:,:)
      LOGICAL, &
        INTENT(INOUT)           :: step_grid(:,:)
      LOGICAL                   :: shift_step(SIZE(step_grid,1), SIZE(step_grid,2))
      INTEGER(KIND=int32)       :: shift, dim

      shift_step(:,:) = .FALSE.
      DO shift=-1,1,2
       DO dim=1,2
         shift_step = &
           ((shift_step) .OR. (EOSHIFT(step_grid, shift, DIM=dim) .AND. grid))
        END DO
      END DO
      step_grid = shift_step

    END SUBROUTINE take_step

    SUBROUTINE read_input(filename, grid, start_row, start_col)
      IMPLICIT NONE
      CHARACTER(LEN=*), &
        INTENT(IN)              :: filename
      LOGICAL, &
        ALLOCATABLE, &
        INTENT(INOUT)           :: grid(:,:)
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: start_row, start_col
      INTEGER(KIND=int32)       :: input_file, n_rows, n_columns

      IF (ALLOCATED(grid)) DEALLOCATE(grid)
      OPEN(NEWUNIT=input_file, FILE=filename, ACCESS="stream", &
        FORM="formatted")
      CALL calc_gridsize(input_file, n_rows, n_columns)
      ALLOCATE(grid(n_rows, n_columns))
      CALL read_grid(input_file, grid, start_row, start_col)
      CLOSE(UNIT=input_file)

    END SUBROUTINE read_input

    SUBROUTINE calc_gridsize(funit, rows, columns)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: columns, rows
      INTEGER(KIND=int32)       :: ios
      INTEGER(KIND=int32), &
        PARAMETER               :: line_len=500
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

    SUBROUTINE read_grid(funit, grid, start_row, start_col)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      LOGICAL, &
        INTENT(OUT)             :: grid(:,:)
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: start_row, start_col
      INTEGER(KIND=int32), &
        PARAMETER               :: line_len=500
      CHARACTER(LEN=line_len)   :: line
      INTEGER(KIND=int32)       :: i, j

      grid(:,:) = .FALSE.
      start_row = -1
      start_col = -1
      DO i=1,SIZE(grid,1)
        READ(UNIT=funit, FMT="(A)") line
        DO j=1,SIZE(grid,2)
          SELECT CASE (line(j:j))
          CASE (symbol(sym_rock))
            grid(i,j) = .FALSE. 
          CASE (symbol(sym_plot))
            grid(i,j) = .TRUE.
          CASE (symbol(sym_start))
            start_row = i
            start_col = j
            grid(i,j) = .TRUE.
          END SELECT
        END DO
      END DO

    END SUBROUTINE read_grid

END PROGRAM garden
