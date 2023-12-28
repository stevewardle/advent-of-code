PROGRAM garden
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, int64, real64, IOSTAT_END
  IMPLICIT NONE
  LOGICAL, &
    ALLOCATABLE             :: grid(:,:), step_grid(:,:), big_grid(:,:)
  INTEGER(KIND=int32), &
    PARAMETER               :: steps=64, expanded_steps=26501365
  INTEGER(KIND=int32), &
    PARAMETER               :: sym_rock=0, sym_plot=1, sym_start=2
  CHARACTER(LEN=1), &
    PARAMETER               :: symbol(0:2) = ["#", ".", "S"], &
                               output(0:1) = [".", "O"]
  INTEGER(KIND=int32)       :: start_row, start_col, i, j
  INTEGER(KIND=int32)       :: step_counts(3)
  REAL(KIND=real64)         :: final_count

  CALL read_input("input.txt", grid, start_row, start_col)
  
  ALLOCATE(step_grid(SIZE(grid,1), SIZE(grid,2)))
  step_grid(:,:) = .FALSE. 
  step_grid(start_row, start_col) = .TRUE.

  DO i=1,steps
    CALL take_step(grid, step_grid)
  END DO
 
  WRITE(*, "(2(A,I0))") "Total after ",steps," steps: ", COUNT(step_grid)

  ! Expand the grid to a large enough size to simulate a few
  ! sample points for fitting a quadratic polynomial - since the input
  ! appears to be designed carefully to fit it exactly
  
  ! After the expansion reaches the edge of the initial grid, we want to
  ! simulate 3 repetitions of the full grid width - this will be enough
  ! to reach the edge of a 7x7 grid (i.e. it will have 3 tiles in all
  ! directions around the initial grid)
  CALL expand_grid(grid, big_grid, step_grid, 7)

  ! Now simulate to the desired range
  j = 0
  DO i=1,SIZE(grid,1)/2 + 3*(SIZE(grid,1))
    CALL take_step(big_grid, step_grid)
    ! Capture just the counts as the expansion hits the edge of each new
    ! layer of tiles
    IF ((MOD(i-SIZE(grid,1)/2,SIZE(grid,1)) == 0) &
        .AND. (i > SIZE(grid,1)/2)) THEN
      j = j + 1
      step_counts(j) = COUNT(step_grid)
    END IF
  END DO

  ! With these 3 points we can fit a quadratic polynomial and use it
  ! to target the desired step count.  Noting that the target count
  ! is conventenetly also a multiple of the grid size
  CALL lagrange_interp( &
    REAL([1,2,3], KIND=real64), &
    REAL(step_counts, KIND=real64), &
    REAL((expanded_steps-SIZE(grid,1)/2)/SIZE(grid,1), KIND=real64), &
    final_count)

  WRITE(*, "(2(A,I0))") &
    "Reachable plots after ", expanded_steps, " steps: ", INT(final_count, KIND=int64)

  DEALLOCATE(big_grid)
  DEALLOCATE(step_grid)
  DEALLOCATE(grid)

  CONTAINS

    SUBROUTINE lagrange_interp(x_fit, y_fit, x, y)
      IMPLICIT NONE

      REAL(KIND=real64), &
        INTENT(IN)           :: x_fit(:), y_fit(SIZE(x_fit))
      REAL(KIND=real64), &
        INTENT(IN)           :: x
      REAL(KIND=real64), &
        INTENT(OUT)          :: y

      INTEGER(KIND=int32)    :: j,k
      REAL(KIND=real64)      :: p

      y = 0.0_real64
      DO j=1,SIZE(x_fit)
          p = y_fit(j)
          DO k=1,SIZE(x_fit)
            IF (k /= j) THEN
              p = p * (x - x_fit(k)) / (x_fit(j) - x_fit(k))
            END IF
          END DO
          y = y + p
      END DO

    END SUBROUTINE lagrange_interp

    SUBROUTINE expand_grid(grid, big_grid, step_grid, expansion)
      IMPLICIT NONE
      LOGICAL, &
        INTENT(IN)           :: grid(:,:)
      LOGICAL, &
        INTENT(INOUT), &
        ALLOCATABLE          :: step_grid(:,:), big_grid(:,:)
      INTEGER(KIND=int32), &
        INTENT(IN)           :: expansion
      INTEGER(KIND=int32)    :: i,j

      ALLOCATE(big_grid(SIZE(grid,1)*expansion, SIZE(grid,2)*expansion))

      DO i=0,expansion-1
        DO j=0,expansion-1
          big_grid(SIZE(grid,1)*i+1:SIZE(grid,1)*(i+1),SIZE(grid,2)*j+1:SIZE(grid,2)*(j+1)) = grid
        END DO
      END DO

      DEALLOCATE(step_grid)
      ALLOCATE(step_grid(SIZE(big_grid,1), SIZE(big_grid,2)))
      step_grid(:,:) = .FALSE. 
      step_grid(start_row+SIZE(grid,1)*(expansion/2), &
                start_col+SIZE(grid,2)*(expansion/2)) = .TRUE.

    END SUBROUTINE expand_grid

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
