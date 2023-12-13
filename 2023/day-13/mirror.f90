PROGRAM mirror
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  IMPLICIT NONE
  INTEGER(KIND=int32)       :: input_file, points, total
  INTEGER(KIND=int32), &
    PARAMETER               :: line_len=500
  LOGICAL, &
    ALLOCATABLE             :: grid(:,:)

  OPEN(NEWUNIT=input_file, FILE="input.txt", ACCESS="stream", &
    FORM="formatted")

  total = 0
  DO
    CALL read_next_grid(input_file, grid)
    IF (.NOT. ALLOCATED(grid)) EXIT

    CALL detect_mirror(grid, points)
    IF (points > 0) THEN
      total = total + points
    ELSE
      CALL detect_mirror(TRANSPOSE(grid), points)
      total = total + 100*points
    END IF
  END DO

  WRITE(*, "(A, I0)") "Total: ", total
  CLOSE(UNIT=input_file)

  CONTAINS

    SUBROUTINE detect_mirror(grid, points_below)
      IMPLICIT NONE
      LOGICAL, &
        INTENT(IN)              :: grid(:,:)
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: points_below
      LOGICAL                   :: grid_shift(SIZE(grid,1),SIZE(grid,2))
      LOGICAL                   :: mirror_valid(SIZE(grid,2))
      INTEGER(KIND=int32)       :: i, j

      mirror_valid(:) = .TRUE.
      points_below = 0
      ! Iterate to half the length of the grid (the maximum distance
      ! a reflection can be from the edge)
      DO i=1,SIZE(grid,2)/2
        ! Translates to compare pairs of positions radiating outwards
        ! so i=1 compares each point X to its neighbour X+1
        !    i=2 compares X-1 to X+2 (first reflection)
        !    i=3 compares X-2 to X+3 (second reflection) and so on
        grid_shift = (EOSHIFT(grid(:,:),-(i-1),DIM=2) &
                .EQV. EOSHIFT(grid(:,:),i,DIM=2))
        ! Now compare relevant points (gradually shrinking) to see if
        ! a given position is still valid as a mirror
        DO j=i,SIZE(grid,2)+1-i
          ! The previous reflections need to have all matched at the
          ! given position for that position to still be a valid
          ! mirror
          mirror_valid(j) = mirror_valid(j) &
            .AND. (ALL(grid_shift(:,j)) .EQV. .TRUE.)
        END DO
        ! After each update, if one either mirror position where
        ! the reflection just calculated would touch the edge
        ! of the grid - we have found our mirror)
        IF (mirror_valid(i)) THEN
          points_below = i
          EXIT
        ELSE IF (mirror_valid(SIZE(grid,2)-i)) THEN
          points_below = SIZE(grid,2)-i
          EXIT
        END IF
      END DO

    END SUBROUTINE detect_mirror

    SUBROUTINE read_next_grid(funit, grid)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      LOGICAL, &
        ALLOCATABLE, &
        INTENT(INOUT)           :: grid(:,:)
      INTEGER(KIND=int32)       :: n_rows, n_columns, fpos

      IF (ALLOCATED(grid)) DEALLOCATE(grid)
      INQUIRE(UNIT=funit, POS=fpos)
      CALL calc_next_gridsize(funit, n_rows, n_columns)
      IF ((n_rows == 0) .AND. (n_columns == 0)) RETURN
      ALLOCATE(grid(n_rows, n_columns))
      READ(UNIT=funit, FMT="()", ADVANCE="NO", POS=fpos)
      CALL read_grid(funit, grid)

    END SUBROUTINE read_next_grid

    SUBROUTINE calc_next_gridsize(funit, rows, columns)
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

    END SUBROUTINE calc_next_gridsize

    SUBROUTINE read_grid(funit, grid)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      LOGICAL, &
        INTENT(OUT)             :: grid(:,:)
      CHARACTER(LEN=line_len)   :: line
      INTEGER(KIND=int32)       :: i, j, ios
      CHARACTER(LEN=1), &
        PARAMETER               :: ash=".", rock="#"

      DO i=1,SIZE(grid,1)
        READ(UNIT=funit, FMT="(A)") line
        DO j=1,SIZE(grid,2)
          IF (line(j:j) == ash) THEN
            grid(i,j) = .FALSE.
          ELSE IF (line(j:j) == rock) THEN
            grid(i,j) = .TRUE.
          ELSE
            WRITE(*, "(A,I0,I0)") "Grid read error at: ",i,j
            CALL ABORT()
          END IF
        END DO
      END DO
      ! Skip the blank line
      READ(UNIT=funit, FMT="()", IOSTAT=ios)

    END SUBROUTINE read_grid

END PROGRAM mirror
