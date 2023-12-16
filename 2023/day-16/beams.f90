PROGRAM beams
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  IMPLICIT NONE
  INTEGER(KIND=int32), &
    PARAMETER               :: line_len=500
  INTEGER(KIND=int32), &
    ALLOCATABLE             :: grid(:,:)
  INTEGER(KIND=int32), &
    ALLOCATABLE             :: energised(:,:)
  INTEGER(KIND=int32), &
    PARAMETER               :: e_ns=1, e_ew=2, e_cross=3, e_obj=4
  INTEGER(KIND=int32), &
    PARAMETER               :: empty=0, v_split=1, h_split=2,  &
                               r_mirror=3, l_mirror=4
  CHARACTER(LEN=1), &
    PARAMETER               :: symbols(0:4) = &
                                 [".", "|", "-", "/", "\"]

  CALL read_input("input.txt", grid)
  ALLOCATE(energised(SIZE(grid,1), SIZE(grid,2)))
  energised(:,:) = 0
  CALL track_beam(1,1,0,1)
  WRITE(*,"(A,I0)") "Energised tiles: ", COUNT(energised /= 0)

  CONTAINS
    RECURSIVE SUBROUTINE track_beam(row, col, dir_s, dir_e)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)          :: row, col, dir_s, dir_e
      INTEGER(KIND=int32)   :: cur_row, cur_col
      INTEGER(KIND=int32)   :: cur_dir_s, cur_dir_e

      cur_row = row
      cur_col = col
      cur_dir_s = dir_s
      cur_dir_e = dir_e

      DO
        ! If we have hit the edge of the grid
        ! we are finished
        IF ((cur_row > SIZE(grid,1)) &
          .OR. (cur_col > SIZE(grid,2)) &
          .OR. (cur_row == 0) &
          .OR. (cur_col == 0)) THEN
          EXIT
        END IF
        ! If this square already contains a
        ! beam in our orientation we should
        ! exit as we are now on a path that
        ! cannot reveal anything new
        IF ((energised(cur_row, cur_col) == e_ns) &
          .AND. (cur_dir_s /= 0)) THEN
          EXIT
        END IF
        IF ((energised(cur_row, cur_col) == e_ew) &
          .AND. (cur_dir_e /= 0)) THEN
          EXIT
        END IF

        SELECT CASE (grid(cur_row, cur_col))
        CASE (empty)
          ! No change to movement, but update
          ! the energised array to show there
          ! is a beam in this location
          IF (cur_dir_s /= 0) THEN
            energised(cur_row, cur_col) = e_ns
          ELSE IF (cur_dir_e /= 0) THEN
            energised(cur_row, cur_col) = e_ew
          END IF
        CASE (r_mirror)
          IF (cur_dir_e /= 0) THEN
            cur_dir_s = -cur_dir_e
            cur_dir_e = 0
          ELSE IF (cur_dir_s /= 0) THEN
            cur_dir_e = -cur_dir_s
            cur_dir_s = 0
          END IF
          energised(cur_row, cur_col) = e_obj
        CASE (l_mirror)
          IF (cur_dir_e /= 0) THEN
            cur_dir_s = cur_dir_e
            cur_dir_e = 0
          ELSE IF (cur_dir_s /= 0) THEN
            cur_dir_e = cur_dir_s
            cur_dir_s = 0
          END IF
          energised(cur_row, cur_col) = e_obj
        CASE (v_split)
          energised(cur_row, cur_col) = e_obj
          IF (cur_dir_e /= 0) THEN
            ! Split the beam here by calling ourselves
            ! for the two new beam directions
            CALL track_beam(cur_row + 1, cur_col,  1, 0)
            CALL track_beam(cur_row - 1, cur_col, -1, 0)
            ! And then stop
            EXIT
          END IF
          ! For the north/south case the beam
          ! carries on
        CASE (h_split)
          energised(cur_row, cur_col) = e_obj
          IF (cur_dir_s /= 0) THEN
            ! Split the beam the other way
            CALL track_beam(cur_row, cur_col + 1, 0,  1)
            CALL track_beam(cur_row, cur_col - 1, 0, -1)
            ! And stop
            EXIT
          END IF
          ! For the east/west case the beam
          ! carries on
        CASE DEFAULT
          CALL ABORT()
        END SELECT

        ! Update the row positions
        cur_row = cur_row + cur_dir_s
        cur_col = cur_col + cur_dir_e

      END DO

    END SUBROUTINE 

    SUBROUTINE read_input(filename, grid)
      IMPLICIT NONE
      CHARACTER(LEN=*), &
        INTENT(IN)              :: filename
      INTEGER(KIND=int32), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: grid(:,:)
      INTEGER(KIND=int32)       :: input_file, n_rows, n_columns

      IF (ALLOCATED(grid)) DEALLOCATE(grid)
      OPEN(NEWUNIT=input_file, FILE=filename, ACCESS="stream", &
        FORM="formatted")
      CALL calc_gridsize(input_file, n_rows, n_columns)
      ALLOCATE(grid(n_rows, n_columns))
      CALL read_grid(input_file, grid)
      CLOSE(UNIT=input_file)

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

END PROGRAM beams
