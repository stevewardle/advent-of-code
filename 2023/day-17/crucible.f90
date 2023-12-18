PROGRAM crucible
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  IMPLICIT NONE
  INTEGER(KIND=int32), &
    PARAMETER               :: line_len=500
  INTEGER(KIND=int32), &
    ALLOCATABLE             :: grid(:,:)
  INTEGER(KIND=int32)       :: heatloss

  CALL read_input("input.txt", grid)
  CALL find_path(grid, 1, 3, heatloss)

  WRITE(*, "(A,I0)") "Minimum heatloss: ", heatloss

  CALL find_path(grid, 4, 10, heatloss)
  WRITE(*, "(A,I0)") "Minimum heatloss (Ultra Crucibles): ", heatloss

  DEALLOCATE(grid)

  CONTAINS

    SUBROUTINE find_path(grid, min_steps, max_steps, min_heatloss)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: grid(:,:), min_steps, max_steps
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: min_heatloss
      INTEGER(KIND=int32), &
        ALLOCATABLE             :: heatloss(:,:,:), &
                                   scan1_rowcol(:,:), scan2_rowcol(:,:)
      LOGICAL, &
        ALLOCATABLE             :: not_done(:,:,:)
      INTEGER(KIND=int32), &
        PARAMETER               :: dir_ns=1, dir_ew=2
      INTEGER(KIND=int32)       :: cur_rowcoldir(3), row, col, dir, i, &
                                   tgt_row, tgt_col, hloss, new_dir

      min_heatloss = 0
      ALLOCATE(heatloss(SIZE(grid,1),SIZE(grid,2),2))
      ALLOCATE(not_done(SIZE(grid,1),SIZE(grid,2),2))

      ! Initialise scan arrays
      ALLOCATE(scan1_rowcol(max_steps, 2))
      ALLOCATE(scan2_rowcol(max_steps, 2))

      ! Initialise loss map to be large numbers
      ! no direction and all points unvisited
      heatloss(:,:,:) = HUGE(0_int32)
      not_done(:,:,:) = .TRUE.

      ! Starting point
      heatloss(1,1,dir_ns) = 0 
      heatloss(1,1,dir_ew) = 0

      outer: DO
        ! Find minimum unvisited point
        cur_rowcoldir = MINLOC(heatloss, MASK=not_done)
        row = cur_rowcoldir(1)
        col = cur_rowcoldir(2)
        dir = cur_rowcoldir(3)
        IF ((row == 0) .OR. (col == 0) .OR. (dir == 0)) EXIT
        
        ! Figure out which 3 points in each possible
        ! direction we are going to need to consider next
        SELECT CASE (dir)
        CASE (dir_ns)
          new_dir = dir_ew
          DO i=1,max_steps
            ! First scan goes east
            scan1_rowcol(i,1) = row
            scan1_rowcol(i,2) = col + i
            ! Next scan goes west
            scan2_rowcol(i,1) = row
            scan2_rowcol(i,2) = col - i
          END DO
        CASE (dir_ew)
          new_dir = dir_ns
          DO i=1,max_steps
            ! First scan goes south
            scan1_rowcol(i,1) = row + i
            scan1_rowcol(i,2) = col
            ! Next scan goes north
            scan2_rowcol(i,1) = row - i
            scan2_rowcol(i,2) = col
          END DO
        CASE DEFAULT
          PRINT*, "Direction unknown"
          CALL ABORT()
        END SELECT

        ! 1st direction scan
        hloss = heatloss(row, col, dir)
        DO i=1,max_steps
          tgt_row = scan1_rowcol(i,1)
          tgt_col = scan1_rowcol(i,2)
          
          ! Detect out of bounds and stop
          IF ((tgt_row < 1) .OR. (tgt_row > SIZE(grid,1)) &
            .OR. (tgt_col < 1) .OR. (tgt_col > SIZE(grid,2))) THEN
            EXIT
          END IF

          ! Get potential new heat loss value by
          ! adding the target point value
          hloss = &
            hloss + grid(tgt_row, tgt_col)
          
          ! If we have a minimum we aren't in range of yet
          ! loop back around
          IF (i < min_steps) CYCLE

          ! If this step would incur a lower loss
          ! than any existing one, replace it
          IF (hloss < heatloss(tgt_row, tgt_col, new_dir)) THEN
            heatloss(tgt_row, tgt_col, new_dir) = hloss
            ! If we have reached the target point, we are done
            IF (tgt_row == SIZE(grid,1) .AND. tgt_col == SIZE(grid,2)) THEN
              EXIT outer
            END IF
          END IF
        END DO

        ! 2nd direction scan
        hloss = heatloss(row, col, dir)
        DO i=1,max_steps
          tgt_row = scan2_rowcol(i,1)
          tgt_col = scan2_rowcol(i,2)
          
          ! Detect out of bounds and stop
          IF ((tgt_row < 1) .OR. (tgt_row > SIZE(grid,1)) &
            .OR. (tgt_col < 1) .OR. (tgt_col > SIZE(grid,2))) THEN
            EXIT
          END IF

          ! Get potential new heat loss value by
          ! adding the target point value
          hloss = &
            hloss + grid(tgt_row, tgt_col)
          
          ! If we have a minimum we aren't in range of yet
          ! loop back around
          IF (i < min_steps) CYCLE

          ! If this step would incur a lower loss
          ! than any existing one, replace it
          IF (hloss < heatloss(tgt_row, tgt_col, new_dir)) THEN
            heatloss(tgt_row, tgt_col, new_dir) = hloss
            ! If we have reached the target point, we are done
            IF (tgt_row == SIZE(grid,1) .AND. tgt_col == SIZE(grid,2)) THEN
              EXIT outer
            END IF
          END IF
        END DO

        ! This point is now done, which we confusingly
        ! indicate by setting "not done" to false :/ 
        not_done(row, col, dir) = .FALSE.

      END DO outer

      min_heatloss = MINVAL(heatloss(SIZE(grid,1), SIZE(grid,2),:))

      DEALLOCATE(scan2_rowcol)
      DEALLOCATE(scan1_rowcol)
      DEALLOCATE(not_done)
      DEALLOCATE(heatloss)

    END SUBROUTINE find_path

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
      INTEGER(KIND=int32)       :: i, j

      grid(:,:) = 0
      DO i=1,SIZE(grid,1)
        READ(UNIT=funit, FMT="(A)") line
        DO j=1,SIZE(grid,2)
          READ(line(j:j), "(I1)") grid(i,j)
        END DO
      END DO
      IF (MINVAL(grid) == 0) THEN
        WRITE(*, "(A)") "Grid read error"
        CALL ABORT()
      END IF

    END SUBROUTINE read_grid

END PROGRAM crucible
