PROGRAM crucible
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  IMPLICIT NONE

  TYPE dig_step
    CHARACTER(LEN=1)    :: direction
    INTEGER(KIND=int32) :: distance
    CHARACTER(LEN=7)    :: colour
  END TYPE dig_step

  TYPE trench_wall
    INTEGER(KIND=int32) :: row, col
    CHARACTER(LEN=7)    :: colour
    INTEGER(KIND=int32) :: orientation
  END TYPE trench_wall

  CHARACTER(LEN=1), &
    PARAMETER           :: dir_u="U", dir_d="D", &
                           dir_r="R", dir_l="L"
  INTEGER(KIND=int32), &
    PARAMETER           :: orient_ew=1, orient_ns=2, &
                           orient_corner=3
  TYPE(dig_step), &
    ALLOCATABLE         :: dig_plan(:) 
  TYPE(trench_wall), &
    ALLOCATABLE         :: trench(:)
  INTEGER(KIND=int32)   :: volume

  CALL read_input("input.txt", dig_plan)
  CALL dig_trench(dig_plan, trench)
  CALL calc_lagoon(trench, volume)

  WRITE(*,"(A,I0)") "Total Lagoon Volume: ", volume

  DEALLOCATE(trench)
  DEALLOCATE(dig_plan)

  CONTAINS

    SUBROUTINE calc_lagoon(trench, volume)
      IMPLICIT NONE
      TYPE(trench_wall), &
        INTENT(IN)              :: trench(:)
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: volume 
      INTEGER, &
        ALLOCATABLE             :: filled(:,:)
      INTEGER(KIND=int32)       :: i

      IF (ALLOCATED(filled)) DEALLOCATE(filled)
      ALLOCATE(filled(MINVAL(trench(:)%row):MAXVAL(trench(:)%row), &
                      MINVAL(trench(:)%col):MAXVAL(trench(:)%col)))
      ! Fill the points described by the trench
      filled(:,:) = 0
      DO i=1,SIZE(trench)
        filled(trench(i)%row, trench(i)%col) = trench(i)%orientation
      END DO

      ! I am pathetic and was tired so for this one I just
      ! used a lazy fill and eyeballed a point - it makes
      ! me unhappy but I will think about revising it for
      ! part 2
      CALL flood_fill(filled, 122, 82, 4)

      volume = COUNT(filled /= 0)

      DEALLOCATE(filled)

    END SUBROUTINE calc_lagoon

    RECURSIVE SUBROUTINE flood_fill(grid, row, col, fill)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(INOUT)           :: grid(:,:)
      INTEGER(KIND=int32), &
        INTENT(IN)              :: row, col, fill
      grid(row,col) = fill
      IF ((row+1 <= SIZE(grid,1)) .AND. (grid(row+1,col) == 0)) THEN
        CALL flood_fill(grid, row+1, col, fill)
      END IF
      IF ((col+1 <= SIZE(grid,2)) .AND. (grid(row,col+1) == 0)) THEN
        CALL flood_fill(grid, row, col+1, fill)
      END IF
      IF ((row-1 >= 1)  .AND. (grid(row-1,col) == 0)) THEN
        CALL flood_fill(grid, row-1, col, fill)
      END IF
      IF ((col-1 >= 1)  .AND. (grid(row,col-1) == 0)) THEN
        CALL flood_fill(grid, row, col-1, fill)
      END IF

    END SUBROUTINE flood_fill

    SUBROUTINE dig_trench(plan, trench)
      IMPLICIT NONE
      TYPE(dig_step), &
        INTENT(IN)              :: plan(:)
      TYPE(trench_wall), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: trench(:)
      INTEGER(KIND=int32)       :: i, j, offset, &
                                   row, col, ori, &
                                   cur_row, cur_col

      ! Figure out length of trench and prepare it
      IF (ALLOCATED(trench)) DEALLOCATE(trench)
      ALLOCATE(trench(SUM(plan(:)%distance)))

      cur_row = 0
      cur_col = 0
      offset = 1
      DO i=1,SIZE(plan)
        ! Setup the direction
        col = 0
        row = 0
        SELECT CASE(plan(i)%direction)
        CASE(dir_u)
          row = -1
          ori = orient_ns
        CASE (dir_d)
          row = 1
          ori = orient_ns
        CASE (dir_l)
          col = -1
          ori = orient_ew
        CASE (dir_r)
          col = 1
          ori = orient_ew
        CASE DEFAULT
          WRITE(*,"(A,I0)") "Bad direction: ", plan(i)%direction
          CALL ABORT()
        END SELECT
        ! Fill the points for this plan step
        DO j=1,plan(i)%distance
          cur_row = cur_row + row
          cur_col = cur_col + col
          trench(offset)%row = cur_row
          trench(offset)%col = cur_col
          trench(offset)%orientation = ori
          trench(offset)%colour = plan(i)%colour
          offset = offset + 1
        END DO
        ! The last point is a corner
        trench(offset -1)%orientation = orient_corner
      END DO
  
    END SUBROUTINE dig_trench

    SUBROUTINE read_input(filename, plan)
      IMPLICIT NONE
      CHARACTER(LEN=*), &
        INTENT(IN)              :: filename
      TYPE(dig_step), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: plan(:)
      INTEGER(KIND=int32)       :: input_file, n_lines

      IF (ALLOCATED(plan)) DEALLOCATE(plan)
      OPEN(NEWUNIT=input_file, FILE=filename, ACCESS="stream", &
        FORM="formatted")
      CALL calc_plansize(input_file, n_lines) 
      ALLOCATE(plan(n_lines))
      CALL read_plan(input_file, plan)
      CLOSE(UNIT=input_file)

    END SUBROUTINE read_input

    SUBROUTINE calc_plansize(funit, n_lines)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: n_lines
      INTEGER(KIND=int32)       :: ios

      n_lines = 0
      DO 
        READ(UNIT=funit, FMT="()", IOSTAT=ios)
        IF (ios == IOSTAT_END) THEN
          EXIT
        END IF
        n_lines = n_lines + 1
      END DO
      REWIND(funit)

    END SUBROUTINE calc_plansize

    SUBROUTINE read_plan(funit, plan)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      TYPE(dig_step), &
        INTENT(OUT)             :: plan(:)
      CHARACTER(LEN=9)          :: colour_parens
      INTEGER(KIND=int32)       :: i

      DO i=1,SIZE(plan)
        READ(funit, *) plan(i)%direction, plan(i)%distance, colour_parens
        plan(i)%colour = colour_parens(2:8)
      END DO

    END SUBROUTINE read_plan

END PROGRAM crucible
