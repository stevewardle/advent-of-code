PROGRAM crucible
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, int64, IOSTAT_END
  IMPLICIT NONE

  TYPE dig_step
    CHARACTER(LEN=1)    :: direction
    INTEGER(KIND=int64) :: distance
    CHARACTER(LEN=7)    :: colour
  END TYPE dig_step

  TYPE trench_wall
    INTEGER(KIND=int64) :: row, col
  END TYPE trench_wall

  CHARACTER(LEN=1), &
    PARAMETER           :: dir_u="U", dir_d="D", &
                           dir_r="R", dir_l="L"
  TYPE(dig_step), &
    ALLOCATABLE         :: dig_plan(:) 
  TYPE(trench_wall), &
    ALLOCATABLE         :: trench(:)
  INTEGER(KIND=int64)   :: trench_length, volume

  CALL read_input("input.txt", dig_plan)
  CALL dig_trench(dig_plan, trench)

  trench_length = SUM(dig_plan(:)%distance)
  CALL calc_lagoon(trench, trench_length, volume)

  WRITE(*,"(A,I0)") "Total Lagoon Volume: ", volume

  CALL expand_input(dig_plan)
  CALL dig_trench(dig_plan, trench)

  trench_length = SUM(dig_plan(:)%distance)
  CALL calc_lagoon(trench, trench_length, volume)

  WRITE(*,"(A,I0)") "ACTUAL Total Lagoon Volume: ", volume

  DEALLOCATE(trench)
  DEALLOCATE(dig_plan)

  CONTAINS

    SUBROUTINE calc_lagoon(trench, length, volume)
      IMPLICIT NONE
      TYPE(trench_wall), &
        INTENT(IN)              :: trench(:)
      INTEGER(KIND=int64), &
        INTENT(IN)              :: length
      INTEGER(KIND=int64), &
        INTENT(OUT)             :: volume 
      INTEGER(KIND=int64)       :: cross_1, cross_2

      ! Use Gauss Area 
      cross_1 = SUM(trench(:)%row * CSHIFT(trench(:)%col,1))
      cross_2 = SUM(trench(:)%col * CSHIFT(trench(:)%row,1))
      volume = ABS((cross_1)/2 - (cross_2)/2)
      ! Since the trench is a 1m cube around each point need
      ! to add on the additional outer permiter
      volume = volume + length/2 + 1

    END SUBROUTINE calc_lagoon

    SUBROUTINE dig_trench(plan, trench)
      IMPLICIT NONE
      TYPE(dig_step), &
        INTENT(IN)              :: plan(:)
      TYPE(trench_wall), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: trench(:)
      INTEGER(KIND=int64)       :: i, row, col

      ! Figure out length of trench and prepare it
      IF (ALLOCATED(trench)) DEALLOCATE(trench)
      ALLOCATE(trench(SIZE(plan)))

      trench(1)%row = 0
      trench(1)%col = 0
      trench_length = 0
      DO i=2,SIZE(plan)
        ! Setup the direction
        col = 0
        row = 0
        SELECT CASE(plan(i)%direction)
        CASE(dir_u)
          row = -1
        CASE (dir_d)
          row = 1
        CASE (dir_l)
          col = -1
        CASE (dir_r)
          col = 1
        CASE DEFAULT
          WRITE(*,"(A,I0)") "Bad direction: ", plan(i)%direction
          CALL ABORT()
        END SELECT
        ! Set the next pair of coordinates
        trench(i)%row = trench(i-1)%row + plan(i)%distance*row
        trench(i)%col = trench(i-1)%col + plan(i)%distance*col
      END DO
  
    END SUBROUTINE dig_trench

    SUBROUTINE expand_input(plan)
      IMPLICIT NONE
      TYPE(dig_step), &
        INTENT(INOUT)           :: plan(:)
      INTEGER(KIND=int64)       :: i, hex_dist, dir
      DO i=1,SIZE(plan)
        READ(plan(i)%colour(2:7), "(Z5,I1)") hex_dist, dir
        SELECT CASE(dir)
          CASE(0)
            plan(i)%direction = dir_r
          CASE (1)
            plan(i)%direction = dir_d
          CASE (2)
            plan(i)%direction = dir_l
          CASE (3)
            plan(i)%direction = dir_u
          CASE DEFAULT
            CALL ABORT()
        END SELECT
        plan(i)%distance = hex_dist
      END DO
    END SUBROUTINE expand_input

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
