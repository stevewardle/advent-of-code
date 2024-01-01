PROGRAM day_24
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, int64, real64, IOSTAT_END
  IMPLICIT NONE

  TYPE hailstone
    INTEGER(KIND=int64) :: px, py, pz
    INTEGER(KIND=int64) :: vx, vy, vz
  END TYPE hailstone

  TYPE(hailstone), &
    ALLOCATABLE         :: hailstones(:)
  INTEGER(KIND=int64), &
    PARAMETER           :: test_area_min = 200000000000000_int64, &
                           test_area_max = 400000000000000_int64
  INTEGER(KIND=int32)   :: collisions

  CALL read_input("input.txt", hailstones) 

  CALL calc_collisions(hailstones, collisions)

  WRITE(*, "(A,I0)") "Total collisions: ", collisions

  CONTAINS

    SUBROUTINE calc_collisions(hailstones, collisions)
      IMPLICIT NONE
      TYPE(hailstone), &
        INTENT(IN)              :: hailstones(:)
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: collisions
      INTEGER(KIND=int32)       :: i, j
      LOGICAL                   :: intercept
      REAL(KIND=real64)         :: x, y

      collisions = 0
      DO i=1,SIZE(hailstones)
        DO j=i+1,SIZE(hailstones)
          CALL calc_intercept(hailstones(i), hailstones(j), intercept, x, y)
          IF (intercept) THEN
            IF ((x > test_area_min) &
                .AND. (x < test_area_max) &
                .AND. (y > test_area_min) &
                .AND. (y < test_area_max)) THEN
              collisions = collisions + 1 
            END IF
          END IF
        END DO
      END DO

    END SUBROUTINE calc_collisions

    SUBROUTINE calc_intercept(hailstone_1, hailstone_2, intercept, x, y)
      IMPLICIT NONE
      TYPE(hailstone), &
        INTENT(IN)              :: hailstone_1, hailstone_2
      LOGICAL, &
        INTENT(OUT)             :: intercept
      REAL(KIND=real64), &
        INTENT(OUT)             :: x, y
      INTEGER(KIND=int64)       :: cross, diff_x, diff_y
      REAL(KIND=real64)         :: u, v

      x = 0.0_real64
      y = 0.0_real64
      cross = hailstone_2%vx * hailstone_1%vy - hailstone_2%vy*hailstone_1%vx
      IF (cross == 0_int64) THEN
        ! Cross product of velocities is zero - meaning the paths
        ! are parallel and will *never* intersect
        intercept = .FALSE.
        RETURN
      END IF
      diff_x = hailstone_2%px - hailstone_1%px
      diff_y = hailstone_2%py - hailstone_1%py

      u = REAL(diff_y*hailstone_2%vx - diff_x*hailstone_2%vy, KIND=real64) / cross
      v = REAL(diff_y*hailstone_1%vx - diff_x*hailstone_1%vy, KIND=real64) / cross
      IF ((u < 0.0_real64) .OR. (v < 0.0_real64)) THEN
        ! They will intersect but only in the past, which is not valid
        intercept = .FALSE.
        RETURN
      END IF
      x = hailstone_1%px + u*hailstone_1%vx
      y = hailstone_1%py + u*hailstone_1%vy
      intercept = .TRUE.

    END SUBROUTINE calc_intercept

    SUBROUTINE read_input(filename, hailstones)
      IMPLICIT NONE
      CHARACTER(LEN=*), &
        INTENT(IN)              :: filename
      TYPE(hailstone), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: hailstones(:)
      INTEGER(KIND=int32)       :: input_file

      OPEN(NEWUNIT=input_file, FILE=filename, ACCESS="stream", &
        FORM="formatted")
      CALL read_hailstones(input_file, hailstones)  

      CLOSE(UNIT=input_file)

    END SUBROUTINE read_input

    SUBROUTINE read_hailstones(funit, hailstones)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      TYPE(hailstone), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: hailstones(:)
      INTEGER(KIND=int32)       :: ios, i, n_hailstones, separator
      INTEGER(KIND=int32), &
        PARAMETER               :: linelen=100
      CHARACTER(LEN=linelen)    :: line
      
      n_hailstones = 0
      DO
        READ(UNIT=funit, FMT="()", IOSTAT=ios)
        IF (ios == IOSTAT_END) THEN
          EXIT
        END IF
        n_hailstones = n_hailstones + 1
      END DO
      REWIND(funit)
      IF (ALLOCATED(hailstones)) DEALLOCATE(hailstones)
      ALLOCATE(hailstones(n_hailstones))

      DO i=1,n_hailstones
        READ(UNIT=funit, FMT="(A)", IOSTAT=ios) line
        separator = SCAN(line, "@")
        READ(line(1:separator-1), *) &
          hailstones(i)%px, hailstones(i)%py, hailstones(i)%pz
        READ(line(separator+1:linelen), *) &
          hailstones(i)%vx, hailstones(i)%vy, hailstones(i)%vz
      END DO

    END SUBROUTINE read_hailstones

END PROGRAM day_24
