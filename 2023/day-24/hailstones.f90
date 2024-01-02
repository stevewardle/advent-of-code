PROGRAM day_24
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, int64, real64, real128, IOSTAT_END
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
  REAL(KIND=real128)     :: coeffs(6,7), solution(7)

  CALL read_input("input.txt", hailstones) 

  CALL calc_collisions(hailstones, collisions)

  WRITE(*, "(A,I0)") "Total collisions: ", collisions

  CALL create_coeffs(hailstones, coeffs)

  CALL gaussian_elimination(coeffs, solution)

  WRITE(*, "(A,I0)") "Sum of rock's coords: ", &
    INT(solution(1)+solution(2)+solution(3), KIND=int64)

  CONTAINS

    ! Gaussian Elimination formula I ripped from the internet
    ! because I couldn't remember it
    SUBROUTINE gaussian_elimination(A, X)
      IMPLICIT NONE
      REAL(KIND=real128), &
        INTENT(INOUT)           :: A(:,:)
      REAL(KIND=real128), &
        INTENT(OUT)             :: X(SIZE(A,1))
      INTEGER(KIND=int32)       :: i, j, n
      REAL(KIND=real128)        :: s

      n = SIZE(A,1)
      DO j=1,n
        CALL pivot(A, j)
        DO i=j+1,n
          A(i,:) = A(i,:) - A(j,:)*A(i,j)/A(j,j)
        END DO
      END DO

      DO i=n,1,-1
        s = A(i, n+1)
        DO j=i+1,n
          s = s - A(i,j)*X(j)
        END DO
        X(i) = s/A(i,i)
      END DO

    END SUBROUTINE gaussian_elimination

    SUBROUTINE pivot(A, offset)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: offset
      REAL(KIND=real128), &
        INTENT(INOUT)           :: A(:,:)
      INTEGER(KIND=int32)       :: i, j
      REAL(KIND=real128)        :: T(SIZE(A,2))

      j = offset
      DO i=offset,SIZE(A,1)
        IF (ABS(A(i,offset)) > ABS(A(j,offset))) THEN
          j = i
        END IF
      END DO
      T = A(offset,:)
      A(offset,:) = A(j,:)
      A(j,:) = T

    END SUBROUTINE

    SUBROUTINE create_coeffs(hailstones, coeffs)
      IMPLICIT NONE
      TYPE(hailstone), &
        INTENT(IN)              :: hailstones(:)
      REAL(KIND=real128), &
        INTENT(OUT)             :: coeffs(6,7)
      ! Rock position - P(X,Y,Z) and velocity - V(X,Y,Z)
      ! Hailstone positions - p1(x,y,z)...pn(x,y,z) 
      !   and velocities v1(x,y,z)...vn(x,y,z)

      ! Collision:
      !  PX + t*VX = px + t*vx
      !  PY + t*VY = py + t*vy
      !  PZ + t*VZ = pz + t*vz

      ! Rearrange:
      !  t = (PX - px)/(vx - VX)  (1)
      !  t = (PY - py)/(vy - VY)  (2)
      !  t = (PZ - pz)/(vz - VZ)  (3)
      
      ! Set (1) == (2) and rearrange
      !   PY*VX - PX*VY = PY*vx - py*vx + py*VX - PX*vy + px*vy - px*VY
      ! Set (1) == (3) and rearrange
      !   PZ*VX - PX*VZ = PZ*vx - pz*vx + pz*VX - PX*vz + px*vz - px*VZ
      ! Set (2) == (3) and rearrange
      !   PZ*VY - PY*VZ = PZ*vy - pz*vy + pz*VY - PY*vz + py*vz - py*VZ

      ! The RHS of the above expressions must be equal for any pair
      ! of hailstones (as the LHS do not depend on hailstones) so by
      ! selecting 3 different pairs of hailstones we have
      !   (v2y-v1y)PX + (v1x-v2x)PY + (p1y-p2y)VX + (p2x-p1x)VY 
      !           = p2x*v2y - p2y*v2x - p1x*v1y + p1y*v1x
      !   (v4z-v3z)PX + (v3x-v4x)PZ + (p3z-p4z)VX + (p4x-p3x)VZ
      !           = p4x*v4z - p4z*v4x - p3x*v3z + p3z*v3x
      !   (v5z-v6z)PY + (v6y-v5y)PZ + (p6z-p5z)VY + (p5y-p6y)VZ
      !           = -p6y*v6z + p6z*v6y + p5y*v5z - p5z*v5y

      ! Once we substitute in the p + v values this creates a
      ! set of linear equations e.g.
      !  A*PX + B*PY + 0*PZ + C*VX + D*VY + 0*VZ  = E
      !  F*PX + 0*PY + G*PZ + H*VX + 0*VY + I*VZ  = J
      !  0*PX + K*PY + L*PZ + 0*VX + M*VY + N*VZ  = O
      ! And we can use Gaussian elimination to solve for P + V

      ! Equation 1 with hailstones 1 and 2
      coeffs(1,1) = hailstones(2)%vy - hailstones(1)%vy
      coeffs(1,2) = hailstones(1)%vx - hailstones(2)%vx
      coeffs(1,3) = 0
      coeffs(1,4) = hailstones(1)%py - hailstones(2)%py
      coeffs(1,5) = hailstones(2)%px - hailstones(1)%px
      coeffs(1,6) = 0
      coeffs(1,7) = hailstones(2)%px*hailstones(2)%vy &
                  - hailstones(2)%py*hailstones(2)%vx &
                  - hailstones(1)%px*hailstones(1)%vy &
                  + hailstones(1)%py*hailstones(1)%vx

      ! Equation 1 with hailstones 1 and 3
      coeffs(2,1) = hailstones(3)%vy - hailstones(1)%vy
      coeffs(2,2) = hailstones(1)%vx - hailstones(3)%vx
      coeffs(2,3) = 0
      coeffs(2,4) = hailstones(1)%py - hailstones(3)%py
      coeffs(2,5) = hailstones(3)%px - hailstones(1)%px
      coeffs(2,6) = 0
      coeffs(2,7) = hailstones(3)%px*hailstones(3)%vy &
                  - hailstones(3)%py*hailstones(3)%vx &
                  - hailstones(1)%px*hailstones(1)%vy &
                  + hailstones(1)%py*hailstones(1)%vx

      ! Equation 2 with hailstones 1 and 2
      coeffs(3,1) = hailstones(2)%vz - hailstones(1)%vz 
      coeffs(3,2) = 0
      coeffs(3,3) = hailstones(1)%vx - hailstones(2)%vx
      coeffs(3,4) = hailstones(1)%pz - hailstones(2)%pz
      coeffs(3,5) = 0
      coeffs(3,6) = hailstones(2)%px - hailstones(1)%px
      coeffs(3,7) = hailstones(2)%px*hailstones(2)%vz &
                  - hailstones(2)%pz*hailstones(2)%vx &
                  - hailstones(1)%px*hailstones(1)%vz &
                  + hailstones(1)%pz*hailstones(1)%vx
      
      ! Equation 2 with hailstones 1 and 3
      coeffs(4,1) = hailstones(3)%vz - hailstones(1)%vz 
      coeffs(4,2) = 0
      coeffs(4,3) = hailstones(1)%vx - hailstones(3)%vx
      coeffs(4,4) = hailstones(1)%pz - hailstones(3)%pz
      coeffs(4,5) = 0
      coeffs(4,6) = hailstones(3)%px - hailstones(1)%px
      coeffs(4,7) = hailstones(3)%px*hailstones(3)%vz &
                  - hailstones(3)%pz*hailstones(3)%vx &
                  - hailstones(1)%px*hailstones(1)%vz &
                  + hailstones(1)%pz*hailstones(1)%vx

      ! Equation 3 with hailstones 1 and 2
      coeffs(5,1) = 0 
      coeffs(5,2) = hailstones(1)%vz - hailstones(2)%vz
      coeffs(5,3) = hailstones(2)%vy - hailstones(1)%vy
      coeffs(5,4) = 0
      coeffs(5,5) = hailstones(2)%pz - hailstones(1)%pz
      coeffs(5,6) = hailstones(1)%py - hailstones(2)%py
      coeffs(5,7) = -hailstones(2)%py*hailstones(2)%vz &
                  + hailstones(2)%pz*hailstones(2)%vy &
                  + hailstones(1)%py*hailstones(1)%vz &
                  - hailstones(1)%pz*hailstones(1)%vy

      ! Equation 3 with hailstones 1 and 3
      coeffs(6,1) = 0 
      coeffs(6,2) = hailstones(1)%vz - hailstones(3)%vz
      coeffs(6,3) = hailstones(3)%vy - hailstones(1)%vy
      coeffs(6,4) = 0
      coeffs(6,5) = hailstones(3)%pz - hailstones(1)%pz
      coeffs(6,6) = hailstones(1)%py - hailstones(3)%py
      coeffs(6,7) = -hailstones(3)%py*hailstones(3)%vz &
                  + hailstones(3)%pz*hailstones(3)%vy &
                  + hailstones(1)%py*hailstones(1)%vz &
                  - hailstones(1)%pz*hailstones(1)%vy
      
    END SUBROUTINE create_coeffs 

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
