PROGRAM day22
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  IMPLICIT NONE

  TYPE slab
    INTEGER(KIND=int32) :: x1, y1, z1
    INTEGER(KIND=int32) :: x2, y2, z2
  END TYPE slab

  TYPE(slab), &
    ALLOCATABLE         :: slabs(:)
  LOGICAL, &
    ALLOCATABLE         :: mask(:), footprint(:,:)
  INTEGER(KIND=int32), &
    ALLOCATABLE         :: order(:), max_z(:,:), filled(:,:), &
                           supports(:,:), supported(:,:)
  LOGICAL               :: safe 
  INTEGER(KIND=int32)   :: i, j, k, l, grid_x, grid_y, new_z, &
                           max_support_map, total


  CALL read_input("input.txt", slabs)

  ! Find extent of "grid"
  grid_x = MAX(MAXVAL(slabs(:)%x1), MAXVAL(slabs(:)%x2))
  grid_y = MAX(MAXVAL(slabs(:)%y1), MAXVAL(slabs(:)%y2))
  max_support_map = MAX(grid_x, grid_y) + 1
  
  ! Iterate in lowest Z order
  ALLOCATE(mask(SIZE(slabs)))
  ALLOCATE(order(SIZE(slabs)))
  mask = .TRUE.
  DO i=1,SIZE(slabs)
    order(i) = &
      MINLOC(slabs(:)%z1 + slabs(:)%z2, DIM=1, MASK=mask)
    mask(order(i)) = .FALSE. 
  END DO

  ! Figure out where each piece will fit
  ALLOCATE(max_z(0:grid_x, 0:grid_y))
  ALLOCATE(footprint(0:grid_x, 0:grid_y))
  ALLOCATE(filled(0:grid_x, 0:grid_y))
  ALLOCATE(supports(SIZE(slabs),max_support_map))
  ALLOCATE(supported(SIZE(slabs),max_support_map))
  max_z(:,:) = 0
  filled(:,:) = 0
  supports(:,:) = 0
  supported(:,:) = 0
  DO i=1,SIZE(slabs)
    footprint(:,:) = .FALSE.
    footprint(slabs(order(i))%x1:slabs(order(i))%x2, &
              slabs(order(i))%y1:slabs(order(i))%y2) = .TRUE.

    ! Find the Z value this piece will have - it's
    ! one more than the highest Z value already filled
    ! within the footprint of this slab
    new_z = MAXVAL(max_z, MASK=footprint) + 1

    ! Update the arrays describing which slab supports which
    ! other slabs (and vice versa)
    DO j=0,grid_x
      DO k=0,grid_y
        IF (footprint(j,k) &
            .AND. (max_z(j,k) == new_z-1) &
            .AND. (filled(j,k) > 0)) THEN
          DO l=1,max_support_map
            IF (supported(order(i),l) == filled(j,k)) EXIT
            IF (supported(order(i),l) == 0) THEN
              supported(order(i),l) = filled(j,k)
              EXIT
            END IF
          END DO
          DO l=1,max_support_map
            IF (supports(filled(j,k),l) == order(i)) EXIT
            IF (supports(filled(j,k),l) == 0) THEN
              supports(filled(j,k),l) = order(i)
              EXIT
            END IF
          END DO
        END IF
      END DO
    END DO

    ! Update the Z array by the Z size of the
    ! given piece within its footprint, and the
    ! filled array to indicate the id of each slab
    WHERE(footprint)
      max_z = &
        new_z &
        + ABS(slabs(order(i))%z1 - slabs(order(i))%z2)
      filled = order(i)
    END WHERE

  END DO

  ! Can now check for disintegrations
  total = 0
  DO i=1,SIZE(slabs)
    safe = .TRUE.
    DO j=1,max_support_map
      IF (supports(i,j) == 0) EXIT
      IF (COUNT(supported(supports(i,j),:) > 0) == 1) THEN
        safe = .FALSE.
        EXIT
      END IF
    END DO
    IF (safe) THEN
      total = total + 1
    END IF
  END DO

  WRITE(*, "(A,I0)") "Slabs that can be disintegrated: ", total

  DEALLOCATE(supported)
  DEALLOCATE(supports)
  DEALLOCATE(filled)
  DEALLOCATE(footprint)
  DEALLOCATE(max_z)
  DEALLOCATE(order)
  DEALLOCATE(mask)
  DEALLOCATE(slabs)

  CONTAINS

    SUBROUTINE read_input(filename, slabs)
      IMPLICIT NONE
      CHARACTER(LEN=*), &
        INTENT(IN)              :: filename
      TYPE(slab), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: slabs(:)
      INTEGER(KIND=int32)       :: input_file

      OPEN(NEWUNIT=input_file, FILE=filename, ACCESS="stream", &
        FORM="formatted")
      CALL read_slabs(input_file, slabs)

      CLOSE(UNIT=input_file)

    END SUBROUTINE read_input

    SUBROUTINE read_slabs(funit, slabs)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      TYPE(slab), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: slabs(:)
      INTEGER(KIND=int32)       :: ios
      INTEGER(KIND=int32), &
        PARAMETER               :: linelen=100
      CHARACTER(LEN=linelen)    :: line
      INTEGER(KIND=int32)       :: i, n_slabs
      n_slabs = 0
      DO
        READ(UNIT=funit, FMT="()", IOSTAT=ios)
        IF (ios == IOSTAT_END) THEN
          EXIT
        END IF
        n_slabs = n_slabs + 1
      END DO
      REWIND(funit)
      IF (ALLOCATED(slabs)) DEALLOCATE(slabs)
      ALLOCATE(slabs(n_slabs))

      DO i=1,n_slabs
        READ(UNIT=funit, FMT="(A)", IOSTAT=ios) line
        READ(line(1:SCAN(line,"~")-1), *) &
          slabs(i)%x1, slabs(i)%y1, slabs(i)%z1
        READ(line(SCAN(line,"~")+1:linelen), *) &
          slabs(i)%x2, slabs(i)%y2, slabs(i)%z2
      END DO

    END SUBROUTINE read_slabs

END PROGRAM day22
