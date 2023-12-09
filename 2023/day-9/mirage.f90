PROGRAM mirage
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, real64, IOSTAT_END 
  
  IMPLICIT NONE

  INTEGER(KIND=int32)       :: input_file, i, j, last_idx, total
  INTEGER(KIND=int32), &
    PARAMETER               :: linelen=500, max_degrees=100
  INTEGER(KIND=int32), &
    ALLOCATABLE             :: history(:), work_array(:,:)

  OPEN(NEWUNIT=input_file, FILE="input.txt")

  total = 0
  DO
    CALL read_next_history(input_file, history)
    IF (.NOT. ALLOCATED(history)) THEN
      EXIT
    END IF

    ALLOCATE(work_array(max_degrees, SIZE(history)))

    work_array(1,:) = history
    DO i=2,max_degrees
      work_array(i,:) = EOSHIFT(work_array(i-1,:),1) - work_array(i-1,:)
      IF (ALL(work_array(i,1:SIZE(history) - i) == 0)) THEN
        EXIT
      END IF
    END DO
    
    DO j=i,1,-1
      last_idx = SIZE(history)+1-j
      IF (j == i) THEN
        work_array(j,last_idx+1) = 0
        CYCLE
      END IF
      work_array(j,last_idx+1) = work_array(j+1,last_idx) + work_array(j,last_idx) 
    END DO

    total = total + work_array(1,SIZE(history)+1)

    DEALLOCATE(work_array)
    DEALLOCATE(history)

  END DO

  WRITE(*, "(A,I0)") "Total of all next values: ", total

  CLOSE(UNIT=input_file)

  CONTAINS
    SUBROUTINE read_next_history(funit, history)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)           :: funit
      INTEGER(KIND=int32), &
        INTENT(INOUT),     &
        ALLOCATABLE          :: history(:)
      CHARACTER(LEN=linelen) :: line, chopline
      INTEGER(KIND=int32)    :: i, n_values, ios

      READ(UNIT=funit, FMT="(A)", IOSTAT=ios) line
      IF (ios == IOSTAT_END) THEN
        RETURN
      END IF
      n_values = 0
      chopline = line
      DO
        i = INDEX(chopline, " ")
        IF (TRIM(chopline) /= "") THEN
          n_values = n_values + 1
          chopline = chopline(i+1:linelen)
        ELSE
          EXIT
        END IF
      END DO
      ALLOCATE(history(n_values))
      READ(line, *) history

    END SUBROUTINE read_next_history

END PROGRAM mirage
