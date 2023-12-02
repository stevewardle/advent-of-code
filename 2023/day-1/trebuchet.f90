PROGRAM trebuchet
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  
  IMPLICIT NONE

  INTEGER(KIND=int32)     :: i, ios, input_file
  INTEGER(KIND=int32), &
    PARAMETER             :: linelen=500
  INTEGER(KIND=int32)     :: val, first, last
  INTEGER(KIND=int32)     :: total = 0
  CHARACTER(LEN=linelen)  :: line

  OPEN(UNIT=input_file, FILE="input.txt")

  DO 
    READ(UNIT=input_file, FMT=*, IOSTAT=ios) line
    IF (ios == IOSTAT_END) THEN
      EXIT
    END IF
    first = -1
    last = -1
    DO i=1,LEN_TRIM(line)
      READ(line(i:i), FMT="(I1)", IOSTAT=ios) val
      IF (ios /= 0) THEN
        CYCLE
      END IF
      IF (first == -1) THEN
        first = val
      END IF
      last = val
    END DO
    total = total + (first*10 + last)
  END DO

  CLOSE(UNIT=input_file)

  PRINT*, total

END PROGRAM trebuchet
