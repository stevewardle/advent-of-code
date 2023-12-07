PROGRAM boat
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, int64, real64
  
  IMPLICIT NONE

  INTEGER(KIND=int32)       :: input_file
  INTEGER(KIND=int32), &
    PARAMETER               :: linelen=500, n_races=4
  CHARACTER(LEN=linelen)    :: line
  INTEGER(KIND=int64)       :: times(n_races), distances(n_races), time, distance

  OPEN(NEWUNIT=input_file, FILE="input.txt")
  READ(UNIT=input_file, FMT="(10XA)") line
  READ(line, *) times
  READ(UNIT=input_file, FMT="(10XA)") line
  READ(line, *) distances
  CLOSE(UNIT=input_file)

  WRITE(line,"(4(I0))") times
  READ(line, *) time
  WRITE(line, "(4(I0))") distances
  READ(line, *) distance

  WRITE(*,"(AI0)") "Win count: ", &
    CEILING(0.5*(time + SQRT(REAL(time**2 - 4*distance, KIND=real64)))) - &
    CEILING(0.5*(time - SQRT(REAL(time**2 - 4*distance, KIND=real64))))

END PROGRAM boat
