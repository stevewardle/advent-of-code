PROGRAM boat
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, real32
  
  IMPLICIT NONE

  INTEGER(KIND=int32)       :: input_file
  INTEGER(KIND=int32), &
    PARAMETER               :: linelen=500, n_races=4
  CHARACTER(LEN=linelen)    :: line
  INTEGER(KIND=int32)       :: time(n_races), distance(n_races)

  OPEN(NEWUNIT=input_file, FILE="input.txt")
  READ(UNIT=input_file, FMT="(10XA)") line
  READ(line, *) time
  READ(UNIT=input_file, FMT="(10XA)") line
  READ(line, *) distance
  CLOSE(UNIT=input_file)

  WRITE(*,"(AI0)") "Product of win counts: ", &
    PRODUCT(CEILING(0.5*(time + SQRT(REAL(time**2 - 4*distance, KIND=real32)))) - &
            CEILING(0.5*(time - SQRT(REAL(time**2 - 4*distance, KIND=real32)))))

END PROGRAM boat
