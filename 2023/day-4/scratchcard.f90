PROGRAM scratchcard
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  
  IMPLICIT NONE

  INTEGER(KIND=int32)       :: ios, input_file, i, j
  INTEGER(KIND=int32)       :: score, total = 0
  INTEGER(KIND=int32), &
    PARAMETER               :: n_winning_numbers = 10, &
                               n_numbers = 25, &
                               header = 9
  CHARACTER(LEN=50)         :: fmt_string
  CHARACTER(LEN=header)     :: id
  INTEGER(KIND=int32)       :: winning_numbers(n_winning_numbers)
  INTEGER(KIND=int32)       :: numbers(n_numbers)

  OPEN(NEWUNIT=input_file, FILE="input.txt")

  WRITE(fmt_string, "(AI0AI0AI0A)") "(A",header,"X",n_winning_numbers,"(I3)X",n_numbers,"(I3))"

  DO
    READ(UNIT=input_file, FMT=TRIM(fmt_string), IOSTAT=ios) id, winning_numbers, numbers

    score = 0
    DO i=1,n_winning_numbers
      DO j=1,n_numbers
        IF (winning_numbers(i) == numbers(j)) THEN
          IF (score == 0) THEN
            score = 1
          ELSE
            score = score*2
          END IF
          EXIT
        END IF
      END DO
    END DO

    total = total + score

    IF (ios == IOSTAT_END) THEN
      EXIT
    END IF
  END DO

  WRITE(*, "(AI0)") "Final total: ", total

  CLOSE(UNIT=input_file)

END PROGRAM scratchcard
