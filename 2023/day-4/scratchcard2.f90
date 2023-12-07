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
  INTEGER(KIND=int32)       :: process(n_winning_numbers+1)

  OPEN(NEWUNIT=input_file, FILE="input.txt")

  WRITE(fmt_string, "(A,I0,A,I0,A,I0,A)") &
    "(A",header,"X",n_winning_numbers,"(I3)X",n_numbers,"(I3))"

  process(:) = 1

  DO
    READ(UNIT=input_file, &
         FMT=TRIM(fmt_string), &
         IOSTAT=ios) id, winning_numbers, numbers

    IF (ios == IOSTAT_END) THEN
      EXIT
    END IF

    score = 0
    DO i=1,n_winning_numbers
      DO j=1,n_numbers
        IF (winning_numbers(i) == numbers(j)) THEN
          score = score + 1
          EXIT
        END IF
      END DO
    END DO

    DO i=1,process(1)
      total = total + 1
      DO j=1,score
        process(j+1) = process(j+1) + 1  
      END DO
    END DO

    process = eoshift(process,1)
    process(n_winning_numbers+1) = 1

  END DO

  WRITE(*, "(A,I0)") "Total number of cards: ", total

  CLOSE(UNIT=input_file)

END PROGRAM scratchcard
