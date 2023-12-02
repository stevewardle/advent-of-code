PROGRAM trebuchet
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  
  IMPLICIT NONE

  INTEGER(KIND=int32)     :: i, ios, input_file
  INTEGER(KIND=int32), &
    PARAMETER             :: linelen=500
  INTEGER(KIND=int32)     :: idx, first_idx, last_idx
  INTEGER(KIND=int32)     :: first, last, total=0
  CHARACTER(LEN=linelen)  :: line

  CHARACTER(LEN=5), DIMENSION(19), &
    PARAMETER             :: match = [ &
      "one  ", &
      "two  ", &
      "three", &
      "four ", &
      "five ", &
      "six  ", &
      "seven", &
      "eight", &
      "nine ", &
      "%    ", &
      "1    ", &
      "2    ", &
      "3    ", &
      "4    ", &
      "5    ", &
      "6    ", &
      "7    ", &
      "8    ", &
      "9    "  & 
  ]

  OPEN(UNIT=input_file, FILE="input.txt")

  DO 
    READ(UNIT=input_file, FMT=*, IOSTAT=ios) line
    IF (ios == IOSTAT_END) THEN
      EXIT
    END IF
    first_idx = linelen 
    last_idx = 0_int32
    DO i=1,SIZE(match)
      ! Forwards search
      idx = INDEX(TRIM(line), TRIM(match(i)), KIND=int32)
      IF (idx /= 0_int32 .AND. idx < first_idx) THEN
        first_idx = idx
        first = MOD(i, 10_int32)
      END IF

      ! Backwards search
      idx = INDEX(TRIM(line), TRIM(match(i)), KIND=int32, BACK=.TRUE.)
      IF (idx /= 0_int32 .AND. idx > last_idx) THEN
        last_idx = idx
        last = MOD(i, 10_int32)
      END IF
    END DO

    total = total + (first*10_int32 + last)
  END DO

  CLOSE(UNIT=input_file)

  PRINT*, total

END PROGRAM trebuchet
