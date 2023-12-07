PROGRAM cube
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  
  IMPLICIT NONE

  INTEGER(KIND=int32)     :: ios, input_file
  INTEGER(KIND=int32), &
    PARAMETER             :: linelen=500
  CHARACTER(LEN=linelen)  :: line, turn, cuberesult, cubecount
  INTEGER(KIND=int32)     :: id, n_cubes, total=0
  CHARACTER(LEN=5)        :: dummy
  ! 3 for red, green, blue, 10 max buffer for turns in a game
  INTEGER(KIND=int32)     :: n_results, results(3, 10)
  INTEGER(KIND=int32)     :: minimal(3)

  OPEN(NEWUNIT=input_file, FILE="input.txt")

  DO 
    READ(UNIT=input_file, FMT="(A)", IOSTAT=ios) line
    IF (ios == IOSTAT_END) THEN
      EXIT
    END IF

    ! Extract the Game ID part of the line
    CALL chomp(line, ":", turn)

    ! Read the ID
    CALL chomp(turn, " ", dummy)
    READ(turn, "(I4)") id

    n_results = 0
    DO
      ! Walk through each turn (set of cubes)
      CALL chomp(line, ";", turn)
      IF (TRIM(turn) == "") THEN
        EXIT
      END IF
      n_results = n_results + 1
      results(:, n_results) = 0
      DO
        ! Within each turn walk through each colour
        CALL chomp(turn, ",", cuberesult)
        IF (TRIM(cuberesult) == "") THEN
          EXIT
        END IF

        ! Extract the colour and number of cubes
        ! Repeated chomp here; as the cuberesult starts
        ! with the delimiter
        CALL chomp(cuberesult, " ", cubecount)
        CALL chomp(cuberesult, " ", cubecount)
        READ(cubecount, "(I2)") n_cubes

        SELECT CASE (TRIM(cuberesult))
        CASE ("red")
          results(1, n_results) = n_cubes
        CASE ("green")
          results(2, n_results) = n_cubes
        CASE ("blue")
          results(3, n_results) = n_cubes
        END SELECT

      END DO
    END DO

    ! Calculating the minimal cubesets (RGB)
    minimal(1) = MAXVAL(results(1,1:n_results))
    minimal(2) = MAXVAL(results(2,1:n_results))
    minimal(3) = MAXVAL(results(3,1:n_results))

    ! Calculate the power
    total = total + minimal(1)*minimal(2)*minimal(3)

  END DO

  WRITE(*, "(A,I0)") "Total Cube Power Sum : ", total

  CLOSE(UNIT=input_file)

CONTAINS
  SUBROUTINE chomp(input, delimiter, fragment)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(INOUT) :: input
    CHARACTER(LEN=1), INTENT(IN) :: delimiter
    CHARACTER(LEN=LEN(input)), INTENT(OUT) :: fragment

    INTEGER(KIND=int32) :: next

    next = INDEX(input, delimiter)
    IF (next > 0) THEN
      READ(input(1:next-1), "(A)") fragment
      input = input(next+1:LEN(input))
    ELSE
      READ(input(1:LEN(input)), "(A)") fragment
      input = ""
    END IF

  END SUBROUTINE chomp

END PROGRAM cube
