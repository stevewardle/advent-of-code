PROGRAM seed
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, int64, IOSTAT_END, IOSTAT_EOR
  
  IMPLICIT NONE

  INTEGER(KIND=int32)       :: ios, input_file, i
  INTEGER(KIND=int32), &
    PARAMETER               :: linelen=500, mapnamelen=50
  INTEGER(KIND=int64), &
    ALLOCATABLE             :: values(:)
  CHARACTER(LEN=mapnamelen) :: mapname

  OPEN(NEWUNIT=input_file, FILE="input.txt")

  CALL read_seeds(input_file, values)
  WRITE(*, "(AI0A)") "Found ", SIZE(values), " seeds" 

  DO
    CALL read_to_next_map(input_file, mapname)
    IF (TRIM(mapname) == "") THEN
      EXIT
    END IF
    PRINT*, "Processing: "//TRIM(mapname)
    CALL filter_values(input_file, values)
  END DO

  WRITE(*,"(AI0)") "Closest Location: ", MINVAL(values)

  DEALLOCATE(values)
  CLOSE(UNIT=input_file)

  CONTAINS
    SUBROUTINE read_seeds(funit, seeds)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)           :: funit
      INTEGER(KIND=int64), &
        INTENT(INOUT),     &
        ALLOCATABLE          :: seeds(:)
      CHARACTER(LEN=linelen) :: line
      INTEGER(KIND=int32)    :: i, n_seeds, ios

      READ(UNIT=input_file, FMT="(7XA)", IOSTAT=ios) line
      n_seeds = 0
      DO
        i = INDEX(line, " ")
        IF (TRIM(line) /= "") THEN
          n_seeds = n_seeds + 1
          line = line(i+1:linelen)
        ELSE
          EXIT
        END IF
      END DO
      REWIND(UNIT=input_file)
      ALLOCATE(seeds(n_seeds))
      READ(UNIT=input_file, FMT="(7XA)", IOSTAT=ios) line
      READ(line, *) seeds

    END SUBROUTINE read_seeds

    SUBROUTINE read_to_next_map(funit, mapname)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)           :: funit
      CHARACTER(LEN=mapnamelen), &
        INTENT(OUT)          :: mapname
      CHARACTER(LEN=linelen) :: line

      mapname = ""
      DO
        READ(UNIT=funit, FMT="(A)", IOSTAT=ios) line
        IF (ios == IOSTAT_END .OR. ios > 0) THEN
          EXIT
        END IF
        IF (INDEX(line, "map:") /= 0) THEN
          READ(line(1:INDEX(line, " ")), *) mapname
          EXIT
        END IF
      END DO

    END SUBROUTINE read_to_next_map

    SUBROUTINE filter_values(funit, values)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)           :: funit
      INTEGER(KIND=int64), &
        INTENT(INOUT)        :: values(:)
      CHARACTER(LEN=linelen) :: line
      INTEGER(KIND=int32)    :: i, ios
      INTEGER(KIND=int64)    :: destination, source, span
      LOGICAL                :: in_range(SIZE(values)), seen(SIZE(values))

      seen(:) = .FALSE.

      DO
        READ(UNIT=funit, FMT="(A)", IOSTAT=ios) line
        IF (TRIM(line) == "" .OR. ios == IOSTAT_END) THEN
          EXIT
        END IF

        READ(line, *) destination, source, span
        in_range = (values - source >= 0) .AND. (values - source) <= span

        DO i=1,SIZE(values)
          IF ((.NOT. seen(i)) .AND. in_range(i)) THEN
            values(i) = values(i) - source + destination
            seen(i) = .TRUE.
          END IF
        END DO
      END DO

    END SUBROUTINE filter_values

END PROGRAM seed
