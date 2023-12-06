PROGRAM seed
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, int64, IOSTAT_END, IOSTAT_EOR
  
  IMPLICIT NONE

  INTEGER(KIND=int32)       :: ios, input_file
  INTEGER(KIND=int32), &
    PARAMETER               :: linelen=500, mapnamelen=50, n_maps=7, n_values=100000
  INTEGER(KIND=int64), &
    ALLOCATABLE             :: range_starts(:), range_lengths(:)
  INTEGER(KIND=int64)       :: values(n_values)
  LOGICAL                   :: in_range(n_values)
  INTEGER(KIND=int64)       :: i, j, offset, location
  CHARACTER(LEN=mapnamelen), &
    PARAMETER               :: mapnames(n_maps) = &
    ["humidity-to-location   ", &
     "temperature-to-humidity", &
     "light-to-temperature   ", &
     "water-to-light         ", &
     "fertilizer-to-water    ", &
     "soil-to-fertilizer     ", &
     "seed-to-soil           "]

  OPEN(NEWUNIT=input_file, FILE="input.txt")

  CALL read_seed_ranges(input_file, range_starts, range_lengths)

  offset = 0
  location = -1
  outer: DO
    DO i=0,n_values-1
      values(i+1) = offset*n_values + i
    END DO

    DO i=1,n_maps
      CALL read_to_map(input_file, mapnames(i))
      CALL filter_values(input_file, values)
    END DO
 
    DO i=1,SIZE(range_starts)
      in_range = (values - range_starts(i) >= 0) & 
        .AND. (values - range_starts(i)) < range_lengths(i)
      DO j=1,n_values
        IF (in_range(j)) THEN
          location = j
          EXIT outer
        END IF
      END DO
    END DO

    offset = offset + 1
  END DO outer

  WRITE(*, "(AI0)") "Minimum location: ",offset*n_values + j - 1

  DEALLOCATE(range_lengths)
  DEALLOCATE(range_starts)
  CLOSE(UNIT=input_file)

  CONTAINS
    SUBROUTINE read_seed_ranges(funit, range_starts, range_lengths)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)           :: funit
      INTEGER(KIND=int64), &
        INTENT(INOUT),     &
        ALLOCATABLE          :: range_starts(:), range_lengths(:)
      INTEGER(KIND=int64), &
        ALLOCATABLE          :: entries(:)
      CHARACTER(LEN=linelen) :: line
      INTEGER(KIND=int32)    :: i, n_entries, ios

      READ(UNIT=input_file, FMT="(7XA)", IOSTAT=ios) line
      n_entries = 0
      DO
        i = INDEX(line, " ")
        IF (TRIM(line) /= "") THEN
          n_entries = n_entries + 1
          line = line(i+1:linelen)
        ELSE
          EXIT
        END IF
      END DO

      REWIND(UNIT=input_file)
      ALLOCATE(entries(n_entries))
      READ(UNIT=input_file, FMT="(7XA)", IOSTAT=ios) line
      READ(line, *) entries

      ALLOCATE(range_starts(SIZE(entries)/2))
      ALLOCATE(range_lengths(SIZE(entries)/2))
      DO i=1, SIZE(entries)/2
        range_starts(i) = entries(i*2-1)
        range_lengths(i) = entries(i*2)
      END DO
      DEALLOCATE(entries)

    END SUBROUTINE read_seed_ranges

    SUBROUTINE read_to_map(funit, mapname)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)           :: funit
      CHARACTER(LEN=mapnamelen), &
        INTENT(IN)           :: mapname
      CHARACTER(LEN=linelen) :: line

      REWIND(UNIT=input_file)
      DO
        READ(UNIT=funit, FMT="(A)", IOSTAT=ios) line
        IF (ios == IOSTAT_END .OR. ios > 0) THEN
          WRITE(*, "(A)") "Map "//TRIM(mapname)//" not found"
          STOP
        END IF
        IF (INDEX(line, TRIM(mapname)) /= 0) THEN
          EXIT
        END IF
      END DO

    END SUBROUTINE read_to_map

    SUBROUTINE filter_values(funit, values)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)           :: funit
      INTEGER(KIND=int64), &
        INTENT(INOUT)        :: values(:)
      CHARACTER(LEN=linelen) :: line
      INTEGER(KIND=int32)    :: ios
      INTEGER(KIND=int64)    :: i, destination, source, span
      LOGICAL                :: in_range(SIZE(values)), seen(SIZE(values))

      seen(:) = .FALSE.

      DO
        READ(UNIT=funit, FMT="(A)", IOSTAT=ios) line
        IF (TRIM(line) == "" .OR. ios == IOSTAT_END) THEN
          EXIT
        END IF

        READ(line, *) destination, source, span
        in_range = (values - destination >= 0) .AND. (values - destination) < span

        DO i=1,SIZE(values)
          IF ((.NOT. seen(i)) .AND. in_range(i)) THEN
            values(i) = values(i) - destination + source
            seen(i) = .TRUE.
          END IF
        END DO
      END DO

    END SUBROUTINE filter_values

END PROGRAM seed
