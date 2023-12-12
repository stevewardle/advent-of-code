PROGRAM springs
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  IMPLICIT NONE
  INTEGER(KIND=int32)       :: input_file, n_valid, total
  INTEGER(KIND=int32), &
    PARAMETER               :: line_len=500
  INTEGER(KIND=int32), &
    ALLOCATABLE             :: damaged(:)
  CHARACTER(LEN=1), &
    ALLOCATABLE             :: record(:)

  OPEN(NEWUNIT=input_file, FILE="input.txt")

  total = 0
  DO
    CALL read_next_input(input_file, record, damaged)
    IF (.NOT. ALLOCATED (record)) THEN
      EXIT
    END IF
    CALL calc_arrangements(record, damaged, n_valid)
    total = total + n_valid
  END DO

  WRITE(*, "(A,I0)") "Total Spring Arrangements: ", total

  CLOSE(UNIT=input_file)

  CONTAINS

    RECURSIVE SUBROUTINE calc_arrangements(record, damaged, n_valid)
      IMPLICIT NONE
      CHARACTER(LEN=1), &
        INTENT(IN)              :: record(:)
      INTEGER(KIND=int32), &
        INTENT(IN)              :: damaged(:)
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: n_valid
      INTEGER(KIND=int32)       :: i, sub_valid
      CHARACTER(LEN=1)          :: record_new(SIZE(record))

      n_valid = 0
      i = 0
      ! Recursion always gets a little wild, so bear with me...
      DO
        ! Find next place which could be the first of the
        ! damaged blocks in the current list (this ends up
        ! iterating when we call this routine again to
        ! mode down the call-stack)
        i = next_valid_match(record, damaged(1), i+1)
        ! If no more places are valid we are finished
        ! and should not try anymore so break out of
        ! the loop and routine and head back up the 
        ! call-stack
        IF (i == 0) THEN
          EXIT
        END IF
        ! If we *have* found a valid place, construct a
        ! modified copy of the record which has everything
        ! prior to the valid position we found above empty
        record_new(1:i+damaged(1)) = "."
        record_new(i+damaged(1)+1:SIZE(record)) = &
          record(i+damaged(1)+1:SIZE(record))
        ! Now if we are not at the lowest level of the
        ! call-stack (i.e. the array of damaged points
        ! has further values) call ourselves to calculate
        ! the next group of damaged values; using the
        ! masked version of the original record 
        IF (SIZE(damaged) > 1) THEN
          CALL calc_arrangements(&
            record_new, damaged(2:SIZE(damaged)), sub_valid)
          ! Accumulate valid points from further down
          ! the call stack - this is the sum of all valid
          ! routes found at the lowest level cascaded back
          ! up the stack via each damage group processed
          n_valid = n_valid + sub_valid
        ELSE
          ! If we *are* at the bottom of the call-stack
          ! Add a counter to register the valid path we
          ! just arrived at, after a final check that
          ! this is truly valid (which it isn't if
          ! there are leftover original damaged values
          ! beyond what we have found)
          IF (ALL(record_new /= "#")) THEN
            n_valid = n_valid + 1
          END IF
        END IF
      END DO

    END SUBROUTINE calc_arrangements

    FUNCTION next_valid_match(record, damaged, start) RESULT(found)
      IMPLICIT NONE
      CHARACTER(LEN=1)          :: record(:)
      INTEGER(KIND=int32)       :: damaged, start, found
      INTEGER(KIND=int32)       :: i, limit
      found = 0

      limit = FINDLOC(record, "#", DIM=1)
      IF (limit == 0) THEN
        limit = SIZE(record)
      END IF

      DO i=start,limit
        ! Found a potential damaged location
        IF (record(i) /= ".") THEN
          ! See if this could be a valid damaged block
          IF ( &    ! Forms a big enough block 
                    (ALL(record(i:i+damaged-1) /= ".")) &
                    ! That block is not next to an
                    ! existing damaged spring
              .AND. (record(i-1) /= "#") &
              .AND. (record(i+damaged) /= "#")) THEN
            found = i
            EXIT 
          END IF
        END IF
      END DO

    END FUNCTION next_valid_match

    SUBROUTINE read_next_input(funit, record, damaged) 
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      CHARACTER(LEN=1), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: record(:)
      INTEGER(KIND=int32), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: damaged(:)
      CHARACTER(LEN=line_len)   :: line
      INTEGER(KIND=int32)       :: i, j, ios, n_records, n_damaged

      IF (ALLOCATED(record)) DEALLOCATE(record)
      IF (ALLOCATED(damaged)) DEALLOCATE(damaged)
      READ(UNIT=funit, FMT="(A)", IOSTAT=ios) line
      IF (ios == IOSTAT_END) THEN
        RETURN
      END IF

      n_records = INDEX(line, " ") - 1
      ! Pad our record array with blanks to make comparisons easier
      ! later i.e. without going out of bounds
      ALLOCATE(record(n_records + 2))
      DO i=1,n_records
        record(i+1) = line(i:i)
      END DO
      record(1) = "."
      record(n_records + 2) = "."
     
      i = 0
      n_damaged = 1
      DO
        j = INDEX(line(n_records+2+i:line_len), ",")
        IF (j == 0) THEN
          EXIT
        END IF
        i = i + j
        n_damaged = n_damaged + 1
      END DO
      ALLOCATE(damaged(n_damaged))
      READ(line(n_records+2:line_len), *) damaged

    END SUBROUTINE read_next_input

END PROGRAM springs
