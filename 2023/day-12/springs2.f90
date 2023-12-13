PROGRAM springs
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, int64, IOSTAT_END
  IMPLICIT NONE
  INTEGER(KIND=int32)       :: input_file, n_cache
  INTEGER(KIND=int64)       :: n_valid, total
  INTEGER(KIND=int32), &
    PARAMETER               :: line_len=500, cache_size=1000
  INTEGER(KIND=int32), &
    ALLOCATABLE             :: damaged(:), damaged_cache(:,:)
  INTEGER(KIND=int64), &
    ALLOCATABLE             :: valid_cache(:)
  CHARACTER(LEN=1), &
    ALLOCATABLE             :: record(:), record_cache(:,:)

  OPEN(NEWUNIT=input_file, FILE="input.txt")

  total = 0
  DO
    CALL read_next_input(input_file, record, damaged)
    CALL create_cache(record, damaged)
    IF (.NOT. ALLOCATED (record)) THEN
      EXIT
    END IF

    CALL calc_arrangements(record, damaged, n_valid)
    total = total + n_valid

  END DO

  WRITE(*, "(A,I0)") "Total Spring Arrangements: ", total

  CLOSE(UNIT=input_file)

  CONTAINS

    SUBROUTINE create_cache(record, damaged)
      IMPLICIT NONE
      CHARACTER(LEN=1), &
        ALLOCATABLE, &
        INTENT(IN)              :: record(:)
      INTEGER(KIND=int32), &
        ALLOCATABLE, &
        INTENT(IN)              :: damaged(:)

      IF (ALLOCATED(record_cache)) DEALLOCATE(record_cache)
      IF (ALLOCATED(damaged_cache)) DEALLOCATE(damaged_cache)
      IF (ALLOCATED(valid_cache)) DEALLOCATE(valid_cache)

      IF (.NOT. ALLOCATED(record)) RETURN
      n_cache = 0
      ALLOCATE(record_cache(SIZE(record), cache_size))
      ALLOCATE(damaged_cache(SIZE(damaged), cache_size))
      ALLOCATE(valid_cache(cache_size))

    END SUBROUTINE create_cache

    SUBROUTINE read_cache(record, damaged, n_valid)
      IMPLICIT NONE
       CHARACTER(LEN=1), &
        INTENT(IN)              :: record(:)
      INTEGER(KIND=int32), &
        INTENT(IN)              :: damaged(:)     
      INTEGER(KIND=int64), &
        INTENT(OUT)             :: n_valid
      INTEGER(KIND=int32)       :: i
        n_valid = -1
        IF (n_cache > 0) THEN
          DO i=1,n_cache
            IF (ALL(damaged_cache(:,i) == damaged) &
              .AND. ALL(record_cache(:,i) == record)) THEN
              n_valid = valid_cache(i)
              RETURN
            END IF
          END DO
        END IF

    END SUBROUTINE read_cache

    SUBROUTINE write_cache(record, damaged, n_valid)
      IMPLICIT NONE
       CHARACTER(LEN=1), &
        INTENT(IN)              :: record(:)
      INTEGER(KIND=int32), &
        INTENT(IN)              :: damaged(:)     
      INTEGER(KIND=int64), &
        INTENT(IN)              :: n_valid

        n_cache = n_cache + 1
        damaged_cache(:, n_cache) = damaged
        record_cache(:, n_cache) = record
        valid_cache(n_cache) = n_valid

    END SUBROUTINE write_cache

    RECURSIVE SUBROUTINE calc_arrangements(record, damaged, n_valid)
      IMPLICIT NONE
      CHARACTER(LEN=1), &
        INTENT(IN)              :: record(:)
      INTEGER(KIND=int32), &
        INTENT(IN)              :: damaged(:)
      INTEGER(KIND=int64), &
        INTENT(OUT)             :: n_valid
      INTEGER(KIND=int32)       :: i, damaged_new(SIZE(damaged))
      INTEGER(KIND=int64)       :: sub_valid
      CHARACTER(LEN=1)          :: record_new(SIZE(record))

      ! If we already know the answer (as we have called this routine
      ! with these exact arguments before, just return it from cache)
      CALL read_cache(record, damaged, n_valid)
      IF (n_valid /= -1) RETURN
        
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
          ! Save this valid result to the cache
          CALL write_cache(record, damaged, n_valid)
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
        IF (damaged(2) > 0) THEN
          damaged_new = EOSHIFT(damaged, 1)
          CALL calc_arrangements(&
            record_new, damaged_new, sub_valid)
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
      INTEGER(KIND=int32)       :: i, j, ios, n_records, n_damaged, &
                                   n_records_expanded, offset
      INTEGER(KIND=int32), &
        PARAMETER               :: expand=5

      IF (ALLOCATED(record)) DEALLOCATE(record)
      IF (ALLOCATED(damaged)) DEALLOCATE(damaged)
      READ(UNIT=funit, FMT="(A)", IOSTAT=ios) line
      IF (ios == IOSTAT_END) THEN
        RETURN
      END IF

      n_records = INDEX(line, " ") - 1

      ! Expand by 5 for part 2
      n_records_expanded = n_records*expand + (expand - 1)

      ! Pad our record array with blanks to make comparisons easier
      ! later i.e. without going out of bounds
      ALLOCATE(record(n_records_expanded + 2))
      offset = 0
      DO j=1,expand
        DO i=1,n_records
          record(i+1 + (j-1)*n_records + offset) = line(i:i)
        END DO
        offset = offset + 1
        record(i + (j-1)*n_records + offset) = "?"
      END DO
      record(1) = "."
      record(n_records_expanded + 2) = "."
    
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
      ! Pad the damage array as well
      ALLOCATE(damaged(n_damaged*expand))
      READ(line(n_records+2:line_len), *) damaged(1:n_damaged)
      DO i=2,expand
        damaged(n_damaged*(i-1)+1:i*n_damaged) = damaged(1:n_damaged)
      END DO

    END SUBROUTINE read_next_input

END PROGRAM springs
