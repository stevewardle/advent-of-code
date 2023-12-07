PROGRAM gears
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  
  IMPLICIT NONE

  INTEGER(KIND=int32)       :: ios, input_file

  INTEGER(KIND=int32), &
    PARAMETER               :: query_len=500
  CHARACTER(LEN=query_len)  :: query

  INTEGER(KIND=int32)       :: line_len, part_sum, total = 0
  CHARACTER(LEN=:), &
    ALLOCATABLE             :: line_buffer(:)
  INTEGER(KIND=int32), &
    ALLOCATABLE             :: part_numbers(:), part_number_map(:,:)

  OPEN(NEWUNIT=input_file, FILE="input.txt")

  ! Setup correctly sized line buffer - want a buffer
  ! which covers 3 lines of the input at a time and
  ! acts as a rolling window
  READ(UNIT=input_file, FMT="(A)", IOSTAT=ios) query
  line_len = LEN_TRIM(query)
  REWIND(input_file)
  
  ALLOCATE(CHARACTER(LEN=line_len) :: line_buffer(3))
  line_buffer(:) = ""

  ALLOCATE(part_numbers(line_len*3))
  ALLOCATE(part_number_map(line_len, 3))


  ! Start rolling through by shifting and consuming
  ! the next line each time
  DO
    CALL buffer_next(line_buffer, input_file) 
    ! An empty last record shows we are at the end
    IF (TRIM(line_buffer(3)) == "") THEN
      EXIT
    END IF

    ! If all 3 records aren't populated yet,
    ! let them advance until they are
    IF (TRIM(line_buffer(1)) == "") THEN
      CYCLE
    END IF

    ! Extract the required mappings - the map acts as
    ! a lookup into the part number array
    CALL gen_part_number_map(line_buffer, part_number_map, part_numbers)

    ! Now process the middle row of the 3 looking for parts
    CALL check_part_symbol( &
      line_buffer, &
      part_number_map, &
      part_numbers(1:MAXVAL(part_number_map)), &
      part_sum)

    total = total + part_sum

  END DO

  DEALLOCATE(part_number_map)
  DEALLOCATE(part_numbers)
  DEALLOCATE(line_buffer)
  CLOSE(input_file)

  WRITE(*,"(A,I0)") "Total sum of gear ratio products: ", total

  CONTAINS
    SUBROUTINE buffer_next(buffer, funit)
      IMPLICIT NONE
      CHARACTER(LEN=*), &
        INTENT(INOUT)         :: buffer(3)
      INTEGER(KIND=int32), &
        INTENT(IN)            :: funit
      INTEGER(KIND=int32)     :: ios
      ! Shift the buffer to lose the first line then read in the
      ! next record into the end of the buffer
      buffer = EOSHIFT(buffer,1)
      READ(UNIT=funit, FMT="(A)", IOSTAT=ios) buffer(3)
      IF (ios == IOSTAT_END) THEN
        buffer(3) = ""
      END IF
    END SUBROUTINE buffer_next

    SUBROUTINE gen_part_number_map(buffer, map, part_numbers)
      IMPLICIT NONE
      CHARACTER(LEN=*), &
        INTENT(IN)         :: buffer(:)
      INTEGER(KIND=int32), &
        INTENT(OUT)        :: map(LEN(buffer),SIZE(buffer))
      INTEGER(KIND=int32), &
        INTENT(OUT)        :: part_numbers(LEN(buffer)*SIZE(buffer))
      INTEGER(KIND=int32)  :: i, j, ios, digit, i_part
      LOGICAL              :: l_last_digit
      CHARACTER(LEN=LEN(buffer)) :: line

      map(:,:) = 0
      part_numbers(:) = 0
      i_part = 0
      l_last_digit = .FALSE.

      DO i=1,SIZE(buffer)
        line = buffer(i)
        DO j=1,LEN(buffer)
          ! Horrible having to do a test internal read of every character
          ! to find out if it is a digit, but whatever
          READ(line(j:j), "(I1)", IOSTAT=ios) digit
          IF (ios == 0) THEN
            IF (l_last_digit) THEN
              ! We are already "in progress" reading a part number, so
              ! extend what we already had to include this digit
              part_numbers(i_part) = 10*part_numbers(i_part) + digit
            ELSE
              ! This is a new part number, so shift to the next record
              ! in the part number array
              i_part = i_part + 1
              l_last_digit = .TRUE.
              part_numbers(i_part) = digit
            END IF
            ! The map should store the index in the part number array
            ! at the position of the given digit
            map(j, i) = i_part
          ELSE
            l_last_digit = .FALSE.
          END IF
        END DO
      END DO

    END SUBROUTINE gen_part_number_map

    SUBROUTINE check_part_symbol(buffer, map, part_numbers, part_sum)
      IMPLICIT NONE
      CHARACTER(LEN=*), &
        INTENT(IN)         :: buffer(:)
      INTEGER(KIND=int32), &
        INTENT(IN)        :: map(LEN(buffer),SIZE(buffer))
      INTEGER(KIND=int32), &
        INTENT(IN)        :: part_numbers(:)
      INTEGER(KIND=int32), &
        INTENT(OUT)        :: part_sum
      INTEGER(KIND=int32)  :: neighbourhood(9)
      INTEGER(KIND=int32)  :: i, j, part_number
      CHARACTER(LEN=LEN(buffer)) :: line
      CHARACTER(LEN=1), &
        PARAMETER          :: gears = "*"
      LOGICAL              :: seen(SIZE(part_numbers))
      INTEGER(KIND=int32)  :: part_count, first_part 

      line = buffer(2)
      seen = .FALSE.
      part_sum = 0

      DO i=1,LEN(line)
        IF (line(i:i) == gears) THEN
          part_count = 0

          neighbourhood(1:3) = map(i-1:i+1,1)
          neighbourhood(4:6) = map(i-1:i+1,2)
          neighbourhood(7:9) = map(i-1:i+1,3) 

          DO j=1,9
            IF (neighbourhood(j) > 0) THEN
              IF (.NOT. seen(neighbourhood(j))) THEN
                part_number = part_numbers(neighbourhood(j)) 
                seen(neighbourhood(j)) = .TRUE.
                part_count = part_count + 1
                IF (part_count == 1) THEN
                  first_part = part_number
                ELSE IF (part_count == 2) THEN
                  part_sum = part_sum + first_part*part_number
                  part_count = 0
                END IF
              END IF
            END IF
          END DO

        END IF
      END DO

    END SUBROUTINE check_part_symbol

END PROGRAM gears
