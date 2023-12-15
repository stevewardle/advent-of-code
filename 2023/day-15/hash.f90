PROGRAM hashing
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  IMPLICIT NONE
  INTEGER(KIND=int32)       :: i, hash_sum, focusing_power_sum
  CHARACTER(LEN=1), &
    ALLOCATABLE             :: steps(:,:)
  INTEGER(KIND=int32), &
    PARAMETER               :: n_boxes=256, box_size=10
  INTEGER(KIND=int32)       :: target_box, focusing_power
  CHARACTER(LEN=1)          :: target_op
  CHARACTER(LEN=1), &
    PARAMETER               :: op_add = "=", op_remove = "-"

  TYPE lens
    CHARACTER(LEN=1), ALLOCATABLE :: label(:)
    INTEGER(KIND=int32) :: focal_length
  END TYPE lens
  TYPE(lens) :: target_lens

  TYPE box
    INTEGER(KIND=int32) :: n_lenses = 0
    TYPE(lens) :: lenses(box_size)
  END TYPE box
  TYPE(box) :: boxes(0:n_boxes-1)

  CALL read_input("input.txt", steps)

  ! Calculate the hash sum whilst also shuffling 
  ! lenses around 
  hash_sum = 0
  DO i=1,SIZE(steps,1)
    hash_sum = hash_sum + hash(steps(i,:))
    CALL extract_operation(steps(i,:), target_box, target_lens, target_op)

    IF (target_op == op_add) THEN
      CALL add_lens(target_box, target_lens)
    END IF
    IF (target_op == op_remove) THEN
      CALL remove_lens(target_box, target_lens % label)
    END IF

  END DO 

  ! Calculate the combined focusing power
  focusing_power_sum = 0
  DO i=0,n_boxes-1
    IF (boxes(i) % n_lenses > 0) THEN
      CALL calculate_focusing_power(i, focusing_power)
      focusing_power_sum = focusing_power_sum + focusing_power
    END IF
  END DO

  WRITE(*,"(A,I0)") "Sum of hashes: ", hash_sum
  WRITE(*,"(A,I0)") "Sum of focusing power: ", focusing_power_sum

  CONTAINS

    SUBROUTINE calculate_focusing_power(t_box, power)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)             :: t_box
      INTEGER(KIND=int32), &
        INTENT(OUT)            :: power
      INTEGER(KIND=int32)      :: i

      power = 0
      DO i=1,boxes(t_box) % n_lenses
        power = power + i*(boxes(t_box) % lenses(i) % focal_length)
      END DO
      IF (power > 0) THEN
        power = power*(t_box + 1)
      END IF

    END SUBROUTINE calculate_focusing_power

    SUBROUTINE remove_lens(t_box, label)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)             :: t_box
      CHARACTER(LEN=1), &
        INTENT(IN)             :: label(:)
      INTEGER(KIND=int32)      :: i
      TYPE(lens)               :: empty_lens

      DO i=1,boxes(t_box) % n_lenses
        ! If a box with the label exists, remove it 
        IF (ALL(boxes(t_box) % lenses(i) % label == label)) THEN
          boxes(t_box) % lenses(i:boxes(t_box) % n_lenses) = &
            EOSHIFT(boxes(t_box) % lenses(i:boxes(t_box) % n_lenses),1,empty_lens)
          boxes(t_box) % n_lenses = boxes(t_box) % n_lenses - 1
          EXIT
        END IF
      END DO

    END SUBROUTINE

    SUBROUTINE add_lens(t_box, t_lens)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)             :: t_box
      TYPE(lens), &
        INTENT(IN)             :: t_lens 
      INTEGER(KIND=int32)      :: i
      
      DO i=1,boxes(t_box) % n_lenses
        ! If a box with the label exists already, keep it but
        ! update its focal length
        IF (ALL(boxes(t_box) % lenses(i) % label == t_lens % label)) THEN
          boxes(t_box) % lenses(i) % focal_length = t_lens % focal_length
          EXIT
        END IF
      END DO
      ! If we did not find the label, add this lens to the end
      IF (i == boxes(t_box) % n_lenses + 1) THEN
        boxes(t_box) % lenses(boxes(t_box) % n_lenses + 1) = t_lens
        boxes(t_box) % n_lenses = i 
      END IF

    END SUBROUTINE add_lens

    SUBROUTINE extract_operation(string, t_box, t_lens, t_op)
      IMPLICIT NONE
      CHARACTER(LEN=1), &
        INTENT(IN)              :: string(:)
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: t_box
      CHARACTER(LEN=1), &
        INTENT(OUT)             :: t_op
      TYPE(lens), &
        INTENT(OUT)             :: t_lens 
      INTEGER(KIND=int32)       :: delim_pos

      t_lens % focal_length = 0
      delim_pos = FINDLOC(string, op_add, DIM=1)
      IF (delim_pos /= 0) THEN
        t_op = op_add
        READ(string(delim_pos+1:SIZE(string)),"(I1)") t_lens % focal_length
      ELSE
        delim_pos = FINDLOC(string, op_remove, DIM=1)
        t_op = op_remove
      END IF
      ALLOCATE(t_lens % label(delim_pos))
      t_lens % label(:) = ACHAR(0)
      t_lens % label(1:delim_pos-1) = string(1:delim_pos-1)
      t_box = hash(t_lens % label)

    END SUBROUTINE extract_operation

    FUNCTION hash(string)
      IMPLICIT NONE
      CHARACTER(LEN=1), &
        INTENT(IN)              :: string(:)
      INTEGER(KIND=int32)       :: hash
      INTEGER(KIND=int32)       :: i

    hash = 0
    DO i=1,FINDLOC(string, ACHAR(0), DIM=1) - 1
      hash = hash + IACHAR(string(i))
      hash = hash*17
      hash = MOD(hash,256)
    END DO

    END FUNCTION hash

    SUBROUTINE read_input(filename, steps)
      IMPLICIT NONE
      CHARACTER(LEN=*), &
        INTENT(IN)              :: filename
      CHARACTER(LEN=1), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: steps(:,:)
      CHARACTER(LEN=1), &
        ALLOCATABLE             :: seq(:)
      INTEGER(KIND=int32)       :: i, ios, n_steps, &
                                   delim_pos, input_file
      INTEGER(KIND=int32), &
        PARAMETER               :: line_len=50000, &
                                   max_step_len=10
      CHARACTER(LEN=line_len)   :: line

    IF (ALLOCATED(seq)) DEALLOCATE(seq)
    IF (ALLOCATED(steps)) DEALLOCATE(steps)

    OPEN(NEWUNIT=input_file, FILE=filename, ACCESS="stream", &
      FORM="formatted")
    READ(UNIT=input_file, FMT="(A)", IOSTAT=ios) line
    CLOSE(UNIT=input_file)

    ALLOCATE(seq(LEN_TRIM(line)))
    DO i=1,LEN_TRIM(line)
      seq(i) = line(i:i)
    END DO

    n_steps = COUNT(seq == ",")+1
    ALLOCATE(steps(n_steps, max_step_len))
    steps(:,:) = ACHAR(0)

    DO i=1,n_steps
       delim_pos = FINDLOC(seq, ",", DIM=1)
       IF (delim_pos == 0) delim_pos = FINDLOC(seq, " ", DIM=1)
       steps(i, 1:delim_pos-1) = seq(1:delim_pos-1)
       seq = EOSHIFT(seq,delim_pos)
    END DO
    DEALLOCATE(seq)

    END SUBROUTINE read_input

END PROGRAM hashing
