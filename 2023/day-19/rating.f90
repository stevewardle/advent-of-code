PROGRAM rating
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  IMPLICIT NONE

  INTEGER(KIND=int32), &
    PARAMETER          &
      :: cat_x=1, cat_m=2, cat_a=3, cat_s=4, &
         cond_gt=1, cond_lt=2, &
         accepted=-1, rejected=-2

  TYPE rule
    INTEGER(KIND=int32) :: category
    INTEGER(KIND=int32) :: condition
    INTEGER(KIND=int32) :: threshold
    INTEGER(KIND=int32) :: destination
    CHARACTER(LEN=:), &
      ALLOCATABLE        :: destination_name
  END TYPE

  TYPE workflow
    CHARACTER(LEN=:), &
      ALLOCATABLE        :: name
    TYPE(rule), &
      ALLOCATABLE        :: rules(:)
    CHARACTER(LEN=:), &
      ALLOCATABLE        :: default_destination_name
    INTEGER(KIND=int32)  :: default_destination
  END TYPE

  TYPE(workflow), &
    ALLOCATABLE          :: workflows(:)
  INTEGER(KIND=int32), &
    ALLOCATABLE          :: parts(:,:)
  INTEGER(KIND=int32)    :: rating_sum

  CALL read_input("input.txt", workflows, parts)

  CALL sort_parts(parts, workflows, rating_sum)

  WRITE(*,"(A,I0)") "Sum of accepted ratings: ", rating_sum

  DEALLOCATE(parts)
  DEALLOCATE(workflows)

  CONTAINS

    SUBROUTINE sort_parts(parts, workflows, rating_sum)
      IMPLICIT NONE
      TYPE(workflow), &
        INTENT(IN)              :: workflows(:)
      INTEGER(KIND=int32), &
        INTENT(IN)              :: parts(:,:)
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: rating_sum
      INTEGER(KIND=int32)       :: i, j, start, i_workflow, o_workflow

      ! Find starting workflow
      start = 0
      DO i=1,SIZE(workflows)
        IF (workflows(i)%name == "in") THEN
          start = i
          EXIT
        END IF
      END DO

      rating_sum = 0
      DO i=1,SIZE(parts,1)
        i_workflow = start
        DO
          ! Check each rule looking to match up to a new workflow
          o_workflow = 0
          DO j=1,SIZE(workflows(i_workflow)%rules)
            SELECT CASE(workflows(i_workflow)%rules(j)%condition)
            CASE(cond_gt)
              IF (parts(i, &
                    workflows(i_workflow)%rules(j)%category) > &
                    workflows(i_workflow)%rules(j)%threshold) THEN
                o_workflow = workflows(i_workflow)%rules(j)%destination
                EXIT
              END IF
            CASE (cond_lt)
              IF (parts(i, &
                    workflows(i_workflow)%rules(j)%category) < &
                    workflows(i_workflow)%rules(j)%threshold) THEN
                o_workflow = workflows(i_workflow)%rules(j)%destination
                EXIT
              END IF
            END SELECT
          END DO
          ! If we did not find a workflow in any rule above, go with
          ! the default
          IF (o_workflow == 0) THEN
            o_workflow = workflows(i_workflow)%default_destination
          END IF
          ! Check for accepted/rejected and exit
          IF (o_workflow == accepted) THEN
            ! Update the score for accepted parts
            rating_sum = rating_sum + SUM(parts(i,:))
            EXIT
          ELSE IF (o_workflow == rejected) THEN
            EXIT
          END IF
          ! Failing all of the above continue to the next workflow 
          i_workflow = o_workflow
        END DO
      END DO

    END SUBROUTINE sort_parts

    SUBROUTINE read_input(filename, workflows, parts)
      IMPLICIT NONE
      CHARACTER(LEN=*), &
        INTENT(IN)              :: filename
      TYPE(workflow), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: workflows(:)
      INTEGER(KIND=int32), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: parts(:,:)
      INTEGER(KIND=int32)       :: input_file, n_workflows, n_parts

      OPEN(NEWUNIT=input_file, FILE=filename, ACCESS="stream", &
        FORM="formatted")
      CALL calc_sizes(input_file, n_workflows, n_parts)

      ALLOCATE(workflows(n_workflows))
      CALL read_workflows(input_file, workflows)

      ALLOCATE(parts(n_parts, 4))
      CALL read_parts(input_file, parts)

      CLOSE(UNIT=input_file)

    END SUBROUTINE read_input

    SUBROUTINE calc_sizes(funit, workflows, parts)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: workflows
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: parts
      INTEGER(KIND=int32)       :: ios
      CHARACTER(LEN=1)          :: single

      parts = 0
      workflows = 0
      DO
        READ(UNIT=funit, FMT="(A1)", IOSTAT=ios) single
        IF (TRIM(single) == "") THEN
          EXIT
        END IF
        workflows = workflows + 1
      END DO
      DO
        READ(UNIT=funit, FMT="()", IOSTAT=ios)
        IF (ios == IOSTAT_END) THEN
          EXIT
        END IF
        parts = parts + 1
      END DO
      REWIND(funit)

    END SUBROUTINE calc_sizes

    SUBROUTINE read_parts(funit, parts)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: parts(:,:)
      INTEGER(KIND=int32), &
        PARAMETER               :: linelen=500
      CHARACTER(LEN=linelen)    :: line
      INTEGER(KIND=int32)       :: ios, i, j, offset, &
                                   next, category
      DO i=1,SIZE(parts,1)
        READ(UNIT=funit, FMT="(A)", IOSTAT=ios) line
        IF (ios == IOSTAT_END) THEN
          EXIT
        END IF
        ! Get each rating
        offset = 1
        DO j=1,4
          next = INDEX(line(offset:linelen), "=")
          IF (next == 0) EXIT
          offset = offset + next - 2
          SELECT CASE(line(offset:offset))
          CASE("x")
            category = cat_x 
          CASE("m")
            category = cat_m
          CASE("a")
            category = cat_a
          CASE("s")
            category = cat_s
          CASE DEFAULT
            WRITE(*,"(A)") "Bad Category"
            CALL ABORT()
          END SELECT
          offset = offset + 2
          next = SCAN(line(offset:linelen),",}")
          READ(line(offset:offset+next-2),*) &
            parts(i, category)
          offset = offset + next-1
        END DO
      END DO

    END SUBROUTINE read_parts

    SUBROUTINE read_workflows(funit, workflows)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      TYPE(workflow), &
        INTENT(OUT)             :: workflows(:)
      INTEGER(KIND=int32), &
        PARAMETER               :: linelen=500
      CHARACTER(LEN=linelen)    :: line
      INTEGER(KIND=int32)       :: ios, i, j, k, offset, &
                                   roffset, next, n_rules

      DO i=1,SIZE(workflows) 
        READ(UNIT=funit, FMT="(A)", IOSTAT=ios) line

        ! Get the name
        offset = INDEX(line, "{")
        workflows(i)%name = line(1:offset-1)

        ! Get the rules
        n_rules = 0
        roffset = offset
        DO 
          next = INDEX(line(roffset:linelen), ",")
          IF (next == 0) EXIT
          n_rules = n_rules + 1
          roffset = roffset + next
        END DO
        ALLOCATE(workflows(i)%rules(n_rules))
        DO j=1,n_rules
          ! Category
          offset = offset + 1
          SELECT CASE(line(offset:offset))
          CASE("x")
            workflows(i)%rules(j)%category = cat_x
          CASE("m")
            workflows(i)%rules(j)%category = cat_m
          CASE("a")
            workflows(i)%rules(j)%category = cat_a
          CASE("s")
            workflows(i)%rules(j)%category = cat_s
          CASE DEFAULT
            WRITE(*,"(A)") "Bad category"
            CALL ABORT()
          END SELECT
          ! Condition
          offset = offset + 1
          SELECT CASE(line(offset:offset))
          CASE(">")
            workflows(i)%rules(j)%condition = cond_gt
          CASE("<")
            workflows(i)%rules(j)%condition = cond_lt
          CASE DEFAULT
            WRITE(*,"(A)") "Bad condition"
            CALL ABORT()
          END SELECT
          ! Threshold
          offset = offset + 1
          next = INDEX(line(offset:linelen),":")
          READ(line(offset:offset+next-2),*) &
            workflows(i)%rules(j)%threshold
          offset = offset + next
          ! Destination
          next = INDEX(line(offset:linelen),",")
          workflows(i)%rules(j)%destination_name = &
            line(offset:offset+next-2)
          offset = offset + next-1
        END DO
        ! Default destination
        offset = offset + 1
        next = INDEX(line(offset:linelen),"}")
        workflows(i)%default_destination_name = &
          line(offset:offset+next-2)
      END DO

      ! Now precalculate the destination indices
      DO i=1,SIZE(workflows)
        DO j=1,SIZE(workflows(i)%rules)
          IF (workflows(i)%rules(j)%destination_name == "A") THEN
            workflows(i)%rules(j)%destination = accepted
          ELSE IF (workflows(i)%rules(j)%destination_name == "R") THEN
            workflows(i)%rules(j)%destination = rejected
          ELSE
            DO k=1,SIZE(workflows)
              IF (workflows(k)%name &
                    == workflows(i)%rules(j)%destination_name) THEN
                workflows(i)%rules(j)%destination = k
              END IF
            END DO
          END IF
        END DO
        ! Also deal with the default destination
        IF (workflows(i)%default_destination_name == "A") THEN
          workflows(i)%default_destination = accepted
        ELSE IF (workflows(i)%default_destination_name == "R") THEN
          workflows(i)%default_destination = rejected
        ELSE
          DO k=1,SIZE(workflows)
            IF (workflows(k)%name &
                  == workflows(i)%default_destination_name) THEN
              workflows(i)%default_destination = k
            END IF
          END DO
        END IF
      END DO
      READ(UNIT=funit, FMT="()", IOSTAT=ios)

    END SUBROUTINE read_workflows

END PROGRAM rating
