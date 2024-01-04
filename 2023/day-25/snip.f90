PROGRAM day_25
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, real32, IOSTAT_END
  IMPLICIT NONE

  INTEGER(KIND=int32), &
    PARAMETER           :: max_connections = 10, &
                           max_components = 1500

  TYPE component
    CHARACTER(LEN=3)    :: name = ""
    INTEGER(KIND=int32) :: n_connections = 0
    INTEGER(KIND=int32) :: connections(max_connections) = 0
  END TYPE component

  TYPE(component), &
    ALLOCATABLE         :: components(:)

  INTEGER(KIND=int32)   :: n_components, groupA, groupB

  CALL read_input("input.txt", components, n_components) 

  CALL find_group(components(1:n_components), groupA, groupB)

  WRITE(*, "(A,I0)") "Product of Group Sizes: ", groupA*groupB

  DEALLOCATE(components)

  CONTAINS

    RECURSIVE SUBROUTINE find_group(components, groupA, groupB)
      IMPLICIT NONE
      TYPE(component), &
        INTENT(IN)              :: components(:)
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: groupA, groupB
      INTEGER(KIND=int32)       :: connections(SIZE(components))
      LOGICAL                   :: mask(SIZE(components))
      INTEGER(KIND=int32)       :: start, next, i
      REAL(KIND=real32)         :: rand

      ! Size of group A and group B to return
      groupA = 0
      groupB = 0

      ! Choose a random point from the components
      CALL RANDOM_NUMBER(rand)
      start = INT(rand*SIZE(components), KIND=int32)

      ! The total number of connections to the group
      ! being accumulated (group A)
      connections(:) = 0
      ! A mask to indicate points still in the group
      ! not being accumulated (group B)
      mask(:) = .TRUE.

      ! Initialise the random start value
      connections(start) = 1
      DO
        ! Find the "most connected" node that has not
        ! been considered
        next = MAXLOC(connections, DIM=1, MASK=mask)
        ! Mask it from future consideration and cancel
        ! out its connection score - this is in effect
        ! "removing" it from group B
        mask(next) = .FALSE.
        connections(next) = 0
        ! Now all of its connections that are still in
        ! group B gain 1 connection to group A
        DO i=1,components(next)%n_connections
          IF (mask(components(next)%connections(i))) THEN
            connections(components(next)%connections(i)) = &
              connections(components(next)%connections(i)) + 1
          END IF
        END DO
        ! As points are successively removed the overall
        ! connection score will grow and then contract as
        ! group A is consumed - and ultimately there will
        ! only be 3 connections left
        IF (SUM(connections) == 3) THEN
          groupA = COUNT(mask)
          groupB = COUNT(.NOT. mask)
          RETURN
        END IF
        ! *Unless* we accidentally started with a point
        ! that was right on one of the bridge edges - if
        ! that happens just try again with a different
        ! random starting number
        IF (SUM(connections) == 0) THEN
          CALL find_group(components, groupA, groupB)
          RETURN
        END IF
      END DO

    END SUBROUTINE find_group

    SUBROUTINE add_connection(comp, connection)
      IMPLICIT NONE
      TYPE(component), &
        INTENT(INOUT)           :: comp
      INTEGER(KIND=int32), &
        INTENT(IN)              :: connection
      INTEGER(KIND=int32)       :: i
      IF (comp%n_connections == max_connections) THEN
        WRITE(*,"(A)") "Connections exceeded for "//comp%name
        CALL ABORT()
      END IF
      ! Don't add the same connection more than once
      DO i=1,comp%n_connections
        IF (comp%connections(i) == connection) THEN
          RETURN
        END IF
      END DO
      comp%n_connections = comp%n_connections + 1
      comp%connections(comp%n_connections) = connection
    END SUBROUTINE

    SUBROUTINE read_input(filename, components, n_components)
      IMPLICIT NONE
      CHARACTER(LEN=*), &
        INTENT(IN)              :: filename
      TYPE(component), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: components(:)
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: n_components
      INTEGER(KIND=int32)       :: input_file

      OPEN(NEWUNIT=input_file, FILE=filename, ACCESS="stream", &
        FORM="formatted")
      CALL read_components(input_file, components, n_components)

      CLOSE(UNIT=input_file)

    END SUBROUTINE read_input

    SUBROUTINE read_components(funit, components, n_components)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      TYPE(component), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: components(:)
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: n_components
      INTEGER(KIND=int32)       :: ios, i, j, k, last, node, &
                                   separator
      INTEGER(KIND=int32), &
        PARAMETER               :: linelen=100
      CHARACTER(LEN=linelen)    :: line
      CHARACTER(LEN=3)          :: connections(max_connections), name
      LOGICAL                   :: found
      
      n_components = 0
      DO
        READ(UNIT=funit, FMT="()", IOSTAT=ios)
        IF (ios == IOSTAT_END) THEN
          EXIT
        END IF
        n_components = n_components + 1
      END DO
      REWIND(funit)
      IF (ALLOCATED(components)) DEALLOCATE(components)
      ALLOCATE(components(max_components))

      ! Read the components
      last = 0
      DO i=1,n_components
        READ(UNIT=funit, FMT="(A)", IOSTAT=ios) line
        ! Read the next name + connections list
        separator = SCAN(line, ":")
        READ(line(1:separator-1), *) name
        READ(line(separator+2:linelen), "(1000(A3,1X))") connections
        ! Did we initialise this node already - do a quick
        ! scan to find its index
        found = .FALSE.
        DO j=1,max_components
          IF (components(j)%name == name) THEN
            found = .TRUE.
            EXIT
          END IF
          IF (components(j)%name == "") THEN
            EXIT
          END IF
        END DO
        IF (found) THEN
          ! We found an existing node
          node = j
        ELSE
          ! Otherwise create this node at the
          ! next position and increment the count
          last = last + 1
          IF (last > max_components) THEN
            WRITE(*,"(A)") "Max connections too low"
            CALL ABORT()
          END IF
          components(last)%name = name
          node = last
        END IF
        ! Now process the connections
        DO j=1,max_connections
          IF (connections(j) == "") EXIT
          ! Scan the component list to see if each
          ! connection name is already initialised
          found = .FALSE.
          DO k=1,max_components
            IF (components(k)%name == connections(j)) THEN
              found = .TRUE.
              EXIT
            END IF
            IF (components(k)%name == "") THEN
              EXIT
            END IF
          END DO
          IF (found) THEN
            ! If it is, add its index to this node's list
            ! of connections, and vice versa
            CALL add_connection(components(node), k)
            CALL add_connection(components(k), node)
          ELSE
            ! Otherwise, initialise this node at the
            ! next position and increment the count
            last = last + 1
            IF (last > max_components) THEN
              WRITE(*,"(A)") "Max connections too low"
              CALL ABORT()
            END IF
            components(last)%name = connections(j)
            CALL add_connection(components(node), last)
            CALL add_connection(components(last), node)
          END IF
        END DO
      END DO
      n_components = last

    END SUBROUTINE read_components

END PROGRAM day_25
