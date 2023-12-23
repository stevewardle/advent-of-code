PROGRAM pulse
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  USE linked_list_mod, ONLY: ll_list, ll_node
  USE pulse_module_mod, ONLY: ptype_low, ptype_high, flip_flop_module, pulse_send, &
                              broadcast_module, conjunction_module, pulse_module
  IMPLICIT NONE

  TYPE(ll_list) :: pulse_queue
  TYPE(ll_node), POINTER :: node_query

  TYPE(pulse_send) :: current_pulse
  CLASS(*), POINTER :: list_element

  TYPE(ll_list) :: module_list

  CALL read_input("input.txt", module_list)

  ! Checking values read into the module list right
  CALL module_list%reset()
  DO WHILE (module_list%more())
    node_query => module_list%current()
    SELECT TYPE(p => node_query%value)
    CLASS IS (pulse_module)
      PRINT*, p%name
    END SELECT
    CALL module_list%next()
  END DO

  ! Messing about to show linked list working
  ! for the pulse queue
  current_pulse%ptype = ptype_low
  ALLOCATE(list_element, source=current_pulse)
  CALL pulse_queue%add(list_element)
  NULLIFY(list_element)
  current_pulse%ptype = ptype_high
  ALLOCATE(list_element, source=current_pulse)
  CALL pulse_queue%add(list_element)
  NULLIFY(list_element)

  PRINT*, pulse_queue%length()

  node_query => pulse_queue%first()
  SELECT TYPE(p => node_query%value)
  TYPE IS (pulse_send)
    PRINT*, p%ptype
  END SELECT
  node_query => node_query%next
  SELECT TYPE(p => node_query%value)
  TYPE IS (pulse_send)
    PRINT*, p%ptype
  END SELECT

  CONTAINS

    SUBROUTINE read_input(filename, module_list)
      IMPLICIT NONE
      CHARACTER(LEN=*), &
        INTENT(IN)              :: filename
      TYPE(ll_list), &
        INTENT(INOUT)           :: module_list
      INTEGER(KIND=int32)       :: input_file

      OPEN(NEWUNIT=input_file, FILE=filename, ACCESS="stream", &
        FORM="formatted")
      CALL read_modules(input_file, module_list)  

      CLOSE(UNIT=input_file)

    END SUBROUTINE read_input

    SUBROUTINE read_modules(funit, module_list)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      TYPE(ll_list), &
        INTENT(INOUT)           :: module_list
      INTEGER(KIND=int32)       :: ios
      INTEGER(KIND=int32), &
        PARAMETER               :: linelen=100
      CHARACTER(LEN=linelen)    :: line
      CHARACTER(LEN=:), &
        ALLOCATABLE             :: module_name
      CLASS(*), &
        POINTER                 :: list_element
      TYPE(broadcast_module)    :: broadcast
      TYPE(flip_flop_module)    :: flip_flop
      TYPE(conjunction_module)  :: conjunction

      ! Read the first set of entries from the input to create
      ! the entries in the linked list
      DO
        READ(UNIT=funit, FMT="(A)", IOSTAT=ios) line
        IF (ios == IOSTAT_END) THEN
          EXIT
        END IF
        IF (ALLOCATED(module_name)) DEALLOCATE(module_name)
        module_name = line(1:SCAN(line," ")-1) 
        IF (module_name == "broadcaster") THEN
          broadcast%name = module_name
          ALLOCATE(list_element, source=broadcast)
          CALL module_list%add(list_element)
          NULLIFY(list_element)
        ELSE
          SELECT CASE(module_name(1:1))
          CASE("%")
            flip_flop%name = module_name(2:LEN(module_name))
            ALLOCATE(list_element, source=flip_flop)
            CALL module_list%add(list_element)
            NULLIFY(list_element)
          CASE("&")
            conjunction%name = module_name(2:LEN(module_name))
            ALLOCATE(list_element, source=conjunction)
            CALL module_list%add(list_element)
            NULLIFY(list_element)
          END SELECT
        END IF
      END DO
      REWIND(funit)

    END SUBROUTINE read_modules

END PROGRAM pulse
