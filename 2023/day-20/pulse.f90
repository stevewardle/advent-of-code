PROGRAM pulse
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, int64, IOSTAT_END
  IMPLICIT NONE

  ! Different types of pulses
  INTEGER(KIND=int32), &
    PARAMETER :: ptype_low=1, ptype_high=2
  CHARACTER(LEN=4), &
    PARAMETER :: ptypes(2)=["low ", "high"]

  ! Different types of module
  INTEGER(KIND=int32), &
    PARAMETER :: mod_broadcast=1, mod_conjunction=2, &
                 mod_flip_flop=3, mod_exit=4, mod_button=5

  ! Different special destinations
  INTEGER(KIND=int32), &
    PARAMETER :: dest_exit=1, dest_button=2

  ! A pulse module - it can be any of them but will store
  ! slightly different data depending on which
  TYPE pulse_module
    CHARACTER(LEN=:), &
      ALLOCATABLE       :: name
    INTEGER(KIND=int32) :: mod_type
    INTEGER(KIND=int32), &
      ALLOCATABLE       :: destinations(:)
    INTEGER(KIND=int32), &
      ALLOCATABLE       :: sources(:)
    INTEGER(KIND=int32), &
      ALLOCATABLE       :: last_pulse(:)
    LOGICAL             :: active
    INTEGER(KIND=int32) :: count_low=0
    INTEGER(KIND=int32) :: count_high=0
  END TYPE pulse_module
  
  TYPE(pulse_module), &
    ALLOCATABLE         :: pulse_modules(:)

  ! Stores a single pulse event
  TYPE pulse_send
    INTEGER(KIND=int32) :: ptype
    INTEGER(KIND=int32) :: source
    INTEGER(KIND=int32) :: destination
  END TYPE pulse_send

  INTEGER(KIND=int32), &
    PARAMETER           :: pulse_queue_max = 1000, &
                           button_presses = 5000
  INTEGER(KIND=int32)   :: pulse_queue_size = 0, dest_broadcast, i, j, k
  TYPE(pulse_send)      :: pulse_send_queue(pulse_queue_max)
  INTEGER(KIND=int32)   :: total_low, total_high

  ! For part 2 these are the 4 modules connected to the final one
  ! before "rx" - they are looping so need to capture their loop
  ! periods
  INTEGER(KIND=int64)   :: emit_high(4) = -1
  INTEGER(KIND=int64)   :: gcd, presses
  CHARACTER(LEN=2), &
    PARAMETER           :: emit_high_name(4) = &
                              ["vz", "bq", "qh", "lt"]

  CALL read_input("input.txt", pulse_modules) 

  ! Locate the broadcaster
  dest_broadcast = locate_broadcaster(pulse_modules)

  DO i=1,button_presses
    ! Add a button press
    CALL pulse_to_queue(ptype_low, dest_button, dest_broadcast)

    ! Process the queue
    DO WHILE (.NOT. pulse_queue_empty())
      CALL process_pulse(pulse_modules, pulse_from_queue())
    END DO

    IF (i == 1000) THEN
      ! Count the pulses
      total_low = 0
      total_high = 0
      DO j=1,SIZE(pulse_modules)
        total_low = total_low + pulse_modules(j)%count_low
        total_high = total_high + pulse_modules(j)%count_high
      END DO
  
      WRITE(*,"(A,I0)") "Multiple of low and high pulses: ", total_low*total_high
    END IF

    ! This ends up being rather specific... but these 4 modules are the
    ! ones in my input connected to "ft" the conjunction module which will
    ! send the output signal to "rx" - find out when they each first emit
    ! the high signal required to trigger "ft" to send low to "rx"
    DO j=1,SIZE(pulse_modules)
      IF (pulse_modules(j)%count_high > 0) THEN
        DO k=1,SIZE(emit_high_name)
          IF ((emit_high_name(k) == pulse_modules(j)%name) &
            .AND. (emit_high(k) == -1)) THEN
            emit_high(k) = i
          END IF
        END DO
      END IF
    END DO
  END DO

  ! And since the loops are mercifully nicely aligned again as in
  ! day-08 the answer is just the LCM of these
  presses = emit_high(1)
  DO i=2,4
    CALL gcdx(presses, emit_high(i), gcd)
    presses = presses / gcd * emit_high(i)
  END DO

  WRITE(*,"(A,I0)") "Number of presses required: ", presses

  CONTAINS

    ! Shameless-ly ripped from Wikipedia this is the
    ! Extended Euclidean Algorithm
    SUBROUTINE gcdx(a, b, old_r, old_s_out, old_t_out)
      IMPLICIT NONE
      INTEGER(KIND=int64), &
        INTENT(IN)          :: a, b
      INTEGER(KIND=int64), &
        INTENT(OUT)         :: old_r
      INTEGER(KIND=int64), &
        INTENT(OUT), &
        OPTIONAL            :: old_s_out, old_t_out
      INTEGER(KIND=int64)   :: old_s, old_t, r, s, t, quotient, prov
      old_r = a
      r     = b
      old_s = 1
      s     = 0
      old_t = 0
      t     = 1
      DO WHILE (r /= 0)
        quotient = old_r / r
        prov = r
        r = old_r - quotient*prov
        old_r = prov
        prov = s
        s = old_s - quotient*prov
        old_s = prov
        prov = t
        t = old_t - quotient*prov
        old_t = prov
      END DO
      IF (PRESENT(old_s_out)) old_s_out = old_s
      IF (PRESENT(old_t_out)) old_t_out = old_t

    END SUBROUTINE gcdx

    SUBROUTINE process_pulse(modules, pulse)
      IMPLICIT NONE
      TYPE(pulse_module), &
        INTENT(INOUT)       :: modules(:)
      TYPE(pulse_send), &
        INTENT(IN)          :: pulse
      INTEGER(KIND=int32)   :: i, ptype

      ! Update the count of pulse type (count at the sender)
      IF (pulse%ptype == ptype_high) THEN
        modules(pulse%source)%count_high = &
          modules(pulse%source)%count_high + 1
      ELSE
        modules(pulse%source)%count_low = &
          modules(pulse%source)%count_low + 1
      END IF
      ! Act based on the type of module being sent to
      SELECT CASE (modules(pulse%destination)%mod_type)
      CASE (mod_broadcast)
        ptype = pulse%ptype
      CASE (mod_flip_flop)
        IF (pulse%ptype == ptype_low) THEN
          IF (modules(pulse%destination)%active) THEN
            ptype = ptype_low
          ELSE
            ptype = ptype_high
          END IF
          modules(pulse%destination)%active = &
            (.NOT. modules(pulse%destination)%active)
        ELSE
          RETURN
        END IF
      CASE (mod_conjunction)
        DO i=1,SIZE(modules(pulse%destination)%sources)
          IF (modules(pulse%destination)%sources(i) == pulse%source) THEN
            modules(pulse%destination)%last_pulse(i) = pulse%ptype
          END IF
        END DO
        IF (ALL(modules(pulse%destination)%last_pulse == ptype_high)) THEN
          ptype = ptype_low
        ELSE
          ptype = ptype_high
        END IF
      CASE (mod_exit)
        RETURN
      CASE DEFAULT
        WRITE(*,"(A)") "Unknown mod type"
        CALL ABORT()
      END SELECT
      ! Lodge the output pulses onto the queue
      DO i=1,SIZE(modules(pulse%destination)%destinations)
        CALL pulse_to_queue(&
          ptype, &
          pulse%destination, &
          modules(pulse%destination)%destinations(i))
      END DO

    END SUBROUTINE process_pulse

    FUNCTION locate_broadcaster(modules) RESULT(dest)
      IMPLICIT NONE
      TYPE(pulse_module), &
        INTENT(IN)          :: modules(:)
      INTEGER(KIND=int32)   :: dest, i
      dest= -1
      DO i=1,SIZE(modules)
        IF (modules(i)%mod_type == mod_broadcast) THEN
          dest= i
          EXIT
        END IF
      END DO
      IF (dest == -1) THEN
        WRITE(*, "(A)") "Broadcaster not found"
        CALL ABORT()
      END IF
    END FUNCTION locate_broadcaster

    SUBROUTINE print_pulse(pulse)
      IMPLICIT NONE
      TYPE(pulse_send), &
        INTENT(IN)          :: pulse
      WRITE(*,"(A)") &
        pulse_modules(pulse%source)%name//" -"// &
        TRIM(ptypes(pulse%ptype))//"-> "//&
        pulse_modules(pulse%destination)%name
    END SUBROUTINE print_pulse

    FUNCTION pulse_from_queue() RESULT(pulse) 
      IMPLICIT NONE
      TYPE(pulse_send)     :: pulse
      TYPE(pulse_send)     :: empty
      IF (pulse_queue_empty()) THEN
        WRITE(*,"(A)") "Queue empty"
        CALL ABORT()
      END IF
      pulse = pulse_send_queue(1)
      pulse_queue_size = pulse_queue_size - 1
      pulse_send_queue = EOSHIFT(pulse_send_queue,1, BOUNDARY=empty)
    END FUNCTION pulse_from_queue

    FUNCTION pulse_queue_empty() RESULT(empty)
      IMPLICIT NONE
      LOGICAL :: empty
      empty = (pulse_queue_size == 0)
    END FUNCTION pulse_queue_empty

    SUBROUTINE pulse_to_queue(ptype, source, destination)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)            :: ptype, source, destination
      IF (pulse_queue_size == pulse_queue_max) THEN
        WRITE(*,"(A)") "Queue size exceeded"
        CALL ABORT()
      END IF
      pulse_queue_size = pulse_queue_size + 1
      pulse_send_queue(pulse_queue_size) = &
        pulse_send(ptype=ptype, source=source, destination=destination)
    END SUBROUTINE pulse_to_queue

    SUBROUTINE read_input(filename, modules)
      IMPLICIT NONE
      CHARACTER(LEN=*), &
        INTENT(IN)              :: filename
      TYPE(pulse_module), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: modules(:)
      INTEGER(KIND=int32)       :: input_file

      OPEN(NEWUNIT=input_file, FILE=filename, ACCESS="stream", &
        FORM="formatted")
      CALL read_modules(input_file, modules)  

      CLOSE(UNIT=input_file)

    END SUBROUTINE read_input

    SUBROUTINE read_modules(funit, modules)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      TYPE(pulse_module), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: modules(:)
      INTEGER(KIND=int32)       :: ios
      INTEGER(KIND=int32), &
        PARAMETER               :: linelen=100
      CHARACTER(LEN=linelen)    :: line
      CHARACTER(LEN=15), &
        ALLOCATABLE             :: destinations(:)
      INTEGER(KIND=int32)       :: sources(10)
      CHARACTER(LEN=linelen), &
        ALLOCATABLE             :: destination_names(:)
      INTEGER(KIND=int32)       :: i, j, k, next, &
                                   roffset, n_destinations, &
                                   n_modules, n_sources
      n_modules = 2 ! Allow extra for exit and button modules
      DO
        READ(UNIT=funit, FMT="()", IOSTAT=ios)
        IF (ios == IOSTAT_END) THEN
          EXIT
        END IF
        n_modules = n_modules + 1
      END DO
      REWIND(funit)
      IF (ALLOCATED(modules)) DEALLOCATE(modules)
      ALLOCATE(modules(n_modules))
      ALLOCATE(destination_names(n_modules))

      ! Add button module
      modules(dest_button)%mod_type = mod_button
      modules(dest_button)%name = "button"
      destination_names(dest_button) = "broadcaster"

      ! Initially populate the array with correctly typed
      ! modules
      DO i=3,n_modules
        READ(UNIT=funit, FMT="(A)", IOSTAT=ios) line
        ! Get the module name
        modules(i)%name = line(1:SCAN(line," ")-1) 
        ! Select type of module based on name
        IF (modules(i)%name == "broadcaster") THEN
          modules(i)%mod_type = mod_broadcast
        ELSE
          SELECT CASE(modules(i)%name(1:1))
          CASE("%")
            modules(i)%mod_type = mod_flip_flop
            modules(i)%active = .FALSE.
          CASE("&")
            modules(i)%mod_type = mod_conjunction
          END SELECT
          modules(i)%name = modules(i)%name(2:LEN(modules(i)%name))
        END IF
        ! Save destinations
        READ(line(SCAN(line, ">")+1:linelen), "(A)") destination_names(i)
      END DO

      ! Update the destinations to reference indices of the array
      DO i=2,n_modules
        ! Count how many destinations there are
        n_destinations = 1
        line = destination_names(i)
        roffset = 1 
        DO 
          next = INDEX(line(roffset:linelen), ",")
          IF (next == 0) EXIT
          n_destinations = n_destinations + 1
          roffset = roffset + next
        END DO
        ! Read them into a temporary array
        IF (ALLOCATED(destinations)) DEALLOCATE(destinations)
        ALLOCATE(destinations(n_destinations))
        READ(line, *) destinations
        ! Now lookup the index of the destination in the module list
        ! and add it to the pulse module type's destination list
        ALLOCATE(modules(i)%destinations(n_destinations))
        modules(i)%destinations(:) = -1
        DO j=1,SIZE(destinations)
          DO k=1,n_modules 
            IF (modules(k)%name == destinations(j)) THEN
              modules(i)%destinations(j) = k
            END IF
          END DO
          ! If no destination could be found, this must be
          ! the exit node - so set it up here if not already
          IF ((modules(i)%destinations(j) == -1) &
            .AND. (.NOT. ALLOCATED(modules(dest_exit)%name))) THEN
            modules(dest_exit)%mod_type = mod_exit
            modules(dest_exit)%name = destinations(j)
            destination_names(dest_exit) = ""
            modules(i)%destinations(j) = dest_exit
          END IF
        END DO
      END DO

      ! Need to populate the sources for the
      ! conjunction modules
      DO i=1,n_modules
        IF (modules(i)%mod_type == mod_conjunction) THEN
          n_sources = 0
          sources(:) = 0
          DO j=1,n_modules
            DO k=1,SIZE(modules(j)%destinations) 
              IF (modules(j)%destinations(k) == i) THEN
                n_sources = n_sources + 1
                sources(n_sources) = j
              END IF
            END DO
          END DO
          ALLOCATE(modules(i)%sources(n_sources))
          ALLOCATE(modules(i)%last_pulse(n_sources))
          modules(i)%sources = sources(1:n_sources)
          modules(i)%last_pulse = ptype_low
        END IF
      END DO

    END SUBROUTINE read_modules

END PROGRAM pulse
