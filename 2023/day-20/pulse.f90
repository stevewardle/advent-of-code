PROGRAM pulse
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  IMPLICIT NONE

  ! The different types of pulses
  INTEGER(KIND=int32), &
    PARAMETER :: ptype_low=1, ptype_high=2

  INTEGER(KIND=int32), &
    PARAMETER :: mod_broadcast=1, mod_conjunction=2, &
                 mod_flip_flop=3, mod_exit=4

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
  END TYPE pulse_module

  TYPE(pulse_module), &
    ALLOCATABLE         :: pulse_modules(:)

  INTEGER(KIND=int32)   :: i, j
  CHARACTER(LEN=1) :: mod_type

  CALL read_input("input.txt", pulse_modules) 

  CONTAINS

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
      CHARACTER(LEN=2), &
        ALLOCATABLE             :: destinations(:)
      INTEGER(KIND=int32)       :: sources(10)
      CHARACTER(LEN=linelen), &
        ALLOCATABLE             :: destination_names(:)
      INTEGER(KIND=int32)       :: i, j, k, next, &
                                   roffset, n_destinations, &
                                   n_modules, n_sources
      n_modules = 1 ! Allow one extra for exit module
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

      DO i=1,n_modules-1
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
          CASE("&")
            modules(i)%mod_type = mod_conjunction
          END SELECT
          modules(i)%name = modules(i)%name(2:LEN(modules(i)%name))
        END IF
        ! Save destinations
        READ(line(SCAN(line, ">")+1:linelen), "(A)") destination_names(i)
      END DO

      ! Add exit module
      modules(n_modules)%mod_type = mod_exit
      modules(n_modules)%name = "rx"

      DO i=1,n_modules
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
        DO j=1,SIZE(destinations)
          DO k=1,n_modules 
            IF (modules(k)%name == destinations(j)) THEN
              modules(i)%destinations(j) = k
            END IF
          END DO
        END DO
      END DO

      ! Finally need to populate the source of the
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
