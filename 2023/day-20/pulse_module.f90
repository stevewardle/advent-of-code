MODULE pulse_module_mod
  USE, INTRINSIC ::  ISO_FORTRAN_ENV, ONLY: int32
  IMPLICIT NONE
  PRIVATE

  ! The different types of pulses
  INTEGER(KIND=int32), PUBLIC, &
    PARAMETER :: ptype_low=1, ptype_high=2

  ! Type which can store the pulses and destinations
  ! on the queue
  TYPE, PUBLIC :: pulse_send
    INTEGER(KIND=int32) :: ptype
    INTEGER(KIND=int32) :: source
    INTEGER(KIND=int32) :: destination
  END TYPE pulse_send 

  ! Abstract base class for the pulse modules
  TYPE, ABSTRACT, PUBLIC :: pulse_module
    CHARACTER(LEN=:), ALLOCATABLE    :: name
    INTEGER(KIND=int32), ALLOCATABLE :: destinations(:)
  END TYPE pulse_module

  ! Define the different module types
  TYPE, PUBLIC, EXTENDS(pulse_module) :: flip_flop_module
    LOGICAL  :: active = .FALSE.
  END TYPE flip_flop_module

  TYPE, PUBLIC, EXTENDS(pulse_module) :: conjunction_module
    INTEGER(KIND=int32), ALLOCATABLE :: sources(:)
    INTEGER(KIND=int32), ALLOCATABLE :: recents(:)
  END TYPE conjunction_module

  TYPE, PUBLIC, EXTENDS(pulse_module) :: broadcast_module
  END TYPE broadcast_module

END MODULE pulse_module_mod
