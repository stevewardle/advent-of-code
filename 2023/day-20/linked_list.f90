MODULE linked_list_mod
  USE, INTRINSIC::  ISO_FORTRAN_ENV, ONLY: int32
  IMPLICIT NONE
  PRIVATE

  TYPE, PUBLIC :: ll_node
    CLASS(*),      POINTER :: value => NULL()
    TYPE(ll_node), POINTER :: next  => NULL()
  CONTAINS
    FINAL :: node_destroy
  END TYPE ll_node

  TYPE, PUBLIC :: ll_list
    PRIVATE
    INTEGER(KIND=int32)    :: n_elements = 0_int32
    TYPE(ll_node), POINTER :: head => NULL()
    TYPE(ll_node), POINTER :: tail => NULL()
    TYPE(ll_node), POINTER :: selected => NULL()
  CONTAINS
    PROCEDURE :: length
    PROCEDURE :: first
    PROCEDURE :: last
    PROCEDURE :: get
    PROCEDURE :: add
    PROCEDURE :: next
    PROCEDURE :: current
    PROCEDURE :: more
    PROCEDURE :: reset
    FINAL     :: list_destroy
  END TYPE ll_list

  CONTAINS

  SUBROUTINE node_destroy(this)
    IMPLICIT NONE
    TYPE(ll_node), INTENT(INOUT) :: this
    IF (ASSOCIATED(this%value)) THEN
      DEALLOCATE(this%value)
      NULLIFY(this%value)
      NULLIFY(this%next)
    END IF
  END SUBROUTINE node_destroy

  FUNCTION length(this) RESULT(n_elements)
    IMPLICIT NONE
    CLASS(ll_list), INTENT(IN) :: this
    INTEGER(KIND=int32)        :: n_elements
    n_elements = this%n_elements
  END FUNCTION length

  FUNCTION first(this) RESULT(node)
    IMPLICIT NONE
    CLASS(ll_list), INTENT(IN) :: this
    TYPE(ll_node),  POINTER    :: node
    node => this%head
  END FUNCTION first

  FUNCTION last(this) RESULT(node)
    IMPLICIT NONE
    CLASS(ll_list), INTENT(IN) :: this
    TYPE(ll_node),  POINTER    :: node
    node => this%tail
  END FUNCTION last

  FUNCTION current(this) RESULT(node)
    IMPLICIT NONE
    CLASS(ll_list), INTENT(IN) :: this
    TYPE(ll_node),  POINTER    :: node
    node => this%selected
  END FUNCTION current  

  SUBROUTINE reset(this)
    IMPLICIT NONE
    CLASS(ll_list), INTENT(INOUT) :: this
    this%selected => this%head
  END SUBROUTINE reset

  SUBROUTINE next(this)
    IMPLICIT NONE
    CLASS(ll_list), INTENT(INOUT) :: this
    this%selected => this%selected%next
  END SUBROUTINE next

  FUNCTION more(this) RESULT(yes)
    IMPLICIT NONE
    CLASS(ll_list), INTENT(IN) :: this
    LOGICAL                    :: yes
    yes = .FALSE.
    IF (ASSOCIATED(this%selected%next)) yes = .TRUE.
  END FUNCTION more

  FUNCTION get(this, index) RESULT(node)
    IMPLICIT NONE
    CLASS(ll_list),      INTENT(IN) :: this
    INTEGER(KIND=int32), INTENT(IN) :: index
    TYPE(ll_node),       POINTER    :: node
    INTEGER(KIND=int32) :: i
    NULLIFY(node)
    IF (index > 0_int32 .AND. index <= this%n_elements) THEN
      node => this%head
      DO i=1,index-1
        node => node%next
      END DO
    END IF
  END FUNCTION get

  SUBROUTINE add(this, value)
    IMPLICIT NONE
    CLASS(ll_list), INTENT(INOUT) :: this
    CLASS(*), INTENT(IN), POINTER :: value

    TYPE(ll_node), POINTER :: new

    ALLOCATE(new)
    new%value => value
    new%next => NULL()

    this%n_elements = this%n_elements + 1_int32
    IF (.NOT. ASSOCIATED(this%head)) THEN
      this%head => new
      this%tail => new
    ELSE
      this%tail%next => new
      this%tail => new
    END IF

  END SUBROUTINE add



  SUBROUTINE list_destroy(this)
    IMPLICIT NONE
    TYPE(ll_list), INTENT(INOUT) :: this

    TYPE(ll_node), POINTER :: current
    INTEGER(KIND=int32)    :: i

    current => this%head
    DO i=1,this%n_elements
      this%head => current%next
      DEALLOCATE(current)
      NULLIFY(current)
      this%n_elements = this%n_elements - 1_int32
    END DO
    NULLIFY(this%head)
    NULLIFY(this%tail)

  END SUBROUTINE list_destroy

END MODULE linked_list_mod
