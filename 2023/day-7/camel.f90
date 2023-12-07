PROGRAM camel
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, int64, IOSTAT_END 
  
  IMPLICIT NONE

  INTEGER(KIND=int32)       :: input_file, ios
  INTEGER(KIND=int32), &
    PARAMETER               :: n_hand=5, n_cardvalues=14, n_handranks=7
  INTEGER(KIND=int32)       :: i, j, k, total
  INTEGER(KIND=int32)       :: n_hands, cardcount(n_hand)
  LOGICAL                   :: seen(n_cardvalues)
  LOGICAL, &
    ALLOCATABLE             :: sortmask(:)
  CHARACTER(LEN=n_hand)     :: hand
  CHARACTER(LEN=50)         :: longform
  INTEGER(KIND=int32), &
    ALLOCATABLE             :: ihand(:,:), bids(:), hand_rank(:), hand_order(:)
  INTEGER(KIND=int64), &
    ALLOCATABLE             :: hand_long(:)
  CHARACTER(LEN=1), &
    PARAMETER               :: card_to_int(n_cardvalues) = &
    ["%","2","3","4","5","6","7","8","9","T","J","Q","K","A"]

  ! Find number of hands
  OPEN(NEWUNIT=input_file, FILE="input.txt")
  n_hands = 0
  DO
    READ(input_file, *, IOSTAT=ios)
    IF (ios == IOSTAT_END) THEN
      EXIT
    END IF
    n_hands = n_hands + 1
  END DO
  REWIND(UNIT=input_file)
  ! Read bids and hands and convert hands to integer arrays
  ALLOCATE(ihand(n_hand, n_hands))
  ALLOCATE(bids(n_hands))
  DO i=1,n_hands
    READ(UNIT=input_file, FMT="(A,5I4)",IOSTAT=ios) hand, bids(i)
    DO j=1,n_hand
      DO k=2,n_cardvalues
        IF (hand(j:j) == card_to_int(k)) THEN
          ihand(j,i) = k
          EXIT
        END IF
      END DO
    END DO 
  END DO
  CLOSE(UNIT=input_file)

  ! Categorise hands
  ALLOCATE(hand_rank(n_hands))
  hand_rank(:) = 0
  DO i=1,n_hands
    seen(:) = .FALSE.
    cardcount(:) = 0
    DO j=1,n_hand
      IF (.NOT. seen(ihand(j,i))) THEN
        cardcount(j) = COUNT(ihand(:,i) == ihand(j,i))
        seen(ihand(j,i)) = .TRUE.
      END IF
    END DO
    ! I will go to hell for this
    IF (COUNT(cardcount == 5) == 1) THEN
      hand_rank(i) = 7   ! 5 of a kind
    ELSE IF (COUNT(cardcount == 4) == 1) THEN
      hand_rank(i) = 6   ! 4 of a kind
    ELSE IF (COUNT(cardcount == 3) == 1) THEN
      IF (COUNT(cardcount == 2) == 1) THEN
        hand_rank(i) = 5 ! Full house
      ELSE
        hand_rank(i) = 4 ! 3 of a kind
      END IF
    ELSE IF (COUNT(cardcount == 2) == 2) THEN
      hand_rank(i) = 3 ! 2 pair
    ELSE IF (COUNT(cardcount == 2) == 1) THEN
      hand_rank(i) = 2 ! Pair
    ELSE IF (ALL(cardcount == 1)) THEN 
      hand_rank(i) = 1   ! High
    ELSE
      PRINT*, "Unrankable hand?!"
      STOP
    END IF
  END DO

  ! Now create a longform hand number for each hand
  ! that leads with the hand rank and then the numbers in
  ! the hand as a single long number
  ALLOCATE(sortmask(n_hands))
  ALLOCATE(hand_long(n_hands))
  ALLOCATE(hand_order(n_hands))
  DO i=1,n_hands
    WRITE(longform, "(I1,5I2.2)") hand_rank(i),ihand(:,i)
    READ(longform, *) hand_long(i)
  END DO

  ! Sorting this array will provide the order for the hand
  sortmask(:) = .TRUE.
  DO i=1,n_hands
    hand_order(i) = MINLOC(hand_long, 1, sortmask)
    sortmask(hand_order(i)) = .FALSE.
  END DO

  ! Calculate the final result
  total = 0
  DO i=1,n_hands
    !PRINT*, i, ihand(:,hand_order(i)), hand_rank(hand_order(i)), hand_long(hand_order(i)), bids(hand_order(i))
    total = total + i*bids(hand_order(i))
    WRITE(*,"(I4,XA,XI0,XI0,XI3,XI10,XI0)") i, card_to_int(ihand(1,hand_order(i)))//&
      card_to_int(ihand(2,hand_order(i)))//&
      card_to_int(ihand(3,hand_order(i)))//&
      card_to_int(ihand(4,hand_order(i)))//&
      card_to_int(ihand(5,hand_order(i))), hand_rank(hand_order(i)), hand_long(hand_order(i)), bids(hand_order(i)), &
      i*bids(hand_order(i)), total
  END DO
 
  WRITE(*, "(A,I0)") "Total sum: ", total

  DEALLOCATE(hand_long)
  DEALLOCATE(sortmask)
  DEALLOCATE(hand_order)
  DEALLOCATE(hand_rank)
  DEALLOCATE(bids)
  DEALLOCATE(ihand)

END PROGRAM camel
