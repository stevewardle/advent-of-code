PROGRAM camel
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, int64, IOSTAT_END 
  
  IMPLICIT NONE

  INTEGER(KIND=int32)       :: input_file, ios
  INTEGER(KIND=int32), &
    PARAMETER               :: n_hand=5, n_cardvalues=14
  INTEGER(KIND=int32)       :: i, j, k, total, i_joker, n_jokers
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
    ["J","2","3","4","5","6","7","8","9","T","%","Q","K","A"]

  ! Find number of hands - basically have to skim the file to the end
  ! counting the lines then rewind it afterwards
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
  ! Read bids and hands and convert hands to integer arrays -
  ! Fortran doesn't do string processing well. Uses the parameter
  ! array above to translate "2" = 2 up to "A" = 14 and stores
  ! them in a 5-element long array of all hands
  ALLOCATE(ihand(n_hand, n_hands))
  ALLOCATE(bids(n_hands))
  DO i=1,n_hands
    READ(UNIT=input_file, FMT="(A5,XI4)",IOSTAT=ios) hand, bids(i)
    DO j=1,n_hand
      DO k=1,n_cardvalues
        IF (hand(j:j) == card_to_int(k)) THEN
          ihand(j,i) = k
          EXIT
        END IF
      END DO
    END DO 
  END DO
  CLOSE(UNIT=input_file)

  ! Categorise hands - each type of hand has a rank with 7 being
  ! the best (5 of a kind) down to 1 being a (card high) hand
  ALLOCATE(hand_rank(n_hands))
  hand_rank(:) = 0
  DO i=1,n_hands
    seen(:) = .FALSE.
    cardcount(:) = 0
    i_joker = 0
    ! Create a count of how many of each unique card value exist
    ! in each hand (with only one count value per card value)
    DO j=1,n_hand
      IF (.NOT. seen(ihand(j,i))) THEN
        ! Save the index of the joker/s if present 
        IF ((ihand(j,i)) == 1) THEN
          i_joker = j
        END IF
        cardcount(j) = COUNT(ihand(:,i) == ihand(j,i))
        seen(ihand(j,i)) = .TRUE.
      END IF
    END DO
    ! Manipulate the count based on any jokers - the optimal
    ! hand is created by having them duplicate whatever card is
    ! already most numerous, unless the hand is a "high" in which
    ! case they should duplicate the highest value card
    IF (i_joker > 0) THEN
      n_jokers = cardcount(i_joker)
      cardcount(i_joker) = 0
      IF (COUNT(cardcount == 1) == 5) THEN
        cardcount(MAXLOC(ihand(:,i),1)) = &
          cardcount(MAXLOC(ihand(:,i),1)) + n_jokers
      ELSE
        cardcount(MAXLOC(cardcount,1)) = &
          cardcount(MAXLOC(cardcount,1)) + n_jokers
      END IF
    END IF

    ! Can now apply the logic from the puzzle description more or less verbatim
    ! to figure out what rank the given hand has
    IF (COUNT(cardcount == 5) == 1) THEN
      hand_rank(i) = 7 ! 5 of a kind
    ELSE IF ((COUNT(cardcount == 4) == 1) .AND. (COUNT(cardcount == 1) == 1)) THEN
      hand_rank(i) = 6 ! 4 of a kind
    ELSE IF ((COUNT(cardcount == 3) == 1) .AND. (COUNT(cardcount == 2) == 1)) THEN
      hand_rank(i) = 5 ! Full house
    ELSE IF ((COUNT(cardcount == 3) == 1) .AND. (COUNT(cardcount == 1) == 2)) THEN
      hand_rank(i) = 4 ! 3 of a kind
    ELSE IF ((COUNT(cardcount == 2) == 2) .AND. (COUNT(cardcount == 1) == 1)) THEN
      hand_rank(i) = 3 ! 2 pair
    ELSE IF ((COUNT(cardcount == 2) == 1) .AND. (COUNT(cardcount == 1) == 3)) THEN
      hand_rank(i) = 2 ! Pair
    ELSE IF (COUNT(cardcount == 1) == 5) THEN 
      hand_rank(i) = 1 ! High
    ELSE
      PRINT*, "Unrankable hand?!"
      STOP
    END IF
  END DO

  ! Create a long-form hand number for each hand
  ! that leads with the hand rank and then the numbers in
  ! the hand as a single long number.  This makes it so
  ! the rank of the hand from above takes precedence followed
  ! by each card value in the hand also in order
  ALLOCATE(sortmask(n_hands))
  ALLOCATE(hand_long(n_hands))
  ALLOCATE(hand_order(n_hands))
  DO i=1,n_hands
    longform = ""
    WRITE(longform, "(I1,5I2.2)") hand_rank(i),ihand(:,i)
    READ(longform, *) hand_long(i)
  END DO

  ! A fairly inefficient sorting method using a minimum
  ! location intrinsic that masks out each minimum value
  ! as it finds it whilst collecting the index of each
  ! into an array
  sortmask(:) = .TRUE.
  DO i=1,n_hands
    hand_order(i) = MINLOC(hand_long, 1, sortmask)
    sortmask(hand_order(i)) = .FALSE.
  END DO

  ! Now we can iterate over the list of hands + bids in
  ! the order given by the above to calculate the result
  total = 0
  DO i=1,n_hands
    total = total + i*bids(hand_order(i))
  END DO
 
  WRITE(*, "(A,I0)") "Total sum: ", total

  DEALLOCATE(hand_long)
  DEALLOCATE(sortmask)
  DEALLOCATE(hand_order)
  DEALLOCATE(hand_rank)
  DEALLOCATE(bids)
  DEALLOCATE(ihand)

END PROGRAM camel
