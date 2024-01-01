PROGRAM day_23
  
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int32, IOSTAT_END
  IMPLICIT NONE
  INTEGER(KIND=int32), &
    ALLOCATABLE             :: grid(:,:), nodes(:,:), edge_map(:,:)
  INTEGER(KIND=int32), &
    PARAMETER               :: sym_forest=0, sym_path=1, &
                               sym_slope_n=2, sym_slope_e=3, sym_slope_s=4, &
                               sym_slope_w=5
  CHARACTER(LEN=1), &
    PARAMETER               :: symbol(0:5) = ["#", ".", "^", ">", "v", "<"]
  INTEGER(KIND=int32)       :: path

  CALL read_input("input.txt", grid) 

  CALL calc_nodes(grid, nodes)

  CALL calc_edge_map(grid, nodes, edge_map)

  CALL find_longest_path(edge_map, 1, path)

  WRITE(*,"(A,I0)") "Longest path: ", path
  
  DEALLOCATE(grid)

  CONTAINS

    RECURSIVE SUBROUTINE find_longest_path(edge_map, node, path)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: edge_map(:,:), node
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: path
      INTEGER(KIND=int32)       :: i, path_sub

      path = 0
      DO i=1,SIZE(edge_map,2)
        IF (edge_map(node,i) > 0) THEN
          IF (i /= SIZE(edge_map,1)) THEN
            ! Invoke this routine again on each possible route
            ! that can be taken from the current node - unless
            ! it is the route to the exit node
            CALL find_longest_path(edge_map, i, path_sub)
            IF (path_sub > 0) THEN
              ! If a route was returned, add our distance to
              ! the total length
              path_sub = path_sub + edge_map(node,i)
            END IF
            ! And now consider if this new path is longer
            ! than any returned by any other routes - if it
            ! is then keep it to return up the call stack
            IF (path_sub > path) THEN
              path = path_sub
            END IF
          ELSE
            ! If this *is* a path to the exit node then
            ! begin the return calls, starting with the
            ! final distance
            path = edge_map(node, i)
          END IF
        END IF
      END DO

    END SUBROUTINE find_longest_path

    SUBROUTINE calc_nodes(grid, nodes)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: grid(:,:)
      INTEGER(KIND=int32), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: nodes(:,:)
      INTEGER(KIND=int32)       :: path(SIZE(grid,1), SIZE(grid,2)), i, j, k
      LOGICAL                   :: node_map(SIZE(grid,1), SIZE(grid,2))

      WHERE (grid /= sym_forest)
        path = 1
      ELSEWHERE
        path = 0
      END WHERE

      path = EOSHIFT(path, -1, DIM=1) &
             + EOSHIFT(path,  1, DIM=1) &
             + EOSHIFT(path, -1, DIM=2) &
             + EOSHIFT(path,  1, DIM=2)

      node_map = ((path > 2) .AND. (grid /= sym_forest))
      IF (ALLOCATED(nodes)) DEALLOCATE(nodes)
      ALLOCATE(nodes(COUNT(node_map) + 2,2))
      ! Start point
      nodes(1,1) = 1
      nodes(1,2) = 2
      ! End point
      nodes(SIZE(nodes,1),1) = SIZE(grid,1)
      nodes(SIZE(nodes,1),2) = SIZE(grid,2) - 1

      ! Remaining points
      k = 1
      DO i=1,SIZE(node_map,1)
        DO j=1,SIZE(node_map,2)
          IF (node_map(i,j)) THEN
            k = k + 1
            nodes(k, 1) = i
            nodes(k, 2) = j
          END IF
        END DO
      END DO

    END SUBROUTINE calc_nodes

    SUBROUTINE calc_edge_map(grid, nodes, edge_map)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: grid(:,:), nodes(:,:)
      INTEGER(KIND=int32), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: edge_map(:,:)
      INTEGER(KIND=int32)       :: i, row, col, steps, node

      IF (.NOT. ALLOCATED(edge_map)) THEN
        ALLOCATE(edge_map(SIZE(nodes,1), SIZE(nodes,1)))
        edge_map(:,:) = 0
      END IF

      DO i=1,SIZE(nodes,1)-1
        ! Setup row/col at node
        row = nodes(i,1)
        col = nodes(i,2)
        
        ! Figure out which directions we can exit this node
        ! and walk to the next node, recording the number of
        ! steps taken in the map
        IF ((grid(row+1, col) == sym_path) &
          .OR. ((grid(row+1,col) == sym_slope_s))) THEN
          CALL walk(grid, nodes, i, 1, 0, node, steps)
          edge_map(i, node) = steps
        END IF
        IF ((grid(row-1, col) == sym_path) &
          .OR. ((grid(row-1,col) == sym_slope_n))) THEN
          CALL walk(grid, nodes, i, -1, 0, node, steps)
          edge_map(i, node) = steps
        END IF
        IF ((grid(row, col+1) == sym_path) &
          .OR. ((grid(row,col+1) == sym_slope_e))) THEN
          CALL walk(grid, nodes, i, 0, 1, node, steps)
          edge_map(i, node) = steps
        END IF
        IF ((grid(row, col-1) == sym_path) &
          .OR. ((grid(row,col-1) == sym_slope_w))) THEN
          CALL walk(grid, nodes, i, 0, -1, node, steps)
          edge_map(i, node) = steps
        END IF
      END DO

      END SUBROUTINE calc_edge_map

      SUBROUTINE walk(grid, nodes, node, rowdir, coldir, reached, steps)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: grid(:,:), nodes(:,:), node, rowdir, coldir
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: reached, steps
      LOGICAL                   :: visited(SIZE(grid,1), SIZE(grid,2))
      INTEGER(KIND=int32)       :: i, row, col

      ! Mark starting node as visited to avoid backtracking
      row = nodes(node,1)
      col = nodes(node,2)
      visited(:,:) = .FALSE.
      visited(row,col) = .TRUE.
      ! Move to starting path tile
      row = row + rowdir
      col = col + coldir
      steps = 1
      DO 
        ! Check for having arrived at a node
        IF (ANY((row == nodes(:,1)) .AND. (col == nodes(:,2)))) THEN
          EXIT
        END IF
        ! Consider all directions for a walkable tile
        DO i=-1,1,2
          IF ((grid(row+i, col) /= sym_forest) &
              .AND. (.NOT. visited(row+i, col))) THEN
            visited(row, col) = .TRUE.
            row = row + i
            steps = steps + 1
            EXIT
          END IF
          IF ((grid(row, col+i) /= sym_forest) &
              .AND. (.NOT. visited(row, col+i))) THEN
            visited(row, col) = .TRUE.
            col = col + i
            steps = steps + 1
            EXIT 
          END IF
        END DO
      END DO

      ! Once we exit work out which node we arrived at
      DO i=1,SIZE(nodes,1)
        IF ((row == nodes(i,1)) .AND. (col == nodes(i,2))) THEN
          reached = i
          EXIT
        END IF
      END DO

    END SUBROUTINE walk

    SUBROUTINE read_input(filename, grid)
      IMPLICIT NONE
      CHARACTER(LEN=*), &
        INTENT(IN)              :: filename
      INTEGER(KIND=int32), &
        ALLOCATABLE, &
        INTENT(INOUT)           :: grid(:,:)
      INTEGER(KIND=int32)       :: input_file, n_rows, n_columns

      IF (ALLOCATED(grid)) DEALLOCATE(grid)
      OPEN(NEWUNIT=input_file, FILE=filename, ACCESS="stream", &
        FORM="formatted")
      CALL calc_gridsize(input_file, n_rows, n_columns)
      ALLOCATE(grid(n_rows, n_columns))
      CALL read_grid(input_file, grid)
      CLOSE(UNIT=input_file)

    END SUBROUTINE read_input

    SUBROUTINE calc_gridsize(funit, rows, columns)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: columns, rows
      INTEGER(KIND=int32)       :: ios
      INTEGER(KIND=int32), &
        PARAMETER               :: line_len=500
      CHARACTER(LEN=line_len)   :: line

      columns = 0
      rows = 0
      DO 
        READ(UNIT=funit, FMT="(A)", IOSTAT=ios) line
        IF ((TRIM(line) == "") .OR. (ios == IOSTAT_END)) THEN
          EXIT
        END IF
        rows = rows + 1
        IF (rows == 1) THEN
          columns = LEN_TRIM(line)
        END IF
      END DO
      REWIND(funit)

    END SUBROUTINE calc_gridsize

    SUBROUTINE read_grid(funit, grid)
      IMPLICIT NONE
      INTEGER(KIND=int32), &
        INTENT(IN)              :: funit
      INTEGER(KIND=int32), &
        INTENT(OUT)             :: grid(:,:)
      INTEGER(KIND=int32), &
        PARAMETER               :: line_len=500
      CHARACTER(LEN=line_len)   :: line
      INTEGER(KIND=int32)       :: i, j, k

      grid(:,:) = -1
      DO i=1,SIZE(grid,1)
        READ(UNIT=funit, FMT="(A)") line
        DO j=1,SIZE(grid,2)
          DO k=LBOUND(symbol,DIM=1),UBOUND(symbol,DIM=1)
            IF (line(j:j) == symbol(k)) THEN
              grid(i,j) = k
              EXIT
            END IF
          END DO
        END DO
      END DO
      IF (ANY(grid == -1)) THEN
        WRITE(*,"(A)") "Grid read error"
        CALL ABORT()
      END IF

    END SUBROUTINE read_grid

END PROGRAM day_23
