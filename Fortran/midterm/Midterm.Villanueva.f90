REAL FUNCTION percentile(arr, c, m) RESULT(p95c)
        INTEGER, INTENT(IN) :: c, m
        REAL, INTENT(IN) :: arr(c)
        INTEGER :: p95
        
        p95 = ((m*c)/100)
        p95c = arr(p95)
END FUNCTION percentile


PROGRAM midterm
        IMPLICIT NONE
        
        INTEGER :: ios, iosr, i, lcount = 0
        REAL, DIMENSION (:), ALLOCATABLE :: array
        CHARACTER(100) :: line
        REAL, EXTERNAL :: percentile

        OPEN(UNIT = 10, IOSTAT = ios, FILE = 'sample.dat', STATUS ='old', ACTION = 'READ')
    
        IF (ios == 0) THEN
                DO 
                        READ(10, '(A)', IOSTAT = iosr) line
                        IF (iosr /= 0) THEN
                                EXIT
                        ELSE
                                lcount = lcount + 1
                        END IF
                END DO 

                ALLOCATE(array(lcount-1))
        
                REWIND(10)

                DO i = 1, lcount - 1
                        READ(10, *) array(i)
                        !PRINT*, array(i)
                END DO
                CLOSE(10)
                CALL sort(array, lcount-1)
                
                PRINT*, 'The 95th percentile is', percentile(array, lcount-1, 95)
                
        ELSE
                PRINT'(a25)', 'Error: file not opened.'
        END IF

CONTAINS
               
        SUBROUTINE sort(a,n)
                IMPLICIT NONE
                REAL, INTENT(inout), DIMENSION(n) :: a
                INTEGER, INTENT(IN) :: n
                REAL :: temp
                INTEGER :: i, j
                LOGICAL :: swapped
               
                DO j = n-1, 1, -1
                        swapped = .FALSE.
                        DO i = 1, j
                                IF (a(i) > a(i+1)) THEN
                                      temp = a(i)
                                      a(i) = a(i+1)
                                      a(i+1) = temp
                                      swapped = .TRUE.
                                END IF
                        END DO
                        IF (.NOT. swapped) EXIT
               END DO
        END SUBROUTINE sort


END PROGRAM midterm
