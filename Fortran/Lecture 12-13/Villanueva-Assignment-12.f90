PROGRAM assign12
        IMPLICIT NONE
        
        INTEGER(16) :: I0, I1
        INTEGER :: i, ios, N, seed
        INTEGER(16), ALLOCATABLE :: rand(:)
        REAL , ALLOCATABLE :: randr(:), xyrand(:,:)
        
        N = 1000        
!---------------------------------------------
        ! midsquare method
        ALLOCATE(rand(N))
            I0 = 167435
            DO i = 1, N     
                CALL midsquare(I0)   
                rand(i) = I0
            END DO

        OPEN(UNIT=10, IOSTAT = ios, FILE = 'midsquare.txt', ACTION ='WRITE')       
            IF (ios == 0) THEN
                DO i = 1, N     
                    WRITE(10, *) rand(i)
                END DO
                CLOSE(10)
            ELSE
                    PRINT '(a25)', 'Error: file not opened.'
            END IF
        
        DEALLOCATE(rand)
!-----------------------------------------------------------------
        ! LCM method
        ALLOCATE(rand(N))
            I0 = 742321
            DO i = 1, N     
              CALL LCM(I0)
              rand(i) = I0
            END DO

        OPEN(UNIT=10, IOSTAT = ios, FILE = 'LCM.txt', ACTION ='WRITE')       
            IF (ios == 0) THEN
                DO i = 1, N     
                    WRITE(10, *) rand(i)
                END DO
                CLOSE(10)
            ELSE
                    PRINT '(a25)', 'Error: file not opened.'
            END IF

        DEALLOCATE(rand)
!-----------------------------------------------------------------
        ! Number 2

        ALLOCATE(xyrand(2,N))        
        I0 = 343232
        I1 = 743
        DO i = 1, N         
            CALL LCM(I0)
            CALL LCM(I1) 
            xyrand(1,i) = I0
            xyrand(2,i) = I1 
        END DO
       
        OPEN(UNIT=10, IOSTAT = ios, FILE = 'LCMxy.txt', ACTION ='WRITE')       
            IF (ios == 0) THEN
                DO i = 1, N 
                  WRITE(10, *) xyrand(1,i)/(2147483647.0), xyrand(2,i)/(2147483647.0)   !normalized the random result
                END DO
                CLOSE(10)
            ELSE
                    PRINT '(a25)', 'Error: file not opened.'
            END IF
        DEALLOCATE(xyrand)
!-----------------------------------------------------------------
        !Number 3
        seed = 27
        CALL srand(seed)
        ALLOCATE(xyrand(2,N))        
        CALL random_number(xyrand)

        OPEN(UNIT=10, IOSTAT = ios, FILE = 'rnxy.txt', ACTION ='WRITE')       
            IF (ios == 0) THEN
                DO i = 1, N 
                  WRITE(10, *) xyrand(1,i), xyrand(2,i)
                END DO
                CLOSE(10)
            ELSE
                    PRINT '(a25)', 'Error: file not opened.'
            END IF
        DEALLOCATE(xyrand)
!-----------------------------------------------------------------
        !Number 4
        N = 100000
        ALLOCATE(randr(N))
        seed = 12
        !CALL srand(seed)
        CALL random_number(randr)
        OPEN(UNIT=10, IOSTAT = ios, FILE = 'f3x2.txt', ACTION ='WRITE')       
            IF (ios == 0) THEN
                DO i = 1, N 
                  WRITE(10, *) randr(i)**(1.0/3.0)
                END DO
                CLOSE(10)
            ELSE
                    PRINT '(a25)', 'Error: file not opened.'
            END IF
        DEALLOCATE(randr)
!-----------------------------------------------------------------
        !Number 5
        N = 10000
        seed = 1997        
        ALLOCATE(xyrand(2,N))
        CALL BMM(xyrand, N, seed)
        OPEN(UNIT=10, IOSTAT = ios, FILE = 'box-muller.txt', ACTION ='WRITE')       
            IF (ios == 0) THEN
                DO i = 1, N 
                  WRITE(10, *) xyrand(1, i), xyrand(2, i)
                END DO
                CLOSE(10)
            ELSE
                    PRINT '(a25)', 'Error: file not opened.'
            END IF
        DEALLOCATE(xyrand)

!-----------------------------------------------------------------
CONTAINS
        SUBROUTINE midsquare(I0)
            INTEGER(16), INTENT(INOUT) :: I0
            INTEGER :: d
                
            d = 3

            I0 = MOD(I0**2 / 10**d, 10**(d*2))
        END SUBROUTINE
!-----------------------------------------------------------------
        SUBROUTINE LCM(I0)
            INTEGER(16), INTENT(INOUT) :: I0
            INTEGER(16) :: a, c, m
                        
            a = 16807
            c = 0
            m = 2147483647

            I0 = MOD( (a*I0)+c , m)         
        END SUBROUTINE LCM
!-----------------------------------------------------------------
        SUBROUTINE BMM(xyrand, N, seed)
            REAL, INTENT(INOUT) :: xyrand(:, :) 
            INTEGER, INTENT(IN) :: N, seed
            REAL, PARAMETER :: pi = 4.0*ATAN(1.0)
            REAL :: Z1, Z2
                
            CALL srand(seed)
            CALL random_number(xyrand)
            DO i = 1, N        
                       
                Z1 = SQRT(-2*LOG(xyrand(1, i)))*COS(2*pi*xyrand(2, i))
                Z2 = SQRT(-2*LOG(xyrand(1, i)))*SIN(2*pi*xyrand(2, i))
                xyrand(1, i) = Z1
                xyrand(2, i) = Z2
            END DO
        END SUBROUTINE BMM

END PROGRAM assign12
