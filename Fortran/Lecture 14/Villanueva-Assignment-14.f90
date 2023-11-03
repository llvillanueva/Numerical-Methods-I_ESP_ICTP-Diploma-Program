REAL FUNCTION func(x) RESULT(y)
    REAL, INTENT(IN) :: x
    y = 3*x**2
END FUNCTION func

REAL FUNCTION pi(N) RESULT(A)
    INTEGER, INTENT(IN) :: N 
    REAL :: x, y
    INTEGER :: i, Nc
    !INTEGER, PARAMETER :: seed = 27
    !CALL srand(seed)

    Nc = 0
    A = 0
    DO i = 1, N
        CALL random_number(x)
        x = (2*x) - 1
        CALL random_number(y)
        y = (2*y) - 1
        IF (x**2 + y**2 <= 1)  Nc = Nc + 1
    END DO
    A = 4*(Nc/(N*1.0))
    !PRINT*, A
END FUNCTION pi

REAL FUNCTION cMCm(N) RESULT(A)
    INTEGER, INTENT(IN) :: N 
    REAL :: x
    INTEGER :: i
    !INTEGER, PARAMETER :: seed = 27
    !CALL srand(seed)

    A = 0
    DO i = 1, N
        CALL random_number(x)
        A = A + EXP(1.0*(-x**2))
    END DO
    A = A/(N*1.0)
END FUNCTION cMCm

REAL FUNCTION picmcm(N) RESULT(A)
    INTEGER, INTENT(IN) :: N 
    REAL :: x, y, xy(2, N)
    INTEGER :: i
    !INTEGER, PARAMETER :: seed = 27
    !CALL srand(seed)

    A = 0
    CALL random_number(xy)
    DO i = 1, N
        x = (2*xy(1,i)) - 1
        y = (2*xy(2,i)) - 1
        IF (x**2 + y**2 <= 1)  A = A + 1
    END DO
    A = (A/(N*1.0))*(2)*(2)
END FUNCTION picmcm

PROGRAM assign14
    IMPLICIT NONE
    INTEGER :: i, ios, N
    REAL :: A
    REAL, ALLOCATABLE :: randn(:)
    REAL, EXTERNAL :: pi, cMCm, picmcm

    !Number 1
    N = 10E4
    ALLOCATE(randn(N))
    CALL rejection(N, randn)

    OPEN(UNIT=10, IOSTAT = ios, FILE = 'rejection.txt', ACTION ='WRITE')       
            IF (ios == 0) THEN
                DO i = 1, N     
                    WRITE(10, *) randn(i)
                END DO
                CLOSE(10)
            ELSE
                    PRINT '(a25)', 'Error: file not opened.'
            END IF
    DEALLOCATE(randn)

    !Number 2
    OPEN(UNIT=10, IOSTAT = ios, FILE = 'pi.txt', ACTION ='WRITE')       
            IF (ios == 0) THEN
                DO i = 1000, 1000000, 5000
                    A = pi(i)     
                    WRITE(10, *) i/(1000.0), A
                END DO
                CLOSE(10)
            ELSE
                    PRINT '(a25)', 'Error: file not opened.'
            END IF

    !Number 3
    N = 10E5
    A = cMCm(N)
    PRINT*, 'The integral is estimated to be: ', A 

    !Number 4
    N = 10E4
    A = picmcm(N)
    PRINT*, 'The value of pi is estimated to be: ', A 

CONTAINS
    SUBROUTINE rejection(N, randn)
        INTEGER, INTENT(IN) :: N 
        REAL, INTENT(OUT) :: randn(:)
        REAL, EXTERNAL :: func
        REAL :: x, r, A
        INTEGER :: i 

        i = 1
        DO 
            CALL random_number(x)
            !PRINT*, x
            CALL random_number(r)
            !PRINT*, r
            A = func(x)/(3.0)

            IF (r <= A) THEN
                randn(i) = x
                !PRINT*, r
                i = i + 1
                IF (N == i) EXIT
            END IF
        END DO  
    END SUBROUTINE rejection


END PROGRAM assign14
