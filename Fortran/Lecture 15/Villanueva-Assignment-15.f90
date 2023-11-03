REAL FUNCTION GE(x, xav, s) RESULT(y)
    REAL, INTENT(IN) :: x, xav, s
    REAL , PARAMETER :: pi = 4.0*ATAN(1.0)
    y = (1/(s*SQRT(2*pi)))*EXP((-0.5)*((x-xav)/s)**2)
END FUNCTION GE

REAL FUNCTION func(x) RESULT(y)
    REAL, INTENT(IN) :: x
    REAL, EXTERNAL :: GE
    y = 15*x*x*GE(x, -0.5, 0.25) + 13*GE(x, -1.5, 0.3)+ 7*GE(x, 3.0, 1.0)
END FUNCTION func


PROGRAM assign15
    IMPLICIT NONE
    INTEGER :: i, ios, N, j, Nbin, M
    REAL, ALLOCATABLE :: randn(:), H(:), parr(:), xp(:)
    REAL :: C25, C75, IQR, fdr_dx, Cmax, Cmin
    
    
    N = 10E4
    ALLOCATE(randn(N))

    CALL rejection(N, randn)



    !data for plotting the datapoints (trial vs. points)
    OPEN(UNIT=10, IOSTAT = ios, FILE = 'fx.txt', ACTION ='WRITE')       
            IF (ios == 0) THEN
                DO i = 1, N
                    WRITE(10, *) i, randn(i)
                END DO
                CLOSE(10)
            ELSE
                    PRINT '(a25)', 'Error: file not opened.'
            END IF
    
    CALL sort(randn, N)
       

    !data for plotting the emperical cumulative distribution
    OPEN(UNIT=10, IOSTAT = ios, FILE = 'C(x).txt', ACTION ='WRITE')       
            IF (ios == 0) THEN
                DO i = 1, N
                    DO j = 1, N
                        IF (randn(i) < randn(j)) EXIT
                    END DO
                    WRITE(10, *) randn(i), (j-1)/(N*1.0)
                END DO
                CLOSE(10)
            ELSE
                    PRINT '(a25)', 'Error: file not opened.'
            END IF

    !Number 2
    Cmin = randn(1)
    Cmax = randn(N) 
    C25 = randn(FLOOR(N/4.0))
    C75 = randn(FLOOR(3*N/4.0))
    IQR = C75 - C25

    fdr_dx = IQR*(2/(N)**(1/3.0))
    Nbin = FLOOR((Cmax-Cmin)/fdr_dx) + 1
    fdr_dx = (Cmax-Cmin)/(Nbin*1.0)
    ALLOCATE(H(Nbin))
    H = 0
    PRINT*, C25, C75, IQR, fdr_dx, Nbin, Cmin, Cmax

    !data for plotting the freedman draconian rule
    OPEN(UNIT=10, IOSTAT = ios, FILE = 'A15hist.txt', ACTION ='WRITE')       
            IF (ios == 0) THEN
                DO i = 1, N
                    j = FLOOR((randn(i)-Cmin)/fdr_dx) + 1
                    H(j) = H(j) + 1
                END DO
                DO i = 1, Nbin
                    WRITE(10, *) Cmin + ((i-0.5)*fdr_dx), H(i)/(N*fdr_dx)
                END DO
                CLOSE(10)
            ELSE
                PRINT '(a25)', 'Error: file not opened.'
            END IF    
       
    !Number 3
    M = 10E5
    ALLOCATE(parr(M))
    ALLOCATE(xp(M))

    !PRINT*, "SUCCESS"
    DO i = 1, M
        xp(i) = Cmin + i*(Cmax-Cmin)/(1.0*M)
    END DO     
    !PRINT*, "SUCCESS"
    CALL p(xp, randn, IQR, N, M, parr)
    !PRINT*, "SUCCESS"

    !data for p(x)
    OPEN(UNIT=10, IOSTAT = ios, FILE = 'GKDE.txt', ACTION ='WRITE')       
            IF (ios == 0) THEN
                DO i = 1, M
                    WRITE(10, *) xp(i), parr(i)
                END DO
                CLOSE(10)
            ELSE
                    PRINT '(a25)', 'Error: file not opened.'
            END IF
    !PRINT*, "SUCCESS"




CONTAINS
    !subroutine for rejection methdd
    SUBROUTINE rejection(N, randn)
        INTEGER, INTENT(IN) :: N
        REAL, INTENT(OUT) :: randn(:)
        REAL, EXTERNAL :: func
        REAL :: x, r, A
        INTEGER :: i 
        INTEGER, PARAMETER :: seed = 27
        CALL srand(seed)
        i = 1
        DO 
            !CALL random_seed()
            !CALL random_number(x)
            !CALL random_number(r)
            x = rand()
            r = rand()    
            x = x*(10-(-10)) - 10
            A = func(x)/(17.5)
            IF (r <= A) THEN
                randn(i) = x
                !PRINT*, r
                i = i + 1
                IF (N == i) EXIT
            END IF
        END DO  
    END SUBROUTINE rejection

    !subroutine for sorting
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

    !subroutine for p(x)
    SUBROUTINE p(xp, randn, IQR, N, M, parr)
        INTEGER, INTENT(IN) :: M, N
        REAL, INTENT(IN) :: IQR, randn(:), xp(:)
        REAL, INTENT(INOUT) :: parr(:)
        REAL, EXTERNAL :: GE
        REAL :: A, s, y, stddev
        INTEGER :: i, j
        CALL std(randn, N, stddev)
        A = MIN(stddev, IQR/1.34)
        s = 0.9*A/N**(0.2)
        DO i=1, M
            y = 0
            DO j =1, N
                y = y + GE(xp(i), randn(j), s)
            END DO
            parr(i) = y/(N*1.0)
        END DO
    END SUBROUTINE p

    !subroutine for calculating standard dev of the data
    SUBROUTINE std(randn, N, stddev)
        INTEGER, INTENT(IN) :: N
        REAL, INTENT(IN) :: randn(N)
        REAL, INTENT(OUT) :: stddev
        REAL :: variance, mean
        INTEGER :: i
        mean = SUM(randn)/(N*1.0)
        variance = SUM((randn - mean)**2)
        variance = variance/(N*1.0)
        stddev = SQRT(variance)
    END SUBROUTINE std

END PROGRAM assign15
