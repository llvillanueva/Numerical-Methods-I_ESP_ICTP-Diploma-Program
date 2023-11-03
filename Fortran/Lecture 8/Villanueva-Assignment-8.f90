REAL FUNCTION func(x) RESULT(y)
        REAL, INTENT(IN) :: x
        y = EXP(x)
END FUNCTION func


REAL FUNCTION riemann(a, b, N) RESULT(Iold)
        REAL, INTENT(IN) :: a, b
        INTEGER, INTENT(IN) :: N
        REAL, EXTERNAL :: func
        REAL :: h, xk
        INTEGER :: k
                h = (b - a)/(N*1.0)
                Iold = 0
                DO k = 0, N - 1
                        xk = a + (k*h)
                        Iold = Iold + (func(xk)*h)
                END DO

END FUNCTION riemann


PROGRAM assign08
        IMPLICIT NONE
        REAL :: a, b, eps, Inew, diff, Iold
        INTEGER :: N
        REAL, EXTERNAL :: func, riemann

        a = 0
        b = 1.0
        eps = 10E-5
        
        Inew = func(b-a)*(b-a)
        N = 1
        PRINT*, 'N               ', 'Integral             ', 'eps'
        DO
            Iold = riemann(a, b, N)
            diff = ABS(Inew-Iold)
            PRINT*, N, Iold, diff
            IF (diff < eps) EXIT
            Inew = Iold
            N = 2*N      
        END DO

        Inew = Iold        
            
        PRINT*, 'The integral of exp() from ', a,' to ', b, ' is ', Inew, ' with difference of ', diff
END PROGRAM assign08
