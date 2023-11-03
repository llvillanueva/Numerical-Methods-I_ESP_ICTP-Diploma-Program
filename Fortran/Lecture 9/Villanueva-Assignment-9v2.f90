!Define the function
REAL FUNCTION func(x) RESULT(y)
        REAL, INTENT(IN) :: x
        y = EXP(x)
END FUNCTION func

!Initializing integral for N
REAL FUNCTION riemann_sum(a, b, N) RESULT(I)
        REAL, INTENT(IN) :: a, b
        INTEGER, INTENT(IN) :: N
        REAL, EXTERNAL :: func
        REAL :: h, xk
        INTEGER :: k

        h = (b - a)/(N*1.0)
        DO k = 0, N - 1                 !Summing all the rectangles
                        xk = a + (k*h)
                        I = I + (func(xk)*h)
        END DO
END FUNCTION riemann_sum

!Function for Riemann integration
REAL FUNCTION riemann(a, b, eps) RESULT(Iold)
        REAL, INTENT(IN) :: a, b, eps
        REAL, EXTERNAL :: func, riemann_sum
        REAL :: Inew, diff
        INTEGER :: N

        !Initialize the first integral approximation
        N = 7
        Inew = riemann_sum(a, b, N)
        
        !PRINT*, 'N               ', 'Integral           ', 'eps '
        DO              !loop for calculating the integral until a desired precision is obtained               
                N = N*2                
                Iold = riemann_sum(a, b, N)
                diff = ABS(Inew-Iold)
                PRINT*, 'N = ',N,'I = ', Iold,'eps = ', diff
                IF (diff < eps) EXIT            !Exit the program when the desired precision is obtained
                Inew = Iold         
        END DO
END FUNCTION riemann


REAL FUNCTION midpoint_sum(a, b, N) RESULT(I)
        REAL, INTENT(IN) :: a, b    
        INTEGER, INTENT(IN) :: N
        REAL, EXTERNAL :: func
        REAL :: h, xk, xk1
        INTEGER :: k

        h = (b - a)/(N*1.0)
        DO k = 0, N - 1                 !Summing all the rectangles
                        xk = a + (k*h)
                        xk1 = a + ((k+1)*h)
                        I = I + (func((xk+xk1)/(2*1.0))*h)
        END DO
END FUNCTION midpoint_sum

!Function for rienmann sum but using the midpoint
REAL FUNCTION midpoint(a, b, eps) RESULT(Iold)
        REAL, INTENT(IN) :: a, b, eps
        REAL, EXTERNAL :: func, midpoint_sum
        REAL :: Inew, diff
        INTEGER :: N

        !Initialize the first integral approximation
        N = 7
        Inew = midpoint_sum(a, b, N)

        !PRINT*, 'N               ', 'Integral           ', 'eps '
        DO              !loop for calculating the integral until a desired precision is obtained
                N = N*2                
                Iold = midpoint_sum(a, b, N)
                diff = ABS(Inew-Iold)
                PRINT*, 'N = ',N,'I = ', Iold,'eps = ', diff
                IF (diff < eps) EXIT            !Exit the program when the desired precision is obtained
                Inew = Iold  
        END DO
END FUNCTION midpoint

REAL FUNCTION trapeze_sum(a, b, N) RESULT(I)
        REAL, INTENT(IN) :: a, b
        INTEGER, INTENT(IN) :: N
        REAL, EXTERNAL :: func
        REAL :: h, xk, xk1
        INTEGER :: k

        h = (b - a)/(N*1.0)
        DO k = 0, N - 1                 !Summing all the rectangles
            xk = a + (k*h)
            xk1 = a + ((k+1)*h)
            I = I + ((func(xk)+func(xk1))*(h/(2*1.0)))
        END DO
END FUNCTION trapeze_sum


!Function for Trapeze method
REAL FUNCTION trapeze(a, b, eps) RESULT(Iold)
        REAL, INTENT(IN) :: a, b, eps
        REAL, EXTERNAL :: func, trapeze_sum
        REAL :: Inew, diff
        INTEGER :: N

        !Initialize the first integral approximation
        N = 7
        Inew = trapeze_sum(a, b, N)

        !PRINT*, 'N               ', 'Integral           ', 'eps '
        DO              !loop for calculating the integral until a desired precision is obtained
                N = N*2                
                Iold = trapeze_sum(a, b, N)
                diff = ABS(Inew-Iold)
                PRINT*, 'N = ',N,'I = ', Iold,'eps = ', diff
                IF (diff < eps) EXIT            !Exit the program when the desired precision is obtained
                Inew = Iold  
        END DO
END FUNCTION trapeze

REAL FUNCTION simpson_sum(a, b, N) RESULT(I)
        REAL, INTENT(IN) :: a, b
        INTEGER, INTENT(IN) :: N
        REAL, EXTERNAL :: func
        REAL :: h, xk, xk1
        INTEGER :: k

                h = (b - a)/(N*1.0)
                I = (func(b) - func(a))*(h/(6*1.0))
                DO k = 0, N - 1                 !Summing all the rectangles
                        xk = a + (k*h)
                        xk1 = a + ((k+1)*h)
                        I = I + ((2*func(xk)) + (4*func((xk+xk1)/(2*1.0))))*(h/(6*1.0))
                END DO
                diff = ABS(Inew-Iold)
END FUNCTION simpson_sum


!Function for Simpson method
REAL FUNCTION Simpson(a, b, eps) RESULT(Iold)
        REAL, INTENT(IN) :: a, b, eps
        REAL, EXTERNAL :: func, simpson_sum
        REAL :: Inew, diff
        INTEGER :: N

        !Initialize the first integral approximation
        N = 7
        Inew = simpson_sum(a, b, N)

        !PRINT*, 'N               ', 'Integral           ', 'eps '
        DO              !loop for calculating the integral until a desired precision is obtained
                N = N*2                
                Iold = simpson_sum(a, b, N)
                diff = ABS(Inew-Iold)
                PRINT*, 'N = ',N,'I = ', Iold,'eps = ', diff
                IF (diff < eps) EXIT            !Exit the program when the desired precision is obtained
                Inew = Iold  
        END DO
END FUNCTION Simpson

PROGRAM assign09
        IMPLICIT NONE
        REAL :: a, b, eps, Irie, Imid, Itrap, Isimp
        REAL, EXTERNAL :: riemann, midpoint, trapeze, Simpson
        
        a = 0
        b = 1.0
        eps = 10E-5    

        PRINT*, 'Riemann Sum'
        Irie = riemann(a, b, eps)
        PRINT*, 'The integral is ', Irie
        
        PRINT*, '------------------------------'

        PRINT*, 'Midpoint'
        Imid = midpoint(a, b, eps)   
        PRINT*, 'The integral is ', Imid

        PRINT*, '------------------------------'

        PRINT*, 'Trapeze'
        Itrap = trapeze(a, b, eps)      
        PRINT*, 'The integral is ', Itrap

        PRINT*, '------------------------------'

        PRINT*, 'Simpson'
        Isimp = Simpson(a, b, eps)            
        PRINT*, 'The integral is ', Isimp
END PROGRAM assign09
