REAL FUNCTION func(x) RESULT(y)
        REAL, INTENT(IN) :: x
        y = x**2 + (4*x) - sin(x) + log(x/(4*1.0)) -3
END FUNCTION func

PROGRAM assign05
        IMPLICIT NONE
        REAL :: a, b, diff
        
        a = 0.5
        b = 1.5
        diff = abs(a-b)
        
                
        !PRINT*, 'Bisection Method'
        OPEN (unit = 10, file = 'bisec.txt')
        CALL bisec(a, b, diff)
        WRITE(10, *) 'The root is', a 
        WRITE(10, *) 'with an epsilon of', diff
        CLOSE(10)

        a = 0.5
        b = 1.5
        diff = abs(a-b)

       ! PRINT*, 'Regula Falsi '
        OPEN(unit = 20, file = 'falsi.txt')
        CALL falsi(a, b, diff)
        WRITE(20, *) 'The root is', a
        WRITE(20, *) 'with an epsilon of', diff
        CLOSE(20)

CONTAINS

        SUBROUTINE bisec(a, b, diff) 
                REAL, INTENT(INOUT) :: a, b, diff
                REAL, EXTERNAL :: func
                REAL :: c, fa, fb, fc, eps = 1.0E-6
                INTEGER :: i = 1

                WRITE(10, *) 'Iteration    ', 'Calculated Root    ', 'Epsilon'
        
                fa = func(a)
                fb = func(b)

                !IF (fa == 0) THEN
                        
                !IF (fb == 0) THEN

                IF ((fa*fb) > 0) THEN
                        PRINT*, 'No roots in the given interval'
                ELSE
                        DO
                                c = (a + b)/(2*1.0)
                                fc = func(c)
                                IF ((fc*fb) < 0) THEN
                                        a = c
                                        diff = abs(a-b)
                                        fa = func(a)
                                        !PRINT*, a, diff
                                        WRITE(10, *) i, c, diff
                                        IF (diff < eps) EXIT
                                ELSE
                                        b = c
                                        diff = abs(a-b)
                                        fb = func(b)
                                        !PRINT*, b, diff
                                        WRITE(10, *) i, c, diff
                                        IF (abs(a-b) < eps) EXIT
                                END IF
                                i = i+1
                        END DO
                END IF 
        END SUBROUTINE bisec

        SUBROUTINE falsi(a, b, diff) 
                REAL, INTENT(INOUT) :: a, b, diff
                REAL, EXTERNAL :: func
                REAL :: c, fa, fb, fc, eps = 1.0E-6
                INTEGER :: i = 1

                WRITE(20, *) 'Iteration     ', 'Calculated Root     ', 'Epsilon'
        
                fa = func(a)
                fb = func(b)

                !IF (fa == 0) THEN
                        
                !IF (fb == 0) THEN

                IF ((fa*fb) > 0) THEN
                        PRINT*, 'No roots in the given interval'
                ELSE
                        DO
                                c = ((fb*a) - (fa*b))/(fb - fa)
                                fc = func(c)
                                IF ((fc*fb) < 0) THEN
                                        a = c
                                        diff = abs(a-b)
                                        fa = func(a)
                                        !PRINT*, c, diff
                                        WRITE(20,*) i, c, diff
                                        IF (diff < eps) EXIT
                                ELSE
                                        b = c
                                        diff = abs(a-b)
                                        fb = func(b)
                                        !PRINT*, c, diff
                                        WRITE(20,*) i, c, diff
                                        IF (abs(a-b) < eps) EXIT
                                END IF
                                i = i + 1
                        END DO
                END IF 
        END SUBROUTINE falsi
END PROGRAM assign05
