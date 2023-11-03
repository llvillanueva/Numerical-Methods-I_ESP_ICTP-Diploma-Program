REAL FUNCTION func(x) RESULT(y) 
        REAL, INTENT(IN) :: x
        y = 2*LOG(x) - x + 4
END FUNCTION func

PROGRAM assign07
        IMPLICIT NONE
        !declaration
        REAL :: x0, x1, eps, diff, a, b        
        INTEGER :: ios

        PRINT '(a)', 'Enter first estimate x0: '
        READ*, a
        PRINT '(a)', 'Enter first estimate x1: '
        READ*, b
        PRINT '(a)', 'Enter your desired precision: '
        READ*, eps
        
        x0 = a
        x1 = b

        OPEN(UNIT=10, IOSTAT = ios, FILE = 'bisec.txt', ACTION ='WRITE')       
        IF (ios == 0) THEN
            WRITE(10, *) 'Bisection Method' 
            CALL bisec(x0, x1, eps) 
            CLOSE(10)
        ELSE
            PRINT '(a25)', 'Error: file not opened.'
        END IF

        !reset the value of a and b
        x0 = a
        x1 = b

        OPEN(UNIT=10, IOSTAT = ios, FILE = 'secant.txt', ACTION ='WRITE')       
        IF (ios == 0) THEN
            WRITE(10, *) 'Secant Method' 
            CALL secant(x0, x1, eps) 
            CLOSE(10)
        ELSE
            PRINT '(a25)', 'Error: file not opened.'
        END IF

CONTAINS

        SUBROUTINE bisec(x0, x1, eps) 
                ! declaration
                REAL, INTENT(INOUT) :: x1, x0
                REAL, INTENT(IN) :: eps
                REAL, EXTERNAL :: func
                REAL :: x2, fx0, fx1, fx2, diff
                INTEGER :: i = 1
                CHARACTER(13) :: s1 = "iteration", s2 = 'calc_root', s3 = 'epsilon'

                !Writing the header
                WRITE(10, *) s1, s2 ,s3
        
                !calculating the f(a) and f(b)
                DO
                
                fx0 = func(x0)
                fx1 = func(x1)

                IF ((fx0*fx1) > 0) THEN
                    PRINT*, 'Algorithm Error'
                    EXIT
                ELSE
                        !Bisection method
                    x2 = (x0 + x1)/(2*1.0)
                    fx2 = func(x2)
                        IF ((fx2*fx1) <= 0) THEN
                            x0 = x2
                            diff = abs(x0-x1)
                            !fx0 = func(x0)
                                        !PRINT*, a, diff
                            IF (diff < eps) EXIT
                            WRITE(10, *) i, x1, diff
                        ELSE
                            x1 = x2
                            diff = abs(x0-x1)
                            !fx1 = func(x1)
                                        !PRINT*, b, diff
                            IF (diff < eps) EXIT
                            WRITE(10, *) i, x1, diff
                        END IF
                        i = i+1
                END IF 
                END DO
                WRITE(10, *) 'The root is: ', x1
                WRITE(10, *) 'with an error of', diff
                WRITE(10, *) 'in ', i, ' iteration.'
                PRINT*, 'The result of Secant method was successfully written in bisec.txt'
        END SUBROUTINE bisec

        SUBROUTINE secant(x0, x1, eps)
                REAL, INTENT(INOUT) :: x1, x0
                REAL, INTENT(IN) :: eps
                REAL, EXTERNAL :: func
                REAL :: x2, fx0, fx1, diff
                INTEGER :: i = 1
                CHARACTER(13) :: s1 = "iteration", s2 = 'calc_root', s3 = 'epsilon'

                WRITE(10, *) 'Secant Method' 
                WRITE(10, *) s1, s2 ,s3

                DO
                        fx0 = func(x0)
                        fx1 = func(x1)

                        IF ((ABS(fx0 - fx1) < TINY(x0)) .OR. (ISNAN(fx0)) .OR. (ISNAN(fx0))) THEN
                                PRINT*, 'Algorithm Failure'
                                !PRINT*, 'Try different first estimate: '
                                !READ*, x0
                                !i = 1
                                EXIT
                        ELSE
                                x2 = x1 - fx1*((x1-x0)/(fx1-fx0))
                                diff = ABS(x2 - x1)
                                IF (diff < eps) THEN
                                        x1 = x2
                                        EXIT
                                ELSE
                                        x0 = x1
                                        x1 = x2
                                        WRITE(10, *) i, x0, diff
                                        i = i + 1
                                END IF
                        END IF
                END DO
                WRITE(10, *) 'The root is: ', x1
                WRITE(10, *) 'with an error of', diff
                WRITE(10, *) 'in ', i, ' iteration.'
                PRINT*, 'The result of Secant method was successfully written in secant.txt'
        END SUBROUTINE secant
END PROGRAM assign07
