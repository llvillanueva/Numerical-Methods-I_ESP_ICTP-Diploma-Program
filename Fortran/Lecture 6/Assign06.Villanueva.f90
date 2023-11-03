REAL FUNCTION func(x) RESULT(y) 
        REAL, INTENT(IN) :: x
        y = abs(tan(x)) - ((x**3)/3) - 1
END FUNCTION func

REAL FUNCTION dfunc(x) RESULT(y)
        REAL, INTENT(IN) :: x
        y = ((acos(x))**2)*(tan(x)/abs(tan(x))) - x**2
END FUNCTION dfunc

PROGRAM assign06
        IMPLICIT NONE
        REAL :: x0, eps = 10E-6
        !INTEGER :: ios 
        
        !OPEN(UNIT=10, IOSTAT = ios, FILE = 'Newton.txt', ACTION ='WRITE')
        PRINT '(a25)', 'Enter your first estimate: '
        READ*, x0
        
        CALL newton(x0, eps)        
        !IF (ios == 0) THEN
        PRINT*, 'The root is: ', x0
        !ELSE
        !        PRINT '(a25)', 'Error: file not opened.'
        !END IF

CONTAINS
        SUBROUTINE newton(x0, eps)
                REAL, INTENT(INOUT) :: x0
                REAL, INTENT(IN) :: eps
                REAL, EXTERNAL :: func, dfunc
                REAL :: x1, fx0, dfx0
                INTEGER :: i

        
                fx0 = func(x0)
                dfx0 = dfunc(x0)
 
                IF (ABS(dfx0) < TINY(x0)) THEN
                        PRINT*, 'Algorithm Failure'
                ELSE
                        x1 = x0 - (fx0/dfx0)
                        IF (ABS(x1 - x0) < eps) THEN
                                x0 = x1
                        ELSE
                                DO
                                        PRINT*, i, x1
                                        fx0 = func(x0)
                                        dfx0 = dfunc(x0)
                                        IF (ABS(dfx0)<TINY(x0)) PRINT*, 'Algorithm Failure'
                                        IF (ABS(dfx0)< TINY(x0)) EXIT
                                        x1 = x0 - (fx0/dfx0)
                                        IF (ABS(x1 - x0) < eps) EXIT
                                        x0 = x1
                                        i = i + 1
                                END DO
                        END IF
                END IF
        END SUBROUTINE newton
END PROGRAM assign06
