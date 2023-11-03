INTEGER RECURSIVE FUNCTION myfac(n) RESULT(res) !This is the function for the calculation of factorial
        INTEGER, INTENT(IN) :: n
        IF (n <2) THEN
                res = 1
        ELSE
                res = n * myfac(n -1)
        END IF
END FUNCTION myfac

PROGRAM factorial
        IMPLICIT NONE
        
        INTEGER :: m, n                                         
        INTEGER, EXTERNAL :: myfac
        INTEGER, PARAMETER :: kr = SELECTED_REAL_KIND(8)
        INTEGER, PARAMETER :: lr = SELECTED_REAL_KIND(12)
        REAL(8) ::  e=1.0, error=1.0E-6 , diff, inv             ! Declaration of variables
        DO n = 1, 20                                            ! This is the loop
        m = myfac(n)                                            ! Use the function to calculate for specific factorial using the function we defined and store it in m
        inv = (1/(1.0_kr*m))                                    ! Calculate the inverse, take note that _kr is for the precision of the value
        e = e + inv                                             ! this is the sum of every inverse factorial (incrementation)
        diff = abs(exp(1.0_lr) - e)                             ! This is the calculated error
                IF (diff > error) THEN                          ! Condition for checking the error

                       !PRINT*, n, m, e, diff
                ELSE                                            ! When we meet are wanteed threshold, we will print the Eulers value and stop the program
                       !PRINT*, n, m ,e, diff
                       PRINT*, 'Calculated Eulers number is', e, 'with an error of', diff 
                       STOP
                END IF
        END DO
END PROGRAM factorial
