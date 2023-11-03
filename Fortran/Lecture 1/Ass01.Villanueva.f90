PROGRAM fibprog                 ! program name 
        IMPLICIT NONE
        INTEGER :: x, y, z, m, n
        INTEGER :: ll = 9, up = 51       ! declare the lower and upper limit or allowed integer to input
        PRINT*, 'Get the nth term Fibonacci from n=', ll, ' up to ', up  !Print a prompt about the allowed integers 
        DO                               ! This loop is for the user to input a number until he/she gives the allowed integer  
                PRINT*, 'Please enter an integer: '    !print in program, asking for an integer
                READ*, m                               !read in m
                PRINT*, '----------------------------------'
                IF (m <= ll .OR. m >= up)  THEN          !The program will go here if the user didnt input an allowed integer
                        PRINT*, 'The input number is not in the allowed range!'  !prompt an error message
                        PRINT*, '----------------------------------'
                ELSE            !the program will go here if he/she input an allowed integer
                        z = 1   ! initial condition
                        y = 1   ! initial condition
                        DO n = 3, m     !this loop is just calculating the fibonacci at mth term
                                x = y + z
                                z = y
                                y = x
                        END DO
                        PRINT*, 'The Fib(', m,') = ', x !prompt the calculated fibonacci number
                        STOP    !stop the program
                END IF
        END DO
END PROGRAM fibprog             ! end the program



!What happens if m > 36? Can you tell why this happens? Next lesson will give you the answer!

!I think the error happens because the data type integer can only take up to a certain precision. Similar to python, which I am more
!familiar too, there are int16, int32, etc.. I expect similar to fortran, we can define different integer with a more precision.


