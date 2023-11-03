REAL FUNCTION fxm(x, n) RESULT(res)
      !declaration 
      REAL, INTENT(IN) :: x
      INTEGER, INTENT(IN) :: n
      REAL , PARAMETER :: pi = 4.0*ATAN(1.0)
      res = (SIN(2.0*pi*(x/(2.0*n))))**2
END FUNCTION fxm



PROGRAM assign03
      IMPLICIT NONE
      
      !declaration for variables and function
      INTEGER :: m, n
      REAL :: a
      REAL, EXTERNAL :: fxm
      REAL, DIMENSION(:), ALLOCATABLE :: x,y
    
      !declaration for character
      CHARACTER(*) , parameter :: s1 = "This is the result of"
      CHARACTER(*) , parameter:: s2 = &
                      "the function f(x) = (sin(2*pi*(x/(2*n))))**2"
      CHARACTER(100) :: s3

      !prompt to get an integer from the user
      PRINT*, 'Enter an integer value m:'
      READ*, m
      
      !allocating the dimension of array
      ALLOCATE( y(m+1) )
      ALLOCATE( x(m+1) )
      
      !calculate the array x and y
      DO n = 1, m+1
        a = 2.0*(n-1)
        x(n) = a
        y(n) = fxm(a, m)
      END DO
      
      !code to prompt result
      s3 = s1 // ' ' // s2
      
      PRINT*, TRIM(s3)
      DO n = 1, m+1
        PRINT*, x(n), y(n)
      END DO

    ! Save data to a data.txt
    OPEN (unit=48,  file='data.txt')            
    DO n=1, m+1
        WRITE (48, *) x(n), y(n)
    END DO
    CLOSE (48)

    ! Call Gnuplot to create a plot
    CALL create_plot()                       
CONTAINS

  subroutine create_plot()
    ! Create a Gnuplot script to plot the data
    OPEN(unit=20, file="plot_script.gp", status="replace")
    WRITE(20, *) "set terminal png"
    WRITE(20, *) "set output 'Ass03.Villanueva.png'"
    write(20, *) "set xlabel 'X(n)'"
    write(20, *) "set ylabel 'Y(n)'"
    write(20, *) "plot 'data.txt' with lines notitle"
    CLOSE(20)

    ! Run Gnuplot with the script
    CALL system("gnuplot plot_script.gp")
  end subroutine create_plot

END PROGRAM assign03
