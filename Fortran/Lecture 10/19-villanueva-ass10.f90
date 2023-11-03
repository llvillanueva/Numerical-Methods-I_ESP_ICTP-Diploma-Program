! GEFux:
!	overall: All correct and complete except the algorithm of the midpoint
!	         method. I've corrected this part in the code below.
!	comment: The function funcv() in this example only depends on x, however 
!	         it might in principle also depend on v and t. For example a
!	         a friction term will add '-gamma*v' or a drive term will add
!	         '+cos(omega*t)'. Hence it would be better write the function as 
!	         funcv(t,x,v) even if you don't use the v and t here.
!	         The same for funcx().

REAL FUNCTION funcx(v) RESULT(y)
    REAL, INTENT(IN) :: v
    y = v
END FUNCTION funcx

REAL FUNCTION funcv(x) RESULT(y)
    REAL, INTENT(IN) :: x
    y = -SIN(x)
END FUNCTION funcv

SUBROUTINE euler_method(t0, x0, v0, dt, N)
    REAL, INTENT(IN) :: t0, x0, v0, dt
    REAL, DIMENSION(N + 1) :: x, t, v
    INTEGER, INTENT(IN) :: N
    REAL, EXTERNAL :: funcv,  funcx
    REAL :: fx, fv
    INTEGER :: i
    x(1) = x0
    t(1) = t0
    v(1) = v0
    WRITE(10, *) t(1), x(1), v(1)
    DO i = 1, N 
        fx = funcx(v(i))
        fv = funcv(x(i))
        x(i+1) = x(i) + (fx*dt)
        v(i+1) = v(i) + (fv*dt)
        t(i+1) = t(i) + dt
        WRITE(10, *) t(i+1), x(i+1), v(i+1)
    END DO
END SUBROUTINE euler_method

SUBROUTINE midpoint_method(t0, x0, v0, dt, N)
    REAL, INTENT(IN) :: t0, x0, v0, dt
    REAL, DIMENSION(N + 1) :: x, t, v
    REAL :: vm, xm, tm ! GEFux
    INTEGER, INTENT(IN) :: N
    REAL, EXTERNAL :: funcv,  funcx
    REAL :: fx, fv
    INTEGER :: i
    x(1) = x0
    t(1) = t0
    v(1) = v0

    WRITE(10, *) t(1), x(1), v(1)
    DO i = 1, N 
        fx = funcx(v(i))
        fv = funcv(x(i))
!        x(i+1) = x(i) + ((fx*dt)/2.0) ! GEFux
!        v(i+1) = v(i) + ((fv*dt)/2.0) ! GEFux
!        t(i+1) = t(i) + (dt/2.0)      ! GEFux
		xm = x(i) + ((fx*dt)/2.0)      ! GEFux
		vm = v(i) + ((fv*dt)/2.0)      ! GEFux
		tm = t(i) + (dt/2.0)           ! GEFux
        x(i+1) = x(i) + funcx(vm)*dt   ! GEFux
        v(i+1) = v(i) + funcv(xm)*dt   ! GEFux
        t(i+1) = t(i) + dt             ! GEFux
        WRITE(10, *) t(i+1), x(i+1), v(i+1)
    END DO
END SUBROUTINE midpoint_method

PROGRAM assign10
    IMPLICIT NONE
    REAL :: x0, v0, t0, dt
    INTEGER :: N, ios
    x0 = 0.1
    v0 = 0.0
    t0 = 0.0
    dt = 0.1
    N = 300

    OPEN(UNIT=10, IOSTAT = ios, FILE = 'eulergfux.txt', ACTION ='WRITE')       
        IF (ios == 0) THEN
                CALL euler_method(t0, x0, v0, dt, N)
                CLOSE(10)
        ELSE
                PRINT '(a25)', 'Error: file not opened.'
        END IF

    x0 = 3.0
    v0 = 0.0
    t0 = 0.0
    dt = 0.1
    N = 300

    OPEN(UNIT=10, IOSTAT = ios, FILE = 'midpointgfux.txt', ACTION ='WRITE')       
        IF (ios == 0) THEN
                CALL midpoint_method(t0, x0, v0, dt, N)
                CLOSE(10)
        ELSE
                PRINT '(a25)', 'Error: file not opened.'
        END IF

    
END PROGRAM assign10
