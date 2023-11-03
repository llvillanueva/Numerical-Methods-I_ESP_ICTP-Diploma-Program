PROGRAM qubit
    IMPLICIT NONE
    REAL :: t0, dt
    COMPLEX, DIMENSION(2,2) :: rho0, H
    INTEGER :: N, ios    

    rho0(1,1) = (1.00, 0.00)
    rho0(1,2) = (0.00, 0.00)
    rho0(2,1) = (0.00, 0.00)
    rho0(2,2) = (0.00, 0.00)

    H(1,1) = (0.00, 0.00)
    H(1,2) = (0.70, 0.20)
    H(2,1) = (0.70, -0.20)
    H(2,2) = (1.00, 0.00)

    t0 = 0.0
    dt = 0.05
    N = 300

    OPEN(UNIT=10, IOSTAT = ios, FILE = 'eulerA11.txt', ACTION ='WRITE')       
        IF (ios == 0) THEN
            CALL euler(t0, rho0, H, dt, N)
            CLOSE(10)
        ELSE
                PRINT '(a25)', 'Error: file not opened.'
        END IF

    OPEN(UNIT=10, IOSTAT = ios, FILE = 'verlet.txt', ACTION ='WRITE')       
        IF (ios == 0) THEN
            CALL verlet(t0, rho0, H, dt, N)
            CLOSE(10)
        ELSE
                PRINT '(a25)', 'Error: file not opened.'
        END IF

CONTAINS

    SUBROUTINE verlet(t0, rho0, H, dt, N)
        COMPLEX, DIMENSION(2,2), INTENT(IN) :: rho0, H
        REAL, INTENT(IN) :: t0, dt
        INTEGER, INTENT(IN) :: N
        COMPLEX, DIMENSION(2,2) :: res, rhop, rhon, rhoc
        REAL :: t
        INTEGER :: i, j, k
        
        t = t0
        WRITE(10, *) t, REAL(rho0(1,1)), REAL(rho0(2,2))

        rhop = rho0
        
        CALL midpoint(rhop, H, dt, rhoc)
    
        DO i=1, N
            CALL drho_dt(t, rhoc, H, res)

            DO j =1 ,2
                DO k=1, 2
                    rhon(j,k) = rhop(j,k) + (2*res(j,k)*dt)
                END DO
            END DO

            t = t + dt
            rhop = rhoc
            rhoc = rhon
            
            WRITE(10, *) t, REAL(rhoc(1,1)), REAL(rhoc(2,2))

         END DO

    END SUBROUTINE verlet

    SUBROUTINE euler(t0, rho0, H, dt, N)
        COMPLEX, DIMENSION(2,2), INTENT(IN) :: rho0, H
        REAL, INTENT(IN) :: t0, dt
        INTEGER, INTENT(IN) :: N
        COMPLEX, DIMENSION(2,2) :: res, rho
        REAL :: t
        INTEGER :: i, j, k

        rho = rho0
        t = t0
        WRITE(10, *) t, REAL(rho(1,1)), REAL(rho(2,2))

        DO i = 1, N  
            CALL drho_dt(t, rho, H, res)
            DO j =1 ,2
                DO k=1, 2
                rho(j, k) = rho(j, k) + res(j, k)*dt
                END DO
            END DO
            t= t + dt
            WRITE(10, *) t, REAL(rho(1,1)), REAL(rho(2,2))
        END DO

    END SUBROUTINE euler

    SUBROUTINE midpoint(rhop, H, dt, rhoc)
        COMPLEX, DIMENSION(2,2), INTENT(IN) :: rhop, H
        COMPLEX, DIMENSION(2,2), INTENT(OUT) :: rhoc ! result
        COMPLEX, DIMENSION(2,2) :: frho, res
        REAL, INTENT(IN) :: dt
        REAL :: t
        INTEGER :: j, k

        CALL drho_dt(t, rhop, H, res)

        DO j =1 ,2
            DO k=1, 2
                frho(j,k) = res(j,k)*dt
            END DO
        END DO

        DO j =1 ,2
            DO k=1, 2
                frho(j,k) = rhop(j,k) + (frho(j,k)/2.0)
            END DO
        END DO

        CALL drho_dt(t, frho, H, res)

        DO j =1 ,2
            DO k=1, 2
                rhoc(j,k) = rhop(j, k) + res(j,k)*dt
            END DO
        END DO
    END SUBROUTINE midpoint

    SUBROUTINE drho_dt(t, rho, H, res)
        COMPLEX, DIMENSION(2,2), INTENT(IN) :: rho, H
        COMPLEX, DIMENSION(2,2), INTENT(OUT) :: res ! result
        REAL, INTENT(IN) :: t

        res = (0.0, -1.0)*(MATMUL(H, rho) - MATMUL(rho, H))

    END SUBROUTINE drho_dt

END PROGRAM qubit
