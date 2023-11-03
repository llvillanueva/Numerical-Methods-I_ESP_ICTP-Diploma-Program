REAL FUNCTION fave(arr, size) RESULT(ave) 
        INTEGER, INTENT(IN) :: size 
        REAL, INTENT(IN) :: arr(size)
        INTEGER :: i
        REAL :: sum 

        sum = 0.0       !This is important and my lead to error if defined in the declaration

        DO i = 1, size
                sum = sum + arr(i)
        END DO

        ave = sum / (1.0 * size)
END FUNCTION fave


PROGRAM assign04
        IMPLICIT NONE
        INTEGER :: u, ios,iosr,i, lcount = 0
        REAL :: bave, cave, dave
        REAL, DIMENSION (3) :: ave
        REAL, DIMENSION (:), ALLOCATABLE :: a, b, c, d
        CHARACTER(100) :: line
        REAL, EXTERNAL :: fave

        OPEN(UNIT = 10, IOSTAT = ios ,FILE = 'sst.dat', STATUS = 'old', ACTION= 'READ') !Opening the data file
        OPEN(unit=20, file='temp.dat', status='unknown')        !Creating a temp file
        
        IF (ios == 0) THEN      !Condition if the data file was successfully read
                !This block of code is for counting the valid lines and writing the valid lines to a temp file
                DO
                        READ(10, '(A)', IOSTAT = iosr) line
                        IF (iosr /= 0) THEN
                                EXIT
                        ELSE
                                IF (line(1:1) /= '#') lcount = lcount + 1
                                IF (line(1:1) /= '#') write(20, *) line
                        END IF
                END DO
                CLOSE(10)       !Closing the data file

                ALLOCATE(a(lcount), b(lcount), c(lcount), d(lcount))    !Allocating a specific number of array
                
                REWIND(20)                                              !Rewinding the temp file for reading
                
                !This loop is for storing the data to the allocated arrays
                DO i = 1, lcount
                        READ(20, *) a(i), b(i), c(i), d(i)
                        !PRINT*, a(i), b(i), c(i), d(i)
                END DO

                CLOSE(20, status = 'DELETE')    !Closing the file and deleting the temp file

                !Calculate the average of column 2 - 4 and storing it to an array
                ave(1) =  fave(b, lcount)
                ave(2) =  fave(c, lcount)
                ave(3) =  fave(d, lcount)

                !promt the result
                PRINT*, 'Average Values:', ave
                CALL bubble_sort(ave, 3)        !To sort the array
                PRINT*, 'Sorted Average Values:', ave 
                PRINT*, '----------------------------'
                PRINT*, 'SST GLOBAL AVERAGE ECMWF REANALYSIS IN THE...'
                PRINT*, 'Year 1986:', ave(1)
                PRINT*, 'Year AVG(1970-2022):', ave(2)
                PRINT*, 'Year 2022:', ave(3)
                
        ELSE
                PRINT '(a25)', 'Error: file not opened.'      
        END IF  

CONTAINS

        SUBROUTINE bubble_sort(arr, n)                  !subroutine for sorting
                REAL, INTENT(INOUT) :: arr(:)           !initialize the needed variable
                INTEGER, INTENT(IN) :: n
                INTEGER :: i, j
                REAL :: temp

                DO i = 1, n - 1                         !bubble sort algorithim
                        DO j = 1, n - i
                                IF (arr(j) > arr(j + 1)) THEN
                                        temp = arr(j)
                                        arr(j) = arr(j + 1)
                                        arr(j + 1) = temp
                                END IF
                END DO
                END DO
        END SUBROUTINE bubble_sort

END PROGRAM assign04
