program p5_parallel
	use omp_lib
	implicit none
	integer :: i
	integer :: sum
	real :: start, finish	
	sum = 0
	start = omp_get_wtime()
!$omp parallel private(i)
!$omp do
	do i = 0, 1000000000
		sum = sum + i
	end do
!$omp end parallel
	finish = omp_get_wtime()
	print *, "time = ", finish-start

	start = omp_get_wtime()
	do i = 0, 1000000000
		sum = sum + i
	end do
	finish = omp_get_wtime()
	print *, "time = ", finish-start


end program p5_parallel
