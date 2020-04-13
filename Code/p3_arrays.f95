program p3_arrays
	implicit none

	integer :: numArray(5)
	integer :: twodNumArray(5,5)
	integer :: i, j

	do i = 1, 5
		numArray(i) = i
		do j = 1, 5
			twodNumArray(i, j) = j
		end do
	end do
	
	print *, "A one dimension array:"
	do i = 1, 5
		write(*,fmt="(1x,a,i0)", advance="no") "", numArray(i)
	end do

	print *, ""
	print *, "A two dimensional array:"
	do i = 1, 5
		do j = 1, 5
			write(*,fmt="(1x,a,i0)", advance="no") "", twodNumArray(i, j)
		end do
		print *, ""
	end do
	
	where(twodNumArray .eq. 5)
		twodNumArray = -1
	end where

	print *, ""
	print *, "2d array modified by 'where' statement:"
	do i = 1, 5
		do j = 1, 5
			write(*,fmt="(1x,a,i0)", advance="no") "", twodNumArray(i, j)
		end do
		print *, ""
	end do
end program p3_arrays
