program p5_arrayStuff
	implicit none

	integer :: matrix(3,3), i, j

	data matrix / 1, 2, 3, 4, 5, 6, 7, 8, 9 /

	print *, "2d arrays are stored in col major order."
	print *, "The 2d array has been hardcoded with values in this order: 1,2,3,4,5,6,7,8,9"
	print *, "Note the column major order upon printing."	
	do i = 1, 3
		do j = 1, 3
			print *, matrix(i,j)
		end do
	end do

	print *, "Arrays start at 1 in FORTRAN. printing matrix(1,1) will give us the first row"
	print *, "and column. Many other launguages use zero indexed arrays and doing this would"
	print *, "give the second row and col."
	print *, "Here is (1,1) of the array:"
	print *, matrix(1,1)
	
	print *, "FORTRAN allows us to use masking to change array elements if the meet certain conditions."
	print *, "This is achieved through the 'where' statement."
	print *, "This is the array after changing all numbers divisible by 3 to 7:"

	where(mod(matrix,3) .eq. 0)
		matrix = 7
	end where

	do i = 1, 3
		do j = 1, 3
			print *, matrix(i,j)
		end do
	end do

        print *, "We can also do things like sum an array without writing our own do loop which"
	print *, "could allow us to write cleaner code that is easy to read and maintain."
	print *, "Here is the sum using the statement 'sum(matrix)'."

	print *, sum(matrix)

	print *, "It is possible to implicitely loop through an array using index slicing:"
	print *, matrix(1, 1:3:1)	
end program p5_arrayStuff
