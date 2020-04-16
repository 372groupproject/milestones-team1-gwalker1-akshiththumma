program p4_function
	implicit none
	integer :: a = 2
	integer :: b = 3
	integer :: result
	result = sum(a, b)
	print *, "The sum returned from the function was:", result

contains

integer function sum(a, b)
	implicit none
	integer :: a, b
	sum = a + b
	return
end function sum
end program p4_function 
