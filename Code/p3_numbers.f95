program p3_numbers
	implicit none

	integer :: int = 3
	real :: r = 3.14
	complex :: x = cmplx(3, 3.14)
	integer(kind = 2) :: short
	integer(kind = 8) :: long 
	print *, "integer val = ", int, "bytes = ", kind(int)
	print *, "real val = ", r, "bytes = ", kind(r)
	print *, "complex val = ", x, "bytes = ", kind(x)
	print *,""
	print *, "we can specify short and long ints with the kind specifier:"
	print *, "max short = ", huge(short), "bytes = ", kind(short)
	print *, "max long = ", huge(long), "bytes = ", kind(long)
end program p3_numbers	
