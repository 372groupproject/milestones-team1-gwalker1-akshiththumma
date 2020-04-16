program p4_interface
	implicit none
	
	interface 
		function addTwoReals(a, b)
			!real, addTwoReals
			real, intent(in) :: a, b
		end function addTwoReals
	end interface

	real :: a, b
	real :: result
	a = 3.14
	b = 42.0

	result = addTwoReals(a, b)
	print *, result

end program p4_interface

real function addTwoReals(a, b)
	implicit none
	real, intent(in) :: a, b
	addTwoReals = a + b
	return
end function addTwoReals

