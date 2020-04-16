module myModule
	implicit none
	integer :: globalInt = 10

contains 

integer function subFive(n)
	implicit none
	integer :: n
	subFive = n - 5
	return
end function subFive

end module myModule	

program p4_module
	use myModule
	implicit none

	print *, "this is a global variable defined in a module."
	print *, "it is being printed in main"
	print *, globalInt

	globalInt = subFive(globalInt)	

	print *, "Here is the same var that has been modified by a function"
	print *, "defined in the module"
	print *, globalInt
end program p4_module
