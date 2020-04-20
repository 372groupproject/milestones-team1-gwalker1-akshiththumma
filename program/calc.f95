!this is the module that will contain all the 
!functions for the calulator
module yungCalc
	! it may or may not use the linked list module, i haven't decided
	use linkedList
	implicit none

! the module contains the following functions
contains

! this function gets the number of numbers 
! the user wants to perform certain operations on
integer function 

! this function will add numbers
real function add()
	implicit none
	type(node) :: head
	integer :: numNum
	numNum = getNumNum()
	
	add = 1
	return
end function add

! this functions will subtract numbers
integer function sub()
	implicit none
	print *, "Sub func"
	sub = 1
	return
end function sub

! this function will multiply numbers
integer function mult()
	implicit none
	print *, "Mult func"
	mult = 1
	return
end function mult

end module yungCalc

! this is the driver program that will get user input and call the
! appropriate functions
program calc
	! the modules we are using
	use yungCalc
	use linkedList
	implicit none
	
	! the operation the user will enter
	character(15) :: operation
	
	! the result of calculations
	integer :: result
	type(node) :: head
	nullify(head%next)
	
	! intro and display the operation choices to the user
	call greeting()
	call showOpChoices()

	! get the users choice of operation
	operation = getOp()
	
	! run this loop until the user is done with the program.
	do 
		! end the program if the user types 'quit'
		if (operation .eq. "quit") then
			exit
		end if
		
		! select/case statements to call the right functions
		! base on user operation input
		select case (operation)
			case("add")
				result = add(head)
			
			case("sub")
				result = sub()
		
			case("mult")
				result = mult()

			case default
				print *, "That is not a valid choice, grasshopper. Try again."
		end select
		
		! shitty way to "clear" the list (placeholder)
		operation = getOp()
	end do
	
	! the user has ended the program
	print *, "You have much to learn, grasshopper."
contains

! getOp reads the users choice of operation
character(15) function getOp()
	implicit none
	character(15) :: op
	read *, op
	
	getOp = op
end function getOp

! showOpChoices displays the choices to the user
subroutine showOpChoices()
	implicit none
	print *, "Pick an operation you would like me to perform or type 'quit' if you suck:"
	print *, ""
	print *, "Add"
	print *, ""
	print *, "Sub"
	print *, ""
	print *, "Mult"
	print *, ""
end subroutine showOpChoices	

! let the user know it's time to party
subroutine greeting()
	implicit none
	print *, "Welcome to my dojo."
	print *, "I will perform calculations for you, young grasshopper."
end subroutine greeting
	
end program calc


