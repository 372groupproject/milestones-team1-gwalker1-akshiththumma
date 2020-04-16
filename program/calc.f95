!this is the module that will contain all the 
!functions for the calulator
module yungCalc
	use linkedList
	implicit none


contains

integer function add()
	implicit none
	print *, "Add func"
	add = 1
	return
end function add

integer function sub()
	implicit none
	print *, "Sub func"
	sub = 1
	return
end function sub

integer function mult()
	implicit none
	print *, "Mult func"
	mult = 1
	return
end function mult

end module yungCalc

program calc
	use yungCalc
	use linkedList
	implicit none
	character(15) :: operation
	integer :: result
	type(node) :: head
	nullify(head%next)
	
	call greeting()
	call showOpChoices()

	operation = getOp()
	
	do 
		if (operation .eq. "quit") then
			exit
		end if
		
		select case (operation)
			case("add")
				result = add()
			
			case("sub")
				result = sub()
		
			case("mult")
				result = mult()

			case default
				print *, "That is not a valid choice, grasshopper. Try again."
		end select
		operation = getOp()
	end do
	
	print *, "You have much to learn, grasshopper."
contains

character(15) function getOp()
	implicit none
	character(15) :: op
	read *, op
	
	getOp = op
end function getOp

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

subroutine greeting()
	implicit none
	print *, "Welcome to my dojo."
	print *, "I will perform calculations for you, young grasshopper."
end subroutine greeting
	
end program calc


