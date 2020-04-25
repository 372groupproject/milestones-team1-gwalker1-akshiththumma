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
real function getNum()
	implicit none
	real :: num
	read *, num
	getNum = num
	return
end function getNum 

! this function will add numbers
real function add()
	implicit none
	type(node) :: head
	real :: numNum
	real :: sum
	print *, "enter numbers to add or 0 to calculate"
	do 
		numNum = getNum()
		if (numNum .eq. 0) then
			exit
		end if
		sum = sum + numNum
	end do
	add = sum
	return
end function add

! this functions will subtract numbers
real function sub()
	implicit none
	real :: difference
	real :: num, num1
	print *, "enter two numbers to subtract or 0 to calculate"
	num = getNum()
	num1 = getNum()
	sub = num - num1
	return
end function sub

! this function will multiply numbers
real function mult()
	implicit none
	real :: num, num1, result
	result = 1
	do 
		num = getNum()
		if (num .eq. 0) then
			exit
		end if
		result = result * num
	end do
	mult = result
	return
end function mult

function matAdd(row, col)
	implicit none
	integer :: row, col, i, j
	real, dimension(:, :),allocatable :: mat1
	real, dimension(:,:),allocatable :: mat2
	real :: num
	
	real, dimension(row,col) :: matAdd

	allocate(mat1(row,col))
	allocate(mat2(row,col))
	print *, "Enter numbers for rows of matrix 1."
	do i = 1, row
		do j = 1, col
			num = getNum()
			mat1(i,j) = num
		end do
	end do
	
	print *, "Enter numbers for rows of matrix 2."
	do i = 1, row
		do j = 1, col
			num = getNum()
			mat2(i,j) = num
		end do
	end do
	matAdd = mat1 + mat2
	return
end function

end module yungCalc

! this is the driver program that will get user input and call the
! appropriate functions
program calc
	! the modules we are using
	use yungCalc
	use linkedList
	implicit none
	real, dimension(:,:), allocatable :: mat
	integer :: row
	integer :: col
	! the operation the user will enter
	character(15) :: operation
	
	! the result of calculations
	real :: result

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
				result = add()
				print *, result
			case("sub")
				result = sub()
				print *, result
		
			case("mult")
				result = mult()
				print *, result

			case ("matadd")
				print *, "Enter number for rows and columns (they will be equal)"
				read *, row
				mat = matAdd(row, row)
				!allocate(mat(2,2))				
				call printMat(mat, row)
			case default
				print *, "That is not a valid choice, grasshopper. Try again."
		end select
		
		! shitty way to "clear" the list (placeholder)
		operation = getOp()
	end do
	
	! the user has ended the program
	print *, "You have much to learn, grasshopper."
contains

!print a 2d matrix
subroutine printMat(mat, row)
	implicit none
	integer :: row1, row, col, i, j
	real, dimension(row,row) :: mat
	print *, size(mat)
	row1 = size(mat) / 2
	col = row1
	do i = 1, row1
		do j = 1, col
			write(*,fmt="(1x,a,f12.4)", advance="no") "", mat(i, j)
		end do
		print *, ""
	end do
end subroutine printMat

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
	print *, "add"
	print *, ""
	print *, "sub"
	print *, ""
	print *, "mult"
	print *, ""
	print *, "matadd"
	print *, ""
end subroutine showOpChoices	

! let the user know it's time to party
subroutine greeting()
	implicit none
	print *, "Welcome to my dojo."
	print *, "I will perform calculations for you, young grasshopper."
end subroutine greeting
	
end program calc


