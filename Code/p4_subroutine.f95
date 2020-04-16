program p4_subroutine
	implicit none
	integer :: i = 5

	call printNTimes(i)

contains

subroutine printNTimes(n)
	integer :: n
	integer :: start

	do start = 1, n
		print *, "You're a jabronie"
	end do
end subroutine printNTimes
end program p4_subroutine

