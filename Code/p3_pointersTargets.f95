program pointersTargets
	implicit none

	type node
		integer :: val
		type(node), pointer :: next
	end type node
	
	type(node) :: node1
	type(node),target :: node2
	type(node),target :: node3
	type(node), target :: tail
	type(node) :: temp
	
	! you don't need to allocate or deallocate simple type pointers
	! how do I know? Because I keep getting deallocation errors.
	! The lesson here: tutorialspoint.com is a dogshit website.	
	!allocate(node1%next)
	!allocate(node2%next)
	!allocate(node3%next)
	!allocate(tail%next)
	!allocate(temp%next)

	node1%val = 1
	node1%next => node2
	node2%val = 2
	node2%next => node3
	node3%val = 3
	node3%next => tail
	nullify(tail%next)
	temp = node1
	
	print *, "These are the values of a clumsily built linked list:"
	do while(associated(temp%next))
		print *, temp%val
		temp = temp%next
	end do	
	!nullify(node1%next)
	!deallocate(node1%next)
	!deallocate(node2%next)
	!deallocate(node3%next)
	!deallocate(tail%next)
	!deallocate(temp%next)
end program pointersTargets
