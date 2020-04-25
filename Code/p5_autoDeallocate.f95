program p5_autoDeallocate
	implicit none
	print *, "variables only exist for the duration of their scope."
	print *, "I'm going to call a subroutine and create a pointer."
	print *, "When we return from the subroutine, the memory will be automatically deallocated."
	call pointerDisplay()
	print *, "We're back from the subroutine and the pointer has been auto deallocated."
contains

subroutine pointerDisplay()
	implicit none
	integer, pointer :: ptr
	integer, target :: tar
	allocate(ptr)
	ptr => tar
	tar = 6
	print *, "We're in the subroutine. ptr val is:", ptr
end subroutine pointerDisplay

end program p5_autoDeallocate
