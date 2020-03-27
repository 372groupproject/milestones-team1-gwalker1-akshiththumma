program p2_selectcase
	integer :: n = 3

	message: select case (n)
		case (0)
			print *, "not too great"
		case (2)
			print *, "wow, what a treat"
		case (3)
			print *, "omg, like, totes awesome"
		case default
			print *, "no, absolutely not"
	end select message
end program p2_selectcase
