program p2_nesteddo
	integer :: i, j

	outter: do i = 0,2
		print *, "Outter loop"
		inner: do j = 0,2
			print *, "Inner loop"
		end do inner
		print *, ""
	end do outter
end program p2_nesteddo

