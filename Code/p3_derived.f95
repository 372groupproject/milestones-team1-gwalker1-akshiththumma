program p3_derived
	implicit none
	
	type Student
		integer :: sid
		character(50) :: name
		character(50) :: year
		character(50) :: favPickle
		character(50) :: cobMethod
	end type Student

	type(Student) :: s1

	s1%sid = 1234
	s1%name = "Sarn Peltnarnisflaven"
	s1%year = "Senior"
	s1%favPickle = "Dill"
	s1%cobMethod = "Rando bites"

	print *, "This is a student whose data type is 'Student':"
	print *, "Student id: ", s1%sid
	print *, "Student name: ", s1%name
	print *, "Student Year: ", s1%year
	print *, "Student's favorite pickle: ", s1%favPickle
	print *, "Student's method of eating corn on the cob: ", s1%cobMethod

end program p3_derived
