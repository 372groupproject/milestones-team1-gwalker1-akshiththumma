program charAndString
	implicit none
	character :: char ="Hello, my name is Frampt the King Seeker"
	character(50) :: str ="Hello, my name is Frampt the King Seeker"
	
	print *, "A character variable with no length specifier will default"
	print *, "to a single character:", char
	print *, ""
	print *, "We can get full strings by providing a length:"
	print *, str
	print *, ""
	print *, "The starting index of the word 'King' is: ", index(str, "King")
	print *, ""
	print *, "The length including trailing space:", len(str)
	print *, "The length without trailing space:", len_trim(str)
	print *, "" 
	print *, "We can also repeat strings:"
	print *, repeat(trim(str),4)
	print *, ""
	print *, "Convert a char to its ascii value:"
	print *, "char = ", char, "  ascii val = ", iachar(char)
end program charAndString
