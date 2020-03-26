program hello
	implicit none
    
 	character (len = 15) :: word
	print *, "Hello, World!"
	print *, "Enter a word for me to say:"
        read *, word
	print *, word
end program hello
