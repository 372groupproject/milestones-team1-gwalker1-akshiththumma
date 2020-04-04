program p2_mildlyuse
	implicit none

	character(150) :: inputWord
	character(5) :: vowels = "aeiou"
	character(5) :: vowelsUp = "AEIOU"
	character(1) :: charToCheck
	integer :: i
	integer :: j
	integer :: consonantCount = 0
	integer :: vowelCount = 0

	print *, "Please enter a word to analyze. No spaces or non-alphabet chars."  
	read (*,'(A)') inputWord
	do while(spaces(inputWord) .eqv. .true.)
		print *, "No spaces. Try again, please."
		read (*, '(A)') inputWord
	end do
	
	do i = 1, len_trim(inputWord)
		charToCheck = inputWord(i:i)
		do j = 1, len(vowels)
			if (index(vowels(j:j),charToCheck) .ne. 0 .or. index(vowelsUp(j:j),charToCheck) .ne. 0) then
				vowelCount = vowelCount + 1
			end if
		end do
	end do
	consonantCount = len_trim(inputWord) - vowelCount
	
	print *, "The length of the word is:", len_trim(inputWord)
	print *, "The number of vowels is:", vowelCount
	print *, "The number of consonants is:", consonantCount
contains

logical function spaces(s)
	implicit none
	character(150) :: s
	logical :: val

	if (index( s(1:len_trim(s)), " ") .eq. 0) then
		val = .false.
	else
		val = .true.
	end if
	spaces = val
end function spaces

end program p2_mildlyuse	
