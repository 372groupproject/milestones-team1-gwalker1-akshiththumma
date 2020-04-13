.PHONY: run
run : hello fizzbuzz p2_do p2_dowhile p2_if p2_nesteddo p2_selectcase p2_mildlyuse \
	p3_numbers p3_charAndString p3_arrays p3_derived p3_pointersTargets \
	p3_mildUse
	
	./hello
	./fizzbuzz
	./p2_do
	./p2_dowhile
	./p2_if
	./p2_nesteddo
	./p2_selectcase
	./p2_mildlyuse
	./p3_numbers
	./p3_charAndString
	./p3_arrays
	./p3_derived
	./p3_pointersTargets
	./p3_mildUse

all : hello fizzbuzz p2_do p2_dowhile p2_if p2_nesteddo p2_selectcase p2_mildlyuse \
	p3_numbers p3_charAndString p3_arrays p3_derived p3_pointersTargets p3_mildUse

hello : hello.f95
	gfortran -o hello hello.f95

fizzbuzz : fizzbuzz.f95
	gfortran -o fizzbuzz fizzbuzz.f95

p2_do : p2_do.f95
	gfortran -o p2_do p2_do.f95

p2_dowhile : p2_dowhile.f95
	gfortran -o p2_dowhile p2_dowhile.f95

p2_if : p2_if.f95
	gfortran -o p2_if p2_if.f95

p2_nesteddo : p2_nesteddo.f95
	gfortran -o p2_nesteddo p2_nesteddo.f95

p2_selectcase : p2_selectcase.f95
	gfortran -o p2_selectcase p2_selectcase.f95

p2_mildlyuse : p2_mildlyuse.f95
	gfortran -o p2_mildlyuse p2_mildlyuse.f95

p3_numbers : p3_numbers.f95
	gfortran -o p3_numbers p3_numbers.f95

p3_charAndString : p3_charAndString.f95
	gfortran -o p3_charAndString p3_charAndString.f95

p3_arrays : p3_arrays.f95
	gfortran -o p3_arrays p3_arrays.f95

p3_derived : p3_derived.f95
	gfortran -o p3_derived p3_derived.f95

p3_pointersTargets : p3_pointersTargets.f95
	gfortran -o p3_pointersTargets p3_pointersTargets.f95

p3_mildUse : p3_mildUse.f95
	gfortran -o p3_mildUse p3_mildUse.f95

clean:
	rm hello fizzbuzz p2_do p2_dowhile p2_if p2_nesteddo p2_selectcase p2_mildlyuse \
	   p3_numbers p3_charAndString p3_arrays p3_derived p3_pointersTargets p3_mildUse
