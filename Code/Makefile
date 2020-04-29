.PHONY: run
run : hello fizzbuzz p2_do p2_dowhile p2_if p2_nesteddo p2_selectcase p2_mildlyuse \
	p3_numbers p3_charAndString p3_arrays p3_derived p3_pointersTargets \
	p3_mildUse p4_subroutine p4_function p4_module p4_interface \
	p5_caseInsensitive p5_parallel p5_autoDeallocate p5_arraysStuff
	
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
	./p4_subroutine
	./p4_function
	./p4_module
	./p4_interface
	./p5_caseInsensitive
	./p5_parallel
	./p5_autoDeallocate
	./p5_arraysStuff

all : hello fizzbuzz p2_do p2_dowhile p2_if p2_nesteddo p2_selectcase p2_mildlyuse \
	p3_numbers p3_charAndString p3_arrays p3_derived p3_pointersTargets p3_mildUse \
	p4_subroutine p4_function p4_module p4_interface p5_caseInsensitive p5_parallel \
        p5_autoDeallocate p5_arraysStuff

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

p4_subroutine : p4_subroutine.f95
	gfortran -o p4_subroutine p4_subroutine.f95

p4_function : p4_function.f95
	gfortran -o p4_function p4_function.f95

p4_module : p4_module.f95
	gfortran -o p4_module p4_module.f95

p4_interface : p4_interface.f95
	gfortran -o p4_interface p4_interface.f95


p5_caseInsensitive : p5_caseInsensitive.f95
	gfortran -o p5_caseInsensitive p5_caseInsensitive.f95

p5_autoDeallocate : p5_autoDeallocate.f95
	gfortran -o p5_autoDeallocate p5_autoDeallocate.f95

p5_arraysStuff : p5_arraysStuff.f95
	gfortran -o p5_arraysStuff p5_arraysStuff.f95

p5_parallel : p5_parallel.f95
	gfortran -o p5_parallel -fopenmp p5_parallel.f95
clean:
	rm hello fizzbuzz p2_do p2_dowhile p2_if p2_nesteddo p2_selectcase p2_mildlyuse \
	   p3_numbers p3_charAndString p3_arrays p3_derived p3_pointersTargets p3_mildUse \
	   p4_subroutine p4_function p4_module p4_interface p5_caseInsensitive p5_autoDeallocate \
           p5_parallel p5_arraysStuff
