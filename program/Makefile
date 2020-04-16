.PHONY : run
run : calc
	
	./calc

calc : calc.f95
	gfortran -o calc calc.f95 linkedList.f95

clean :
	rm calc *.mod
