.PHONY: run
run : hello fizzbuzz p2_do p2_dowhile p2_if p2_nesteddo p2_selectcase
	./hello
	./fizzbuzz
	./p2_do
	./p2_dowhile
	./p2_if
	./p2_nesteddo
	./p2_selectcase

all : hello fizzbuzz p2_do p2_dowhile p2_if p2_nesteddo p2_selectcase

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

clean:
	rm hello fizzbuzz p2_do p2_dowhile p2_if p2_nesteddo p2_selectcase
