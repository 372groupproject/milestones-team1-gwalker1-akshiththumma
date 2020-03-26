.PHONY: run
run : hello fizzbuzz
	./hello
	./fizzbuzz

all : hello fizzbuzz

hello : hello.f95
	gfortran -o hello hello.f95

fizzbuzz : fizzbuzz.f95
	gfortran -o fizzbuzz fizzbuzz.f95

clean:
	rm hello fizzbuzz
