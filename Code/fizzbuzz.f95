program fizzbuzz
   do n = 1,100
      if (mod(n,3) .eq. 0 .and. mod(n,5) .eq. 0) then
         print *, "FizzBuzz"

      else if (mod(n,3) .eq. 0) then
         print *, "Fizz"
      
      else if (mod(n,5) .eq. 0) then
         print *, "Buzz"

      else
         print *, n
      end if
   end do
end program fizzbuzz
