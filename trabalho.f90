program stats
 
   use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
   implicit none

   integer(i8) :: n, f_u, i, io
   real(dp), allocatable :: a(:), b(:)
   real(dp) :: media_a, media_b, soma_a, soma_b

   f_u = 11
   n = 0
   media_a = 0
   media_b = 0

   open(f_u, file="data.txt", iostat=io, status="OLD", action="read")

   do 

      read(f_u, *, iostat=io)
      if (io /= 0) exit
      n = n + 1

   end do 

   close(unit=f_u)
   
   allocate(a(n), b(n))

   open(f_u, file="data.txt", iostat=io, status="OLD", action="read")

   do i=1,n

   read(f_u, *, iostat=io) a(i), b(i)
   media_a = media_a + (a(i)/n)
   media_b = media_b + (b(i)/n)
   if (io /= 0) exit

   end do 

   write(*,*) media_a 
   write(*,*) media_b

end program stats

