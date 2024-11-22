program stats
 
   use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
   implicit none

   integer(i8) :: f_u, io
   real(dp) :: col1(linhas()), col2(linhas())

   f_u = 11

   call define(col1,col2)

   write(*,*) 'Média Coluna 1:',media(col1)
   write(*,*) 'Média Coluna 2:',media(col2)
   write(*,*) 'Variância Coluna 1:',varia(col1)
   write(*,*) 'Variância Coluna 2:',varia(col2)
   write(*,*) 'Desvio Padrão Coluna 1:',desvio(col1)
   write(*,*) 'Desvio Padrão Coluna 2:',desvio(col2)


contains

   integer function linhas() result(n)
   
      integer(i8):: n
      n = 0
      open(f_u, file="data.txt", iostat=io, status="OLD", action="read")
          do 
       
             read(f_u, *, iostat=io)
             if (io /= 0) exit
             n = n + 1
       
          end do 
      
      close(unit=f_u)
   
   end function linhas

   subroutine define(a, b)
      
      integer(i8) :: i
      real(dp) :: a(linhas), b(linhas)

      i = 0

      open(f_u, file="data.txt", iostat=io, status="OLD", action="read")
   
      do i=1, linhas
   
         read(f_u, *, iostat=io) a(i), b(i)
         col1(i)=a(i)
         col2(i)=b(i)
         if (io /= 0) exit
   
      end do 

      
      close(unit=f_u)

   end subroutine define

   function media(m) result(m)

      integer(i8) :: j
      real(dp) :: m

      j = 0
      m = 0

      do j=1, linhas

         m = m + (m(j)/linhas)
         if (io /= 0) exit
   
      end do 

   end function media

   function varia(var) result(var)

      real(dp) :: var
      integer(i8) :: k

      var = 0
      k = 0

      do k=1, linhas 

         var = var + ((var(i) - media(var))*2)/(linhas-1)

      end do
   
   end function varia

   function desvio(dv_p) result(dv_p)
   
      real(dp) :: d_p
      dv_p = 0

      dv_p = sqrt(var(dv_p))

   end function desvio

end program stats

