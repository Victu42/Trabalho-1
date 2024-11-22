program stats
 
   use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
   implicit none

   integer(i8) :: f_u, io, n_linhas
   real(dp), allocatable :: col1(:), col2(:)

   f_u = 11
   n_linhas = linhas()

   allocate(col1(n_linhas), col2(n_linhas))

   call define(col1,col2)

   write(*,*) 'Média Coluna 1:',media(col1)
   write(*,*) 'Média Coluna 2:',media(col2)
   write(*,*) 'Variância Coluna 1:',varia(col1)
   write(*,*) 'Variância Coluna 2:',varia(col2)
   write(*,*) 'Desvio Padrão Coluna 1:',desvio(col1)
   write(*,*) 'Desvio Padrão Coluna 2:',desvio(col2)


contains

   function linhas() result(n)
   
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
      real(dp), dimension(n_linhas) :: a(:), b(:)

      i = 0

      open(f_u, file="data.txt", iostat=io, status="OLD", action="read")
   
      do i=1, n_linhas
   
         read(f_u, *, iostat=io) a(i), b(i)
         col1(i)=a(i)
         col2(i)=b(i)
         if (io /= 0) exit
   
      end do 

      
      close(unit=f_u)

   end subroutine define

   function media(col_m) result(m)

      integer(i8) :: j
      real(dp) :: m, col_m(:)

      j = 0
      m = 0

      do j=1, n_linhas

         m = m + (col_m(j)/n_linhas)
         if (io /= 0) exit
   
      end do 

   end function media

   function varia(col_v) result(var)

      real(dp) :: var, col_v(:), md
      integer(i8) :: k

      var = 0
      md = media(col_v)

      do k=1, n_linhas 

         var = var + ((col_v(k) - md)*2/(n_linhas-1))

      end do
   
   end function varia

   function desvio(col_d) result(dv_p)
   
      real(dp) :: dv_p, col_d(:)

      dv_p = sqrt(varia(col_d))

   end function desvio

end program stats

