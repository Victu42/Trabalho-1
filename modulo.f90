module estatistica_basica

use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
implicit none

real(dp), allocatable :: col1(:), col2(:)
integer(i8) :: f_u, io, n_linhas, f_u_2
character (len = 1) :: input

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

   subroutine media_movel
      
      real(dp), dimension(n_linhas) :: mm_1, mm_2
      real(dp) :: calc_1, calc_2
      integer(i8) :: l 
      
      do
         print *, 'Gostaria de um arquivo com a média móvel dos seus dados? (Y/N)'
         read(*,*) input
         if (input /= "Y" .and. input /= "N") then
            print *, 'Resposta inválida.'
         else
            exit 
         end if
      end do

      if (input == 'Y') then
         
         open(f_u, file="data.txt", iostat=io, status="OLD", action="read")
         open(f_u_2, file="data_media_movel.txt", iostat=io, status="NEW", action="write")
         
         mm_1(1) = (col1(1)+col1(2)+col1(3))/3
         mm_1(2) = (col1(1)+col1(2)+col1(3)+col1(4))/4
         mm_2(1) = (col2(1)+col2(2)+col2(3))/3
         mm_2(2) = (col2(1)+col2(2)+col2(3)+col2(4))/4
         mm_1(n_linhas) = (col1(n_linhas)+col1(n_linhas-1)+col1(n_linhas-2))/3
         mm_1(n_linhas-1) = (col1(n_linhas)+col1(n_linhas-1)+col1(n_linhas-2)+col1(n_linhas-3))/4
         mm_2(n_linhas) = (col2(n_linhas)+col2(n_linhas-1)+col2(n_linhas-2))/3
         mm_2(n_linhas-1) = (col2(n_linhas)+col2(n_linhas-1)+col2(n_linhas-2)+col2(n_linhas-3))/4

         do l = 3, n_linhas-2

            mm_1(l) = (col1(l-2)+col1(l-1)+col1(l)+col1(l+1)+col1(l+2))/5
            mm_2(l) = (col2(l-2)+col2(l-1)+col2(l)+col2(l+1)+col2(l+2))/5
            write(f_u_2,*) mm_1(l), mm_2(l)

         end do

         close(f_u)
         close(f_u_2)

      end if


   end subroutine media_movel


end module