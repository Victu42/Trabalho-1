module estatistica_basica

use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
implicit none

real(dp), allocatable :: col1(:), col2(:)
integer(i8) :: f_u, io, n_linhas, f_u_2
character (len = 1) :: input

contains

   function linhas() result(n) !função que conta as linhas
   
      integer(i8):: n
      n = 0 !define o valor inicial de n como 0 para que possa ser somado mais à frente
      open(f_u, file="data.txt", iostat=io, status="OLD", action="read") !abre o arquivo de dados
          do !loop para contagem de linhas
       
             read(f_u, *, iostat=io) !confere se há uma linha
             if (io /= 0) exit !quebra o loop caso não haja uma linha
             n = n + 1 !adiciona uma linha à contagem em caso de linha existente
       
          end do 
      
      close(unit=f_u) !fecha o arquivo
   
   end function linhas

   subroutine define(a, b) !subrotina que armazena os dados do arquivo em dois vetores
      
      integer(i8) :: i
      real(dp), dimension(n_linhas) :: a(:), b(:)

      i = 0

      open(f_u, file="data.txt", iostat=io, status="OLD", action="read")
   
      do i=1, n_linhas !loop para iterar por todo o arquivo e armazenar as linhas
   
         read(f_u, *, iostat=io) a(i), b(i) !passa os dados para os vetores
         col1(i)=a(i) !escreve o item na variável inicial col1
         col2(i)=b(i) !escreve o item na variável inicial col2
         if (io /= 0) exit !quebra o loop caso o arquivo acabe
   
      end do 

      
      close(unit=f_u)

   end subroutine define

   function media(col_m) result(m)!calcula a média

      integer(i8) :: j
      real(dp) :: m, col_m(:)

      j = 0
      m = 0

      do j=1, n_linhas !loop para iterar pelos dados 

         m = m + (col_m(j)/n_linhas) !adiciona termos à média total
         if (io /= 0) exit !quebra o loop
   
      end do 

   end function media

   function varia(col_v) result(var) !calcula a variância

      real(dp) :: var, col_v(:), md
      integer(i8) :: k

      var = 0
      md = media(col_v) !chama a função media

      do k=1, n_linhas !loop para iterar pelos dados

         var = var + ((col_v(k) - md)*2/(n_linhas-1)) !adiciona termos à variância total

      end do
   
   end function varia

   function desvio(col_d) result(dv_p) !calcula o desvio padrão
   
      real(dp) :: dv_p, col_d(:)

      dv_p = sqrt(varia(col_d)) !fórmula para o desvio padrão

   end function desvio

   subroutine media_movel !calcula a média móvel dos dados
      
      real(dp), dimension(n_linhas) :: mm_1, mm_2
      real(dp) :: calc_1, calc_2
      integer(i8) :: l 
      
      do !loop para consultar se a pessoa quer a média móvel
         print *, 'Gostaria de um arquivo com a média móvel dos seus dados? (Y/N)' !faz a pergunta
         read(*,*) input !lê a resposta
         if (input /= "Y" .and. input /= "N") then !confere se a resposta corresponde ao aceito pelo programa
            print *, 'Resposta inválida.' !caso não corresponda, exibe erro 
         else !caso corresponda, sai do loop
            exit 
         end if
      end do

      if (input == 'Y') then !se a resposta é sim (Y), calcula a média móvel
         
         open(f_u, file="data.txt", iostat=io, status="OLD", action="read") !abre o arquivo
         open(f_u_2, file="data_media_movel.txt", iostat=io, status="NEW", action="write") !cria o novo arquivo
         
         mm_1(1) = (col1(1)+col1(2)+col1(3))/3
         mm_1(2) = (col1(1)+col1(2)+col1(3)+col1(4))/4
         mm_2(1) = (col2(1)+col2(2)+col2(3))/3
         mm_2(2) = (col2(1)+col2(2)+col2(3)+col2(4))/4
         mm_1(n_linhas) = (col1(n_linhas)+col1(n_linhas-1)+col1(n_linhas-2))/3
         mm_1(n_linhas-1) = (col1(n_linhas)+col1(n_linhas-1)+col1(n_linhas-2)+col1(n_linhas-3))/4
         mm_2(n_linhas) = (col2(n_linhas)+col2(n_linhas-1)+col2(n_linhas-2))/3
         mm_2(n_linhas-1) = (col2(n_linhas)+col2(n_linhas-1)+col2(n_linhas-2)+col2(n_linhas-3))/4
         !as linhas acima calculam a média móvel para os valores que não possuem amostras nas vizinhanças, dado o range de 5

         do l = 3, n_linhas-2 !loop que calcula a média móvel para o restante dos dados

            mm_1(l) = (col1(l-2)+col1(l-1)+col1(l)+col1(l+1)+col1(l+2))/5 !tira a média dos valores da vizinhança para a coluna 1
            mm_2(l) = (col2(l-2)+col2(l-1)+col2(l)+col2(l+1)+col2(l+2))/5 !tira a média dos valores da vizinhança para a coluna 2
            write(f_u_2,*) mm_1(l), mm_2(l) !escreve os valores no novo arquivo

         end do

         close(f_u)
         close(f_u_2)

      end if


   end subroutine media_movel


end module