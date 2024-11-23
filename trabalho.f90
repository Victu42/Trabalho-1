program stats

use estatistica_basica !adiciona o módulo
implicit none

   f_u = 11
   f_u_2 = 12
   n_linhas = linhas() !chama a função para contagem de linhas

   allocate(col1(n_linhas), col2(n_linhas)) !define a dimensão dos vetores de acordo com a quantidade de linhas do arquivo

   call define(col1,col2) !chama a subrotina para definir os vetores

   write(*,*) 'Média Coluna 1:',media(col1) !escreve a média da coluna 1
   write(*,*) 'Média Coluna 2:',media(col2) !escreve a média da coluna 2
   write(*,*) 'Variância Coluna 1:',varia(col1) !escreve a variância da coluna 1
   write(*,*) 'Variância Coluna 2:',varia(col2) !escreve a variância da coluna 2
   write(*,*) 'Desvio Padrão Coluna 1:',desvio(col1) !escreve o desvio da coluna 1
   write(*,*) 'Desvio Padrão Coluna 2:',desvio(col2) !escreve o desvio da coluna 2

   call media_movel !chama a subrotina para cálculo da Média Móvel

end program stats

