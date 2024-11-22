program stats

use estatistica_basica
implicit none

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

end program stats

