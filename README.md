#Estatística Básica

O programa lê um arquivo qualquer, de duas colunas de dados, devendo ser nomeado "data.txt", e calcula as grandezas Média Artmética, Variância e Desvio Padrão das duas colunas, escrevendo os valores no próprio terminal. Ao final da execução, também é dada a opção da criação de um arquivo contendo a Média Móvel simples das duas colunas, com um range de 5 amostras. 

## Funções

O módulo estatistica_basica.mod contém as funções:

1. contagem de linhas do arquivo (função "linhas", que não recebe nenhum argumento e retorna o número de linhas.);
2. cálculo da Média (função "media", que tem como argumento uma das colunas - col1 ou col2 - definidas no código e retorna a média);
3. cálculo da Variância (função "varia", que também tem como argumento uma das colunas e retorna a Varância);
4. cálculo do Desvio Padrão (função "desvio", que usa uma das colunas de argumento e retorna o Desvio Padrão)

## Subrotinas

O módulo também contém subrotinas:

1. definição das variáveis col1 e col2 (subrotina "define", que escreve as colunas do arquivo para variáveis dentro do código);
2. cálculo da Média Movel (subrotina "media_movel", que dá a opção da criação de um arquivo com a Média Móvel das colunas - em um intervalo de 5 amostras -, e caso a resposta seja Y - Yes/Sim -, faz o cálculo dessa média e escreve o arquivo "data_media_movel.txt").