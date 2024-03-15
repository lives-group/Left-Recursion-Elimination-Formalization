# Elimination-Left-Recursion
Formalização dos algoritmos de eliminação de recursão à esquerda

## Adições que serão feitas

1. Fazer código para gerar gramáticas recursivas à esquerda.
    - ~~Gerar insumos para o teste (usando racket puro, olhar repositório)~~
    - ~~Verificar se a gramática não é recursiva à esquerda (direta e indireta) (rackcheck)~~
    - ~~Escrever~~ 
    - Verificar a equivalência da linguagem gerada pela a gramática original e a gramática resultante (Thiago)
        - Usar a derivada para gerar a entrada da gramática 1 (com recursão à esquerda)
        - Usar a derivada na entrada gerada pela gramática  1 para verificar se ela pertence a linguagem da gramática 2 (sem recursão à esquerda)
        - Gerar um palavra da gramática 2 e verificar se ela pertence a linguagem da gramática 1
        - Testar palavras que não pertencem a linguagem da gramática 1 e verificar se elas não pertencem a linguagem da gramática 2
        - Testar palavras que não pertencem a linguagem da gramática 2 e verificar se elas não pertencem a linguagem da gramática 1
        - Teste baseado em propriedades
        - Cobertura de código (VAI SER O ÚLTIMO A SER FEITO)
