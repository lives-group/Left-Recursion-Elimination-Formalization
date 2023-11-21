# Elimination-Left-Recursion
Formalização dos algoritmos de eliminação de recursão à esquerda

## Adições que serão feitas

1. Fazer código para gerar gramáticas recursivas à esquerda.
    - Gerar insumos para o teste (usando racket puro, olhar repositório)
    - Verificar se a gramática não é recursiva à esquerda (direta e indireta) (Racket Check)
    - Verificar a equivalência da linguagem gerada pela a gramática original e a gramática resultante (Thiago)
        - Teste baseado em propriedades
        - Cobertura de código (VAI SER O ÚLTIMO A SER FEITO)

2. (Se der tempo)
    - Robert Moore
        - A -> abcBD | abBBA
        - A -> abA'
        - A' -> cBD | BBA
