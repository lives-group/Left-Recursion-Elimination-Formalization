# Formalização do Algoritmo de Remoção de Recursão à Esquerda

Essa é uma formalização do algoritmo clássico de remoção de recursão à esquerda de Greibach. O algoritmo é baseado na premissa de que a recursão à esquerda pode ser eliminada de uma gramática livre de contexto (GLC) sem alterar a linguagem gerada pela GLC.

A implementação do algoritmo foi realizada utilizando a linguagem de programação Racket, com as bibliotecas `PLT Redex` e `RackCheck`. O PLT Redex é uma biblioteca integrada ao Racket que facilita a especificação de sistemas de redução sensíveis ao contexto, enquanto o RackCheck é uma biblioteca de testes baseados em propriedades para Racket. Durante o desenvolvimento, utilizou-se o `DrRacket` como ambiente de desenvolvimento integrado (IDE), na versão 8.8

## Execução

### Testes
Para executar os testes é necessário realizar o download do repositório `CFG entry generator` (https://github.com/lives-group/cfg-entry-generator) e adicionar o diretório `cfg-entry-generator` no mesmo nível do diretório `Left-Recursion-Elimination-Formalization`. Em seguida, abra o arquivo `validador.rkt` e clique em `Run`. Caso deseje modificar o tamanho da gramática, altere os valores das variáveis no arquivo `gerador.rkt`:
1. `max-terminals`: quantidade máxima de terminais
2. `min-terminals`: quantidade mínima de terminais
3. `max-non-terminals`: quantidade máxima de não-terminais
4. `min-non-terminals`: quantidade mínima de não-terminais
5. `max-rhs`: quantidade máxima de termos no lado direito da regra
6. `max-seq`: tamanho máximo de um termo no lado direito da regra

Esses valores são utilizados para gerar gramáticas aleatórias que serão utilizadas nos testes. A quantidade de testes e o tempo limite para execução de cada teste podem ser alterados no arquivo `validador.rkt`.

### Algoritmo
Para executar somente o algoritmo de remoção de recursão à esquerda, abra o arquivo `classico.rkt` e insira o código abaixo e insira a gramática que deseja remover a recursão à esquerda e clique em `Run`.\
Exemplo:
```racket
; Alterar o valor de input para a gramática desejada
(define input '(
               (S ((B 2) (A 4) (2)))
               (C ((A) (7 2)))
               (B ((S 2) (B 3)))
               (A ((C A) (S 2)))
               (B ((A) (7 2)))
               ))

(define ordered-productions (order-rhs (unify-productions input)))
(traces i--> ordered-productions)
```