# Formalização do Algoritmo de Remoção de Recursão à Esquerda

Essa é uma formalização do algoritmo clássico de remoção de recursão à esquerda de Greibach. O algoritmo é baseado na premissa de que a recursão à esquerda pode ser eliminada de uma gramática livre de contexto (GLC) sem alterar a linguagem gerada pela GLC.

A implementação do algoritmo foi realizada utilizando a linguagem de programação Racket, com as bibliotecas `PLT Redex` e `RackCheck`. O PLT Redex é uma biblioteca integrada ao Racket que facilita a especificação de sistemas de redução sensíveis ao contexto, enquanto o RackCheck é uma biblioteca de testes baseados em propriedades para Racket. Durante o desenvolvimento, utilizou-se o `DrRacket` como ambiente de desenvolvimento integrado (IDE), na versão 8.8

## Execução

### Testes
Abra o arquivo `validador.rkt` e clique em `Run`. Caso deseje modificar o tamanho da gramática, altere os valores das variáveis no arquivo `gerador.rkt`:
1. `max-trms`: quantidade máxima de terminais
2. `min-trms`: quantidade mínima de terminais
3. `max-non-trms`: quantidade máxima de não-terminais
4. `min-non-trms`: quantidade mínima de não-terminais
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

(define orded-prds (ord-rhs (unify-prds input)))
(traces i--> orded-prds)
```
