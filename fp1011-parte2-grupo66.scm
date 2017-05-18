; Grupo 66: 70507 Rodrigo Portalet, 70554 Alexandre Pimentel, 70630 David Limpo

; NOTA: O código da segunda parte do projecto começa na linha 178
; Todos os comentários da solução da primeira parte do projecto feito pelos professores foram removidos,
; com a intenção de poupar papel na impressão e reduzir o tamanho do ficheiro.
; A linha de código (require "interface-damas.zo") não está incluída no programa. Para jogar damas seria
; necessário adicioná-la no início do programa.

(define (faz-pos l c)
  (if (and (inteiro-entre?  0 7 l) (inteiro-entre?  0 7 c))
      (cons l c)
      (error "faz-pos: os argumentos devem ser inteiros entre 0 e 7.")))

(define linha-pos car)
 
(define coluna-pos cdr)

(define (pos? x)
  (and (pair? x)
       (inteiro-entre?  0 7 (linha-pos x))
       (inteiro-entre?  0 7 (coluna-pos x))))

(define (inteiro-entre? n1 n2 n)
  (and (integer? n) (<= n1 n) (<= n n2)))
 
(define (pos=? pos1 pos2)
  (and (= (linha-pos pos1) (linha-pos pos2))
       (= (coluna-pos pos1) (coluna-pos pos2))))

(define (distancia pos1 pos2)
  (sqrt (+ (expt (- (linha-pos pos1) (linha-pos pos2)) 2)
           (expt (- (coluna-pos pos1) (coluna-pos pos2)) 2))))

(define (mesma-direccao? pos1 pos2)
  (or (= (linha-pos pos1) (linha-pos pos2)) 
      (= (coluna-pos pos1) (coluna-pos pos2))))

(define (adjacentes? pos1 pos2)
  (and (mesma-direccao? pos1 pos2)
       (= 1 (distancia pos1 pos2))))

(define (adjacentes pos)
  (let* ((l (linha-pos pos))
         (c (coluna-pos pos))
         (tentativas (list (cons (+ l 1) c)
                           (cons (- l 1) c)
                           (cons l (+ c 1))
                           (cons l (- c 1)))))
    (transforma (lambda (par)
                  (faz-pos (car par) (cdr par)))
                (filtra (lambda (tent)
                          (and (inteiro-entre?  0 7 (car tent))
                               (inteiro-entre?  0 7 (cdr tent))))
                        tentativas))))

(define (faz-jogada pos1 pos2)
  (if (and (pos? pos1) (pos? pos2))
      (cons pos1 pos2)
      (error "faz-jogada: os argumentos devem ser posições.")))

(define inicio-jogada car)

(define fim-jogada cdr)

(define (jogada? x)
  (and (pair? x)
       (pos? (car x))
       (pos? (cdr x))))

(define (jogada-nula? j)
  (pos=? (inicio-jogada j) (fim-jogada j)))

(define (faz-posicoes-de-jogada pecas-capturadas posicoes-livres)
  (if (or (not (lista? pecas-capturadas)) (not (lista? posicoes-livres))
          (not (todos-satisfazem? pos? pecas-capturadas))
          (not (todos-satisfazem? pos? posicoes-livres)))
      (error "faz-posicoes-de-jogada: os argumentos devem listas de posicoes")
      (cons pecas-capturadas posicoes-livres)))

(define pecas-capturadas car)

(define posicoes-livres cdr)

(define (posicoes-de-jogada jogada)
  
  (define (posicoes-de-jogada-aux jogada)
    (let* ((pos1 (inicio-jogada jogada))
           (pos2 (fim-jogada jogada))
           (l1 (linha-pos pos1))
           (c1 (coluna-pos pos1))
           (l2 (linha-pos pos2))
           (c2 (coluna-pos pos2)))
      (if (= l1 l2)
          (faz-posicoes-de-jogada
           (transforma (lambda (col) (faz-pos l1 col))
                       (pecas c1 c2))
           (transforma (lambda (col) (faz-pos l1 col))
                       (livres c1 c2)))
          (faz-posicoes-de-jogada
           (transforma (lambda (linha) (faz-pos linha c1))
                       (pecas l1 l2))
           (transforma (lambda (linha) (faz-pos linha c1))
                       (livres l1 l2))))))
  
  (if (or (not (jogada? jogada))
          (not (mesma-direccao? (inicio-jogada jogada) (fim-jogada jogada)))
          (not (even? (distancia (inicio-jogada jogada) (fim-jogada jogada)))))
      (nova-lista)
      (posicoes-de-jogada-aux jogada)))

(define (de-2-em-2 lim-inf lim-sup)
  (if (> lim-inf lim-sup)
      (nova-lista)
      (insere lim-inf
              (de-2-em-2 (+ 2 lim-inf)
                         lim-sup))))

(define (pecas inicio fim)  
  (de-2-em-2 (add1 (min inicio fim)) (max inicio fim)))

(define (livres inicio fim)  
  (if (> fim inicio)
      (de-2-em-2 (+ 2  inicio) fim)
      (de-2-em-2 fim (- inicio 2))))


(define (nova-lista)
  ())

(define (insere el lst)
  (cons el lst))

(define (primeiro lst)
  (if (null? lst)
      (error "primeiro: a lista não tem  elementos")
      (car lst)))

(define (resto lst)
  (if (null? lst)
      (error "resto: a lista não tem  elementos")
      (cdr lst)))

(define (lista? x)
  (cond ((null? x) #t)
        ((pair? x) (lista? (cdr x)))
        (else #f)))

(define (lista-vazia? lst)
  (null? lst))

(define (listas=? lst1 lst2 elem=?)
  (cond ((null? lst1) (null? lst2))
        ((null? lst2) #f)
        ((elem=? (car lst1) (car lst2)) 
         (listas=? (cdr lst1) (cdr lst2) elem=?))
        (else #f)))

(define (filtra tst? lst)
  (cond ((lista-vazia? lst) lst)
        ((tst? (primeiro lst))
         (insere (primeiro lst)
                 (filtra tst? (resto lst))))
        (else (filtra tst? (resto lst)))))

(define (transforma tr lst)
  (if (lista-vazia? lst) 
      lst
      (insere (tr (primeiro lst))
              (transforma tr (resto lst)))))

(define (todos-satisfazem? tst? lst)
  (cond ((lista-vazia? lst) #t)
        ((tst? (primeiro lst))
         (todos-satisfazem? tst?(resto lst)))
        (else #f)))


; Código pre-definido (pedido no enunciado)

(define (em proc mens . args)
(apply (proc mens) args))

(define controle null)

(define (damas-havaianas)
(set! controle (cria-controle)))

; Library necessária para o uso de ciclos
(require (lib "misc.ss" "swindle"))

;;; Tipo matriz (apresentado no livro)

; Construtor

(define (matriz l c)
  (define (preenche mat l inicio fim)
    (if (= inicio fim)
        (vector-set! mat inicio (make-vector l))
        (begin
          (vector-set! mat inicio (make-vector l))
          (preenche mat l (+ 1 inicio) fim))))
  (let ((mat (make-vector c)))
    (preenche mat l 0 (- c 1))
    mat))

; Selectores
(define (el-matriz mat l c)
  (vector-ref (vector-ref mat c) l))

(define (linha-matriz mat)
  (vector-length (vector-ref mat 0)))

(define (colunas-matriz mat)
  (vector-length mat))

; Modificador
(define (coloca-matriz! mat l c el)
  (vector-set! (vector-ref mat c) l el))

; O reconhecedor e o teste presentes no livro não foram incluídos pois nunca vão ser
; utilizados neste programa.

;;; O tipo tabuleiro

; Construtor
; tabuleiro-inicial: {} -> tabuleiro
; tabuleiro-inicial() devolve um tabuleiro em que cada posição contém ou o
; símbolo p ou o símbolo b, correspondendo ao tabuleiro inicial do jogo. Neste,
; todas as posições estão preenchidas com uma peça: peças brancas nas casas
; pretas, e vice-versa.
(define (tabuleiro-inicial)
  (define novo-tabuleiro (matriz 8 8))  ; Cria uma matriz auxiliar que vai ser modificada de acordo com as características do tabuleiro
  (dotimes (lin 8)
           (dotimes (col 8)
                    (if (even? lin)
                        (if (even? col)
                            (coloca-matriz! novo-tabuleiro lin col 'p) ; Ambas a linha e a coluna são pares -> peça preta
                            (coloca-matriz! novo-tabuleiro lin col 'b)) ; Linha par, coluna ímpar -> peça branca
                        (if (odd? col)
                            (coloca-matriz! novo-tabuleiro lin col 'p) ; Ambas a linha e a coluna são ímpares -> peça preta
                            (coloca-matriz! novo-tabuleiro lin col 'b))))) ; Linha ímpar, coluna par -> peça branca
  novo-tabuleiro)   ; É retornado o novo tabuleiro com as características do tabuleiro de jogada.


; Selector
; conteudo-tabuleiro : tabuleiro x posiçãao -> conteúdo
; conteudo-tabuleiro(t; p) devolve o conteúdo da posição p do tabuleiro t.
(define (conteudo-tabuleiro t p)
  (el-matriz t (linha-pos p) (coluna-pos p)))


; Modificadores
; coloca-peca! : tabuleiro x posição x conteúdo -> tabuleiro
; coloca-peca!(t; p; c) altera destrutivamente o tabuleiro t colocando c na posição p.
(define (coloca-peca! t p c)
  (coloca-matriz! t (linha-pos p) (coluna-pos p) c))


; retira-peca! : tabuleiro x posição -> tabuleiro
; retira-peca!(t; p) altera destrutivamente o tabuleiro t colocando o símbolo v na
; posição p.
(define (retira-peca! t p)
  (coloca-matriz! t (linha-pos p) (coluna-pos p) 'v))


; Procedimentos auxiliares
; modifica-tabuleiro! : tabuleiro x lista de posições x nova-peça -> tabuleiro
; modifica-tabuleiro!(t; lista; nova-peca) altera destrutivamente o tabuleiro t colocando
; a nova peça em todas as posições do tabuleiro presentes na lista
(define (modifica-tabuleiro! t lista nova-peca)
  (dolist (elem lista)
    (coloca-peca! t elem nova-peca)))

; faz-jog-tab! : tabuleiro x jogada x cor da peça que executa a jogada -> tabuleiro
; faz-jog-tab!(t; jog; cor) altera destrutivamente o tabuleiro t executando a jogada
; jog nesse tabuleiro, ou seja, colocando em todas as posições das pecas capturadas
; e na posição do inicio da jogada o conteudo v e na posição do fim da jogada a cor dada
(define (faz-jog-tab! t jog cor)
  (modifica-tabuleiro! t (pecas-capturadas (posicoes-de-jogada jog)) 'v)
  (retira-peca! t (inicio-jogada jog))
  (coloca-peca! t (fim-jogada jog) cor))

; imprime-tabuleiro: tabuleiro -> ()
; imprime-tabuleiro(t) imprime no ecrã (faz "display") ao conteúdo do tabuleiro segundo a
; seguinte forma do tabuleiro (exemplo do tabuleiro inicial):
; p b p b p b p b     (posições (0.0) a (0.7)
; b p b p b p b p     (posições (1.0) a (1.7)
; p b p b p b p b               ...
; b p b p b p b p
; p b p b p b p b
; b p b p b p b p
; p b p b p b p b
; b p b p b p b p     (posições (7.0) a (7.7)
; este procedimento não é utilizado no código final do projecto mas tendo em conta que nos
; foi muito útil para resolver problemas no código decidimos mantê-lo aqui
(define (imprime-tabuleiro t)
      (dotimes (lin 8)
               (dotimes (col 8)
                        (display (conteudo-tabuleiro t (faz-pos lin col)))
                        (display " "))
               (newline))
      (newline))


; Jogador automático
; cria-jogador66 : cor -> jogador
; (cria-jogador66 ’b) devolve um jogador automático que joga com peças brancas
; (cria-jogador66 ’p) devolve um jogador automático que joga com peças pretas

(define (cria-jogador66 cor)
  (let ((t (tabuleiro-inicial)))
    ; O jogador automático guarda internamente o estado do tabuleiro de jogo e actualiza-o sempre que 
    ; ele ou o jogador humano tiram uma peça ou jogam.
    
    ; Tira-primeira-peca
    ; Esta mensagem, enviada pela IGDH no início do jogo, pede ao jogador automático para tirar a primeira peça
    ; (se tiver as peças pretas) ou tirar a segunda peça (se tiver as peças brancas). No primeiro caso a posição
    ; devolvida terá de ser da diagonal principal e estar no meio ou nas pontas do tabuleiro, ou seja, ser
    ; uma das posições (0.0), (3.3), (4.4) ou (7.7). No segundo caso essa peça terá de ser adjacente à peça
    ; tirada pelo jogador humano (que faz parte da mensagem enviada pela IGDH). Em qualquer um dos casos
    ; o jogador automático escolhe uma peça aleatoriamente, retira-a do seu tabuleiro interno e por fim devolve-a. 
    (define (tira-primeira-peca . pos)
      (let ((x (random 4)) ; É atribuído a x um valor aleatório entre 0 e 3
            (peca (faz-pos 0 0))) ; Se algo correr mal a peça devolvida será a posição (0.0), para termos a certeza que é uma posição
        (if (eq? cor 'p)
            (cond ((= x 0) (set! peca (faz-pos 0 0))) ; Dependendo do valor de x é atribuída uma posição a "peca"
                  ((= x 1) (set! peca (faz-pos 3 3)))
                  ((= x 2) (set! peca (faz-pos 4 4)))
                  (else    (set! peca (faz-pos 7 7))))
            (if (lista-vazia? pos)
                (error "Jogador preto tira primeira peça.") ; Se o jogador criado tiver as peças brancas e não for dada nenhuma posição
                     ; na mensagem enviada pela IGDH então é devolvido um erro, pois não se consegue escolher nenhuma posição adjacente
                (do ((y x (- y 1)) ; Neste ciclo é escolhida aleatoriamente uma posição adjacente à posição dada
                     (lista-posicoes (adjacentes (primeiro pos)) (resto lista-posicoes)))
                  ((or (lista-vazia? (resto lista-posicoes)) (= y 0)) (set! peca (primeiro lista-posicoes))))))
        (retira-peca! t peca) ; É modificado o tabuleiro interno retirando a peça escolhida
        peca)) ; É devolvida a peça escolhida
    
    ; Humano-tirou
    ; É actualizado o tabuleiro interno do jogador automático retirando a peça que o jogador retirou
    (define (humano-tirou pos)
      (retira-peca! t pos))
    
    ; Joga
    ; Devolve uma jogada que depende do estado actual do tabuleiro e da cor do jogador.
    ; Essa jogada apenas "come" uma peça adversária, podendo ser o código melhorado para o fazer
    ; a mais do que uma peça quando possível.
    (define (joga)
      (let ((jog (faz-jogada (faz-pos 0 0) (faz-pos 0 0)))) ; É atribuído a jog uma jogada nula para depois ser possível
                                                            ; entrar no "if" do segundo ciclo "dotimes", o qual deixa de
                                                            ; ser verificado assim que for encontrada uma jogada
                
        (define (joga-aux pos cor)
          
          ; Se a linha actual for inferior ou igual a cinco, o conteúdo da posição na linha a seguir mas na mesma coluna 
          ; for igual à cor do adversario e o conteúdo da posição duas linhas a seguir for v, então a jogada que é devolvida
          ; vai começar na posição actual e acabar na posição duas linhas a seguir mas na mesma coluna.    
          (if (and (<= (linha-pos pos) 5) (eq? (conteudo-tabuleiro t (faz-pos (+ (linha-pos pos) 1) (coluna-pos pos))) cor) (eq? (conteudo-tabuleiro t (faz-pos (+ (linha-pos pos) 2) (coluna-pos pos))) 'v)) 
              (faz-jogada pos (faz-pos (+ (linha-pos pos) 2) (coluna-pos pos)))
          ; Se pelo contrário a condição anterior não se verificar então verifica-se o mesmo mas nas colunas em vez das linhas    
              (if (and (<= (coluna-pos pos) 5) (eq? (conteudo-tabuleiro t (faz-pos (linha-pos pos) (+ (coluna-pos pos) 1))) cor) (eq? (conteudo-tabuleiro t (faz-pos (linha-pos pos) (+ (coluna-pos pos) 2))) 'v))
                  (faz-jogada pos (faz-pos (linha-pos pos) (+ (coluna-pos pos) 2)))
          ; Novamente a verificação nas linhas mas agora no sentido contrário        
                  (if (and (>= (linha-pos pos) 2) (eq? (conteudo-tabuleiro t (faz-pos (- (linha-pos pos) 1) (coluna-pos pos))) cor) (eq? (conteudo-tabuleiro t (faz-pos (- (linha-pos pos) 2) (coluna-pos pos))) 'v)) 
                     (faz-jogada pos (faz-pos (- (linha-pos pos) 2) (coluna-pos pos)))
         ; Agora a verificação nas colunas e também no sentido contrário             
                      (if (and (>= (coluna-pos pos) 2) (eq? (conteudo-tabuleiro t (faz-pos (linha-pos pos) (- (coluna-pos pos) 1))) cor) (eq? (conteudo-tabuleiro t (faz-pos (linha-pos pos) (- (coluna-pos pos) 2))) 'v))
                          (faz-jogada pos (faz-pos (linha-pos pos) (- (coluna-pos pos) 2)))
         ; Por fim se nenhuma das condições anteriores se verificar é devolvida uma jogada nula, que tem início e fim 
         ; numa posição em que se tem a certeza que está uma peça da cor do jogador automático
                          (faz-jogada pos pos))))))
        
        ; Este ciclo do "joga" percorre todas as posições do tabuleiro
        (dotimes (lin 8)
                 (dotimes (col 8)
                          (if (jogada-nula? jog)
                              (if (eq? cor 'p)
                                  ; Se o jogador tiver as peças de cor preta, na posição actual do tabuleiro (no ciclo) estiver uma peça preta e
                                  ; se essa posição tiver linha e coluna pares ou linha e coluna impares (esta última parte é redundante mas
                                  ; tendo em conta que o conteúdo do tabuleiro poderia ficar errado por alguma razão, decidimos mantê-la),
                                  ; atribui-se a "jog" o valor devolvido pelo joga-aux dessa posição, tendo em conta que as posições adjacentes
                                  ; têm de ter a cor adversária.
                                  (if (and (eq? (conteudo-tabuleiro t (faz-pos lin col)) 'p) (or (and (even? lin) (even? col)) (and (odd? lin) (odd? col))))
                                      (set! jog (joga-aux (faz-pos lin col) 'b)))
                                  ; O raciocínio aqui é o mesmo, mas para as peças brancas
                                  (if (and (eq? (conteudo-tabuleiro t (faz-pos lin col)) 'b) (or (and (even? lin) (odd? col)) (and (odd? lin) (even? col))))
                                      (set! jog (joga-aux (faz-pos lin col) 'p)))))))
        ; É actualizado o tabuleiro interno do jogador e devolvida a jogada escolhida.
        (faz-jog-tab! t jog cor)
        jog))
             
    ; Humano-jogou
    ; É actualizado o tabuleiro interno com a jogada do humano. Aqui a única coisa em ter em conta é que a cor da peça
    ; que executa a jogada é contrária à cor do jogador automático.
    (define (humano-jogou jog)
      (faz-jog-tab! t jog (if (eq? cor 'p)
                              'b
                              'p)))
     
;;;;;;;;;;;;;;; Código redundante apenas para a participação no torneio, todos os comentários foram removidos para poupar espaço e papel ;;;;;;;;;;;;;;
    (define (faz-pos l c)
      (if (and (inteiro-entre?  0 7 l) (inteiro-entre?  0 7 c))
          (cons l c)
          (error "faz-pos: os argumentos devem ser inteiros entre 0 e 7.")))
    
    (define linha-pos car)
    
    (define coluna-pos cdr)
    
    (define (pos? x)
      (and (pair? x)
           (inteiro-entre?  0 7 (linha-pos x))
           (inteiro-entre?  0 7 (coluna-pos x))))
    
    (define (inteiro-entre? n1 n2 n)
      (and (integer? n) (<= n1 n) (<= n n2)))
    
    (define (pos=? pos1 pos2)
      (and (= (linha-pos pos1) (linha-pos pos2))
           (= (coluna-pos pos1) (coluna-pos pos2))))
    
    (define (distancia pos1 pos2)
      (sqrt (+ (expt (- (linha-pos pos1) (linha-pos pos2)) 2)
               (expt (- (coluna-pos pos1) (coluna-pos pos2)) 2))))
    
    (define (mesma-direccao? pos1 pos2)
      (or (= (linha-pos pos1) (linha-pos pos2)) 
          (= (coluna-pos pos1) (coluna-pos pos2))))
    
    (define (adjacentes? pos1 pos2)
      (and (mesma-direccao? pos1 pos2)
           (= 1 (distancia pos1 pos2))))
    
    (define (adjacentes pos)
      (let* ((l (linha-pos pos))
             (c (coluna-pos pos))
             (tentativas (list (cons (+ l 1) c)
                               (cons (- l 1) c)
                               (cons l (+ c 1))
                               (cons l (- c 1)))))
        (transforma (lambda (par)
                      (faz-pos (car par) (cdr par)))
                    (filtra (lambda (tent)
                              (and (inteiro-entre?  0 7 (car tent))
                                   (inteiro-entre?  0 7 (cdr tent))))
                            tentativas))))
    
    (define (faz-jogada pos1 pos2)
      (if (and (pos? pos1) (pos? pos2))
          (cons pos1 pos2)
          (error "faz-jogada: os argumentos devem ser posições.")))
    
    (define inicio-jogada car)
    
    (define fim-jogada cdr)
    
    (define (jogada? x)
      (and (pair? x)
           (pos? (car x))
           (pos? (cdr x))))
    
    (define (jogada-nula? j)
      (pos=? (inicio-jogada j) (fim-jogada j)))
    
    (define (faz-posicoes-de-jogada pecas-capturadas posicoes-livres)
      (if (or (not (lista? pecas-capturadas)) (not (lista? posicoes-livres))
              (not (todos-satisfazem? pos? pecas-capturadas))
              (not (todos-satisfazem? pos? posicoes-livres)))
          (error "faz-posicoes-de-jogada: os argumentos devem listas de posicoes")
          (cons pecas-capturadas posicoes-livres)))
    
    (define pecas-capturadas car)
    
    (define posicoes-livres cdr)
    
    (define (posicoes-de-jogada jogada)
      
      (define (posicoes-de-jogada-aux jogada)
        (let* ((pos1 (inicio-jogada jogada))
               (pos2 (fim-jogada jogada))
               (l1 (linha-pos pos1))
               (c1 (coluna-pos pos1))
               (l2 (linha-pos pos2))
               (c2 (coluna-pos pos2)))
          (if (= l1 l2)
              (faz-posicoes-de-jogada
               (transforma (lambda (col) (faz-pos l1 col))
                           (pecas c1 c2))
               (transforma (lambda (col) (faz-pos l1 col))
                           (livres c1 c2)))
              (faz-posicoes-de-jogada
               (transforma (lambda (linha) (faz-pos linha c1))
                           (pecas l1 l2))
               (transforma (lambda (linha) (faz-pos linha c1))
                           (livres l1 l2))))))
      
      (if (or (not (jogada? jogada))
              (not (mesma-direccao? (inicio-jogada jogada) (fim-jogada jogada)))
              (not (even? (distancia (inicio-jogada jogada) (fim-jogada jogada)))))
          (nova-lista)
          (posicoes-de-jogada-aux jogada)))
    
    (define (de-2-em-2 lim-inf lim-sup)
      (if (> lim-inf lim-sup)
          (nova-lista)
          (insere lim-inf
                  (de-2-em-2 (+ 2 lim-inf)
                             lim-sup))))
    
    (define (pecas inicio fim)  
      (de-2-em-2 (add1 (min inicio fim)) (max inicio fim)))
    
    (define (livres inicio fim)  
      (if (> fim inicio)
          (de-2-em-2 (+ 2  inicio) fim)
          (de-2-em-2 fim (- inicio 2))))
    
    
    (define (nova-lista)
      ())
    
    (define (insere el lst)
      (cons el lst))
    
    (define (primeiro lst)
      (if (null? lst)
          (error "primeiro: a lista não tem  elementos")
          (car lst)))
    
    (define (resto lst)
      (if (null? lst)
          (error "resto: a lista não tem  elementos")
          (cdr lst)))
    
    (define (lista? x)
      (cond ((null? x) #t)
            ((pair? x) (lista? (cdr x)))
            (else #f)))
    
    (define (lista-vazia? lst)
      (null? lst))
    
    (define (listas=? lst1 lst2 elem=?)
      (cond ((null? lst1) (null? lst2))
            ((null? lst2) #f)
            ((elem=? (car lst1) (car lst2)) 
             (listas=? (cdr lst1) (cdr lst2) elem=?))
            (else #f)))
    
    (define (filtra tst? lst)
      (cond ((lista-vazia? lst) lst)
            ((tst? (primeiro lst))
             (insere (primeiro lst)
                     (filtra tst? (resto lst))))
            (else (filtra tst? (resto lst)))))
    
    (define (transforma tr lst)
      (if (lista-vazia? lst) 
          lst
          (insere (tr (primeiro lst))
                  (transforma tr (resto lst)))))
    
    (define (todos-satisfazem? tst? lst)
      (cond ((lista-vazia? lst) #t)
            ((tst? (primeiro lst))
             (todos-satisfazem? tst?(resto lst)))
            (else #f)))
       
    (define (matriz l c)
      (define (preenche mat l inicio fim)
        (if (= inicio fim)
            (vector-set! mat inicio (make-vector l))
            (begin
              (vector-set! mat inicio (make-vector l))
              (preenche mat l (+ 1 inicio) fim))))
      (let ((mat (make-vector c)))
        (preenche mat l 0 (- c 1))
        mat))
    
    (define (el-matriz mat l c)
      (vector-ref (vector-ref mat c) l))
    
    (define (linha-matriz mat)
      (vector-length (vector-ref mat 0)))
    
    (define (colunas-matriz mat)
      (vector-length mat))
    
    (define (coloca-matriz! mat l c el)
      (vector-set! (vector-ref mat c) l el))
    
    (define (em proc mens . args)
(apply (proc mens) args))
    
    (define (tabuleiro-inicial)
      (define novo-tabuleiro (matriz 8 8))  
      (dotimes (lin 8)
               (dotimes (col 8)
                        (if (even? lin)
                            (if (even? col)
                                (coloca-matriz! novo-tabuleiro lin col 'p) 
                                (coloca-matriz! novo-tabuleiro lin col 'b))
                            (if (odd? col)
                                (coloca-matriz! novo-tabuleiro lin col 'p) 
                                (coloca-matriz! novo-tabuleiro lin col 'b))))) 
      novo-tabuleiro)  
    
    (define (conteudo-tabuleiro t p)
      (el-matriz t (linha-pos p) (coluna-pos p)))
    
    (define (coloca-peca! t p c)
      (coloca-matriz! t (linha-pos p) (coluna-pos p) c))
    
    (define (retira-peca! t p)
      (coloca-matriz! t (linha-pos p) (coluna-pos p) 'v))
    
    (define (modifica-tabuleiro! t lista nova-peca)
      (dolist (elem lista)
              (coloca-peca! t elem nova-peca)))
    
    (define (faz-jog-tab! t jog cor)
      (modifica-tabuleiro! t (pecas-capturadas (posicoes-de-jogada jog)) 'v)
      (retira-peca! t (inicio-jogada jog))
      (coloca-peca! t (fim-jogada jog) cor))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Fim do código para a participação no torneio ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; Procedimento que é devolvido com a criação do jogador automático.
    (lambda (msg)
      (cond ((eq? msg 'tira-primeira-peca)        
             tira-primeira-peca)

            ((eq? msg 'humano-tirou)
            humano-tirou)
            
            ((eq? msg 'joga)
             joga)
             
            ((eq? msg 'humano-jogou)
             humano-jogou)))))
 
; Controle
; (cria-controle) : {} -> controle

(define (cria-controle)
  ; O controle guarda internamente o tabuleiro, indo modificando-o ao longo do jogo tal como o jogador faz,
  ; a cor correspondente ao jogador humano, sendo esta apenas atribuída quando o procedimento
  ; humano-escolheu-cor é executado e guarda também o jogador automático, o qual é atribuído também no
  ; procedimento humano-escolheu-cor.
  (let ((t (tabuleiro-inicial))
        (cor-hum 'x)
        (j (void)))
    
    ; O procedimento humano-escolheu-cor actualiza as variáveis cor-hum (sendo esta a cor escolhida pelo jogador humano),
    ; cria o jogador automático com a cor contáaria à do jogador, atribuindo esse jogador à variavel j, e invoca o (dh-pede-primeiras-pecas j)
     (define (humano-escolheu-cor cor)
      (set! cor-hum cor)
      (if (eq? cor 'p)
          (set! j (cria-jogador66 'b))
          (set! j (cria-jogador66 'p)))
      (dh-pede-primeiras-pecas j))
    
    ; O procedimento primeiras-pecas retira do tabuleiro interno do controle as peças retiradas pelos jogadores e, dependendo de quem
    ; for o primeiro a jogar, vai pedir, se a cor das peças do humano for preta, a jogada do humano à IGDH ou, caso contrário, vai
    ; pedir uma jogada ao jogador automático, validar essa jogada, terminando o jogo se a jogada for nula ou assinalando erro se for inválida,
    ; e de seguida pedir a jogada do humano à IGDH, actualizando também o tabuleiro interno com a jogada do jogador automático.
    ; A validação de uma jogada (se é válida ou não, se for nula nem sequer chega a ser validada) é feita pelo procedimento auxiliar
    ; valida-jogada, definido abaixo.
    (define (primeiras-pecas pos-humano pos-automatico)
      (retira-peca! t pos-humano)
      (retira-peca! t pos-automatico)
      (if (eq? cor-hum 'p)
          (dh-aceita-jogada)
          (let ((jogada-autom (em j 'joga)))
            (if (jogada-nula? jogada-autom)
                (dh-termina-jogo 'humano)
                (if (valida-jogada t jogada-autom 'p)
                    (begin
                      (dh-executa-jogada jogada-autom (pecas-capturadas (posicoes-de-jogada jogada-autom)))
                      (dh-aceita-jogada)
                      (faz-jog-tab! t jogada-autom 'p))
                    (dh-assinala-erro 'automatico jogada-autom))))))
    
    ; O procedimento recebe-jogada-humano valida a jogada do jogador humano, terminando o jogo se esta jogada for nula, assinalando erro se
    ; for inválida ou executando essa jogada se ela não for nula e for válida. De seguida informa o jogador automático da jogada do humano
    ; e actualiza o seu tabuleiro interno (o tabuleiro do controle). Finalmente, pede uma jogada ao jogador automático, valida essa jogada
    ; vendo se é nula e válida, enviando as mensagems adequadas à IGDH se não for, pede à IGDH uma jogada do jogador humano e actualiza
    ; o seu tabuleiro interno.
    (define (recebe-jogada-humano jogada)
      (if (jogada-nula? jogada)
          (dh-termina-jogo 'automatico)
          (if (valida-jogada t jogada cor-hum)
              (begin
                (dh-executa-jogada jogada (pecas-capturadas (posicoes-de-jogada jogada)))
                (em j 'humano-jogou jogada)
                (faz-jog-tab! t jogada cor-hum)
                (let ((jogada-autom (em j 'joga)))
                  (if (jogada-nula? jogada-autom)
                      (dh-termina-jogo 'humano)
                      (if (valida-jogada t jogada-autom (if (eq? cor-hum 'p) 'b 'p))
                          (begin
                        (dh-executa-jogada jogada-autom (pecas-capturadas (posicoes-de-jogada jogada-autom)))
                        (dh-aceita-jogada)
                        (faz-jog-tab! t jogada-autom (if (eq? cor-hum 'p) 'b 'p)))
                          (dh-assinala-erro 'automatico jogada-autom)))))
              (dh-assinala-erro 'humano jogada))))
    
    ; valida-jogada: tabuleiro x jogada x cor -> lógico
    ; valida-jogada(tab; jogada; cor) devolve #t se uma jogada for válida (podendo também ser nula) e #f se não for.
    ; Utiliza uma variável de controlo (que inicialmente toma o valor 0) com esse mesmo nome que passa 1 assim que
    ; encontrar algo de inválido numa jogada. Devolve o resultado da comparação entre essa variável e 0, ou seja,
    ; #t se for 0 (pois a jogada será então válida) e #f se for 1 (pois será inválida).
    (define (valida-jogada tab jogada cor)
      (let ((variavel-de-controlo 0))
        ; Se uma jogada simplesmente não for uma jogada, o conteúdo da peça inicial não for da cor certa ou
        ; o início e o fim da jogada não estiverem na mesma direcção, então a variável de controlo passa a 1.
        (if (or (not (jogada? jogada)) (not (eq? (conteudo-tabuleiro tab (inicio-jogada jogada)) cor)) (not (mesma-direccao? (inicio-jogada jogada) (fim-jogada jogada))))
                    (set! variavel-de-controlo 1)
                    (begin 
                      ; É percorrida a lista das peças capturadas, tendo todas estas de ter a cor da peça adversária.
                      ; Se alguma delas não tiver a cor da peça adversária, então a variável de controlo passa a 1,
                      ; pois a jogada é inválida.
                      (dolist (x (pecas-capturadas (posicoes-de-jogada jogada)))
                              (if (not (eq? (conteudo-tabuleiro tab x) (if (eq? cor 'p)
                                                                           'b 
                                                                           'p)))
                                  (set! variavel-de-controlo 1)))
                      ; É percorrida a lista das posições livres, tendo todas estas de estar vazias.
                      ; Se alguma delas não estiver vazia, então a variável de controlo passa a 1, pois a jogada é inválida.
                      (dolist (x (posicoes-livres (posicoes-de-jogada jogada)))
                              (if (not (eq? (conteudo-tabuleiro tab x) 'v))
                                  (set! variavel-de-controlo 1)))))  
        (= variavel-de-controlo 0)))
    
    (dh-inicio) ; Invocação do (dh-inicio) assim que o controle é criado
    
    ; Procedimento que é devolvido com a criação do controle.
    (lambda (m)
      (case m
        ((humano-escolheu-cor) humano-escolheu-cor)
        ((primeiras-pecas) primeiras-pecas)
        ((recebe-jogada-humano) recebe-jogada-humano)))))