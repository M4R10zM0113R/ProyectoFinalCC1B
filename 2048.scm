#| Proyecto Final CC1B
Integrantes:
	Mario Möller, 16012890
Control de Versión:
https://github.com/M4R10zM0113R/ProyectoFinalCC1B
|#

#| Librerias Requeridas |#
(require
 graphics/graphics
 )

#| Operaciones Adicionales |#
(define (exponente_del numero base)
  (/ (log numero) (log base))
  )

#| Estructura Matriz |#
(define (crear-matriz filas columnas)
  (build-vector filas (lambda (valor_fila) (build-vector columnas (lambda (valor_columna) 0))))
  )

(define (matriz-set! matriz pos_fila pos_columna valor_nuevo)
  (vector-set! (vector-ref matriz pos_fila) pos_columna valor_nuevo)
  )

(define (matriz-ref matriz pos_fila pos_columna) ; Refiere a 1 sólo dato
  (vector-ref (vector-ref matriz pos_fila) pos_columna)
  )

(define (matriz-ref-fila matriz pos_fila) ; Redundante/Inútil, pero aclara el código más adelante
  (vector-ref matriz pos_fila)
  )

(define (matriz-ref-columna matriz pos_columna)
  (do
      (
       [fila 0 (+ fila 1)]
       [columna (list )]
       )
      (
       (= (vector-length matriz) fila)
       (set! columna (list->vector columna))
       columna
       )
    (set! columna (append columna (list (matriz-ref matriz fila pos_columna))))
    )
  )

#| Operación sobre matrices |#
(define (contar-numero matriz propiedades_de_busqueda numero_a_buscar)
  #|
  propiedades_de_busqueda:
      0 -> en fila
      1 -> en columna
      2 -> toda la matriz
  |#
  (define filas_matriz (vector-length matriz))
  (define columnas_matriz (vector-length (vector-ref matriz 0)))
  ;(define tipo_busqueda (list-ref propiedades_de_busqueda 0))    ; en fila: 0, en columna: 1, todo: 2
  ;(define posicion_inicial (list-ref propiedades_de_busqueda 1)) ; 0 - (sub1 (vector-length vector_a_buscar_en))
  ;(define posicion_final (list-ref propiedades_de_busqueda 2))   ; posicion_inicial - (sub1 (vector-length vector_a_buscar_en))
  (cond
   ((= propiedades_de_busqueda 0)
    
    )
   ((= propiedades_de_busqueda 1)
    
    )
   ((= propiedades_de_busqueda 2)
    (do
	(
	 [posicion_total 0 (+ posicion_total 1)]
	 [posicion_columna 0 (modulo posicion_total filas_matriz)]
	 [posicion_fila 0 (if (and
			       (not (= posicion_total 0))
			       (= posicion_columna 0)
			       )
			      (+ posicion_fila 1)
			      (+ posicion_fila 0)
			      )
			]
	 [coincidencias 0]
	 )
	(
	 (= posicion_total (* (sub1 filas_matriz) (sub1 columnas_matriz)))
	 coincidencias
	 )
      (if (= (matriz-ref matriz posicion_fila posicion_columna) numero_a_buscar)
	  (set! coincidencias (+ coincidencias 1))
	  )
      )
    )
   )
  )

(define (buscar-numero matriz propiedades_de_busqueda numero_a_buscar)
  #|
  propiedades_de_busqueda:
      0 -> en fila
      1 -> en columna
      2 -> toda la matriz
  |#
  (define filas_matriz (vector-length matriz))
  (define columnas_matriz (vector-length (vector-ref matriz 0)))
  (cond
   ((= propiedades_de_busqueda 0)
    
    )
   ((= propiedades_de_busqueda 1)
    
    )
   ((= propiedades_de_busqueda 2)
    (do
	(
	 [posicion_total 0 (+ posicion_total 1)]
	 [posicion_columna 0 (modulo posicion_total filas_matriz)]
	 [posicion_fila 0 (if (and
			       (not (= posicion_total 0)) (= posicion_columna 0)
			       )
			      (+ posicion_fila 1)
			      (+ posicion_fila 0)
			      )
			]
	 [coincidencias (list )]
	 )
	(
	 (= posicion_total (* (sub1 filas_matriz) (sub1 columnas_matriz)))
	 coincidencias
	 )
      (if (= (matriz-ref matriz posicion_fila posicion_columna) numero_a_buscar)
	  (set! coincidencias (append coincidencias (list (list posicion_fila posicion_columna))))
	  )
      )
    )
   )
  )

(define (quitar-ceros vector_numeros)
  (do
      (
       [posicion 0 (+ posicion 1)]
       [lista_numeros (list )]
       [vector_sin_ceros (vector )]
       )
      (
       (= posicion (vector-length vector_numeros))
       (set! vector_sin_ceros (list->vector lista_numeros))
       vector_sin_ceros
       )
    (if (not (= (vector-ref vector_numeros posicion) 0))
	(set! lista_numeros (append lista_numeros (list (vector-ref vector_numeros posicion))))
	)
    )
  )

(define (agregar-numeros-iguales vector_numeros direccion)
  (do
      (
       [posicion 0 (+ posicion 1)]
       [lista_numeros_operados (list )]
       [vector_operados (vector )]
       )
      (
       (= posicion (vector-length vector_numeros))
       (if (= direccion 0)
	   (do
	       (
		)
	       (
		(= (length lista_numeros_operados) (vector-length vector_numeros))
		)
	     (set! lista_numeros_operados (append lista_numeros_operados (list 0)))
	     )
	   (do
	       (
		[lista_reversa (reverse lista_numeros_operados)]
		)
	       (
		(= (length lista_reversa) (vector-length vector_numeros))
		(set! lista_numeros_operados (reverse lista_reversa))
		)
	     (set! lista_reversa (append lista_reversa (list 0)))
	     )
	   )
       (set! vector_operados (list->vector lista_numeros_operados))
       vector_operados
       )
    (if (and
	 (not (= posicion (sub1 (vector-length vector_numeros))))
	 (=
	  (vector-ref vector_numeros posicion) (vector-ref vector_numeros (add1 posicion))
	  )
	 )
	(begin
	  (set! lista_numeros_operados (append lista_numeros_operados (list (+ (vector-ref vector_numeros posicion) (vector-ref vector_numeros (add1 posicion))))))
	  (set! posicion (+ posicion 1))
	  )
	(set! lista_numeros_operados (append lista_numeros_operados (list (vector-ref vector_numeros posicion))))
	)
    )
  )

(define (2048-movimiento matriz direccion jugador) ; Siguiendo convención WASD
  (cond
   ((= direccion 0) ; Arriba
    (display (string-append "Tiró hacia Arriba el jugador " jugador))
    (do
	(
	 [columna_a_sumar 0 (+ columna_a_sumar 1)]
	 [columnas_de_matriz (vector-length (vector-ref matriz 0))]
	 [lista_operadas (list )]
	 [matriz_operadas (vector )]
	 )
	(
	 (= (sub1 columnas_de_matriz) columna_a_sumar)
	 (set! matriz_operadas (list->vector lista_operadas))
	 ; Voltearlo
	 (do
	     (
	      [columna_a_fila 0 (+ columna_a_fila 1)]
	      [lista_inversa (list )]
	      [matriz_inversa (vector )]
	      )
	     (
	      (= columna_a_fila (sub1 (vector-length matriz_operadas)))
	      (set! matriz_operadas (list->vector lista_inversa))
	      )
	   (set! lista_inversa (append lista_inversa (list (matriz-ref-columna matriz_operadas columna_a_fila))))
	   ) ; Se vuelve columna las filas y viceversa
	 matriz_operadas
	 )
      (set! lista_operadas (append lista_operadas (list agregar-numeros-iguales (matriz-ref-columna matriz columna_a_sumar) 0)))
      )
    )
   ((= direccion 1) ; Izquierda
    (display (string-append "Tiró hacia la Izquierda el jugador " jugador))
    (do
	(
	 [fila_a_sumar 0 (+ fila_a_sumar 1)]
	 [filas_de_matriz (vector-length (vector-ref matriz 0))]
	 [lista_operadas (list )]
	 [matriz_operadas (vector )]
	 )
	(
	 (= (sub1 filas_de_matriz) fila_a_sumar)
	 (set! matriz_operadas (list->vector lista_operadas))
	 matriz_operadas
	 )
      (set! lista_operadas (append lista_operadas (list (agregar-numeros-iguales (matriz-ref-fila matriz fila_a_sumar) 0))))
      )
    )
   ((= direccion 2) ; Abajo
    (display (string-append "Tiró hacia Abajo el jugador " jugador))
    (do
	(
	 [columna_a_sumar 0 (+ columna_a_sumar 1)]
	 [columnas_de_matriz (vector-length (vector-ref matriz 0))]
	 [lista_operadas (list )]
	 [matriz_operadas (vector )]
	 )
	(
	 (= (sub1 columnas_de_matriz) columna_a_sumar)
	 (set! matriz_operadas (list->vector lista_operadas))
	 ; Invertirlo
	 (do
	     (
	      [columna_a_fila 0 (+ columna_a_fila 1)]
	      [lista_inversa (list )]
	      [matriz_inversa (vector )]
	      )
	     (
	      (= columna_a_fila (sub1 (vector-length matriz_operadas)))
	      (set! matriz_operadas (list->vector lista_inversa))
	      )
	   (set! lista_inversa (append lista_inversa (list (matriz-ref-columna matriz_operadas columna_a_fila))))
	   ) ; Se vuelven las filas como columnas y viceversa.
	 matriz_operadas
	 )
      (set! lista_operadas (append lista_operadas (list (agregar-numeros-iguales (matriz-ref-columna matriz columna_a_sumar) 1))))
      )
    )
   ((= direccion 3) ; Derecha
    (display (string-append "Tiró hacia la Derecha el jugador " jugador))
    (do
	(
	 [fila_a_sumar 0 (+ fila_a_sumar 1)]
	 [filas_de_matriz (vector-length (vector-ref matriz 0))]
	 [lista_operadas (list )]
	 [matriz_operadas (vector )]
	 )
	(
	 (= (sub1 filas_de_matriz) fila_a_sumar)
	 (set! matriz_operadas (list->vector lista_operadas))
	 matriz_operadas
	 )
      (set! lista_operadas (append lista_operadas (list (agregar-numeros-iguales (matriz-ref-fila matriz fila_a_sumar) 1))))
      )
    )
   )
  )

#| Despliegue en pantalla |#
   #| Propiedades del despliegue |#
(define despliegue_x 640)
(define despliegue_y 480)
(define posicion_tablero (make-posn (* (/ despliegue_x 3) 1) (+ (* (/ despliegue_y 6) 0) 20)))
(define posicion_punteo (make-posn (* (/ despliegue_x 2) 1) 15))
(define posicion_nombre (make-posn (+ (* (/ despliegue_x 2) 0) 5) 15))

   #| Propiedades de los cuadrados |#
(define dimension_cuadro (make-posn (* (/ despliegue_x 12) 1) (/ despliegue_y 12)))
(define posicion_cuadrado_1 (make-posn (+ (* (/ despliegue_x 3) 1) (* (/ despliegue_x 12) 0)) (+ (* (/ despliegue_y 6) 0) 20)))
(define posicion_cuadrado_2 (make-posn (+ (* (/ despliegue_x 3) 1) (* (/ despliegue_x 12) 1)) (+ (* (/ despliegue_y 6) 0) 20)))
(define posicion_cuadrado_3 (make-posn (+ (* (/ despliegue_x 3) 1) (* (/ despliegue_x 12) 2)) (+ (* (/ despliegue_y 6) 0) 20)))
(define posicion_cuadrado_4 (make-posn (+ (* (/ despliegue_x 3) 1) (* (/ despliegue_x 12) 3)) (+ (* (/ despliegue_y 6) 0) 20)))
(define posicion_cuadrado_5 (make-posn (+ (* (/ despliegue_x 3) 1) (* (/ despliegue_x 12) 0)) (+ (* (/ despliegue_y 6) 1) 20)))
(define posicion_cuadrado_6 (make-posn (+ (* (/ despliegue_x 3) 1) (* (/ despliegue_x 12) 1)) (+ (* (/ despliegue_y 6) 1) 20)))
(define posicion_cuadrado_7 (make-posn (+ (* (/ despliegue_x 3) 1) (* (/ despliegue_x 12) 2)) (+ (* (/ despliegue_y 6) 1) 20)))
(define posicion_cuadrado_8 (make-posn (+ (* (/ despliegue_x 3) 1) (* (/ despliegue_x 12) 3)) (+ (* (/ despliegue_y 6) 1) 20)))
(define posicion_cuadrado_9 (make-posn (+ (* (/ despliegue_x 3) 1) (* (/ despliegue_x 12) 0)) (+ (* (/ despliegue_y 6) 2) 20)))
(define posicion_cuadrado_10 (make-posn (+ (* (/ despliegue_x 3) 1) (* (/ despliegue_x 12) 1)) (+ (* (/ despliegue_y 6) 2) 20)))
(define posicion_cuadrado_11 (make-posn (+ (* (/ despliegue_x 3) 1) (* (/ despliegue_x 12) 2)) (+ (* (/ despliegue_y 6) 2) 20)))
(define posicion_cuadrado_12 (make-posn (+ (* (/ despliegue_x 3) 1) (* (/ despliegue_x 12) 3)) (+ (* (/ despliegue_y 6) 2) 20)))
(define posicion_cuadrado_13 (make-posn (+ (* (/ despliegue_x 3) 1) (* (/ despliegue_x 12) 0)) (+ (* (/ despliegue_y 6) 3) 20)))
(define posicion_cuadrado_14 (make-posn (+ (* (/ despliegue_x 3) 1) (* (/ despliegue_x 12) 1)) (+ (* (/ despliegue_y 6) 3) 20)))
(define posicion_cuadrado_15 (make-posn (+ (* (/ despliegue_x 3) 1) (* (/ despliegue_x 12) 2)) (+ (* (/ despliegue_y 6) 3) 20)))
(define posicion_cuadrado_16 (make-posn (+ (* (/ despliegue_x 3) 1) (* (/ despliegue_x 12) 3)) (+ (* (/ despliegue_y 6) 3) 20)))

   #| Objetos de despliegue |#
(define (2048-despliegue-numero nombre_despliegue posicion dim_cuadro base exponente)
  (define color_cuadrado (make-rgb 0 0 0))
  (cond
   (( or (< exponente 10) (= exponente 10))
    (set! color_cuadrado (make-rgb (* 0.1 exponente) (* 0.03 exponente) (* 0.02 exponente)))
    )
   ((> exponente 10)
    (set! color_cuadrado (make-rgb 1 0.4 0.5))
    )
   )
  ((draw-solid-rectangle nombre_despliegue) posicion (posn-x dim_cuadro) (posn-y dim_cuadro) color_cuadrado)
  ((draw-string nombre_despliegue) (make-posn (+ (posn-x posicion) 5) (+ (posn-y posicion) 5)) (number->string (* base exponente)) "white")
  )

(define (2048-tablero nombre_despliegue nombre_jugador pos_punteo pos_tablero punteo)
  ((clear-viewport nombre_despliegue))
  ((draw-viewport nombre_despliegue) "black")
  ((draw-string nombre_despliegue) pos_punteo (number->string punteo) "white")
  ((draw-string nombre_despliegue) pos_nombre nombre_jugador "white")
  (2048-despliegue-numero nombre_despliegue posicion_cuadrado_1 dimension_cuadrado 2 (exponente_del (matriz-ref matriz_jugador 0 0) 2))
  (2048-despliegue-numero nombre_despliegue posicion_cuadrado_2 dimension_cuadrado 2 (exponente_del (matriz-ref matriz_jugador 0 1) 2))
  (2048-despliegue-numero nombre_despliegue posicion_cuadrado_3 dimension_cuadrado 2 (exponente_del (matriz-ref matriz_jugador 0 2) 2))
  (2048-despliegue-numero nombre_despliegue posicion_cuadrado_4 dimension_cuadrado 2 (exponente_del (matriz-ref matriz_jugador 0 3) 2))
  (2048-despliegue-numero nombre_despliegue posicion_cuadrado_5 dimension_cuadrado 2 (exponente_del (matriz-ref matriz_jugador 1 0) 2))
  (2048-despliegue-numero nombre_despliegue posicion_cuadrado_6 dimension_cuadrado 2 (exponente_del (matriz-ref matriz_jugador 1 1) 2))
  (2048-despliegue-numero nombre_despliegue posicion_cuadrado_7 dimension_cuadrado 2 (exponente_del (matriz-ref matriz_jugador 1 2) 2))
  (2048-despliegue-numero nombre_despliegue posicion_cuadrado_8 dimension_cuadrado 2 (exponente_del (matriz-ref matriz_jugador 1 3) 2))
  (2048-despliegue-numero nombre_despliegue posicion_cuadrado_9 dimension_cuadrado 2 (exponente_del (matriz-ref matriz_jugador 2 0) 2))
  (2048-despliegue-numero nombre_despliegue posicion_cuadrado_10 dimension_cuadrado 2 (exponente_del (matriz-ref matriz_jugador 2 1) 2))
  (2048-despliegue-numero nombre_despliegue posicion_cuadrado_11 dimension_cuadrado 2 (exponente_del (matriz-ref matriz_jugador 2 2) 2))
  (2048-despliegue-numero nombre_despliegue posicion_cuadrado_12 dimension_cuadrado 2 (exponente_del (matriz-ref matriz_jugador 2 3) 2))
  (2048-despliegue-numero nombre_despliegue posicion_cuadrado_13 dimension_cuadrado 2 (exponente_del (matriz-ref matriz_jugador 3 0) 2))
  (2048-despliegue-numero nombre_despliegue posicion_cuadrado_14 dimension_cuadrado 2 (exponente_del (matriz-ref matriz_jugador 3 1) 2))
  (2048-despliegue-numero nombre_despliegue posicion_cuadrado_15 dimension_cuadrado 2 (exponente_del (matriz-ref matriz_jugador 3 2) 2))
  (2048-despliegue-numero nombre_despliegue posicion_cuadrado_16 dimension_cuadrado 2 (exponente_del (matriz-ref matriz_jugador 3 3) 2))
  )

(define (2048-despliegue nombre_despliegue nombre_jugador numero_del_jugador)
  (define matriz_del_jugador (crear-matriz 4 4))
  (do
      (
       [turno 0 (+ turno 1)]
       [punteo 0]
       [espacios_restantes (* 4 4) (contar-numero matriz_del_jugador 2 0)]
       [movimiento 'derp]
       [ubicaciones (buscar-numero matriz_del_jugador 2 0)]
       [spawn -1]
       [base_juego 2]
       [despliegue_jugador (open-viewport nombre_jugador despliegue_x despliegue_y)]
       )
      (
       (or (= espacios_restantes 0) (> (contar-numero matriz_del_jugador 2 (* base_juego 1024)) 0))
       (display (string-append "Juego de" nombre_jugador "terminado!"))
       )
    (set! spawn (random (length ubicaciones)))
    (matriz-set! matriz_del_jugador (list-ref (list-ref ubicaciones spawn) 0) (list-ref (list-ref ubicaciones spawn) 1) base_juego)
    (set! movimiento (get-key-press despliegue_jugador))
    (cond
     ((equal? (key-value movimiento) #\w)
      (set! matriz_del_jugador (2048-movimiento matriz_del_jugador 0 nombre_jugador))
      )
     ((equal? (key-value movimiento) #\a)
      (set! matriz_del_jugador (2048-movimiento matriz_del_jugador 1 nombre_jugador))
      )
     ((equal? (key-value movimiento) #\s)
      (set! matriz_del_jugador (2048-movimiento matriz_del_jugador 2 nombre_jugador))
      )
     ((equal? (key-value movimiento) #\d)
      (set! matriz_del_jugador (2048-movimiento matriz_del_jugador 3 nombre_jugador))
      )
     )
    (set! ubicaciones (buscar-numero matriz_del_jugador 2 0))
    )
  )

#| Interfaz Usuario-Programa |#

#| Ciclo Principal |#
(define (ciclo_principal estado)
  (define jugador_1 "")
  (define jugador_2 "Máquina")
  (cond
   ((equal? estado -1)
    (display "Adiós!")
    )
   ((equal? estado 0)
    (printf "1. Jugador vs Jugador\n2. Jugador vs Máquina\n3. Salir\n")
    (ciclo_principal (read-line))
    )
   ((equal? estado "1")
    (open-graphics)
    (display "Nombre del jugador 1: ")
    (set! jugador_1 (read-line))
    (display "Nombre del jugador 2: ")
    (set! jugador_2 (read-line))
    (2048-despliegue jugador_1 jugador_1 1)
    (2048-despliegue jugador_2 jugador_2 2)
    )
   ((equal? estado "2")
    (display "Nombre del jugador 1: ")
    (set! jugador_1 (read-line))
    )
   ((equal? estado "3")
    (ciclo_principal -1)
    )
   (else
    (display "Valor inválido.")(newline)
    (ciclo_principal 0)
    )
   )
  )

(ciclo_principal 0)
