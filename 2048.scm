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

#| Estructura Matriz |#
(define (crear-matriz filas columnas)
  (build-vector filas (lambda (valor_fila) (build-vector columnas (lambda (valor_columna) 0))))
  )

(define (matriz-set! matriz pos_fila pos_columna valor_nuevo)
  (vector-set! (vector-ref matriz pos_fila) pos_columna valor_nuevo)
  )

(define (matriz-ref matriz pos_fila pos_columna)
  (vector-ref (vector-ref matriz pos_fila) pos_columna)
  )

#| Operación sobre matrices |#
(define (2048-movimiento matriz direccion) ; Siguiendo convención WASD
  (cond
   ((= direccion 0) ; Arriba
    )
   ((= direccion 1) ; Izquierda
    )
   ((= direccion 2) ; Abajo
    )
   ((= direccion 3) ; Derecha
    )
   )
  )

(define (buscar-cero matriz propiedades_de_busqueda)
  (define filas_matriz (vector-length matriz))
  (define columnas_matriz (vector-length (vector-ref matriz 0)))
  (define tipo_busqueda (list-ref propiedades_de_busqueda 0))    ; en fila: 0, en columna: 1, todo: 2
  (define posicion_inicial (list-ref propiedades_de_busqueda 1)) ; 0 - (sub1 (vector-length vector_a_buscar_en))
  (define posicion_final (list-ref propiedades_de_busqueda 2))   ; posicion_inicial - (sub1 (vector-length vector_a_buscar_en))
  (cond
   ((= tipo_busqueda 0)
    )
   ((= tipo_busqueda 1)
    )
   ((= tipo_busqueda 2)
    (do
	(
	 [posicion_total 0 (+ posicion_total 1)]
	 [posicion_fila 0 (modulo posicion_total columnas_matriz)]
	 [posicion_columna 0 (modulo posicion_total filas_matriz)]
	 [ceros_total 0]
	 )
	(
	 (= posicion_total (* (- filas_matriz 1) (- columnas_matriz 1)))
	 ceros_total
	 )
      )
    )
   )
  )

#| Despliegue en pantalla |#

#| Interfaz Usuario-Programa |#

#| Ciclo Principal |#
(define (ciclo_principal estado)
  (cond
   ((equal? estado -1)
    (display "Adiós!")
    )
   ((equal? estado 0)
    (printf "1. Jugador vs Jugador\n2. Jugador vs Máquina\n3. Salir\n")
    (ciclo_principal (read-line))
    )
   ((equal? estado "1")
    )
   ((equal? estado "2")
    )
   ((equal? estado "3")
    (ciclo_principal -1)
    )
   (else
    (display "Valor inválido.")
    )
   )
  )

(ciclo_principal 0)
