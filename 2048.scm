#| Proyecto Final CC1B
Integrantes:
	Mario Möller, 16012890
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

#| Despliegue en pantalla |#

#| Interfaz Usuario-Programa |#

#| Ciclo Principal |#
(define (ciclo_principal estado)
  (cond
   ((= estado -1)
    (display "Adiós!")
    )
   ((= estado 0)
    (printf "1. Jugador vs Jugador\n2. Jugador vs Máquina\n3. Salir")
    (ciclo_principal (read))
    )
   ((= estado 1)
    )
   ((= estado 2)
    )
   ((= estado 3)
    )
   )
  )

(ciclo_principal 0)
