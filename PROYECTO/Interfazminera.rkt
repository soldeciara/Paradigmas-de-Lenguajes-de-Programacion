#lang racket/gui ;Interfaz gráfica
(require htdp/gui)

; Base de Datos
; Información Relevos Mineros
(define nombres #("Antamina" "Consorcio Minero Horizonte" "Minera Lincuna"))
(define provincia #("Huaraz" "Pataz" "La Libertad"))
(define departamento #("Ancash" "Recuay" "Ancash"))
(define fechasinicio #("05-Agosto-2021" "18-Febrero-2018" "26-Marzo-2005"))
(define diashastareporte #(40 1304 6016))
(define diast #(68830 65860 68520))
(define mest #(2064900 1975800 2055600))
(define añot #(24778800 23709600 24667200))
(define diametro #(1800 1700 1500))
(define profundidad #(140 150 160))

;Convierte toneladas a volumen segun tiempo *cubicodia*
 (define (cubicodia A) ;A=toneladas

;Volumen=Masa/Densidad   *Densidad relave: 1200kg/m3
  (exact->inexact(*(/ A 1200) 1000)))

;Convierte toneladas a volumen segun tiempo *cubicomes*
 (define (cubicomes A) ;A=toneladas

;Volumen=Masa/Densidad   *Densidad relave: 1200kg/m3
  (exact->inexact(*(/ A 1200) 1000 30)))

;Convierte toneladas a volumen segun tiempo *cubicoano*
 (define (cubicoano A) ;A=toneladas

;Volumen=Masa/Densidad   *Densidad relave: 1200kg/m3
  (exact->inexact(*(/ A 1200) 1000 360)))

;Mostrar conversiones con su respectiva minera
(define (mostrarconver B) ;B=indice del vector
  (printf "\t-PRODUCCIÓN DEL RELAVE-" )
  (printf "\n|t|=Tolenadas\t\t|m^3|=Metros cúbicos" )
  (printf "\nDia(t):" )(write(vector-ref diast B))(printf "\t\tDia(m^3):")(write(cubicodia(vector-ref diast B)))
  (printf "\nMes(t):" )(write(vector-ref mest B))(printf "\t\tMes(m^3):")(write(cubicomes(vector-ref mest B)))
  (printf "\nAño(t):" )(write(vector-ref añot B))(printf "\t\tAño(m^3):")(write(cubicoano(vector-ref añot B))) )

;Area del relave segun diametro *areadelcirculo*
(define (arearelave A) ;A=diametro

;Area=0.25*(pi=3.14)*Diametro*Diametro 
  (exact->inexact(* 0.25 3.14 A A)))

;Volumen del relave segun diametro y profundidad *m3*
(define (volumenrelave A B) ;A=diametro B=profundidad

;Volumen=Area*Profundidad 
  (exact->inexact(* (arearelave A) B)))

;Mostrar caracteristicas relave con su respectiva minera
(define (mostrarcarac B) ;B=indice del vector
  (printf "\n\n\t-CARACTERÍSTICAS DEL RELAVE-" )
  (printf "\n|m|=Metros  |m^2|=Metros cuadrados  |m^3|=Metros cúbicos" )
  (printf "\n*Diametro(m):" )(write(vector-ref diametro B))
  (printf "\t*Área(m^2):")(write(arearelave(vector-ref diametro B)))
  (printf "\n*Profundidad(m):" )(write(vector-ref profundidad B))
  (printf "\t*Volumen total(m^3):")(write(volumenrelave(vector-ref diametro B) (vector-ref profundidad B)))
  )

;Volumen ocupado hasta hoy
(define (volumenocupado B C) ;B=dias C=indice vector
 (exact->inexact(* B(cubicodia(vector-ref diast C)))))

;Volumen disponible hasta hoy
(define (volumendisponible C) ;B=dias C=indice vector
 (exact->inexact(-(volumenrelave(vector-ref diametro C) (vector-ref profundidad C))
   (volumenocupado (vector-ref diashastareporte C) C) )))

;Alerta relave con su respectiva minera
(define (ver- B)
 (cond [(< 1 2)
        (~a  "\n\t  - ALERTA DEL RELAVE -"
             "\n. Estado: ACTIVO "
             "\n. Fecha de inicio de producción: "(vector-ref fechasinicio B)
             "\n. Volumen ocupado hasta hoy: "(volumenocupado (vector-ref diashastareporte B) B)
             "\n. Volumen disponible hasta hoy: "(volumendisponible B)
           )
        ]
))
;-------------------------------------------------------------------------------------------------------------------
; Interaz Gráfica de Usuario
; Ventana Principal
(define vent-principal (new frame%
               [label "Alerta Temprana y Monitoreo de Relaves Mineros"]
               [width 500][ height 100]
               ))
; Ingresar Nombre
(define num (new text-field%
               [parent vent-principal]      
               [label " Digite su nombre -> " ]
               ))

; Ventana Secundaria
(define vent-sec (new frame%
               [label "Empresas Relaves Mineros"]
               [width 600][ height 300]
               ))

; Crea el espacio para el mensaje de bienvenida ventana-sec
(define name (new message%
               [parent vent-sec]
               [label " Empresas Mineras"]               
               [auto-resize #t]
               ))
; Ventana Empresa Minera Antamina
(define vent-ant (new frame%
               [label "Empresas Minera Antamina"]
               [width 600][ height 300]))

; Crea el espacio para el mensaje
(define name-ant (new message%
               [parent vent-ant]
               [label ""]               
               [auto-resize #t]
               ))
(define name-ant-2 (new message%
               [parent vent-ant]
               [label ""]               
               [auto-resize #t]
               ))
; Botones Empresa Minera Antamina
(new button%
            [parent vent-ant]
            [label " Atrás "]
            [callback (lambda (b c)(send vent-ant show #f)(send vent-sec show #t))])               
(new button%
            [parent vent-ant]
            [label " Salir "]
            [callback (lambda (b c)(send vent-ant show #f))])
; Ventana Consorcio Minero Horizonte
(define vent-hor (new frame%
               [label "Consorcio Minero Horizonte"]
               [width 600][ height 300]))
; Crea el espacio para el mensaje
(define name-hor (new message%
               [parent vent-hor]
               [label ""]               
               [auto-resize #t]
               ))
(define name-hor-2 (new message%
               [parent vent-hor]
               [label ""]               
               [auto-resize #t]
               ))               
; Botones Consorcio Minero Horizonte
(new button%
            [parent vent-hor]
            [label " Atrás "]
            [callback (lambda (b c)(send vent-hor show #f)(send vent-sec show #t))])               
(new button%
            [parent vent-hor]
            [label " Salir "]
            [callback (lambda (b c)(send vent-hor show #f))])

; Ventana Minera Lincuna
(define vent-lin (new frame%
               [label "Minera Lincuna"]
               [width 600][ height 300]))
; Crea el espacio para el mensaje
(define name-lin (new message%
               [parent vent-lin]
               [label ""]               
               [auto-resize #t]
               ))
(define name-lin-2 (new message%
               [parent vent-lin]
               [label ""]               
               [auto-resize #t]
               ))               
; Botones Minera Lincuna
(new button%
            [parent vent-lin]
            [label " Atrás "]
            [callback (lambda (b c)(send vent-lin show #f)(send vent-sec show #t))])               
(new button%
            [parent vent-lin]
            [label " Salir "]
            [callback (lambda (b c)(send vent-lin show #f))])

;Mostrar datos totales
(define (alertarelave B) ;B=indice del vector   
(cond [(< (volumenocupado (vector-ref diashastareporte B) B)
            (volumenrelave(vector-ref diametro B) (vector-ref profundidad B)))
            (~a "|NIVEL DE ALERTA MÍNIMA|")
      ]
      [(> (volumenocupado (vector-ref diashastareporte B) B)
            (volumenrelave(vector-ref diametro B) (vector-ref profundidad B)))
            (~a "|NIVEL DE ALERTA MÁXIMA|")
      ]
  )
)
; Botones vent-sec Empresas Mineras
(new button%
            [parent vent-sec]
            [label " Empresa Minera Antamina "]
            [callback (lambda (b c)
                        (send vent-sec show #f)
                        (send vent-ant show #t)              
                        (send name-ant set-label
                              (ver- 0))
                        (send name-ant-2 set-label
                          (alertarelave 0)))]
            )
(new button%
            [parent vent-sec]
            [label " Consorcio Minero Horizonte "]
            [callback (lambda (b c)
                        (send vent-sec show #f)
                        (send vent-hor show #t)
                        (send name-hor set-label
                              (ver- 1))
                        (send name-hor-2 set-label
                          (alertarelave 1)))]
            )
(new button%
            [parent vent-sec]
            [label " Minera Lincuna "]
            [callback (lambda (b c)
                        (send vent-sec show #f)
                        (send vent-lin show #t)
                        (send name-lin set-label
                              (ver- 2))
                        (send name-lin-2 set-label
                          (alertarelave 2)))]
            )
(new button%
            [parent vent-sec]
            [label " Atrás "]
            [callback (lambda (b c)
                        (send vent-sec show #f)
                        (send vent-principal show #t))]
            )
(new button%
            [parent vent-sec]
            [label " Salir "]
            [callback (lambda (b c)(send vent-sec show #f))]
            )
; Botones ventana principal
(new button%
            [parent vent-principal]
            [label "Aceptar"]                    
            [callback (lambda (b c)
                        (send vent-principal show #f) ;Cierra la ventana
                        (send vent-sec show #t) ;Abre ventana-sec
                        (send name set-label
                        (string-append "\n Bienvenid@: "
                                       (send num get-value)
                                       "\n\n Empresas Mineras\n"
                                       ))
                        )])
(new button%
            [parent vent-principal]
            [label "Salir"]
            [callback (lambda (b c)(send vent-principal show #f))]
            )

(send vent-principal show #t) ; Mostramos la ventana al usuario, comenzando la aplicación