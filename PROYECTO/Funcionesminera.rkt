#lang racket
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
  (printf "\n\t-COMPAÑIA MINERA-\n       " )
  (printf(vector-ref nombres B)) (printf "-")
  (printf(vector-ref provincia B)) (printf "-")
  (printf(vector-ref departamento B)) (printf "\n")
  (printf "\t\n-PRODUCCIÓN DEL RELAVE-" )
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
(define (alertarelave B) ;B=indice del vector
  (printf "\n\n\t-ALERTA DEL RELAVE-" )
  (printf "\n*Estado: ACTIVO ")
  (printf "\n*Fecha de inicio de producción: ")(printf (vector-ref fechasinicio B))
  (printf "\n*Días transcurridos hasta hoy: ")(write (vector-ref diashastareporte B)) (printf " días.")
  (printf "\n*Volumen ocupado hasta hoy: ")(write(volumenocupado (vector-ref diashastareporte B) B))
  (printf "\n*Volumen disponible hasta hoy: ")(write(volumendisponible B))

  (cond [(< (volumenocupado (vector-ref diashastareporte B) B)
            (volumenrelave(vector-ref diametro B) (vector-ref profundidad B)))
            (printf "\n\t|NIVEL DE ALERTA MÍNIMA|")
        ]
        [(> (volumenocupado (vector-ref diashastareporte B) B)
            (volumenrelave(vector-ref diametro B) (vector-ref profundidad B)))
            (printf "\n\t|NIVEL DE ALERTA MÁXIMA|")
        ]
  )
)

;Mostrar datos totales
(define (mostrardatos B) ;B=indice del vector
  (mostrarconver B)
  (mostrarcarac B)
  (alertarelave B)
)

(mostrardatos 2)
          
        
