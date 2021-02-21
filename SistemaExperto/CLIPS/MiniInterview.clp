
;;; ***************************
;;; * VARIABLES GLOBALES *
;;; ***************************
;Variable global que ayudará a llevar la puntuación de la evaluación que más tarde este valor
;será asignado la hecho en la propiedad (slot) risk-level-score al finalizar el proceso del sistema experto
(defglobal ?*evaluacion* = 0)

;;; ***************************
;;; * DEFTEMPLATES & DEFFACTS *
;;; ***************************

;Este hecho encapsulará la informacion de como se debe visualizar las preguntas y respuestas.
;Ademas servirá de base para almacenar la respuesta escogida por el usuario en cada pregunta.
(deftemplate UI-state
   ;id => Identificacion asignado de forma secuencial del hecho
   (slot id (default-dynamic (gensym*)))
   ;display=> Codigo del hecho que ayudará a visualizar el texto en la interfaz grafica
   (slot display)
   ;relation-asserted=> Esta propiedad le servirá de guia a la interfaz grafica de como se debe enviar el comando a CLIPS
   ;para generar una respuesta seleccionada por el usuario
   (slot relation-asserted (default none))
   ;response=> Respuesta asignada por el usuario
   (slot response (default none))
   ;valid-answers=> Listado de respuestas que pueden ser asignadas al hecho, esta propiedad
   ;ayudará a la interfaz grafica de que tipo de componente debe dibujar en pantalla
   (multislot valid-answers)
   ;state=> Esta propiedad le servirá de guia a la interfaz grafica en dibujar los botones "Siguiente" y "Previo" 
   ;dependiendo del valor. Los posibles valores de esta propiedad son : initial , middle , next , end
   (slot state (default middle))
   ;score=> Puntuación de la pregunta asignada segun la evaluacion MINI
   (slot score (default 0))
   ;prev-state=> Referencia al hecho anterior. 
   ;Haciendo analogia a la lista doblemente enlazadas este seria el puntero de regreso
   (slot prev-state)
   ;next-state=> Referencia al hecho siguiente.
   ;Haciendo analogia a la lista doblemente enlazadas este seria el puntero de avance
   (slot next-state)
   ;risk-level-score=> Puntuacion final asignada por el sistema experto al finalizar la evaluacion
   (slot risk-level-score)
   ;risk-level=> Categoria del trastorno evaluado (Ninguno,Leve,Moderado,Alto)
   (slot risk-level)
)

;Este hecho servirá de ayuda a la interfaz grafica indicando el hecho que debe visualizar en pantalla
(deftemplate state-list
   (slot current)
   (multislot sequence))

;Listado de hechos agregados al motor de inferencia automaticamente cada vez que se ejecuta
;el comando de reinicio (reset)  
(deffacts startup
   (state-list)
)

;;;****************
;;;* STARTUP RULE *
;;;****************

;Regla que define cual es será el hecho que dará inicio al sistema experto
;para este caso se dará inicio al Hecho "WelcomeMessage"
;este hecho ayuda a la interfaz grafica indicando cual es la siguiente pregunta (next-state)
;para este caso será la pregunta que tenga valor "question-c1" en la propiedad relation-asserted
(defrule system-banner "pantalla inicial"
  =>
  (assert (UI-state (display WelcomeMessage)
                    (relation-asserted start)
                    (state initial)
                    (valid-answers)
                    (prev-state none)
                    (next-state question-c1)
          )
  )
)

;;;***************
;;;* QUERY RULES *
;;;***************
; (load F:\Software\Developer\CLIPS\Sample\sample-mini.clp)
; (reset)
; (run)
; (assert (start))
; (run)
; (assert (question-c1 Yes)) o (assert (question-c1 No))

; (run)
; (assert (question-c1 Yes))
; (run)
; (assert (question-c2 Yes))
; (run)
; (assert (question-c3 Yes))
; (run)
; (assert (question-c4 Yes))
; (run)
; (assert (question-c5 Yes))
; (run)
; (assert (question-c6 Yes))
; (run)


(defrule init-first-question "Regla para iniciar el sistema experto con la primera pregunta QuestionC1"
   (logical (start))   
   =>
   (
     assert (UI-state (display QuestionC1)
                     (relation-asserted question-c1)
                     (response No)
                     (valid-answers No Yes)
                     (state next)
                     (score 1)
                     (prev-state initial)
                     (next-state question-c2)
            )
   )
)

(defrule handling-response-c1 "Regla usada cuando el usuario responda la pregunta QuestionC1"
   (logical (question-c1 ?response))
   =>
  (
    ;El usuario al responder "YES" automaticamente se incrementa
    ;la evaluación en un punto
    if (eq ?response Yes)then 
      (bind ?*evaluacion* (+ ?*evaluacion* 1))
  )
  (
    assert (UI-state (display QuestionC2)
                    (relation-asserted question-c2)
                    (response No)
                    (valid-answers No Yes)
                    (state prevnext)
                    (score 2)
                    (prev-state question-c1)
                    (next-state question-c3)
            )
  )
)

(defrule handling-response-c2 ""

   (logical (question-c2 ?response))

   =>
  (
    if (eq ?response Yes)then 
      (bind ?*evaluacion* (+ ?*evaluacion* 2))
  )
  (
    assert (UI-state (display QuestionC3)
                    (relation-asserted question-c3)
                    (response No)
                    (valid-answers No Yes)
                    (state prevnext)
                    (score 6)
                    (prev-state question-c2)
                    (next-state question-c4)
            )
  )
)

(defrule handling-response-c3 ""

   (logical (question-c3 ?response))

   =>
  (
    if (eq ?response Yes)then 
      (bind ?*evaluacion* (+ ?*evaluacion* 6))
  )
  (
    assert (UI-state (display QuestionC4)
                    (relation-asserted question-c4)
                    (response No)
                    (valid-answers No Yes)
                    (state prevnext)
                    (score 10)
                    (prev-state question-c3)
                    (next-state question-c5)
            )
  )
)

(defrule handling-response-c4 ""

   (logical (question-c4 ?response))

   =>
  (
    if (eq ?response Yes)then 
      (bind ?*evaluacion* (+ ?*evaluacion* 10))
  )
  (
    assert (UI-state (display QuestionC5)
                    (relation-asserted question-c5)
                    (response No)
                    (valid-answers No Yes)
                    (state prevnext)
                    (score 10)
                    (prev-state question-c4)
                    (next-state question-c6)
            )
  )
)

(defrule handling-response-c5 ""

   (logical (question-c5 ?response))

   =>
  (
    if (eq ?response Yes)then 
      (bind ?*evaluacion* (+ ?*evaluacion* 10))
  )
  (
    if (> ?*evaluacion* 0)then
    (
      assert (UI-state (display QuestionC6)
                    (relation-asserted question-c6)
                    (response No)
                    (valid-answers No Yes)
                    (state prev)
                    (score 4)
                    (prev-state question-c5)
                    (next-state end)
              )
    )else
    (
      ;assert (nivel-riesgo 0 ninguno)
      ;No tiene nivel de Riesgo se da por finalizado el sistema experto
      assert (UI-state (display EndQuestion)
                    (relation-asserted end)
                    (state end)
                    (valid-answers)
                    (prev-state end)
                    (next-state end)
                    (risk-level-score ?*evaluacion*)
                    (risk-level ninguno)
              )
      
    )
  )
)

(defrule handling-response-c6 ""

   (logical (question-c6 ?response))

   =>
  (
    if (eq ?response Yes)then 
      (bind ?*evaluacion* (+ ?*evaluacion* 4))
  )
  (
    if (and (>= ?*evaluacion* 1)(<= ?*evaluacion* 5))then
    (
        ;assert (nivel-riesgo 1 leve)
        assert (UI-state (display EndQuestion)
                    (relation-asserted end)
                    (state end)
                    (valid-answers)
                    (prev-state end)
                    (next-state end)
                    (risk-level-score ?*evaluacion*)
                    (risk-level leve)
              )
    )
    else
    (
      if(and (>= ?*evaluacion* 6)(<= ?*evaluacion* 9))then
      (
        ;assert (nivel-riesgo 2 moderado)
        assert (UI-state (display EndQuestion)
                    (relation-asserted end)
                    (state end)
                    (valid-answers)
                    (prev-state end)
                    (next-state end)
                    (risk-level-score ?*evaluacion*)
                    (risk-level moderado)
              )
      )
      else
      (
        if (>= ?*evaluacion* 10)then
        (
          ;assert (nivel-riesgo 3 alto)
          assert (UI-state (display EndQuestion)
                    (relation-asserted end)
                    (state end)
                    (valid-answers)
                    (prev-state end)
                    (next-state end)
                    (risk-level-score ?*evaluacion*)
                    (risk-level alto)
              )
        )else
        (
          ;assert (nivel-riesgo 0 ninguno)
          assert (UI-state (display EndQuestion)
                    (relation-asserted end)
                    (state end)
                    (valid-answers)
                    (prev-state end)
                    (next-state end)
                    (risk-level-score ?*evaluacion*)
                    (risk-level ninguno)
              )
        )
      )

    )
  )
)

;;;*************************
;;;* GUI INTERACTION RULES *
;;;*************************
(defrule ask-question

   (declare (salience 5))
   
   (UI-state (id ?id))
   
   ?f <- (state-list (sequence $?s&:(not (member$ ?id ?s))))
             
   =>
   
    (
     modify ?f (current ?id)(sequence ?id ?s)
    )
   (halt)
)