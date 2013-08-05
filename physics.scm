(define-module (physics)
  #:use-module (oop goops)
  #:export (<physics>
            effector-count
            object-count
            effector-func
            step-count))

(define-class <physics> ()
  ;; Number of effectors/muscles/outputs the agent has.
  (effector-count #:accessor effector-count #:init-keyword #:effector-count #:init-value 2)
  ;; Number of objects in the scene including agent.
  (object-count #:accessor object-count #:init-keyword #:object-count #:init-value 1)
  ;; Number of nodes that can be provided with input for the controller.
  ;(input-count #:accessor input-count #:init-keyword #:input-count #:init-value 1)
  ;; (effector-func t i) -> [-1, 1]
  ;; accepts a time t and an effector identifier i \in [1, effector-count]
  (effector-func #:accessor effector-func #:init-keyword #:effector-func #:init-value #f)
  (step-count #:accessor step-count #:init-value 1)
  ;; (input-func t i) -> [-1, 1]
  ;; accepts a time t and a input number i \in [1, input-count]
  ;(input-func #:accessor input-func #:init-value #f)
  ;; All the parameters should probably be placed in here.
  ;; (agent-diameter #:getter agent-diameter #:init-value 30)
  ;; (object-diameter #:getter object-diameter #:init-value 26)
  ;; (motor-constant #:getter motor-constant #:init-value 5)
  #;(max-sight-distance #:getter max-sight-distance #:init-value 205)
  )
