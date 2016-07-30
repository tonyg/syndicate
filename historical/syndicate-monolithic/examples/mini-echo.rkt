#lang syndicate-monolithic

(struct echo-req (body) #:prefab)
(struct echo-resp (body) #:prefab)

(spawn (lambda (e count)
         (match e
           [(message (echo-req body))
            (transition (+ count 1)
                        (message (echo-resp body)))]
           [_ #f]))
       0
       (scn (subscription (echo-req ?))))

(spawn (lambda (e s)
         (match e
           [(message (echo-resp body))
            (printf "Received: ~v\n" body)
            #f]
           [_ #f]))
       (void)
       (list (scn (subscription (echo-resp ?)))
             (message (echo-req 0))
             (message (echo-req 1))
             (message (echo-req 2))))
