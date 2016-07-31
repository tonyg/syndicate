#lang syndicate/actor
;; File System Demo protocol

(provide (struct-out file)
         (struct-out save)
         (struct-out delete))

(struct file (name content) #:prefab)
(struct save (file) #:prefab)
(struct delete (name) #:prefab)
