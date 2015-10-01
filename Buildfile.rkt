#!/usr/bin/env racket

#lang racket/base

(require racket/file)
(require racket/string)
(require racket/system)

(define (config->string name)
  (let ((fname (string-append ".docker-" name)))
    (if (file-exists? fname)
        (string-replace (file->string fname) "\n" " ")
        null)))

(define (full-config)
  (string-join
   (filter (lambda (s) (not (null? s)))
           (map config->string (list "ports" "volumes" "env" "links" "extra")))
   " "))

(define (cmd command-string)
  (display command-string)
  (system command-string))

(define (docker-command . args)
  (let ((docker-command "docker"))
    (string-append docker-command " " (string-join args " "))))

(define prefix "gnzh")

(define (current-script-name)
  (car
   (regexp-match
    #rx"[^/]*$"
    (path->string
     (find-system-path 'run-file)))))

(define (container-tag)
  (string-append prefix "/" (current-script-name)))

(define (container-name)
  (current-script-name))

(define (dispatch-command command)
  (let ((command-to-run
         (case command
           [("build")  (docker-command "build" "-t" (container-tag) ".")]
           [("tailf")  (docker-command "logs" "-f"  (container-name))]
           [("shell")  (docker-command "exec" "-ti" (container-name) "bash")]
           [("kill")   (docker-command "kill"       (container-name))]
           [("rm")     (docker-command "rm"         (container-name))]
           [("logs")   (docker-command "logs"       (container-name))]
           [("push")   (docker-command "push"       (container-tag))]
           [("pul")    (docker-command "pull"       (container-tag))]
           [("run")    (docker-command "run" "-ti"      "--name" (container-name) (full-config) (container-tag))]
           [("daemon") (docker-command "run" "-d" "-ti" "--name" (container-name) (full-config) (container-tag))]
           [("restart") (begin (dispatch-command "kill")
                               (dispatch-command "rm")
                               (dispatch-command "run"))]
           [else #f])))
    (when (not command-to-run)
      (display (string-append "No such command " command))
      (exit 1))
    (when (not (cmd command-to-run))
      (exit 1))))

(define (main)
  ; switch on first argument
  (let ((args (current-command-line-arguments)))
    (if (not (zero? (vector-length args)))
        (begin
          (map dispatch-command (vector->list args))
          (exit 0))
        (exit 1))))

(main)
