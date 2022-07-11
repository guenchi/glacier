(import (json json))

(define (read-file file-name)
  (let ((p (open-input-file file-name)))
       (let loop ((lst '()) (c (read-char p)))
         (if (eof-object? c)
             (begin 
               (close-input-port p)
               (list->string (reverse lst)))
             (loop (cons c lst) (read-char p))))))

(define (system-return cmd)
  (define tmp "./._##tmp##")
  (define rst "")
  (and (zero? (system (string-append cmd " > " tmp)))
       (file-exists? tmp)
       (begin (set! rst (read-file tmp))))
  (delete-file tmp)
  rst
)

(define get-vaults-list
  (lambda ()
    (json-ref 
      (json-set 
        (string->json 
          (system-return "aws glacier list-vaults --account-id -"))
          "VaultList"
          #t
          (lambda (x)
            (json-ref x "VaultName")))
      "VaultList")))


(define vault-exist?
  (lambda (name)
    (vector-check? 
      (get-vaults-list)
        (symbol->string name))))


(define creat-vault
  (lambda (name)
    (if (vault-exist? name)
        (begin 
          (display "The vault is exist. Creating abord...\n")
          #f)
        (json-ref 
          (string->json 
            (system-return 
              (string-append "aws glacier create-vault --account-id - --vault-name " (symbol->string name)))) 
          "location"))))

(define delete-vault
  (lambda (name)
    (if (vault-exist? name)
        (begin
          (system (string-append "aws glacier delete-vault --account-id - --vault-name " (symbol->string name)))
          (if (vault-exist? name)
              (begin 
                (display "Delete failed...Please try again\n")
                #f)
              (begin 
                (display "The vault is successfully deleted.\n")
                #t)))
        (begin 
          (display "The vault is not exist. Deleting failed...\n")
          #f))))
