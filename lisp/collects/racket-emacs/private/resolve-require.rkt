#lang racket
(provide do-resolve-requires)
(require (for-syntax racket/base racket/require-transform))

(define-namespace-anchor anchor)

(define-for-syntax (bound-to-syntax? identifier [ns #f])
  (let*-values ([(default-ns) (or ns (current-namespace))]
                [(as-sym used-ns)
                 (if (syntax? identifier)
                     (values (syntax->datum identifier)
                             (or ns (module->namespace (syntax-source-module identifier))))
                     (values identifier default-ns))])
    (call/cc (λ (k) (namespace-variable-value as-sym #t (λ () (k #t)) used-ns) #f))))

(define-syntax (resolve-requires stx)
  (syntax-case stx (resolve-requires)
    [(require/dynamic rspec)     
     (let*-values ([(imports sources) (expand-import #'rspec)])
       (define (prepare-import i)
         (define local-id (import-local-id i))
         (define src-mod-path (import-src-mod-path i))
         (define src-sym (import-src-sym i))
         (define resolved-src-mod-path (syntax->datum (convert-relative-module-path src-mod-path)))

         (dynamic-require resolved-src-mod-path #f)

         (define is-macro? (bound-to-syntax? src-sym (module->namespace resolved-src-mod-path)))
         (define export (if is-macro? #`(cons 'macro '#,resolved-src-mod-path '#,src-sym) #`(cons 'value (dynamic-require '#,resolved-src-mod-path '#,src-sym))))
         
         #`(cons '#,(syntax->datum local-id) #,export))
       (datum->syntax stx (cons #'list (map prepare-import imports))))]))

(define (do-resolve-requires spec)
  ;; hack!
  (eval `(resolve-requires ,spec) (namespace-anchor->namespace anchor)))
