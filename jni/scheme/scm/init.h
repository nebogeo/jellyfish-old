const char* init_scm=";    Initialization file for TinySCHEME 1.39\n"
"\n"
"; Per R5RS, up to four deep compositions should be defined\n"
"(define (caar x) (car (car x)))\n"
"(define (cadr x) (car (cdr x)))\n"
"(define (cdar x) (cdr (car x)))\n"
"(define (cddr x) (cdr (cdr x)))\n"
"(define (caaar x) (car (car (car x))))\n"
"(define (caadr x) (car (car (cdr x))))\n"
"(define (cadar x) (car (cdr (car x))))\n"
"(define (caddr x) (car (cdr (cdr x))))\n"
"(define (cdaar x) (cdr (car (car x))))\n"
"(define (cdadr x) (cdr (car (cdr x))))\n"
"(define (cddar x) (cdr (cdr (car x))))\n"
"(define (cdddr x) (cdr (cdr (cdr x))))\n"
"(define (caaaar x) (car (car (car (car x)))))\n"
"(define (caaadr x) (car (car (car (cdr x)))))\n"
"(define (caadar x) (car (car (cdr (car x)))))\n"
"(define (caaddr x) (car (car (cdr (cdr x)))))\n"
"(define (cadaar x) (car (cdr (car (car x)))))\n"
"(define (cadadr x) (car (cdr (car (cdr x)))))\n"
"(define (caddar x) (car (cdr (cdr (car x)))))\n"
"(define (cadddr x) (car (cdr (cdr (cdr x)))))\n"
"(define (cdaaar x) (cdr (car (car (car x)))))\n"
"(define (cdaadr x) (cdr (car (car (cdr x)))))\n"
"(define (cdadar x) (cdr (car (cdr (car x)))))\n"
"(define (cdaddr x) (cdr (car (cdr (cdr x)))))\n"
"(define (cddaar x) (cdr (cdr (car (car x)))))\n"
"(define (cddadr x) (cdr (cdr (car (cdr x)))))\n"
"(define (cdddar x) (cdr (cdr (cdr (car x)))))\n"
"(define (cddddr x) (cdr (cdr (cdr (cdr x)))))\n"
"\n"
";;;; Utility to ease macro creation\n"
"(define (macro-expand form)\n"
"     ((eval (get-closure-code (eval (car form)))) form))\n"
"\n"
"(define (macro-expand-all form)\n"
"   (if (macro? form)\n"
"      (macro-expand-all (macro-expand form))\n"
"      form))\n"
"\n"
"(define *compile-hook* macro-expand-all)\n"
"\n"
"\n"
"(macro (unless form)\n"
"     `(if (not ,(cadr form)) (begin ,@(cddr form))))\n"
"\n"
"(macro (when form)\n"
"     `(if ,(cadr form) (begin ,@(cddr form))))\n"
"\n"
"; DEFINE-MACRO Contributed by Andy Gaynor\n"
"(macro (define-macro dform)\n"
"  (if (symbol? (cadr dform))\n"
"    `(macro ,@(cdr dform))\n"
"    (let ((form (gensym)))\n"
"      `(macro (,(caadr dform) ,form)\n"
"         (apply (lambda ,(cdadr dform) ,@(cddr dform)) (cdr ,form))))))\n"
"\n"
"; Utilities for math. Notice that inexact->exact is primitive,\n"
"; but exact->inexact is not.\n"
"(define exact? integer?)\n"
"(define (inexact? x) (and (real? x) (not (integer? x))))\n"
"(define (even? n) (= (remainder n 2) 0))\n"
"(define (odd? n) (not (= (remainder n 2) 0)))\n"
"(define (zero? n) (= n 0))\n"
"(define (positive? n) (> n 0))\n"
"(define (negative? n) (< n 0))\n"
"(define complex? number?)\n"
"(define rational? real?)\n"
"(define (abs n) (if (>= n 0) n (- n)))\n"
"(define (exact->inexact n) (* n 1.0))\n"
"(define (<> n1 n2) (not (= n1 n2)))\n"
"(define (max . lst)\n"
"     (foldr (lambda (a b) (if (> a b) a b)) (car lst) (cdr lst)))\n"
"(define (min . lst)\n"
"     (foldr (lambda (a b) (if (< a b) a b)) (car lst) (cdr lst)))\n"
"(define (succ x) (+ x 1))\n"
"(define (pred x) (- x 1))\n"
"(define gcd\n"
"  (lambda a\n"
"    (if (null? a)\n"
"      0\n"
"      (let ((aa (abs (car a)))\n"
"            (bb (abs (cadr a))))\n"
"         (if (= bb 0)\n"
"              aa\n"
"              (gcd bb (remainder aa bb)))))))\n"
"(define lcm\n"
"  (lambda a\n"
"    (if (null? a)\n"
"      1\n"
"      (let ((aa (abs (car a)))\n"
"            (bb (abs (cadr a))))\n"
"         (if (or (= aa 0) (= bb 0))\n"
"             0\n"
"             (abs (* (quotient aa (gcd aa bb)) bb)))))))\n"
"\n"
"\n"
"(define (string . charlist)\n"
"     (list->string charlist))\n"
"\n"
"(define (list->string charlist)\n"
"     (let* ((len (length charlist))\n"
"            (newstr (make-string len))\n"
"            (fill-string!\n"
"               (lambda (str i len charlist)\n"
"                    (if (= i len)\n"
"                         str\n"
"                         (begin (string-set! str i (car charlist))\n"
"                         (fill-string! str (+ i 1) len (cdr charlist)))))))\n"
"          (fill-string! newstr 0 len charlist)))\n"
"\n"
"(define (string-fill! s e)\n"
"     (let ((n (string-length s)))\n"
"          (let loop ((i 0))\n"
"               (if (= i n)\n"
"                    s\n"
"                    (begin (string-set! s i e) (loop (succ i)))))))\n"
"\n"
"(define (string->list s)\n"
"     (let loop ((n (pred (string-length s))) (l '()))\n"
"          (if (= n -1)\n"
"               l\n"
"               (loop (pred n) (cons (string-ref s n) l)))))\n"
"\n"
"(define (string-copy str)\n"
"     (string-append str))\n"
"\n"
"(define (string->anyatom str pred)\n"
"     (let* ((a (string->atom str)))\n"
"       (if (pred a) a\n"
"     (error \"string->xxx: not a xxx\" a))))\n"
"\n"
"(define (string->number str) (string->anyatom str number?))\n"
"\n"
"(define (anyatom->string n pred)\n"
"  (if (pred n)\n"
"      (atom->string n)\n"
"      (error \"xxx->string: not a xxx\" n)))\n"
"\n"
"\n"
"(define (number->string n) (anyatom->string n number?))\n"
"\n"
"(define (char-cmp? cmp a b)\n"
"     (cmp (char->integer a) (char->integer b)))\n"
"(define (char-ci-cmp? cmp a b)\n"
"     (cmp (char->integer (char-downcase a)) (char->integer (char-downcase b))))\n"
"\n"
"(define (char=? a b) (char-cmp? = a b))\n"
"(define (char<? a b) (char-cmp? < a b))\n"
"(define (char>? a b) (char-cmp? > a b))\n"
"(define (char<=? a b) (char-cmp? <= a b))\n"
"(define (char>=? a b) (char-cmp? >= a b))\n"
"\n"
"(define (char-ci=? a b) (char-ci-cmp? = a b))\n"
"(define (char-ci<? a b) (char-ci-cmp? < a b))\n"
"(define (char-ci>? a b) (char-ci-cmp? > a b))\n"
"(define (char-ci<=? a b) (char-ci-cmp? <= a b))\n"
"(define (char-ci>=? a b) (char-ci-cmp? >= a b))\n"
"\n"
"; Note the trick of returning (cmp x y)\n"
"(define (string-cmp? chcmp cmp a b)\n"
"     (let ((na (string-length a)) (nb (string-length b)))\n"
"          (let loop ((i 0))\n"
"               (cond\n"
"                    ((= i na)\n"
"                         (if (= i nb) (cmp 0 0) (cmp 0 1)))\n"
"                    ((= i nb)\n"
"                         (cmp 1 0))\n"
"                    ((chcmp = (string-ref a i) (string-ref b i))\n"
"                         (loop (succ i)))\n"
"                    (else\n"
"                         (chcmp cmp (string-ref a i) (string-ref b i)))))))\n"
"\n"
"\n"
"(define (string=? a b) (string-cmp? char-cmp? = a b))\n"
"(define (string<? a b) (string-cmp? char-cmp? < a b))\n"
"(define (string>? a b) (string-cmp? char-cmp? > a b))\n"
"(define (string<=? a b) (string-cmp? char-cmp? <= a b))\n"
"(define (string>=? a b) (string-cmp? char-cmp? >= a b))\n"
"\n"
"(define (string-ci=? a b) (string-cmp? char-ci-cmp? = a b))\n"
"(define (string-ci<? a b) (string-cmp? char-ci-cmp? < a b))\n"
"(define (string-ci>? a b) (string-cmp? char-ci-cmp? > a b))\n"
"(define (string-ci<=? a b) (string-cmp? char-ci-cmp? <= a b))\n"
"(define (string-ci>=? a b) (string-cmp? char-ci-cmp? >= a b))\n"
"\n"
"(define (list . x) x)\n"
"\n"
"(define (foldr f x lst)\n"
"     (if (null? lst)\n"
"          x\n"
"          (foldr f (f x (car lst)) (cdr lst))))\n"
"\n"
"(define (unzip1-with-cdr . lists)\n"
"  (unzip1-with-cdr-iterative lists '() '()))\n"
"\n"
"(define (unzip1-with-cdr-iterative lists cars cdrs)\n"
"  (if (null? lists)\n"
"      (cons cars cdrs)\n"
"      (let ((car1 (caar lists))\n"
"      (cdr1 (cdar lists)))\n"
"  (unzip1-with-cdr-iterative\n"
"   (cdr lists)\n"
"   (append cars (list car1))\n"
"   (append cdrs (list cdr1))))))\n"
"\n"
"(define (map proc . lists)\n"
"  (if (null? lists)\n"
"      (apply proc)\n"
"      (if (null? (car lists))\n"
"    '()\n"
"    (let* ((unz (apply unzip1-with-cdr lists))\n"
"     (cars (car unz))\n"
"     (cdrs (cdr unz)))\n"
"      (cons (apply proc cars) (apply map (cons proc cdrs)))))))\n"
"\n"
"(define (for-each proc . lists)\n"
"  (if (null? lists)\n"
"      (apply proc)\n"
"      (if (null? (car lists))\n"
"    #t\n"
"    (let* ((unz (apply unzip1-with-cdr lists))\n"
"     (cars (car unz))\n"
"     (cdrs (cdr unz)))\n"
"      (apply proc cars) (apply map (cons proc cdrs))))))\n"
"\n"
"(define (list-tail x k)\n"
"    (if (zero? k)\n"
"        x\n"
"        (list-tail (cdr x) (- k 1))))\n"
"\n"
"(define (list-ref x k)\n"
"    (car (list-tail x k)))\n"
"\n"
"(define (last-pair x)\n"
"    (if (pair? (cdr x))\n"
"        (last-pair (cdr x))\n"
"        x))\n"
"\n"
"(define (head stream) (car stream))\n"
"\n"
"(define (tail stream) (force (cdr stream)))\n"
"\n"
"(define (vector-equal? x y)\n"
"     (and (vector? x) (vector? y) (= (vector-length x) (vector-length y))\n"
"          (let ((n (vector-length x)))\n"
"               (let loop ((i 0))\n"
"                    (if (= i n)\n"
"                         #t\n"
"                         (and (equal? (vector-ref x i) (vector-ref y i))\n"
"                              (loop (succ i))))))))\n"
"\n"
"(define (list->vector x)\n"
"     (apply vector x))\n"
"\n"
"(define (vector-fill! v e)\n"
"     (let ((n (vector-length v)))\n"
"          (let loop ((i 0))\n"
"               (if (= i n)\n"
"                    v\n"
"                    (begin (vector-set! v i e) (loop (succ i)))))))\n"
"\n"
"(define (vector->list v)\n"
"     (let loop ((n (pred (vector-length v))) (l '()))\n"
"          (if (= n -1)\n"
"               l\n"
"               (loop (pred n) (cons (vector-ref v n) l)))))\n"
"\n"
";; The following quasiquote macro is due to Eric S. Tiedemann.\n"
";;   Copyright 1988 by Eric S. Tiedemann; all rights reserved.\n"
";;\n"
";; Subsequently modified to handle vectors: D. Souflis\n"
"\n"
"(macro\n"
" quasiquote\n"
" (lambda (l)\n"
"   (define (mcons f l r)\n"
"     (if (and (pair? r)\n"
"              (eq? (car r) 'quote)\n"
"              (eq? (car (cdr r)) (cdr f))\n"
"              (pair? l)\n"
"              (eq? (car l) 'quote)\n"
"              (eq? (car (cdr l)) (car f)))\n"
"         (if (or (procedure? f) (number? f) (string? f))\n"
"               f\n"
"               (list 'quote f))\n"
"         (if (eqv? l vector)\n"
"               (apply l (eval r))\n"
"               (list 'cons l r)\n"
"               )))\n"
"   (define (mappend f l r)\n"
"     (if (or (null? (cdr f))\n"
"             (and (pair? r)\n"
"                  (eq? (car r) 'quote)\n"
"                  (eq? (car (cdr r)) '())))\n"
"         l\n"
"         (list 'append l r)))\n"
"   (define (foo level form)\n"
"     (cond ((not (pair? form))\n"
"               (if (or (procedure? form) (number? form) (string? form))\n"
"                    form\n"
"                    (list 'quote form))\n"
"               )\n"
"           ((eq? 'quasiquote (car form))\n"
"            (mcons form ''quasiquote (foo (+ level 1) (cdr form))))\n"
"           (#t (if (zero? level)\n"
"                   (cond ((eq? (car form) 'unquote) (car (cdr form)))\n"
"                         ((eq? (car form) 'unquote-splicing)\n"
"                          (error \"Unquote-splicing wasn't in a list:\"\n"
"                                 form))\n"
"                         ((and (pair? (car form))\n"
"                               (eq? (car (car form)) 'unquote-splicing))\n"
"                          (mappend form (car (cdr (car form)))\n"
"                                   (foo level (cdr form))))\n"
"                         (#t (mcons form (foo level (car form))\n"
"                                         (foo level (cdr form)))))\n"
"                   (cond ((eq? (car form) 'unquote)\n"
"                          (mcons form ''unquote (foo (- level 1)\n"
"                                                     (cdr form))))\n"
"                         ((eq? (car form) 'unquote-splicing)\n"
"                          (mcons form ''unquote-splicing\n"
"                                      (foo (- level 1) (cdr form))))\n"
"                         (#t (mcons form (foo level (car form))\n"
"                                         (foo level (cdr form)))))))))\n"
"   (foo 0 (car (cdr l)))))\n"
"\n"
";;;;;Helper for the dynamic-wind definition.  By Tom Breton (Tehom)\n"
"(define (shared-tail x y)\n"
"   (let (  (len-x (length x))\n"
"	   (len-y (length y)))\n"
"      (define (shared-tail-helper x y)\n"
"	 (if\n"
"	    (eq? x y)\n"
"	    x\n"
"	    (shared-tail-helper (cdr x) (cdr y))))\n"
"      (cond\n"
"	 ((> len-x len-y)\n"
"	    (shared-tail-helper\n"
"	       (list-tail x (- len-x len-y))\n"
"	       y))\n"
"	 ((< len-x len-y)\n"
"	    (shared-tail-helper\n"
"	       x\n"
"	       (list-tail y (- len-y len-x))))\n"
"	 (#t (shared-tail-helper x y)))))\n"
"\n"
";;;;;Dynamic-wind by Tom Breton (Tehom)\n"
"\n"
";;Guarded because we must only eval this once, because doing so\n"
";;redefines call/cc in terms of old call/cc\n"
"(unless (defined? 'dynamic-wind)\n"
"   (let\n"
"      ;;These functions are defined in the context of a private list of\n"
"      ;;pairs of before/after procs.\n"
"      (  (*active-windings* '())\n"
"	 ;;We'll define some functions into the larger environment, so\n"
"	 ;;we need to know it.\n"
"	 (outer-env (current-environment)))\n"
"\n"
"      ;;Poor-man's structure operations\n"
"      (define before-func car)\n"
"      (define after-func  cdr)\n"
"      (define make-winding cons)\n"
"\n"
"      ;;Manage active windings\n"
"      (define (activate-winding! new)\n"
"	 ((before-func new))\n"
"	 (set! *active-windings* (cons new *active-windings*)))\n"
"      (define (deactivate-top-winding!)\n"
"	 (let ((old-top (car *active-windings*)))\n"
"	    ;;Remove it from the list first so it's not active during its\n"
"	    ;;own exit.\n"
"	    (set! *active-windings* (cdr *active-windings*))\n"
"	    ((after-func old-top))))\n"
"\n"
"      (define (set-active-windings! new-ws)\n"
"	 (unless (eq? new-ws *active-windings*)\n"
"	    (let ((shared (shared-tail new-ws *active-windings*)))\n"
"\n"
"	       ;;Define the looping functions.\n"
"	       ;;Exit the old list.  Do deeper ones last.  Don't do\n"
"	       ;;any shared ones.\n"
"	       (define (pop-many)\n"
"		  (unless (eq? *active-windings* shared)\n"
"		     (deactivate-top-winding!)\n"
"		     (pop-many)))\n"
"	       ;;Enter the new list.  Do deeper ones first so that the\n"
"	       ;;deeper windings will already be active.  Don't do any\n"
"	       ;;shared ones.\n"
"	       (define (push-many new-ws)\n"
"		  (unless (eq? new-ws shared)\n"
"		     (push-many (cdr new-ws))\n"
"		     (activate-winding! (car new-ws))))\n"
"\n"
"	       ;;Do it.\n"
"	       (pop-many)\n"
"	       (push-many new-ws))))\n"
"\n"
"      ;;The definitions themselves.\n"
"      (eval\n"
"	 `(define call-with-current-continuation\n"
"	     ;;It internally uses the built-in call/cc, so capture it.\n"
"	     ,(let ((old-c/cc call-with-current-continuation))\n"
"		 (lambda (func)\n"
"		    ;;Use old call/cc to get the continuation.\n"
"		    (old-c/cc\n"
"		       (lambda (continuation)\n"
"			  ;;Call func with not the continuation itself\n"
"			  ;;but a procedure that adjusts the active\n"
"			  ;;windings to what they were when we made\n"
"			  ;;this, and only then calls the\n"
"			  ;;continuation.\n"
"			  (func\n"
"			     (let ((current-ws *active-windings*))\n"
"				(lambda (x)\n"
"				   (set-active-windings! current-ws)\n"
"				   (continuation x)))))))))\n"
"	 outer-env)\n"
"      ;;We can't just say \"define (dynamic-wind before thunk after)\"\n"
"      ;;because the lambda it's defined to lives in this environment,\n"
"      ;;not in the global environment.\n"
"      (eval\n"
"	 `(define dynamic-wind\n"
"	     ,(lambda (before thunk after)\n"
"		 ;;Make a new winding\n"
"		 (activate-winding! (make-winding before after))\n"
"		 (let ((result (thunk)))\n"
"		    ;;Get rid of the new winding.\n"
"		    (deactivate-top-winding!)\n"
"		    ;;The return value is that of thunk.\n"
"		    result)))\n"
"	 outer-env)))\n"
"\n"
"(define call/cc call-with-current-continuation)\n"
"\n"
"\n"
";;;;; atom? and equal? written by a.k\n"
"\n"
";;;; atom?\n"
"(define (atom? x)\n"
"  (not (pair? x)))\n"
"\n"
";;;;    equal?\n"
"(define (equal? x y)\n"
"     (cond\n"
"          ((pair? x)\n"
"               (and (pair? y)\n"
"                    (equal? (car x) (car y))\n"
"                    (equal? (cdr x) (cdr y))))\n"
"          ((vector? x)\n"
"               (and (vector? y) (vector-equal? x y)))\n"
"          ((string? x)\n"
"               (and (string? y) (string=? x y)))\n"
"          (else (eqv? x y))))\n"
"\n"
";;;; (do ((var init inc) ...) (endtest result ...) body ...)\n"
";;\n"
"(macro do\n"
"  (lambda (do-macro)\n"
"    (apply (lambda (do vars endtest . body)\n"
"             (let ((do-loop (gensym)))\n"
"               `(letrec ((,do-loop\n"
"                           (lambda ,(map (lambda (x)\n"
"                                           (if (pair? x) (car x) x))\n"
"                                      `,vars)\n"
"                             (if ,(car endtest)\n"
"                               (begin ,@(cdr endtest))\n"
"                               (begin\n"
"                                 ,@body\n"
"                                 (,do-loop\n"
"                                   ,@(map (lambda (x)\n"
"                                            (cond\n"
"                                              ((not (pair? x)) x)\n"
"                                              ((< (length x) 3) (car x))\n"
"                                              (else (car (cdr (cdr x))))))\n"
"                                       `,vars)))))))\n"
"                  (,do-loop\n"
"                    ,@(map (lambda (x)\n"
"                             (if (and (pair? x) (cdr x))\n"
"                               (car (cdr x))\n"
"                               '()))\n"
"                        `,vars)))))\n"
"      do-macro)))\n"
"\n"
";;;; generic-member\n"
"(define (generic-member cmp obj lst)\n"
"  (cond\n"
"    ((null? lst) #f)\n"
"    ((cmp obj (car lst)) lst)\n"
"    (else (generic-member cmp obj (cdr lst)))))\n"
"\n"
"(define (memq obj lst)\n"
"     (generic-member eq? obj lst))\n"
"(define (memv obj lst)\n"
"     (generic-member eqv? obj lst))\n"
"(define (member obj lst)\n"
"     (generic-member equal? obj lst))\n"
"\n"
";;;; generic-assoc\n"
"(define (generic-assoc cmp obj alst)\n"
"     (cond\n"
"          ((null? alst) #f)\n"
"          ((cmp obj (caar alst)) (car alst))\n"
"          (else (generic-assoc cmp obj (cdr alst)))))\n"
"\n"
"(define (assq obj alst)\n"
"     (generic-assoc eq? obj alst))\n"
"(define (assv obj alst)\n"
"     (generic-assoc eqv? obj alst))\n"
"(define (assoc obj alst)\n"
"     (generic-assoc equal? obj alst))\n"
"\n"
"(define (acons x y z) (cons (cons x y) z))\n"
"\n"
";;;; Handy for imperative programs\n"
";;;; Used as: (define-with-return (foo x y) .... (return z) ...)\n"
"(macro (define-with-return form)\n"
"     `(define ,(cadr form)\n"
"          (call/cc (lambda (return) ,@(cddr form)))))\n"
"\n"
";;;; Simple exception handling\n"
";\n"
";    Exceptions are caught as follows:\n"
";\n"
";         (catch (do-something to-recover and-return meaningful-value)\n"
";              (if-something goes-wrong)\n"
";              (with-these calls))\n"
";\n"
";    \"Catch\" establishes a scope spanning multiple call-frames\n"
";    until another \"catch\" is encountered.\n"
";\n"
";    Exceptions are thrown with:\n"
";\n"
";         (throw \"message\")\n"
";\n"
";    If used outside a (catch ...), reverts to (error \"message)\n"
"\n"
"(define *handlers* (list))\n"
"\n"
"(define (push-handler proc)\n"
"     (set! *handlers* (cons proc *handlers*)))\n"
"\n"
"(define (pop-handler)\n"
"     (let ((h (car *handlers*)))\n"
"          (set! *handlers* (cdr *handlers*))\n"
"          h))\n"
"\n"
"(define (more-handlers?)\n"
"     (pair? *handlers*))\n"
"\n"
"(define (throw . x)\n"
"     (if (more-handlers?)\n"
"          (apply (pop-handler))\n"
"          (apply error x)))\n"
"\n"
"(macro (catch form)\n"
"     (let ((label (gensym)))\n"
"          `(call/cc (lambda (exit)\n"
"               (push-handler (lambda () (exit ,(cadr form))))\n"
"               (let ((,label (begin ,@(cddr form))))\n"
"                    (pop-handler)\n"
"                    ,label)))))\n"
"\n"
"(define *error-hook* throw)\n"
"\n"
"\n"
";;;;; Definition of MAKE-ENVIRONMENT, to be used with two-argument EVAL\n"
"\n"
"(macro (make-environment form)\n"
"     `(apply (lambda ()\n"
"               ,@(cdr form)\n"
"               (current-environment))))\n"
"\n"
"(define-macro (eval-polymorphic x . envl)\n"
"  (display envl)\n"
"  (let* ((env (if (null? envl) (current-environment) (eval (car envl))))\n"
"         (xval (eval x env)))\n"
"    (if (closure? xval)\n"
"  (make-closure (get-closure-code xval) env)\n"
"  xval)))\n"
"\n"
"; Redefine this if you install another package infrastructure\n"
"; Also redefine 'package'\n"
"(define *colon-hook* eval)\n"
"\n"
";;;;; I/O\n"
"\n"
"(define (input-output-port? p)\n"
"     (and (input-port? p) (output-port? p)))\n"
"\n"
"(define (close-port p)\n"
"     (cond\n"
"          ((input-output-port? p) (close-input-port (close-output-port p)))\n"
"          ((input-port? p) (close-input-port p))\n"
"          ((output-port? p) (close-output-port p))\n"
"          (else (throw \"Not a port\" p))))\n"
"\n"
"(define (call-with-input-file s p)\n"
"     (let ((inport (open-input-file s)))\n"
"          (if (eq? inport #f)\n"
"               #f\n"
"               (let ((res (p inport)))\n"
"                    (close-input-port inport)\n"
"                    res))))\n"
"\n"
"(define (call-with-output-file s p)\n"
"     (let ((outport (open-output-file s)))\n"
"          (if (eq? outport #f)\n"
"               #f\n"
"               (let ((res (p outport)))\n"
"                    (close-output-port outport)\n"
"                    res))))\n"
"\n"
"(define (with-input-from-file s p)\n"
"     (let ((inport (open-input-file s)))\n"
"          (if (eq? inport #f)\n"
"               #f\n"
"               (let ((prev-inport (current-input-port)))\n"
"                    (set-input-port inport)\n"
"                    (let ((res (p)))\n"
"                         (close-input-port inport)\n"
"                         (set-input-port prev-inport)\n"
"                         res)))))\n"
"\n"
"(define (with-output-to-file s p)\n"
"     (let ((outport (open-output-file s)))\n"
"          (if (eq? outport #f)\n"
"               #f\n"
"               (let ((prev-outport (current-output-port)))\n"
"                    (set-output-port outport)\n"
"                    (let ((res (p)))\n"
"                         (close-output-port outport)\n"
"                         (set-output-port prev-outport)\n"
"                         res)))))\n"
"\n"
"(define (with-input-output-from-to-files si so p)\n"
"     (let ((inport (open-input-file si))\n"
"           (outport (open-input-file so)))\n"
"          (if (not (and inport outport))\n"
"               (begin\n"
"                    (close-input-port inport)\n"
"                    (close-output-port outport)\n"
"                    #f)\n"
"               (let ((prev-inport (current-input-port))\n"
"                     (prev-outport (current-output-port)))\n"
"                    (set-input-port inport)\n"
"                    (set-output-port outport)\n"
"                    (let ((res (p)))\n"
"                         (close-input-port inport)\n"
"                         (close-output-port outport)\n"
"                         (set-input-port prev-inport)\n"
"                         (set-output-port prev-outport)\n"
"                         res)))))\n"
"\n"
"; Random number generator (maximum cycle)\n"
"(define *seed* 1)\n"
"(define (random-next)\n"
"     (let* ((a 16807) (m 2147483647) (q (quotient m a)) (r (modulo m a)))\n"
"          (set! *seed*\n"
"               (-   (* a (- *seed*\n"
"                         (* (quotient *seed* q) q)))\n"
"                    (* (quotient *seed* q) r)))\n"
"          (if (< *seed* 0) (set! *seed* (+ *seed* m)))\n"
"          *seed*))\n"
";; SRFI-0\n"
";; COND-EXPAND\n"
";; Implemented as a macro\n"
"(define *features* '(srfi-0))\n"
"\n"
"(define-macro (cond-expand . cond-action-list)\n"
"  (cond-expand-runtime cond-action-list))\n"
"\n"
"(define (cond-expand-runtime cond-action-list)\n"
"  (if (null? cond-action-list)\n"
"      #t\n"
"      (if (cond-eval (caar cond-action-list))\n"
"          `(begin ,@(cdar cond-action-list))\n"
"          (cond-expand-runtime (cdr cond-action-list)))))\n"
"\n"
"(define (cond-eval-and cond-list)\n"
"  (foldr (lambda (x y) (and (cond-eval x) (cond-eval y))) #t cond-list))\n"
"\n"
"(define (cond-eval-or cond-list)\n"
"  (foldr (lambda (x y) (or (cond-eval x) (cond-eval y))) #f cond-list))\n"
"\n"
"(define (cond-eval condition)\n"
"  (cond ((symbol? condition)\n"
"   (if (member condition *features*) #t #f))\n"
"  ((eq? condition #t) #t)\n"
"  ((eq? condition #f) #f)\n"
"  (else (case (car condition)\n"
"    ((and) (cond-eval-and (cdr condition)))\n"
"    ((or) (cond-eval-or (cdr condition)))\n"
"    ((not) (if (not (null? (cddr condition)))\n"
"         (error \"cond-expand : 'not' takes 1 argument\")\n"
"         (not (cond-eval (cadr condition)))))\n"
"    (else (error \"cond-expand : unknown operator\" (car condition)))))))\n"
"\n"
"(gc-verbose #f)\n";