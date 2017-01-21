(define (null-ld? obj)
	(if (eq? (car obj) (cdr obj)) #t #f)) ;if the two lists are equal, their listdiff is empty
	;(if (null (listdiff obj)) #t #f)

(define (listdiff? obj)
	(cond
		[(empty? obj) #f]
		[(not (pair? obj)) #f]
		[(empty? (car obj)) #f]
		[(eq? (car obj) (cdr obj)) #t] ; if list and sublist are equal, empty listdiff
		[(not (pair? (car obj))) #f] ; if list is an atom, nonempty and not equal to sublist, then false
		[#t (listdiff? (cons (cdar obj) (cdr obj)))]))

(define (cons-ld obj listdiff)
	; error check for if last item is NOT a listdiff?
	; check if listdiff is a listdiff??
	(if (empty? obj) listdiff (cons (cons obj (car listdiff)) (cdr listdiff))))

(define (car-ld listdiff)
	(if (equal? (car listdiff) (cdr listdiff)) 'error (caar listdiff)))

(define (cdr-ld listdiff)
	(if (equal? (car listdiff) (cdr listdiff)) 'error (cons (cdar listdiff) (cdr listdiff))))

(define (listdiff . obj) ; take arbitrary obj
	(let ((tail '(z)))
		(cons (append obj tail) tail)))
	; return any listdiff, such that car contains objs z, cdr is z

(define (length-ld listdiff)
	(cond
		[(empty? listdiff) 'error]
		[(not (pair? listdiff)) 'error]
		[(empty? (car listdiff)) 0]
		[(eq? (car listdiff) (cdr listdiff)) 0] ; if list and sublist are equal, empty listdiff
		[(not (pair? (car listdiff))) 'error] ; if list is an atom, nonempty and not equal to sublist, then false
		[#t (let ((result (length-ld (cons (cdar listdiff) (cdr listdiff)))))
			(if (eqv? 'error result) 'error (+ 1 result)))]))

; helper function to append all tails of listdiffs
(define (appenddiff listdiffs)
	(cond
		[(empty? listdiffs) empty]
		[#t (append (listdiff->list (car listdiffs)) (appenddiff (cdr listdiffs)))]))

(define (appendtail listdiffs)
	(cond
		[(empty? listdiffs) null]
		[#t (append (cdar listdiffs) (appendtail (cdr listdiffs)))]))

(define (append-ld . listdiff)
	(cond 
		[(empty? listdiff) empty]
		[#t (let ((tail '(z))) (cons (append (appenddiff listdiff) tail) tail))]))
	
(define (checkpairs obj pairs)
	(cond
		[(empty? pairs) #f]
		[(if (eq? obj (caar pairs)) (car pairs) (checkpairs obj (cdr pairs)))]))

(define (assq-ld obj alistdiff) ;alistdiff has members which are all pairs
	(cond 
		[(empty? alistdiff) #f]
		[#t (checkpairs obj (car alistdiff))]))
			
(define (list->listdiff list)
	(let ((tail '(z))) ; ensures same obj??
		(cons (append list tail) tail))) ; return any list?? representing listdiff list

(define (listdiff->list listdiff)
	(cond
		[(eq? (car listdiff) (cdr listdiff)) empty]
		[#t (cons (caar listdiff) (listdiff->list (cons (cdar listdiff) (cdr listdiff))))]))

(define-syntax expr-returning
	(syntax-rules ()
		((expr-returning listdiff) '(cons (car listdiff) (cdr listdiff)))))
