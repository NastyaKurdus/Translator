(defstruct token
  name
  row
  col)
(defun stay (lex row col)
  (make-token :name lex
	      :row row
	      :col col))
(defun all-keyword(str)
  (find str '(PROGRAM BEGIN END VAR SIGNAL COMPLEX INTEGER FLOAT BLOCKFLOAT EXT ) :test #'string=))
(defun delimeter(str)
     (cond
      ((char= str #\: ) 'colon)
      ((char= str #\; ) 'semicolon)
      ((char= str #\, ) 'comma)
      ((char= str #\[ ) 'leftpar)
      ((char= str #\] ) 'rightpar)))

(defun whitespace(str)
  (and str (find str '(#\Return #\Space #\Tab #\Newline #\Vt) :test #'char=)))






(defun analiz-symbol(stream symbol row col)
  (cond
    ((whitespace symbol)
     (do* ((sym symbol (read-char stream nil))
	  (cur-row (if (or (eq sym #\Newline) (eq sym #\Return))
			   (+ row 1)
			   row)
		   (if (or (eq sym #\Newline) (eq sym #\Return))
			   (+ cur-row 1)
			   cur-row))
	  (cur-col (if (or (eq sym #\Newline) (eq sym #\Return))
			   0
			   (+ col 1))
		   (if (or (eq sym #\Newline) (eq sym #\Return))
			   0
			   (+ cur-col 1))))
	  ((not (whitespace sym))(list sym nil cur-row cur-col))))
    
    
    ((delimeter symbol) (list (read-char stream nil) (list 'delimeter (stay (delimeter symbol) row col)) row (+ 1 col)))
    
    ((digit-char-p symbol)
     (do* ((sym(read-char stream nil) (read-char stream nil))
	   (buf (list sym symbol) (cons sym buf)))
	  ((not (digit-char-p sym))
	   (list (car buf)
		 (list 'unsigned-integer
		       (stay (reduce (lambda(acc elem)
				       (+ (* acc 10)(digit-char-p elem)))
				     (reverse (cdr buf))
				     :initial-value 0) row col))
		 row
		 (+ col (1- (length buf)))))))
    
    
    ((alpha-char-p symbol)
     (do* ((sym(read-char stream nil) (read-char stream nil))
	   (buf (list sym symbol) (cons sym buf)))
	  ((not (alphanumericp sym))
	   (let ((token (intern (coerce  (reverse (cdr buf)) 'string))))
				       (list (car buf)
					     (list (if (all-keyword token)
						       'keyword
						       'identifier)
						   (stay token row col))
					     row
					     (+ col (1- (length buf))))))))
    
    ((eq symbol #\.)
     (let ((next (read-char stream nil)))
       (if (eq next #\.)
	   (list (read-char stream nil) (list 'delimeter (stay 'dot-dot row col)) row (+ 2 col))
	   (list next (list 'delimeter (stay 'dot row col)) row (+ 1 col)))))
    
    ((eq symbol #\( )
     (if (eq (read-char stream nil) #\*)
	 (do* ((next-symb (read-char stream nil) (read-char stream nil))
	        (cur-row (if (or (eq next-symb #\Newline) (eq next-symb #\Return))
			   (+ row 1)
			   row)
			 (if (or (eq next-symb #\Newline) (eq next-symb #\Return))
			   (+ cur-row 1)
			   cur-row))
	       (cur-col (if (or (eq next-symb #\Newline) (eq next-symb #\Return))
			   0
			   (+ col 3))
			(if (or (eq next-symb #\Newline) (eq next-symb #\Return))
			   0
			   (+ cur-col 1))))
	     ((if next-symb
		  (and (eq next-symb #\*)
		       (do ((other-char (read-char stream nil)(read-char stream nil))
			   (asterix 0 (+ 1 asterix)))
			   ((not (eq other-char #\*))
			    (cond
			      ((or (eq other-char #\Newline) (eq other-char #\Return))
			       (and (incf cur-row) (setf cur-col 0) nil))
			      ((eq other-char #\))
			       (setf cur-col(+ (+ cur-col asterix) 2)) t)
			      ((null other-char) (or  (format t "LEXER-error:End of file ~s ~s~%"  row col) t ))
			      (t (and (setf col (1+ (+ col asterix)))nil))))))
		  (or (format t "LEXER-error:End of file ~s ~s~%" row col) t))
	      (list (read-char stream nil) nil cur-row cur-col)))
	 (format t "LEXER-error:Not asterix after '('~s ~s~%"  row col)))
    ((null symbol) nil)
    (t (format t "LEXER-error:Illegal symbol ~s ~s~%"  row col))))

(defun lexer(file)
  (with-open-file(stream file)
    (do*((symbol ( list (read-char stream nil) nil 1 1) (analiz-symbol stream (car symbol) (third symbol) (fourth symbol)))
	 (buf nil (cons (second symbol) buf)))
	((not (car symbol))
	 (remove-if #'null (reverse buf))))))
(defmethod print-object ((lex token) stream)
  (format stream "~s" (token-name lex)))

