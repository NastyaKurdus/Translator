(load "lexer.lisp")
(defvar *lexem-row*)

(defun scan()
  (pop *lexem-row*))

(defun unscan(lexem)
  (push lexem *lexem-row*)
  nil)

(defun parser (filename)
  (let ((*lexem-row* (lexer filename)))
    (<signal-programm>)))


(defun is-correct-lexem (lexem type value)
  (and (eq (car lexem) type)
       (eq (token-name (second lexem)) value)))

(defun <signal-programm> ()
  (let ((program (<program>)))
    (if (and program (null *lexem-row*))
	(list 'signal-programm program)
	(warn "ERROR: Expected end of file."))))

(defun <program> ()
  (let ((ts (scan)))
    (if (is-correct-lexem ts 'keyword 'PROGRAM)
	(let ((procedure-identifier (<procedure-identifier> (scan))))
	  (when procedure-identifier
	    (let ((semi (scan)))
	      (if (is-correct-lexem semi 'delimeter 'semicolon)
		  (let ((block_ (<block>)))
		    (when block_
		      (if (is-correct-lexem (scan) 'delimeter 'dot)
			  (list 'program 'PROGRAM procedure-identifier block_)
			  (warn "ERROR: dot expected"))))
		  (warn "ERROR: Semicolon expected. at line ~S,column ~S."
			(token-row (second ts))
			(token-col (second ts)))))))
	(warn "ERROR: Keyword 'PROGRAM' expected at line ~S,column ~S."
	      (token-row (second ts))
	      (token-col (second ts))))))

(defun <block>()
  (let ((declarations (<declarations>)))
    (when declarations
      (let ((begin (scan)))
	(if (is-correct-lexem begin 'keyword 'BEGIN)
	    (let ((statements-list (<statements-list>))
		  (end (scan)))
	      (if (is-correct-lexem end 'keyword 'END)
		  (list 'block declarations 'BEGIN statements-list 'END)
		  (warn "ERROR: 'END' expected at line ~S,column ~S.."
			(token-row (second end))
			(token-col (second end)))))
	    (warn "ERROR: 'BEGIN' expected at line ~S,column ~S."
		  (token-row (second begin))
		  (token-col (second begin))))))))
	    
(defun <declarations>()
  (let ((variable-declarations (<variable-declarations>)))
    (when variable-declarations
      (list 'declarations variable-declarations))))

(defun <variable-declarations>()
  (let ((ts (scan)))
    (if (is-correct-lexem ts 'keyword 'VAR)
	(let ((declaration-list (<declaration-list>)))
	  (when declaration-list
	    (list 'variable-declarations 'VAR declaration-list)))
	(prog2
	    (unscan ts)
	    (list 'variable-declarations)))))

(defun <declaration-list>()
  (labels ((%declaration-list ()
	     (let ((ts (scan)))
	       (if (is-correct-lexem ts 'keyword 'BEGIN)
		   (prog2
		       (unscan ts)
		       (list 'declaration-list))
		   (let ((declaration (<declaration> ts)))
		     (when declaration
		       (list 'declaration-list
			     declaration
			     (%declaration-list))))))))
    (%declaration-list)))

(defun <statements-list>()
  (list 'statements-list))

(defun <declaration>(ts)
  (let ((variable-identifier (<variable-identifier> ts)))
    (when variable-identifier
      (let ((identifier-list (<identifier-list>))
	    (colon (scan)))
	(if (is-correct-lexem colon 'delimeter 'COLON)
		(let ((attribute (<attribute> (scan))))
		  (when attribute
		    (let ((attr-list (<attribute-list>))
			  (semi (scan)))
		      (if (is-correct-lexem semi 'delimeter 'semicolon)
			  (list 'declaration
				variable-identifier
				identifier-list
				'colon
				attribute
				attr-list)
			  (warn "Expected ; at line ~S,column ~S."
				(token-row (second semi))
				(token-col (second semi)))))))
		(warn "Expected : at line ~S,column ~S."
		      (token-row (second colon))
		      (token-col (second colon))))))))

(defun <identifier-list> ()
  (labels ((%identifier-list ()
	     (let ((ts (scan)))
	       (if (is-correct-lexem ts 'delimeter 'comma)
		   (let ((variable-identifier (<variable-identifier> (scan))))
		     (when variable-identifier
		       (list 'identifier-list
			     'comma
			     variable-identifier
			     (%identifier-list))))
		   (prog2
		       (unscan ts)
		       (list 'identifier-list))))))
    (%identifier-list)))

(defun <attribute-list>()
  (labels ((%attribute-list()
	     (let ((ts (scan)))
	       (if (is-correct-lexem ts 'delimeter 'semicolon)
		   (prog2
		       (unscan ts)
		       (list 'attribute-list))
		   (let ((attribute (<attribute> ts)))
		     (when attribute
		       (list 'attribute-list
			     attribute
			     (%attribute-list))))))))
    (%attribute-list)))

(defun <attribute>(ts)
  (cond
    ((or (is-correct-lexem ts 'keyword 'SIGNAL)
	 (is-correct-lexem ts 'keyword 'COMPLEX)
	 (is-correct-lexem ts 'keyword 'INTEGER)
	 (is-correct-lexem ts 'keyword 'FLOAT)
	 (is-correct-lexem ts 'keyword 'BLOCKFLOAT)
	 (is-correct-lexem ts 'keyword 'EXT))
     (list 'attribute (token-name (second ts)))) 
    ((is-correct-lexem ts 'delimeter 'leftpar)
     (let ((range (<range> (scan))))
       (when range
	 (let ((range-list (<range-list>))
	       (ts1 (is-correct-lexem (scan) 'delimeter 'rightpar)))
	   (if ts1
	       (list 'attribute 'leftpar range range-list 'rightpar)
	       (warn "Expected ] at line ~S, column ~S."
		     (token-row (second ts))
		     (token-col (second ts))))))))
    (t (warn "Expected [ or one of these: 'SIGNAL' or 'COMPLEX' ... at line ~S,column ~S."
	     (token-row (second ts))
	     (token-col (second ts))))))


(defun <range-list>()
  (labels ((%range-list()
	     (let ((ts (scan)))
	       (if (is-correct-lexem ts 'delimeter 'comma)
		   (let ((range (<range> (scan))))
		     (when range
		       (list 'range-list 'comma range (%range-list))))
		   (prog2
		       (unscan ts)
		       (list 'range-list))))))
    (%range-list)))

(defun <range> (ts)
  (let ((unsigned-integer1 (<unsigned-integer> ts)))
    (when unsigned-integer1
      (let ((dot-dot (scan)))
	(if (is-correct-lexem dot-dot 'delimeter 'dot-dot)
	    (let ((unsigned-integer2 (<unsigned-integer> (scan))))
	      (when unsigned-integer2
		(list 'range unsigned-integer1 'dot-dot unsigned-integer2)))
	    (warn "Expected .. at line ~S,column ~S."
		  (token-row (second dot-dot))
		  (token-col (second dot-dot))))))))

(defun <variable-identifier>(ts)
  (let ((identifier (<identifier> ts)))
    (when identifier
      (list 'variable-identifier identifier))))

(defun <procedure-identifier>(ts)
  (let ((identifier (<identifier> ts)))
    (when identifier
      (list 'procedure-identifier identifier))))


(defun <identifier>(ts)
  (if (eq (car ts) 'identifier)
      ts
      (warn "ERROR: <IDENTIFIER> expected at line ~S,column ~S.."
	    (token-row (second ts))
	    (token-col (second ts)))))

(defun <unsigned-integer>(ts)
  (if (eq (car ts) 'unsigned-integer)
      ts
      (warn "ERROR: <UNSIGNED-INTEGER> expected at line ~S,column ~S.."
	    (token-row (second ts))
	    (token-col (second ts)))))
