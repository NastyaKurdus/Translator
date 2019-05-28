(load "parser.lisp")

(defvar *variables-names*)

(defun translate (filename)
  (let (*variables-names*)
    (traverse (parser filename))))

(defun traverse (tree)
  (case (first tree)
    (signal-programm
     (traverse (second tree)))
    (program
     (format nil ";~A~%~%~A"
	     (traverse (third tree))
	     (traverse (fourth tree))))
    (block
     (format
      nil
      "~A~%.code~%BEGIN:~%~TMOV AX,@DATA~%~TMOV DS,AX~%~A~TMOV AX,4c00h~%~TINT 21h~%END BEGIN~%"
      (traverse (second tree))
      (traverse (fourth tree))))
    (declarations
     (traverse (second tree)))
    (variable-declarations
     (format nil ".data~%~A" (traverse (third tree))))
    (declaration-list
     (format nil "~A~A"
	     (traverse (second tree))
	     (traverse (third tree))))
    (declaration
     (let* ((identifiers (cons (traverse (second tree))
			       (traverse (third tree))))
	    (attributes (canonize-attrs (cons (traverse (nth 4 tree))
					      (traverse (nth 5 tree))))))
       (format nil "~{~^~T~A~%~}"
	       (mapcar (lambda (id)
			 (let ((name (token-name id)))
			   (if (find name *variables-names*)
			       (error "'~A' is already exists.(row ~A,col ~A)"
				      name
				      (token-row id)
				      (token-col id))
			       (prog2
				   (push name *variables-names*)
				   (format nil "~A ~A"
					   id
					   attributes)))))
		       identifiers))))
    (identifier-list
     (when (second tree)
       (cons (traverse (third tree))
	     (traverse (fourth tree)))))
    (attribute-list
     (when (second tree)
       (cons (traverse (second tree))
	     (traverse (third tree)))))
    (attribute
     (if (eq (second tree) 'leftpar)
	 (cons (traverse (third tree))
	       (traverse (fourth tree)))
	 (second tree)))
    (range-list
     (when (second tree)
       (cons (traverse (third tree))
	     (traverse (fourth tree)))))
    (range
     (let* ((ui-1 (traverse (second tree)))
	    (ui-2 (traverse (fourth tree))))
       (if (= (token-name ui-1) 1)
	   (if (<= (token-name ui-1) (token-name ui-2))
	       ui-2
	       (error "~A isn't greater than ~A.(row: ~A,col ~A)"
		      ui-2
		      ui-1
		      (token-row ui-2)
		      (token-col ui-2)))
	   (error "Array must start from 1, but got from ~A.(row: ~A,col ~A)."
		  ui-1
		  (token-row ui-1)
		  (token-col ui-1)))))
    (statements-list (format nil "~TNOP~%"))
    ((or variable-identifier
	 procedure-identifier)
     (traverse (second tree)))
    (identifier
     (second tree))
    (unsigned-integer
     (second tree))
    (t "")))

(defun canonize-attrs (lst &optional lists types spec-types)
  (if lst
      (let ((item (car lst)))
	(if (listp item)
	    (canonize-attrs (cdr lst) (cons item lists) types spec-types)
	    (if (find item (append spec-types types))
		(error "Duplicate type '~A'." item)
		(case item
		  ((or SIGNAL EXT COMPLEX)
		   (canonize-attrs (cdr lst)
				   lists
				   types
				   (cons item spec-types)))
		  (t
		   (canonize-attrs (cdr lst)
				   lists
				   (cons item types)
				   spec-types))))))
      (if (= (length types) 1)
	  (if (<= (length lists) 1)
	      (format nil "~A ~A"
		      (get-type (car types))
		      (create-value (car lists)))
	      (error "Error count of ranges."))
	  (error "Error count of types."))))

(defun create-value (lst)
  (reduce (lambda (item acc)
	    (format nil "~A dup (~A)"
		    item
		    acc))
	  lst
	  :initial-value "?"
	  :from-end t))

(defun get-type (str)
  (case str
    (INTEGER 'DB)
    (FLOAT 'DW)
    (BLOCKFLOAT 'DT)))
