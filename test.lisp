(load "parser.lisp")

(defun create-format-test (file)
  (let ((buff (lexer file)))
    (format t "~5S ~7S ~15S ~S~%~%" 'Line 'Column 'Symbol 'Type)
    (mapc (lambda (token)
	    (let ((symbol (token-name (second token)))
		  (line (token-row (second token)))
		  (column (token-col (second token)))
		  (type (car token)))
	      (format t "~5S ~7S ~15S ~S~%" line column symbol type)))
	  buff)))

(defvar *index* 0)
(defun inc () (incf *index*))
(defun tree-to-graph (source-file &optional (target-file "graph.dot"))
  (let ((graph (cl-graph:make-graph 'cl-graph:dot-graph
            :default-edge-type :directed)))
    (make-pairs (parser source-file)
          (lambda (pair)
      (let ((vertex1 (cl-graph:add-vertex graph
                  (car (car pair))
                  :dot-attributes (list :label (second (car pair)))))
      (vertex2 (cl-graph:add-vertex graph
                  (car (second pair))
                  :dot-attributes (list :label (second (second pair))))))
        (cl-graph:add-edge-between-vertexes graph
              vertex1
              vertex2
              :dot-attributes '(:label "")))))
    (cl-graph:graph->dot graph target-file)))

(defun make-pairs (lst fn)
  (let ((index *index*))
    (mapc (lambda (elem)
	    (inc)
	    (funcall fn (if (listp elem)
			    (prog1
				(list (list index (car lst)) (list *index* (car elem)))
			      (make-pairs elem fn))
			    (list (list index (car lst)) (list *index* elem)))))
	  (cdr lst))))

(defun test()
  (format t "True-test1~%")
  (format t "~S~%~%"(parser "True-test1.txt"))
  (format t "True-test2~%")
  (format t "~S~%~%"(parser "True-test2.txt"))
  (format t "True-test3~%")
  (format t "~S~%~%"(parser "True-test3.txt"))
  (format t "True-test4~%")
  (format t "~S~%~%"(parser "True-test4.txt"))
  (format t "True-test5~%")
  (format t "~S~%~%"(parser "True-test5.txt"))
  (format t "False-test1~%")
  (format t "~S~%~%"(parser "False-test1.txt"))
  (format t "False-test2~%")
  (format t "~S~%~%"(parser "False-test2.txt"))
  (format t "False-test3~%")
  (format t "~S~%~%"(parser "False-test3.txt")))

(defun do-graphs()
  (tree-to-graph "True-test1.txt" "graph1.dot")
  (tree-to-graph "True-test2.txt" "graph2.dot")
  (tree-to-graph "True-test3.txt" "graph3.dot")
  (tree-to-graph "True-test4.txt" "graph4.dot")
  (tree-to-graph "True-test5.txt" "graph5.dot")
  
  t)
