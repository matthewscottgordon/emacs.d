(defun mg-find-matching-bracket ()
  (save-excursion (forward-sexp) (point)))

(defun mg-end-of-line () (save-excursion (move-end-of-line nil) (point)))

(defun mg-remove-default-parameters ()
  (if (save-excursion
	  (let ((found-default-parameter (re-search-forward "=" (mg-find-matching-bracket) t 1)))
		(if found-default-parameter
			(let ((begin (1- (point))))
			  (re-search-forward "[^[:blank:]]" nil nil 1)
			  (delete-region begin (point))))
		found-default-parameter))
	  (mg-remove-default-parameters)))
		  

(defun c++-mock-interface ()
  "Convert in interface declaration into a GMock mock object declaration implementing the interface."
  (interactive)
  (save-excursion (while (re-search-forward "=[[:blank:]]*0[[:blank:]]*;" nil t 1)
	(back-to-indentation)
	(re-search-forward "virtual[[:blank:]]+" (mg-end-of-line) nil 1)
	(backward-kill-word 1) ;Delete "virtual"
	(insert "MOCK_METHOD(")
	; Attempt to match return type
	(re-search-forward "\\(const\\)?[[:blank:]]*[a-zA-Z_][a-zA-Z0-9_]*[*&]?" (mg-end-of-line) nil 1)
	(insert ",")
	; Match function name
	(re-search-forward "[a-zA-Z_][a-zA-Z0-9_]*" (mg-end-of-line) nil 1)
	(insert ", ")
	; Find limits of function parameters
	(mg-remove-default-parameters)
	(forward-sexp)
	(insert ",")
	(re-search-forward "[[:blank:]]*" (mg-end-of-line) nil 1)
	(insert "(")
	(if (re-search-forward "const" (mg-end-of-line) t 1) (insert ", "))
	(insert "override));")
	(kill-line nil)
	(next-line)))
  (save-excursion (while (re-search-forward "^[[:blank:]]*\\(//.*\\)?[[:blank:]]*$" nil t 1)
					(move-beginning-of-line nil)
					(kill-whole-line nil))))
