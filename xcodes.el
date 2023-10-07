(define-derived-mode xcodes-mode tabulated-list-mode "Xcodes" "Lists versions of Xcode"
  (setq tabulated-list-format [("" 1 nil)
                               ("Version" 30 xcodes-version-lessp)
                               ("Build" 8 nil)])
    (setq tabulated-list-padding 2)
    (setq tabulated-list-sort-key (cons "Version" t))
    (tabulated-list-init-header))

(defun xcodes () 
  "Displays a list of Xcodes with status"
  (interactive)
  (pop-to-buffer "*Xcodes*" nil)
  (xcodes-mode)
  (xcodes-get-data)
  (setq tabulated-list-entries xcodes-entries)
  (tabulated-list-print t))

(defun xcodes-get-data ()
  "Displays a list of Xcodes with status"
  (setq xcodes-entries nil)
  (let ((xcodes-output (shell-command-to-string "xcodes list")))
    (dolist (xcode (string-lines xcodes-output))
      (pcase (xcodes-parse-line xcode)
        (`(,version ,build ,status)
         (push (list build (vector (xcodes-formatted-status status) version build)) xcodes-entries))))))

(defun xcodes-formatted-status (status)
  "Formats an Xcodes status"
  (cond ((string-match-p (regexp-quote "Selected") status) (propertize "✓" 'font-lock-face 'success))
        ((string-match-p (regexp-quote "Installed") status) "-")
        (t "")))


(defun xcodes-parse-line (xcode-line)
  "Parses a single Xcodes output line into an Xcode record"
  (save-match-data
    (and (string-match (rx (group (* (not "("))) "(" (group (* (not ")"))) ")" (? " (" (group (* (not ")"))) ")")) xcode-line)
         (let ((version (match-string 1 xcode-line))
               (build (match-string 2 xcode-line))
               (status (or (match-string 3 xcode-line) "")))
           (list version build status)))))

(defun xcodes-version-lessp (lhs rhs)
  "Sorts Xcodes by version"
  (let ((lhs-version (elt (cadr lhs) 1))
        (rhs-version (elt (cadr rhs) 1)))
    (string-version-lessp lhs-version rhs-version)))
