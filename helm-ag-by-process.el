;;; -*- coding: utf-8; mode: emacs-lisp; -*-
(eval-when-compile (require 'cl))
(require 'helm)
(require 'helm-ag)

(defvar helm-ag-by-process-directory '())
(defvar helm-ag-by-process-option-list '())
(defvar helm-ag-by-process-current-command '())

(defvar helm-ag-by-process-source
  '((name . "helm-ag-by-process")
    (header-name . (lambda (name)
                     (format "%s (%s)" name helm-ag-by-process-current-command)))
    (candidates-process . (lambda ()
                            (funcall helm-ag-by-process-function)))
    (candidates-in-buffer)
    (delayed)))

(defvar helm-ag-by-process-actions
  '((:open
     (("Open File" . (lambda (c)
                       (helm-ag-find-file-action c 'find-file)))
      ("Open File Other Window" .
       (lambda (c)
         (helm-ag-find-file-action c 'find-file-other-window)))))
    (:move
     (("Move the line" . (lambda (line)
                           (string-match "^\\([0-9]*\\):" line)
                           (goto-char (point-min))
                           (forward-line (1- (string-to-number
                                              (match-string 1 line))))))))))

(defvar helm-ag-by-process-get-command
  (lambda (pattern)
    (let*
        ((set-attribute
          (lambda (attr)
            (helm-attrset 'action
                               (car
                                (assoc-default attr helm-ag-by-process-actions))
                               helm-ag-by-process-source)))
         (patterns (split-string pattern))
         (dir-or-file helm-ag-by-process-directory)
         (create-ag-command
          (lambda (minibuffer-patterns)
            (loop with ag = helm-ag-base-command
                  with options = (car helm-ag-by-process-option-list)
                  for search-word in minibuffer-patterns
                  for d-f = dir-or-file then ""
                  collect (concat ag options " \"" search-word "\" " d-f " | "))))
         (ag-commands
          (replace-regexp-in-string
           " | $" ""
           (mapconcat 'identity (funcall create-ag-command patterns) ""))))
      (if (< 1 (length patterns))
          (funcall set-attribute :open)
        (funcall set-attribute :move))
      (setq helm-ag-by-process-current-command ag-commands)
      ag-commands)))

(defvar helm-ag-by-process-function
  (lambda ()
    (start-process
     "emacs-helm-ag-process" nil "/bin/sh" "-c"
     (funcall helm-ag-by-process-get-command helm-pattern))))

(defvar helm-ag-by-process-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-o") 'helm-ag-by-process-change-option)
    map))

(defun helm-ag-by-process (&optional directory)
  (interactive)
  (setq helm-ag-by-process-directory (or directory default-directory))
  (helm :sources helm-ag-by-process-source
        :prompt "ag: "
        :buffer "*helm ag process*"
        :keymap helm-ag-by-process-keymap))

(defun helm-ag-by-process-current-file ()
  (interactive)
  (helm-ag-by-process buffer-file-name))

(defun helm-ag-by-process-change-option ()
  (interactive)
  (setq helm-ag-by-process-option-list
        (append
         (cdr helm-ag-by-process-option-list)
         (car helm-ag-by-process-option-list))))

(defun helm-ag-by-process-from-current-file ()
  (interactive)
  (helm-attrset 'action
                (car (assoc-default :move helm-ag-by-process-actions))
                helm-ag-by-process-source)
  (helm-ag-by-process buffer-file-name))

(provide 'helm-ag-by-process)
