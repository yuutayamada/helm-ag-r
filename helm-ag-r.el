;;; -*- coding: utf-8; mode: emacs-lisp; -*-
;;; helm-ag-r.el --- Search something by ag and display by helm

;; Copyright (C) 2013 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/helm-ag-r
;; Version: 0.0.1
;; Package-Requires: ((helm "20130916"))
;; Keywords: Searching

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Usage(sample)
;; set below configuration to your .emacs
;; (add-to-list 'load-path "path/to/this-package-directory")
;; (require 'helm-ag-r)
;; You can change ag's option by pushing C-o from below variable on minibuffer
;; See ag --help about available option
;; (setq helm-ag-r-option-list
;;       '("-S -U --hidden"
;;         "-S -U -l"))

(eval-when-compile (require 'cl))
(require 'helm)

(defvar helm-ag-r-directory '())
(defvar helm-ag-r-option-list '())
(defvar helm-ag-r-current-command '())
(defvar helm-ag-r-base-command nil)
(defvar helm-ag-r-user-option nil)
(defvar helm-ag-r-histfile
  (shell-command-to-string "echo -n $HISTFILE")
  "history file to use at helm-ag-r-shell-history function")

(defvar helm-ag-r-source
  '((name               . "helm-ag-r")
    (header-name        . (lambda (name)
                            (format "%s (%s)" name helm-ag-r-current-command)))
    (candidates-process . (lambda ()
                            (funcall helm-ag-r-function)))
    (candidates-in-buffer)
    (real-to-display . helm-ag-r-remove-dir-name)
    (delayed)))

(defun helm-ag-r-remove-dir-name (line)
  (if (string-match "^.+:[0-9]+:." line)
      (let* ((all (split-string line ":"))
             (path    (file-relative-name (nth 0 all)))
             (num     (nth 1 all))
             (content (nth 2 all)))
        (mapconcat 'identity (list path num content) ":"))
    line))

(defun helm-ag-r-find-file-action (candidate find-func)
  (let* ((elems (split-string candidate ":"))
         (search-this-file (helm-attr 'search-this-file))
         (filename (or search-this-file (first elems)))
         (line (string-to-number (if search-this-file
                                     (first elems)
                                   (second elems)))))
    (funcall find-func filename)
    (goto-char (point-min))
    (forward-line (1- line))))

(defvar helm-ag-r-actions
  '((:open
     (("Open File" . (lambda (candidate)
                       (helm-ag-r-find-file-action candidate 'find-file)))
      ("Open File Other Window" .
       (lambda (candidate)
         (helm-ag-r-find-file-action candidate 'find-file-other-window)))))
    (:move
     (("Move the line" . (lambda (line)
                           (string-match "^\\([0-9]*\\)\\(:\\|-\\)" line)
                           (goto-char (point-min))
                           (forward-line (1- (string-to-number
                                              (match-string 1 line))))))))))

(defvar helm-ag-r-get-command
  (lambda (pattern)
    (let*
        ((set-attribute
          (lambda (attr)
            (helm-attrset 'action
                          (car
                           (assoc-default attr helm-ag-r-actions))
                          helm-ag-r-source)))
         (patterns (split-string pattern))
         (dir-or-file helm-ag-r-directory)
         (ag-commands
          (mapconcat 'identity (helm-ag-r-create-command patterns) " | ")))
      (if (and (file-exists-p dir-or-file) (not (file-directory-p dir-or-file)))
          (funcall set-attribute :move)
        (funcall set-attribute :open))
      (setq helm-ag-r-current-command ag-commands)
      ag-commands)))

(defun helm-ag-r-create-command (patterns)
  (loop with first-command = (lambda (ag search full)
                               (if helm-ag-r-base-command
                                   (concat helm-ag-r-base-command
                                           " | " ag " " search)
                                 full))
        with opt = helm-ag-r-user-option
        with ag-base = (when opt (concat "ag " opt))
        for ag = (or ag-base "ag --nocolor --nogroup") then "ag --nocolor"
        for options = (car helm-ag-r-option-list) then " "
        for search-word in patterns
        for search = (shell-quote-argument search-word)
        for d-f = helm-ag-r-directory then ""
        for full = (concat ag " " options " " search " " d-f)
        for cmd = (funcall first-command ag search full) then full
        collect cmd))

(defun helm-ag-r-pype (command &optional source)
  (let ((helm-ag-r-base-command command))
    (helm-ag-r nil source)))

(defun helm-ag-r-shell-history ()
  "Search shell history(I don't make sure without zsh)"
  (interactive)
  (helm-ag-r-pype
   (concat "tac " helm-ag-r-histfile " | sed 's/^: [0-9]*:[0-9];//'")
   '((action . (lambda (line)
                 (case major-mode
                   (term-mode (term-send-raw-string line))
                   (t (insert line))))))))

(defun helm-ag-r-git-logs (&optional options)
  "Search git's commit"
  (interactive)
  (let ((opts (or "--all --oneline --pretty=format:%s"
                  options)))
    (helm-ag-r-pype
   (concat "git log " opts)
   '((action . (lambda (line) (insert line)))))))

(defvar helm-ag-r-function
  (lambda ()
    (start-process
     "emacs-helm-ag-rrocess" nil "/bin/sh" "-c"
     (funcall helm-ag-r-get-command helm-pattern))))

(defvar helm-ag-r-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-o") 'helm-ag-r-change-option)
    map))

(defun helm-ag-r-override-source (source)
  (loop with result = '()
        for (prefix . content) in helm-ag-r-source
        if (assoc prefix source)
        collect it into result
        else collect (cons prefix content) into result
        finally return result))

;;;###autoload
(defun helm-ag-r (&optional file-or-directory source)
  (interactive)
  (setq helm-ag-r-directory (or file-or-directory default-directory))
  (let* ((src (if source
                  (helm-ag-r-override-source source)
                helm-ag-r-source)))
    (helm :sources src
          :prompt "ag: "
          :buffer "*helm ag r"
          :keymap helm-ag-r-keymap)))

;;;###autoload
(defun helm-ag-r-current-file ()
  (interactive)
  (helm-ag-r buffer-file-name))

(defun helm-ag-r-change-option ()
  (interactive)
  (setq helm-ag-r-option-list
        (append
         (cdr helm-ag-r-option-list)
         (list (car helm-ag-r-option-list))))
  (helm-update))

;;;###autoload
(defun helm-ag-r-from-git-repo ()
  (interactive)
  (helm-ag-r (helm-ag-r-get-top-dir)))

(defun helm-ag-r-get-top-dir (&optional cwd)
  (setq cwd (expand-file-name (file-truename (or cwd default-directory))))
  (when (file-directory-p cwd)
    (let* ((chomp (lambda (str)
                    (when (equal (elt str (- (length str) 1)) ?\n)
                      (substring str 0 (- (length str) 1)))))
           (default-directory (file-name-as-directory cwd))
           (repository-top
            (funcall chomp
                     (shell-command-to-string "git rev-parse --show-toplevel"))))
      (when repository-top
        (file-name-as-directory (expand-file-name repository-top cwd))))))

(provide 'helm-ag-r)

;;; helm-ag-r.el ends here
