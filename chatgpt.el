;;; chatgpt.el --- Use ChatGPT inside Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-openai/chatgpt
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (openai "0.1.0") (ht "2.0"))
;; Keywords: comm openai

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Use ChatGPT inside Emacs
;;

;;; Code:

(require 'cl-lib)

(require 'openai)
(require 'ht)

(defgroup chatgpt nil
  "Use ChatGPT inside Emacs."
  :prefix "chatgpt-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/emacs-openai/chatgpt"))

(defcustom chatgpt-max-tokens 4000
  "The maximum number of tokens to generate in the completion."
  :type 'integer
  :group 'chatgpt)

(defcustom chatgpt-temperature 1.0
  "What sampling temperature to use."
  :type 'number
  :group 'chatgpt)

(defconst chatgpt-buffer-name-format "*ChatGPT: <%s>*"
  "Name of the buffer to use for the `chatgpt' instance.")

(defvar chatgpt-instances (ht-create)
  "List of instances, each pair is consist of (index . buffer).")

(defvar-local chatgpt-instance nil
  "Instance data for each buffer.")

(defvar-local chatgpt-chat-history nil
  "The chat history use to send request.")

(defvar-local chatgpt-requesting-p nil
  "Non-nil when requesting; waiting for the response.")

;;
;;; Util

(defun chatgpt--pop-to-buffer (buffer-or-name)
  "Wrapper to function `pop-to-buffer'.

Display buffer from BUFFER-OR-NAME."
  (pop-to-buffer buffer-or-name `((display-buffer-in-direction)
                                  (dedicated . t))))

(defun chatgpt--get-user ()
  "Return the current user."
  (if (string-empty-p openai-user)
      "user"  ; this is free?
    openai-user))

;;
;;; Core

(defun chatgpt--live-instances ()
  "Return a list of live instances."
  (let ((live-instances))
    (ht-map (lambda (index buffer)
              (when (get-buffer buffer)
                (push buffer live-instances)))
            chatgpt-instances)
    (reverse live-instances)))

(defun chatgpt--shown-instances ()
  "Return a list of live instances that are displayed on the screen."
  (let ((live-instances (chatgpt--live-instances))
        (shown-instances))
    (dolist (instance shown-instances)
      (when (get-buffer-window instance)
        (push instance shown-instances)))
    (reverse shown-instances)))

(defun chatgpt--new-index ()
  "Find killed instance before giving new index."
  (let ((target))
    (cl-some (lambda (index)
               (let ((buffer (ht-get chatgpt-instances index)))
                 (unless (get-buffer buffer)  ; if buffer is killed
                   (setq target index)
                   t)))
             (ht-keys chatgpt-instances))
    (unless target                                ; No killed instance?
      (setq target (ht-size chatgpt-instances)))  ; Create a new one!
    target))

(defun chatgpt--display-message (instance message)
  ""
  (with-current-buffer (get-buffer (cdr instance))
    (let ((inhibit-read-only t))

      )))

(defun chatgpt-type-response ()
  ""
  (interactive)
  (let ((response (read-string "Type response: "))
        (instance chatgpt-instance))
    (setq chatgpt-requesting-p t)
    (openai-chat response
                 (lambda (data)
                   (let ((message ; TODO: ..
                          ))
                     (chatgpt--display-response instance message)))
                 :max-tokens chatgpt-max-tokens
                 :temperature chatgpt-temperature
                 :user (chatgpt--get-user))))

;;
;;; Entry

(defvar chatgpt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'chatgpt-type-response)
    map)
  "Keymap for `chatgpt-mode'.")

;;;###autoload
(define-derived-mode chatgpt-mode fundamental-mode "ChatGPT"
  "Major mode for `chatgpt-mode'.

\\<chatgpt-mode-map>"
  (setq-local buffer-read-only t))

;;;###autoload
(defun chatgpt-new ()
  "Run a new instance of ChatGPT."
  (interactive)
  (let* ((new-index       (chatgpt--new-index))
         (new-buffer-name (format chatgpt-buffer-name-format new-index)))
    (when (get-buffer new-buffer-name)
      (user-error "Internal Error: creating instance that already exists"))
    (ht-set chatgpt-instances new-index (get-buffer-create new-buffer-name))
    (with-current-buffer new-buffer-name
      (setq chatgpt-instance (cons new-index (current-buffer)))
      (chatgpt-mode 1))
    (chatgpt--pop-to-buffer new-buffer-name)))

;;;###autoload
(defun chatgpt ()
  "Start ChatGPT with existing instance, else create a new instance."
  (interactive)
  (let ((live-instances  (chatgpt--live-instances))
        (shown-instances (chatgpt--shown-instances)))
    (cond (shown-instances
           (chatgpt--pop-to-buffer (nth 0 shown-instances)))
          (live-instances
           (chatgpt--pop-to-buffer (nth 0 shown-instances)))
          (t
           (chatgpt-new)))))

(provide 'chatgpt)
;;; chatgpt.el ends here
