;;; chatgpt.el --- Use ChatGPT inside Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-openai/chatgpt
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (openai "0.1.0") (lv "0.0") (ht "2.0"))
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
(require 'let-alist)

(require 'openai)
(require 'lv)
(require 'ht)

(defgroup chatgpt nil
  "Use ChatGPT inside Emacs."
  :prefix "chatgpt-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/emacs-openai/chatgpt"))

(defcustom chatgpt-model "gpt-3.5-turbo"
  "Model to use for chat."
  :type 'string
  :group 'chatgpt)

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

(defvar-local chatgpt-data (ht-create)
  "Store other information other than messages.")

(defvar-local chatgpt--display-pointer 0
  "Display pointer.")

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
;;; Instances

(defmacro chatgpt-with-instance (instance &rest body)
  "Execute BODY within instance."
  (declare (indent 1))
  `(when-let ((buffer (and ,instance
                           (get-buffer (cdr ,instance)))))
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
         ,@body))))

(defun chatgpt--live-instances ()
  "Return a list of live instances."
  (let ((live-instances))
    (ht-map (lambda (index buffer)
              (when (and (get-buffer buffer)
                         (buffer-live-p buffer))
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
                 (when (or (not (get-buffer buffer))
                           (not (buffer-live-p buffer)))  ; if buffer is killed
                   (setq target index)
                   t)))
             (ht-keys chatgpt-instances))
    (unless target                                ; No killed instance?
      (setq target (ht-size chatgpt-instances)))  ; Create a new one!
    target))

(defun chatgpt-restart ()
  "Restart session."
  (interactive)
  (let* ((instance chatgpt-instances)
         (index    (car instance))
         (buffer   (cdr instance))
         (old-name))
    ;; If buffer is alive, kill it!
    (chatgpt-with-instance instance
      (setq old-name (buffer-name))
      (kill-this-buffer))
    ;; `old-name' will remain `nil' if buffer is not killed or invalid!
    (when old-name
      (chatgpt-register-instance index old-name))))

;;
;;; Core

(defun chatgpt--add-tokens (data)
  "Record all tokens information from DATA."
  (let-alist data
    (let-alist .usage
      ;; First we get the current tokens,
      (let ((prompt-tokens     (ht-get chatgpt-data 'prompt_tokens 0))
            (completion-tokens (ht-get chatgpt-data 'completion_tokens 0))
            (total-tokens      (ht-get chatgpt-data 'total_tokens 0)))
        ;; Then we add it up!
        (ht-set chatgpt-data 'prompt_tokens     (+ prompt-tokens     .prompt_tokens))
        (ht-set chatgpt-data 'completion_tokens (+ completion-tokens .completion_tokens))
        (ht-set chatgpt-data 'total_tokens      (+ total-tokens      .total_tokens))))))

(defun chatgpt--add-message (role content)
  "Add a message to history.

The data is consist of ROLE and CONTENT."
  (setq chatgpt-chat-history
        (vconcat (or chatgpt-chat-history '[])
                 `[((role    . ,role)
                    (content . ,(string-trim content)))])))

(defun chatgpt--add-response-messages (data)
  "Add the message to history from DATA, and return the message itself."
  (let-alist data
    (mapc (lambda (choice)
            (let-alist choice
              (let-alist .message
                (chatgpt--add-message .role .content))))
          .choices)))

;;
;;; Display

(defun chatgpt--display-messages ()
  "Display all messages to latest response."
  (while (< chatgpt--display-pointer (length chatgpt-chat-history))
    (let ((message (elt chatgpt-chat-history chatgpt--display-pointer)))
      (let-alist message
        (insert .role ": " .content)
        (insert "\n\n")))
    (cl-incf chatgpt--display-pointer)))

(defun chatgpt-type-response ()
  "Type response to OpenAI."
  (interactive)
  (cond
   (chatgpt-requesting-p
    (message "[BUSY] Waiting for OpanAI to response..."))
   (t
    (let ((response (read-string "Type response: "))
          (user (chatgpt--get-user))
          (instance chatgpt-instance))
      (chatgpt--add-message user response)  ; add user's response
      (chatgpt-with-instance instance
        (chatgpt--display-messages))        ; display it
      (setq chatgpt-requesting-p t)
      (openai-chat chatgpt-chat-history
                   (lambda (data)
                     (chatgpt-with-instance instance
                       (setq chatgpt-requesting-p nil)
                       (chatgpt--add-tokens data)
                       (chatgpt--add-response-messages data)
                       (chatgpt--display-messages)))
                   :model chatgpt-model
                   :max-tokens chatgpt-max-tokens
                   :temperature chatgpt-temperature
                   :user user)))))

;;
;;; Info

(defun chatgpt--pre-command-once (&rest _)
  "One time pre-command after Easky command."
  ;; XXX: We pass on to next post-command!
  (remove-hook 'pre-command-hook #'chatgpt--pre-command-once)
  (add-hook 'post-command-hook #'chatgpt--post-command-once))

(defun chatgpt--post-command-once ()
  "One time post-command after info command."
  ;; XXX: This will allow us to scroll in the lv's window!
  (unless (equal lv-wnd (selected-window))
    ;; Once we select window other than lv's window, then we kill it!
    (remove-hook 'post-command-hook #'chatgpt--post-command-once)
    (lv-delete-window)))

(defun chatgpt-info ()
  "Show session information."
  (interactive)
  (lv-message
   (concat
    (format "model: %s" chatgpt-model) "\n"
    (format "prompt_tokens: %s | completion_tokens: %s | total_tokens: %s"
            (ht-get chatgpt-data 'prompt_tokens 0)
            (ht-get chatgpt-data 'completion_tokens 0)
            (ht-get chatgpt-data 'total_tokens 0))
    "\n"
    (format "max_tokens: %s" chatgpt-max-tokens) "\n"
    (format "temperature: %s" chatgpt-temperature) "\n"
    (format "user: %s" (chatgpt--get-user))))
  ;; Register event to cancel lv window!
  (add-hook 'pre-command-hook #'chatgpt--pre-command-once))

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

(defun chatgpt-register-instance (index buffer-or-name)
  "Register BUFFER-OR-NAME with INDEX as an instance.

Caution, this will overwrite the existing instance!"
  (ht-set chatgpt-instances index (get-buffer-create buffer-or-name))
  (with-current-buffer buffer-or-name
    (chatgpt-mode)
    (setq chatgpt-instance (cons index (current-buffer)))))

;;;###autoload
(defun chatgpt-new ()
  "Run a new instance of ChatGPT."
  (interactive)
  (let* ((new-index       (chatgpt--new-index))
         (new-buffer-name (format chatgpt-buffer-name-format new-index)))
    (when (get-buffer new-buffer-name)
      (user-error "Internal Error: creating instance that already exists"))
    (chatgpt-register-instance new-index new-buffer-name)
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
           (chatgpt--pop-to-buffer (nth 0 live-instances)))
          (t
           (chatgpt-new)))))

(provide 'chatgpt)
;;; chatgpt.el ends here
