;;; chatgpt.el --- Use ChatGPT inside Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-openai/chatgpt
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (openai "0.1.0") (lv "0.0") (ht "2.0") (markdown-mode "2.1"))
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
(require 'markdown-mode)

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

(defcustom chatgpt-input-method 'window
  "The method receive input."
  :type '(choice (const :tag "Read from minibuffer" minibuffer)
                 (const :tag "Read inside new window" window))
  :group 'chatgpt)

(defcustom chatgpt-inhibit-input-afterward t
  "Stop input after sending one."
  :type 'boolean
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

(defface chatgpt-user
  '((t :inherit font-lock-builtin-face))
  "Face used for user."
  :group 'chatgpt)

;;
;;; Util

(defmacro chatgpt--with-no-redisplay (&rest body)
  "Execute BODY without any redisplay execution."
  (declare (indent 0) (debug t))
  `(let ((inhibit-redisplay t)
         (inhibit-modification-hooks t)
         after-focus-change-function
         buffer-list-update-hook
         display-buffer-alist
         window-configuration-change-hook
         window-scroll-functions
         window-size-change-functions
         window-state-change-hook)
     ,@body))

(defun chatgpt--kill-buffer (buffer-or-name)
  "Like function `kill-buffer' (BUFFER-OR-NAME) but in the safe way."
  (when-let ((buffer (get-buffer buffer-or-name)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun chatgpt--pop-to-buffer (buffer-or-name)
  "Wrapper to function `pop-to-buffer'.

Display buffer from BUFFER-OR-NAME."
  (pop-to-buffer buffer-or-name `((display-buffer-in-direction)
                                  (dedicated . t))))

(defun chatgpt-user ()
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
    (ht-map (lambda (_ buffer)
              (when (and (get-buffer buffer)
                         (buffer-live-p buffer))
                (push buffer live-instances)))
            chatgpt-instances)
    (reverse live-instances)))

(defun chatgpt--shown-instances ()
  "Return a list of live instances that are displayed on the screen."
  (let ((live-instances (chatgpt--live-instances))
        (shown-instances))
    (dolist (instance live-instances)
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

(defun chatgpt--render-markdown (content)
  "Render CONTENT in markdown."
  (if (featurep 'markdown-mode)
      (with-temp-buffer
        (insert content)
        (delay-mode-hooks (markdown-mode))
        (ignore-errors (font-lock-ensure))
        (buffer-string))
    content))

(defun chatgpt--fill-region (start end)
  "Like function `fill-region' (START to END), improve readability."
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (not (eobp))
      (end-of-line)
      (when (< fill-column (current-column))
        (fill-region (line-beginning-position) (line-end-position)))
      (forward-line 1))))

(defun chatgpt--display-messages ()
  "Display all messages to latest response."
  (while (< chatgpt--display-pointer (length chatgpt-chat-history))
    (let ((message (elt chatgpt-chat-history chatgpt--display-pointer)))
      (let-alist message
        (goto-char (point-max))
        (let* ((start (point))
               (is-user (string= (chatgpt-user) .role))
               (role (format "<%s>:" .role))
               ;; XXX: If messages from user, don't try to render to markdown!
               ;; else, messages from OpenAI will most likely going to be
               ;; markdown so we render it!
               (content (if is-user .content
                          (chatgpt--render-markdown .content))))
          (add-face-text-property 0 (length role) 'chatgpt-user nil role)
          (insert role " " content)
          (insert "\n\n")
          (chatgpt--fill-region start (point)))))
    (cl-incf chatgpt--display-pointer)))

(defun chatgpt-send-response (response)
  "Send RESPONSE to ChatGPT."
  (let ((user (chatgpt-user))
        (instance chatgpt-instance))
    (when (string-empty-p response)
      (user-error "[INFO] Invalid response or instruction: %s" response))
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
                 :user user)))

(defun chatgpt-type-response ()
  "Type response to OpenAI."
  (interactive)
  (cond
   (chatgpt-requesting-p
    (message "[BUSY] Waiting for OpanAI to response..."))
   (t
    (cl-case chatgpt-input-method
      (`minibuffer
       (chatgpt-send-response (read-string "Type response: ")))
      (`window
       (chatgpt--start-input chatgpt-instance))
      (t
       (user-error "Invalid input method: %s" chatgpt-input-method))))))

;;
;;; Input

(defconst chatgpt-input-buffer-name "*ChatGPT-Input*"
  "Buffer name to receive input.")

(defvar chatgpt-input-instance nil
  "The current instance; there is only one instance at a time.")

(defun chatgpt-input-exit ()
  "Exit the input."
  (interactive)
  (chatgpt--kill-buffer chatgpt-input-buffer-name))

(defun chatgpt--start-input (instance)
  "Start input from INSTANCE."
  (chatgpt-input-exit)
  (let ((dir (if (window-parameter nil 'window-side)
                 'bottom 'down))
        (buffer (get-buffer-create chatgpt-input-buffer-name)))
    ;; XXX: Without this, the highlighting at the end wouldn't work!?
    (chatgpt--with-no-redisplay
      (with-current-buffer buffer
        (chatgpt-input-mode)
        (setq chatgpt-input-instance instance)
        (erase-buffer)
        (insert "Type response here...")
        (call-interactively #'set-mark-command)
        (goto-char (point-min))))  ; waiting for deletion
    (pop-to-buffer buffer `((display-buffer-in-direction)
                            (direction . ,dir)
                            (dedicated . t)
                            (window-height . fit-window-to-buffer)))))

(defun chatgpt-input-send ()
  "Send the input."
  (interactive)
  (cond
   ((not (eq major-mode #'chatgpt-input-mode)) )  ; does nothing
   (chatgpt-requesting-p
    (message "[BUSY] Waiting for OpanAI to response..."))
   ((region-active-p)
    (delete-region (region-beginning) (region-end)))
   (t
    (let ((response (buffer-string)))
      (chatgpt-with-instance chatgpt-input-instance
        (chatgpt-send-response response))
      (erase-buffer))
    (when chatgpt-inhibit-input-afterward
      (chatgpt-input-exit)))))

(defun chatgpt-input--post-command ()
  "Execution after input."
  (let ((max-lines (line-number-at-pos (point-max))))
    (fit-window-to-buffer)
    (enlarge-window (- max-lines (window-text-height)))))

(defun chatgpt-input-header-line ()
  "The display for input header line."
  (format " Session: %s" (cdr chatgpt-input-instance)))

(defvar chatgpt-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "S-<return>") #'newline)
    (define-key map (kbd "RET") #'chatgpt-input-send)
    map)
  "Keymap for `chatgpt-mode'.")

;;;###autoload
(define-derived-mode chatgpt-input-mode fundamental-mode "ChatGPT Input"
  "Major mode for `chatgpt-input-mode'.

\\<chatgpt-input-mode-map>"
  (setq-local header-line-format `((:eval (chatgpt-input-header-line))))
  (add-hook 'post-command-hook #'chatgpt-input--post-command nil t))

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
  (when (eq major-mode #'chatgpt-mode)
    (lv-message
     (concat
      (format "session: %s" (cdr chatgpt-instance)) "\n"
      (format "history size: %s" (length chatgpt-chat-history))
      "\n\n"
      (format "prompt_tokens: %s | completion_tokens: %s | total_tokens: %s"
              (ht-get chatgpt-data 'prompt_tokens 0)
              (ht-get chatgpt-data 'completion_tokens 0)
              (ht-get chatgpt-data 'total_tokens 0))
      "\n\n"
      (format "model: %s" chatgpt-model) "\n"
      (format "max_tokens: %s" chatgpt-max-tokens) "\n"
      (format "temperature: %s" chatgpt-temperature) "\n"
      (format "user: %s" (chatgpt-user))))
    ;; Register event to cancel lv window!
    (add-hook 'pre-command-hook #'chatgpt--pre-command-once)))

;;
;;; Entry

(defun chatgpt-mode--kill-buffer-hook ()
  "Kill buffer hook."
  (let ((instance chatgpt-instances))
    (when (get-buffer chatgpt-input-buffer-name)
      (with-current-buffer chatgpt-input-buffer-name
        ;; kill input if it's the right session
        (when (equal instance chatgpt-instances)
          (kill-this-buffer))))))

(defun chatgpt-header-line ()
  "The display for header line."
  (format " Session: %s, History: %s, User: %s (M-x chatgpt-info)"
          (cdr chatgpt-instance)
          (length chatgpt-chat-history)
          (chatgpt-user)))

(defvar chatgpt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'chatgpt-type-response)
    map)
  "Keymap for `chatgpt-mode'.")

;;;###autoload
(define-derived-mode chatgpt-mode fundamental-mode "ChatGPT"
  "Major mode for `chatgpt-mode'.

\\<chatgpt-mode-map>"
  (setq-local buffer-read-only t)
  (font-lock-mode -1)
  (add-hook 'kill-buffer-hook #'chatgpt-mode--kill-buffer-hook nil t)
  (setq-local header-line-format `((:eval (chatgpt-header-line)))))

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
