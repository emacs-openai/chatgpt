;;; chatgpt.el --- Use ChatGPT inside Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-openai/chatgpt
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (openai "0.1.0") (lv "0.0") (ht "2.0") (markdown-mode "2.1") (spinner "1.7.4"))
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
(require 'subr-x)

(require 'openai)
(require 'lv)
(require 'ht)
(require 'markdown-mode)
(require 'spinner)

(defgroup chatgpt nil
  "Use ChatGPT inside Emacs."
  :prefix "chatgpt-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/emacs-openai/chatgpt"))

(defcustom chatgpt-model "gpt-3.5-turbo"
  "Model to use for chat."
  :type 'string
  :group 'chatgpt)

(defcustom chatgpt-max-tokens 2000
  "The maximum number of tokens to generate in the completion."
  :type 'integer
  :group 'chatgpt)

(defcustom chatgpt-temperature 1.0
  "What sampling temperature to use."
  :type 'number
  :group 'chatgpt)

(defcustom chatgpt-top-p 1.0
  "Nucleus sampling parameter."
  :type 'number
  :group 'chatgpt)

(defcustom chatgpt-input-method 'window
  "The method receive input."
  :type '(choice (const :tag "Read from minibuffer" minibuffer)
                 (const :tag "Read inside new window" window))
  :group 'chatgpt)

(defcustom chatgpt-window-prompt "Type response here..."
  "Text inserted when window is created."
  :type 'string
  :group 'chatgpt)

(defcustom chatgpt-inhibit-input-afterward t
  "Stop input after sending one."
  :type 'boolean
  :group 'chatgpt)

(defcustom chatgpt-spinner-type 'moon
  "The type of the spinner."
  :type '(choice (const :tag "Key to variable `spinner-types'" symbol)
                 (const :tag "Vector of characters" vector))
  :group 'openai)

(defcustom chatgpt-display-tokens-info t
  "Non-nil we display tokens information for each request."
  :type 'boolean
  :group 'chatgpt)

(defcustom chatgpt-priority 100
  "Overlays' priority."
  :type 'integer
  :group 'chatgpt)

(defcustom chatgpt-animate-text t
  "Display text gradually instead of output it all at once."
  :type 'boolean
  :group 'chatgpt)

(defcustom chatgpt-animate-fps 5
  "Frame per seconds to display text animation."
  :type 'integer
  :group 'chatgpt)

(defconst chatgpt-buffer-name-format "*ChatGPT: <%s>*"
  "Name of the buffer to use for the `chatgpt' instance.")

(defvar chatgpt-instances (ht-create)
  "List of instances, each pair is consist of (index . buffer).")

(defvar-local chatgpt-instance nil
  "Instance data for each buffer.")

(defvar-local chatgpt-chat-history nil
  "The chat history use to send request.")

(defvar-local chatgpt-chat-points nil
  "Buffer points every time we add a new message.")

(defvar-local chatgpt-requesting-p nil
  "Non-nil when requesting; waiting for the response.")

(defvar-local chatgpt-spinner nil
  "Spinner.")

(defvar-local chatgpt-data (ht-create)
  "Store other information other than messages.")

(defvar-local chatgpt--display-pointer 0
  "Display pointer for each message to display.")

(defvar-local chatgpt--text-pointer 1
  "Text pointer for text animation.")

(defvar-local chatgpt-text-timer nil
  "Text timer for text animation.")

(defvar-local chatgpt-animating-p nil
  "Non-nil when animating text scroll.")

(defface chatgpt-user
  '((t :inherit font-lock-builtin-face))
  "Face used for user."
  :group 'chatgpt)

(defface chatgpt-tip
  '((t :foreground "#848484"))
  "Face used for tip."
  :group 'chatgpt)

(defface chatgpt-info
  '((t :height 0.8 :foreground "#999999"))
  "Face added to codemetrics display."
  :group 'chatgpt)

;;
;;; Externals

(declare-function string-pixel-width "ext:subr-x.el")
(declare-function shr-string-pixel-width "ext:shr.el")

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

;; TODO: Use function `string-pixel-width' after 29.1
(defun chatgpt--string-pixel-width (str)
  "Return the width of STR in pixels."
  (if (fboundp #'string-pixel-width)
      (string-pixel-width str)
    (require 'shr)
    (shr-string-pixel-width str)))

(defun chatgpt--str-len (str)
  "Calculate STR in pixel width."
  (let ((width (frame-char-width))
        (len (chatgpt--string-pixel-width str)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))  ; add one if exceeed

(defun chatgpt--align (&rest lengths)
  "Align sideline string by LENGTHS from the right of the window."
  (list (* (window-font-width)
           (+ (apply #'+ lengths) (if (display-graphic-p) 1 2)))))

(defun chatgpt--kill-buffer (buffer-or-name)
  "Like function `kill-buffer' (BUFFER-OR-NAME) but in the safe way."
  (when-let ((buffer (get-buffer buffer-or-name)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun chatgpt--cancel-timer (timer)
  "Cancel TIMER."
  (when (timerp timer)
    (cancel-timer timer)))

(defun chatgpt--pop-to-buffer (buffer-or-name)
  "Wrapper to function `pop-to-buffer'.

Display buffer from BUFFER-OR-NAME."
  (pop-to-buffer buffer-or-name `((display-buffer-in-direction)
                                  (dedicated . t))))

(defun chatgpt-busy-p ()
  "Return non-nil if session is busy."
  (or chatgpt-requesting-p chatgpt-animating-p))

(defun chatgpt-user ()
  "Return the current user."
  (if (string-empty-p openai-user)
      "user"  ; this is free?
    openai-user))

;;
;;; Chat Point

(defun chatgpt-chat-points ()
  "Return chat points in correct order."
  (reverse chatgpt-chat-points))

(defun chatgpt-current-chat-point ()
  "Return current chat point."
  (let ((chat-point))
    (cl-some (lambda (pt)
               (when (<= pt (point))
                 (setq chat-point pt)))
             chatgpt-chat-points)
    chat-point))

(defun chatgpt-chat-point-index (chat-point)
  "Return CHAT-POINT's index."
  (cl-position chat-point (chatgpt-chat-points)))

(defun chatgpt-current-chat-point-index ()
  "Return current chat point index."
  (chatgpt-chat-point-index (chatgpt-current-chat-point)))

(defun chatgpt-current-message ()
  "Return current message."
  (elt chatgpt-chat-history (chatgpt-current-chat-point-index)))

(defun chatgpt-current-content ()
  "Return current content."
  (alist-get 'content (chatgpt-current-message)))

(defun chatgpt-current-role ()
  "Return current role."
  (alist-get 'role (chatgpt-current-message)))

(defun chatgpt-shift-chat-points (start delta)
  "Shift all chat points after START by DELTA."
  (let ((index 0) (pt))
    (while (< index (length chatgpt-chat-points))
      (setq pt (nth index chatgpt-chat-points))
      (when (< start pt)
        (setf (nth index chatgpt-chat-points) (+ pt delta)))
      (cl-incf index))))

;;
;;; Instances

(defmacro chatgpt-with-instance (instance &rest body)
  "Execute BODY within INSTANCE."
  (declare (indent 1))
  `(when-let* ((buffer (and ,instance
                            (get-buffer (cdr ,instance))))
               ((buffer-live-p buffer)))
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
  (when (eq major-mode #'chatgpt-mode)
    (let* ((instance chatgpt-instance)
           (index    (car instance))
           (old-name))
      ;; If buffer is alive, kill it!
      (chatgpt-with-instance instance
        (setq old-name (buffer-name))
        (kill-this-buffer))
      ;; `old-name' will remain `nil' if buffer is not killed or invalid!
      (when old-name
        (chatgpt-register-instance index old-name)
        (switch-to-buffer old-name)))))

;;
;;; Core

(defun chatgpt--get-face-height ()
  "Make sure we get the face height."
  (let ((height (face-attribute 'chatgpt-info :height)))
    (if (numberp height) height
      1)))

(defun chatgpt--create-tokens-overlay (prompt-tokens completion-tokens total-tokens)
  "Display tokens information.

Arguments PROMPT-TOKENS, COMPLETION-TOKENS, and TOTAL-TOKENS are the tokens
information we want to display."
  (when chatgpt-display-tokens-info
    (let* ((ov (make-overlay (1- (point)) (1- (point)) nil t t))
           (content (format "prompt %s, completion %s, total: %s"
                            prompt-tokens completion-tokens total-tokens))
           (content-len (* (chatgpt--str-len content)
                           (chatgpt--get-face-height)))
           (str (concat
                 (propertize " " 'display
                             `((space :align-to (- right ,(chatgpt--align (1- content-len))))
                               (space :width 0))
                             `cursor t)
                 (propertize content 'face 'chatgpt-info))))
      (overlay-put ov 'chatgpt t)
      (overlay-put ov 'priority chatgpt-priority)
      (overlay-put ov 'after-string str))))

(defun chatgpt--add-tokens (data)
  "Record all tokens information from DATA."
  (let-alist data
    (let-alist .usage
      ;; First we get the current tokens,
      (let ((prompt-tokens     (ht-get chatgpt-data 'prompt_tokens 0))
            (completion-tokens (ht-get chatgpt-data 'completion_tokens 0))
            (total-tokens      (ht-get chatgpt-data 'total_tokens 0))
            (tokens-history    (ht-get chatgpt-data 'tokens_history nil)))
        ;; Then we add it up!
        (ht-set chatgpt-data 'prompt_tokens     (+ prompt-tokens     .prompt_tokens))
        (ht-set chatgpt-data 'completion_tokens (+ completion-tokens .completion_tokens))
        (ht-set chatgpt-data 'total_tokens      (+ total-tokens      .total_tokens))
        (ht-set chatgpt-data 'tokens_history
                (append tokens-history
                        `((,.prompt_tokens ,.completion_tokens ,.total_tokens))))
        ;; Render it!
        (unless chatgpt-animate-text
          (chatgpt--create-tokens-overlay .prompt_tokens .completion_tokens .total_tokens))))))

(defun chatgpt--add-chat-point ()
  "Add a chat point."
  (if chatgpt-chat-points
      (push (point-max) chatgpt-chat-points)
    (push (point-min) chatgpt-chat-points)))

(defun chatgpt--add-message (role content)
  "Add a message to history.

The data is consist of ROLE and CONTENT."
  (chatgpt--add-chat-point)
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

(defun chatgpt--cancel-text-timer ()
  "Cancel text timer."
  (chatgpt--cancel-timer chatgpt-text-timer)
  (setq chatgpt-text-timer nil
        chatgpt-animating-p nil))

(defun chatgpt--start-text-timer ()
  "Start text timer."
  (chatgpt--cancel-text-timer)
  (setq chatgpt-text-timer (run-with-timer (/ chatgpt-animate-fps 60.0)
                                           nil
                                           #'chatgpt--do-text-animatioin
                                           chatgpt-instance)
        chatgpt-animating-p t))

(defun chatgpt--do-text-animatioin (instance)
  "The main loop for text animation for the INSTANCE."
  (chatgpt-with-instance instance
    (chatgpt--cancel-text-timer)
    (let ((message (elt chatgpt-chat-history chatgpt--display-pointer)))
      (let-alist message
        (goto-char (point-max))
        (let* ((is-user (string= (chatgpt-user) .role))
               (role (format "<%s>:" .role))
               ;; XXX: If messages from user, don't try to render to markdown!
               ;; else, messages from OpenAI will most likely going to be
               ;; markdown so we render it!
               (content (if is-user .content
                          (chatgpt--render-markdown .content)))
               (chunk)
               (text-pointer chatgpt--text-pointer)
               (done))
          (add-face-text-property 0 (length role) 'chatgpt-user nil role)
          (with-temp-buffer  ; Get the chunk!
            ;; --- Standard output ---
            (insert role " " content)
            (insert "\n\n")
            (chatgpt--fill-region (point-min) (point-max))
            ;; ---
            (goto-char text-pointer)
            (unless (eobp)
              (forward-word 1)
              (setq chunk (buffer-substring text-pointer (point))
                    text-pointer (point)
                    done (eobp))))  ; update from temp buffer
          (setq chatgpt--text-pointer text-pointer)  ; update for local buffer
          (insert chunk)
          ;; Ready for next message!
          (when done
            (let* ((tokens-history (ht-get chatgpt-data 'tokens_history))
                   ;; XXX: Divided by 2 since the data is in pair!
                   (index (/ chatgpt--display-pointer 2))
                   (history (nth index tokens-history))
                   (prompt-tokens     (nth 0 history))
                   (completion-tokens (nth 1 history))
                   (total-tokens      (nth 2 history)))
              (chatgpt--create-tokens-overlay prompt-tokens
                                              completion-tokens
                                              total-tokens))
            (cl-incf chatgpt--display-pointer)
            (setq chatgpt--text-pointer 1)))))
    (if (< chatgpt--display-pointer (length chatgpt-chat-history))
        (chatgpt--start-text-timer)
      (spinner-stop chatgpt-spinner))))

(defun chatgpt--display-messages-at-once ()
  "If variable `chatgpt-animate-text' is nil, we display messages all at once."
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

(defun chatgpt--display-messages ()
  "Display all messages to latest response."
  (when (zerop chatgpt--display-pointer)  ; clear up the tip message
    (erase-buffer))
  (if chatgpt-animate-text
      (unless (timerp chatgpt-text-timer)  ; when is already running, don't interfere it
        (spinner-start chatgpt-spinner)
        (chatgpt--start-text-timer))
    (chatgpt--display-messages-at-once)))

(defun chatgpt-send-response (response)
  "Send RESPONSE to ChatGPT."
  (let ((user (chatgpt-user))
        (instance chatgpt-instance))
    (when (string-empty-p response)
      (user-error "[INFO] Invalid response or instruction: %s" response))
    (chatgpt--add-message user response)  ; add user's response
    (chatgpt-with-instance instance
      (let (chatgpt-animate-text)
        (chatgpt--display-messages)))        ; display it
    (setq chatgpt-requesting-p t)
    (spinner-start chatgpt-spinner)
    (openai-chat chatgpt-chat-history
                 (lambda (data)
                   (chatgpt-with-instance instance
                     (setq chatgpt-requesting-p nil)
                     (spinner-stop chatgpt-spinner)
                     (unless openai-error
                       (chatgpt--add-response-messages data)
                       (chatgpt--display-messages)
                       (chatgpt--add-tokens data))))
                 :model chatgpt-model
                 :max-tokens chatgpt-max-tokens
                 :temperature chatgpt-temperature
                 :top-p chatgpt-top-p
                 :user user)))

(defun chatgpt-type-response ()
  "Type response to OpenAI."
  (interactive)
  (cond
   (chatgpt-requesting-p
    (message "[BUSY] Waiting for OpanAI to response..."))
   (chatgpt-animating-p
    (message "[BUSY] Waiting for animation to finish..."))
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
        (insert chatgpt-window-prompt)
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
  (format " [Session] %s" (cdr chatgpt-input-instance)))

(defvar chatgpt-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "S-<return>") #'newline)
    (define-key map (kbd "RET") #'chatgpt-input-send)
    map)
  "Keymap for `chatgpt-input-mode'.")

(define-derived-mode chatgpt-input-mode fundamental-mode "ChatGPT Input"
  "Major mode for `chatgpt-input-mode'.

\\<chatgpt-input-mode-map>"
  (setq-local header-line-format `((:eval (chatgpt-input-header-line))))
  (add-hook 'post-command-hook #'chatgpt-input--post-command nil t))

;;
;;; Edit

(defconst chatgpt-edit-buffer-name "*ChatGPT-Edit*"
  "Buffer name to receive edit.")

(defvar chatgpt-edit-instance nil
  "The current instance; there is only one instance at a time.")

(defvar chatgpt-edit-chat-point-index nil
  "Store the chat point index.")

(defun chatgpt-edit-exit ()
  "Exit the edit."
  (interactive)
  (chatgpt--kill-buffer chatgpt-edit-buffer-name))

(defun chatgpt-edit-start (instance)
  "Start edit from INSTANCE."
  (chatgpt-edit-exit)
  (let ((dir (if (window-parameter nil 'window-side)
                 'bottom 'down))
        (buffer (get-buffer-create chatgpt-edit-buffer-name))
        (content (chatgpt-current-content))
        (index (chatgpt-current-chat-point-index)))
    ;; XXX: Without this, the highlighting at the end wouldn't work!?
    (chatgpt--with-no-redisplay
      (with-current-buffer buffer
        (chatgpt-edit-mode)
        (setq chatgpt-edit-instance instance
              chatgpt-edit-chat-point-index index)
        (erase-buffer)
        (insert content)))
    (pop-to-buffer buffer `((display-buffer-in-direction)
                            (direction . ,dir)
                            (dedicated . t)
                            (window-height . fit-window-to-buffer)))))

(defun chatgpt-edit-send ()
  "Send the edit."
  (interactive)
  (cond
   ((not (eq major-mode #'chatgpt-edit-mode)) )  ; do nothing
   (chatgpt-requesting-p
    (message "[BUSY] Waiting for OpanAI to response..."))
   ((region-active-p)
    (delete-region (region-beginning) (region-end)))
   (t
    (let ((response (buffer-string)))
      (chatgpt-with-instance chatgpt-edit-instance
        ;; Update display!
        (let* ((start (chatgpt-current-chat-point))
               (old-len (length response))
               (new-len (length (chatgpt-current-content)))
               (diff (- old-len new-len)))
          (goto-char start)
          (search-forward ">: " (line-end-position) t)  ; XXX: Improve this!
          (delete-region (point) (+ (point) new-len))
          (insert response)
          (chatgpt--fill-region start (point))
          (chatgpt-shift-chat-points start diff))

        ;; Update history!
        (setf (elt chatgpt-chat-history chatgpt-edit-chat-point-index)
              `((role    . ,(chatgpt-user))
                (content . ,(string-trim response)))))
      (erase-buffer))
    (when chatgpt-inhibit-input-afterward
      (chatgpt-edit-exit)))))

(defvar chatgpt-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "S-<return>") #'newline)
    (define-key map (kbd "RET") #'chatgpt-edit-send)
    map)
  "Keymap for `chatgpt-edit-mode'.")

(define-derived-mode chatgpt-edit-mode fundamental-mode "ChatGPT Edit"
  "Major mode for `chatgpt-edit-mode'.

\\<chatgpt-edit-mode-map>"
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
      (format "top-p: %s" chatgpt-top-p) "\n"
      (format "user: %s" (chatgpt-user))))
    ;; Register event to cancel lv window!
    (add-hook 'pre-command-hook #'chatgpt--pre-command-once)))

;;
;;; Entry

(defun chatgpt-mode--kill-buffer-hook ()
  "Kill buffer hook."
  (ht-clear chatgpt-data)
  (spinner-stop chatgpt-spinner)
  (chatgpt--cancel-text-timer)
  (let ((instance chatgpt-instances))
    (when (get-buffer chatgpt-input-buffer-name)
      (with-current-buffer chatgpt-input-buffer-name
        ;; kill input if it's the right session
        (when (equal instance chatgpt-instances)
          (kill-this-buffer))))))

(defun chatgpt-header-line ()
  "The display for header line."
  (format " %s[Session] %s  [History] %s  [User] %s"
          (if-let ((frame (spinner-print chatgpt-spinner)))
              (concat frame " ")
            "")
          (cdr chatgpt-instance)
          (length chatgpt-chat-history)
          (chatgpt-user)))

(defvar chatgpt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'chatgpt-type-response)
    map)
  "Keymap for `chatgpt-mode'.")

(defun chatgpt-mode-insert-tip ()
  "Insert tip to output buffer."
  (when (string-empty-p (buffer-string))
    (let ((inhibit-read-only t)
          (tip "Press <return> to start asking questions

`M-x chatgpt-info` will print out more information about the current session!
"))
      (add-face-text-property 0 (length tip) 'chatgpt-tip nil tip)
      (insert tip))))

(define-derived-mode chatgpt-mode fundamental-mode "ChatGPT"
  "Major mode for `chatgpt-mode'.

\\<chatgpt-mode-map>"
  (buffer-disable-undo)
  (setq-local buffer-read-only t)
  (font-lock-mode -1)
  (add-hook 'kill-buffer-hook #'chatgpt-mode--kill-buffer-hook nil t)
  (setq-local header-line-format `((:eval (chatgpt-header-line))))
  (setq chatgpt-spinner (spinner-create chatgpt-spinner-type t))
  (setq-local chatgpt-data (ht-create))
  (chatgpt-mode-insert-tip))

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
