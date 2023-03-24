(package "chatgpt"
         "0.1.0"
         "Use ChatGPT inside Emacs")

(website-url "https://github.com/emacs-openai/chatgpt")
(keywords "comm" "openai")

(package-file "chatgpt.el")
(files "chatgpt-*.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'jcs-elpa)
(source 'gnu)
(source 'melpa)

(depends-on "emacs" "27.1")
(depends-on "openai")
(depends-on "lv")
(depends-on "ht")
(depends-on "markdown-mode")
(depends-on "spinner")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
