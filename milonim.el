(use-package org-web-tools :ensure t)

(defvar-local safot-current-language nil)

(defvar milonim nil)
(defvar milonim-alist nil)
(defvar milonim-last-dictionary nil)

(defmacro defmilon (name &rest attrs)
  `(setf (alist-get ',name milonim-alist) (list ,@attrs)))

(setq milonim-alist nil)
(defmilon arabic
    :name "arabic"
    :query "http://www.perseus.tufts.edu/hopper/morph?l=%s&la=ar"
    :dom-id "main_col"
    ;; :buffer-name "*arabic perseus dictionary*"
    :file-name "~/.config/org/milonim/arabic.org"
    :keymap "arabic-qwerty-everywhere")

(defmilon latin
    :name "latin"
    :query "http://www.perseus.tufts.edu/hopper/morph?l=%s&la=la"
    :dom-id "main_col"
    :file-name "~/.config/org/milonim/latin.org"
    :prolog (lambda (word)
              (insert (format "* %s\n" word)))
    :post (lambda ()
            (goto-char 0)
            (flush-lines "statistics")
            (flush-lines "lexicon")
            (flush-lines "\\\\")
            (flush-lines "View this entry")))

(defmilon duden
    :name "duden german dictionary"
    :query "https://www.duden.de/rechtschreibung/%s"
    :dom-els '((class . "tabloid")
               (role . "article"))
    :file-name "~/.config/org/milonim/german.org"
    :post (lambda ()
            (goto-char 0)
            (flush-lines "^$")
            (flush-lines "Anzeige")
            (flush-lines "Als Quelle verwenden")
            (flush-lines "[[/hilfe/rechtschreibung][ⓘ]]")
            (flush-lines "facebook")
            (flush-lines "Dieses Wort kopieren")
            (flush-lines "Melden Sie sich an")
            (flush-lines "svg")
            (flush-lines "W.rterbuch")
            (flush-lines "<<Duden.de")
            (flush-lines "alle Informationen")))

(defmilon english
    :name "english"
    :query "https://www.merriam-webster.com/dictionary/%s"
    ;; :buffer-name "*webster dictionary*"
    :file-name "~/.config/org/milonim/english.org"
    :dom-id "dictionary-entry-1"
    :post (lambda ()
            (goto-char (point-min))
            (dolist (r '("svg"
                         "^$"
                         "Entry"
                         "Save to list"
                         "[0-9]+ of [0-9]+"
                         "kids dictionary words"
                         "<<"))
              (flush-lines r))))

(defun ale/milon-cleanup ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (dolist (rx '((| ".svg" ".jpg")
                  (and "Add to")
                  (and "\\\\")
                  (and "https")
                  (and "My favorites")
                  (and "Would you like to")
                  (and  bol eol)
                  (and bol (one-or-more (any "*" " ")) eol)
                  (and "tables")
                  (and "באנגלית?" eol)))
      (flush-lines (rx-to-string rx)))))

(defmilon english->hebrew
    :name "morfix english ⇒ hebrew"
    :query "https://www.morfix.co.il/en/%s"
    ;; :buffer-name "*morfix dictionary*"
    :file-name "~/.config/org/milonim/hebrew.org"
    ;; :dom-class "Translation_content_enTohe"
    :dom-id "translationContent"
    :post #'ale/milon-cleanup)

(defmilon hebrew->english
    :name "morfix hebrew ⇒ English"
    :query "https://www.morfix.co.il/%s"
    ;; :buffer-name "*morfix dictionary*"
    :file-name "~/.config/org/milonim/hebrew.org"
    :keymap "hebrew-qwerty-everywhere"
    ;; :dom-class "Translation_content_heToen"
    :dom-id "translationContent"
    :post #'ale/milon-cleanup)

(defmilon spanish
    :name "Spanish RAE"
    :query "https://dle.rae.es/?w=%s"
    :file-name "~/.config/org/milonim/spanish.org"
    :prolog (lambda (word)
              (insert (format "* %s\n" word)))
    :dom-els '((id . "resultados"))
    :post (lambda ()
            (goto-char 0)
            (flush-lines "derechos reservados")
            (flush-lines "svg")
            (flush-lines "whatsapp")
            (flush-lines "<<.*>>")
            (flush-lines "https:")))


(defun milon--get-query-url (query word)
  (format query word))


(defun milon-query-dictionary (dict-plist word)
  (cl-destructuring-bind (&key
                          name
                          query
                          file-name
                          (buffer-name
                           (save-window-excursion
                             (when file-name
                               (find-file file-name))))
                          &allow-other-keys)
      dict-plist
    (let* ((org-web-tools-pandoc-sleep-time 2.0)
           (-output-buffer (get-buffer-create buffer-name)))
      (url-retrieve (milon--get-query-url query word)
                    ;; callback to be done
                    (lambda (status word old-buffer-name output-buffer dict-plist)
                      (cl-destructuring-bind (&key
                                              dom-id
                                              dom-tag
                                              dom-els
                                              post
                                              prolog
                                              dom-class
                                              &allow-other-keys)
                          dict-plist
                        (let ((html (libxml-parse-html-region (point-min)
                                                              (point-max))))
                          (with-current-buffer output-buffer
                            (goto-char 0)
                            (when prolog
                              (funcall prolog word))
                            (insert
                             (org-web-tools--html-to-org-with-pandoc
                              (org-web-tools--dom-to-html
                               ;; get the id or class we want
                               (cond
                                 (dom-els
                                  (let ((node html))
                                    (cl-loop for el in dom-els
                                          do (setq node
                                                   (cl-case (car el)
                                                     (tag-name
                                                      (dom-by-tag node
                                                                  (cdr el)))
                                                     (t (dom-elements node
                                                                      (car el)
                                                                      (cdr el))))))
                                    node))
                                 (dom-tag (dom-by-tag html
                                                      dom-tag))
                                 (dom-id (dom-by-id html
                                                    dom-id))
                                 (dom-class (dom-by-class html
                                                          dom-class))
                                 (t html)))))
                            (save-excursion
                              ;; call post function if given
                              (when post
                                (funcall post)))
                            (unless (eq 'org-mode major-mode)
                              (org-mode))
                            (org-show-entry)
                            (goto-char (point-min))))
                        (unless (string= old-buffer-name
                                         (buffer-name output-buffer))
                          (switch-to-buffer-other-window
                           (buffer-name output-buffer))
                          (goto-char (point-min)))))
                    ;;
                    ;; arguments for the lambda expression
                    ;;
                    (list word
                          (buffer-name (current-buffer))
                          -output-buffer dict-plist)))))


(defun milon--read-dictionary-name (&optional arg)
  (when (equal arg '(4))
    (setq milonim-last-dictionary nil))
  (if milonim-last-dictionary
      milonim-last-dictionary
    (setq milonim-last-dictionary
          (intern (completing-read "Dictionary: "
                                   (mapcar
                                    #'car
                                    milonim-alist))))))

(defun milon--read-dictionary (&optional arg)
  (alist-get (milon--read-dictionary-name arg)
             milonim-alist))

(defun milon--read-word (&optional arg)
  (let* ((-dict (milon--read-dictionary current-prefix-arg))
         (keymap (cl-getf -dict :keymap))
         (word (let ((word-atp (word-at-point)))
                 (with-temp-buffer
                   (when keymap (set-input-method keymap))
                   (read-string "Word: " word-atp nil nil t)))))
    (values word -dict)))

(defun milon (word &optional dict universal)
  ;; if C-u then set the last dictionary to null
  (interactive
   (concatenate 'list
                (milon--read-word current-prefix-arg)
                (list current-prefix-arg)))
  (milon-query-dictionary dict word))

(defun milon-at-point (&optional dict universal)
  ;; if C-u then set the last dictionary to null
  (interactive
   (list (milon--read-dictionary current-prefix-arg)
         current-prefix-arg))
  (milon-query-dictionary dict (word-at-point)))


(defun milon-browse (&optional dict universal)
  ;; if C-u then set the last dictionary to null
  (interactive
   (list (milon--read-dictionary current-prefix-arg)
         current-prefix-arg))
  (milon-query-dictionary dict (word-at-point)))
