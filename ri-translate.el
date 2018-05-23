;;; ri-translate.el --- Automated react-intl translation management in Emacs

;; Author: Dylan Kirby <dylan@mojotech.com>

;;; Code:

(require 'json)

(defun ri-translate--xah-hash-to-list (hash-table)
  "Return a list that represent the HASH-TABLE
  Each element is a list: (list key value).

  See also, emacs 24.4's new functions.
  (require 'subr-x)
  `hash-table-keys'
  `hash-table-values'

  http://ergoemacs.org/emacs/elisp_hash_table.html
  Version 2015-04-25"
  (let (result)
    (maphash
      (lambda (k v) (push (list k v) result))
      hash-table)
      result))

(defun ri-translate--list-to-hash (list)
  (let (new-hash)
    (setq new-hash (make-hash-table :test 'equal))
    (dolist (element list)
      (puthash (car element) (car (cdr element)) new-hash))
     new-hash))

(defun ri-translate--read-json-file (file-path)
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file file-path)))
    json))

(defun ri-translate--add-to-hash (hash key value)
  (let (new-hash as-list sorted-list)
    (setq new-hash hash)
    (puthash key value new-hash)
    (setq as-list (ri-translate--xah-hash-to-list new-hash))
    (setq sorted-list (seq-sort (lambda (a b) (string> (car a) (car b))) as-list))
    (json-encode (ri-translate--list-to-hash sorted-list))))

(defun ri-translate--add-to-json-file (file-path key value)
  (let*
      ((existing-translations (ri-translate--read-json-file file-path))
       (updated-translations
         (ri-translate--add-to-hash existing-translations key value)))
    (with-temp-file file-path (insert updated-translations))
    (find-file file-path)
    (prettier-js)
    (save-buffer)
    (kill-buffer)))

(defun ri-translate--add-translation (locale key value)
  (ri-translate--add-to-json-file
    (ri-translate--translation-file-path locale) key value))

(defun ri-translate--translation-file-path (locale)
  (let ((project-dir "~/git/MY-PROJECT/"))
    (format "%sPATH/TO/TRANSLATIONS/%s.json" project-dir locale)))

(defun ri-translate--region-string ()
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun ri-translate--translated-string-at-point ()
  (spacemacs/set-google-translate-languages "en" "nl")
  (google-translate-at-point)
  (switch-to-buffer "*Google Translate*")
  (spacemacs/toggle-maximize-buffer)
  (let* ((translated-block (nth 2 (split-string (buffer-string) "\n\n")))
         (translated-string
           (replace-regexp-in-string
             " $"
             ""
             (replace-regexp-in-string "\n" " " translated-block))))
    (kill-buffer)
    translated-string))

(defun ri-translate--formatted-message (key use-render-prop)
  (if use-render-prop
    (format "<FormattedMessage id='%s'>{msg => }</FormattedMessage>" key)
    (format "<FormattedMessage id='%s' />" key)))

(defun ri-translate--insert-formatted-message (key use-render-prop)
  (delete-region (region-beginning) (region-end))
  (insert (ri-translate--formatted-message key use-render-prop))
  (when use-render-prop (search-backward "<")))

(defun ri-translate--add-english-entry (key)
  (ri-translate--add-translation "en-US" key (ri-translate--region-string)))

(defun ri-translate--add-dutch-entry (key)
  (ri-translate--add-translation
    "nl-NL.google"
    key
    (ri-translate--translated-string-at-point)))

(defun ri-translate ()
  (interactive)
  (let ((key (read-string "Specify a key: "))
        (use-render-prop (y-or-n-p "Use render prop? ")))
    (message "Adding translation...")
    (ri-translate--add-english-entry key)
    (ri-translate--add-dutch-entry key)
    (ri-translate--insert-formatted-message key use-render-prop)
    (message "Added translation")))

;;; ri-translate.el ends here
