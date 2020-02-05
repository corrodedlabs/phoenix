;;; package --- emacs helpers

;;; Commentary:

;;; Code:

(defun replace-text-in-buffer (what with)
  "Replace WHAT WITH in the buffer."
  (goto-char (point-min))
  (while (search-forward what nil t)
    (replace-match with "FIXEDCASE" "LITERAL"))
  (goto-char (point-min)))

(defun convert-c-enum-format-to-scheme (begin end)
  "Convert c style enum to scheme conventions BEGIN and END are region positions."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (message "region not selected")))
  (downcase-region begin end)
  (save-excursion (save-restriction (narrow-to-region begin end)
				    (replace-text-in-buffer "_" "-")
				    (replace-text-in-buffer "=" "")
				    (replace-text-in-buffer "," ")")
				    (replace-text-in-buffer "vk-" "(vk-"))))

(provide 'helpers)
;;; helpers.el ends here
