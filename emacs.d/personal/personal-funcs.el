(defun phi-new-report-buffer ()
  "Create a new buffer not associated with a file."
  (interactive)
  (switch-to-buffer (generate-new-buffer "new-report.txt"))
  (text-mode)
  (visual-line-mode)
  (insert "\n-----\n\n"))


(defun phi-append-to-reports ()
  "Appends the marked text to reports.txt"
  (interactive)
  (mark-whole-buffer)
  (write-region (point-min) (point-max) "~/Documents/reports.txt" t)
  (kill-ring-save (point-min) (point-max))
  (end-of-buffer))
