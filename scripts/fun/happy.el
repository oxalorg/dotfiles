(defun animate-lambda-new-year (&optional year)
  ;; Make a suitable buffer to  ()
  (switch-to-buffer (get-buffer-create
			 (concat "Happy-New-Year" "-2020")))
  (erase-buffer)
  ;; Display the empty buffer.
  (sit-for 0)

  (animate-string "           __         " 2 10)
  (animate-string "           \\ \\        " 3 10)
  (animate-string "            \\ \\       " 4 10)
  (animate-string "             \\ \\      " 5 10)
  (animate-string "              > \\     " 6 10)
  (animate-string "             / ^ \\    " 7 10)
  (animate-string "            / / \\ \\   " 8 10)
  (animate-string "           /_/   \\_\\  " 9 10)
  (animate-string "                         " 10 10)
  (animate-string "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" 11 10)
  (animate-string "|       Happy New Year 2021       |" 12 10)
  (animate-string "|  - by the Lambda Island Team    |" 13 10)
  (animate-string "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" 14 10))







(animate-lambda-new-year "2021")
