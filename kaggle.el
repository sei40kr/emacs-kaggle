;;; kaggle.el --- Kaggle client

;; Copyright (c) 2021 Seong Yong-ju All rights reserved.
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (dash "1.5") (parse-csv "0.3"))
;; Keywords: tools
;; URL: https://github.com/sei40kr/emacs-kaggle

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Kaggle client for Emacs.

;;; Code:

(require 'dash)
(require 'parse-csv)

(defvar kaggle--competition nil)

(defun kaggle--generate-buffer-name (command-args)
  (generate-new-buffer-name
   (format "*kaggle %s*" (string-join command-args " "))))

(defun kaggle--run-kaggle (&rest args)
  (let* ((command (string-join `("kaggle" ,@args) " ")))
    (shell-command-to-string command)))

(defun kaggle--run-kaggle-async (&rest args)
  (let* ((command (string-join `("kaggle" ,@args) " "))
         (output-buffer (kaggle--generate-buffer-name args)))
    (async-shell-command command output-buffer)))

(defun kaggle--warning-line-p (line)
  (string-prefix-p "Warning:" line))

(defun kaggle--competitions-leaderboard-parse (index line)
  (let* ((data (parse-csv->list line))
         (id (car data))
         (no (+ index 1))
         (team-name (nth 1 data))
         (submission-date (nth 2 data))
         (score (nth 3 data)))
    `(,id ,(vector (int-to-string no) team-name submission-date score))))

(defun kaggle--competitions-leaderboard-entries (competition)
  (let* ((data (kaggle--run-kaggle "c" "leaderboard" "-svq" competition))
         (lines (->> (split-string data "\n" t)
                     (-map #'string-trim)
                     (-drop-while #'kaggle--warning-line-p))))
    (if (not (string-equal (car lines) "404 - Not Found"))
        (->> lines
             (-drop 1)
             (-map-indexed #'kaggle--competitions-leaderboard-parse))
      (error "Competition not found: %s" competition))))

(defun kaggle--competitions-leaderboard-refresh ()
  (setq tabulated-list-entries
        (kaggle--competitions-leaderboard-entries kaggle--competition)))

(define-derived-mode kaggle-competitions-leaderboard-mode tabulated-list-mode
  "Leaderboard"
  "Major mode for browsing the top of a Kaggle leaderboard."
  (setq tabulated-list-format [("#" 2 t . (:right-align t
                                           :pad-right 2))
                               ("Team Name" 22 t)
                               ("Submission Date" 20 t)
                               ("Score" 8 t)]
        tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook
            'kaggle--competitions-leaderboard-refresh nil t)
  (tablist-minor-mode)
  (tabulated-list-init-header))

;;;###autoload
(defun kaggle-competitions-download (competition path)
  (interactive "sCompetition URL suffix: \nDDownload directory: ")
  (kaggle--run-kaggle-async "c" "download" "-p" path competition))

;;;###autoload
(defun kaggle-competitions-leaderboard (competition)
  "Display a list of the top of the leaderboard of COMPETITION."
  (interactive "sCompetition URL suffix: ")
  (pop-to-buffer "*kaggle-competitions-leaderboard*")
  (kaggle-competitions-leaderboard-mode)
  (setq-local kaggle--competition competition)
  (tablist-revert))

(provide 'kaggle)

;;; kaggle.el ends here
