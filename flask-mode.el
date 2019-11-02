;;; flask-mode.el --- Minor mode for working with Flask projects -*- lexical-binding: t; -*-


;; Copyright (C) 2019 Felix Weilbach <felix.weilbach@t-online.de>

;; Author: Felix Weilbach <felix.weilbach@t-online.de>
;; Maintainer: Felix Weilbach <felix.weilbach@t-online.de>
;; Created: 2019-11-02
;; Keywords: processes python flask
;; URL: https://gitlab.com/flexw/flask-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Commentary:

;; Minor mode for working with Flask projects.

;;; Code:

(defgroup flask nil
  "Flaskyfication for Emacs"
  :group 'programming
  :prefix "flask-")

(defcustom flask-default-directory "~"
  "The root directory of the application to run.
This should be the directory from which you normally would execute flask run.
`flask-mode' tries to set this variable automatically when starting."
  :group 'flask
  :type 'string)

(defcustom flask-server-debug t
  "Whether the flask server should be run in debug mode or not."
  :group 'flask
  :type 'bool)

(defcustom flask-app "app.py"
  "The name of the application to start.
This gets passed to FLASK_APP. `flask-mode' tries to set this variable
automatically when starting."
  :group 'flask
  :type 'string)

(defcustom flask-executable "flask"
  "The name of the flask executable."
  :group 'flask
  :type 'string)

(defcustom flask-server-host "localhost"
  "The host on which flask should open the server."
  :group 'flask
  :type 'string)

(defcustom flask-server-port "5000"
  "The port on which the flask server should be run."
  :group 'flask
  :type 'string)

(defcustom flask-run-command-line-args nil
  "Additional command line arguments that get passed to to flask run.
--host and --port should be set in `flask-server-host' and `flask-server-port'."
  :group 'flask
  :type 'string)

(defvar flask-run-process nil)

;;;###autoload
(defun flask-run-server ()
  "Start the flask server. If already running, the server gets restarted.
The output of the server will be piped in a buffer named *flask-run*."
  (interactive)
    (if (eq flask-run-process nil)
        (progn
          (message "Starting flask server")
          (setq flask-run-process (let ((process-environment process-environment)
                                        (default-directory flask-default-directory))
                                    (progn
                                      (setenv "FLASK_APP" flask-app)
                                      (when flask-server-debug
                                        (setenv "FLASK_DEBUG" "1"))
                                      (if (eq flask-run-command-line-args nil)
                                          (make-process
                                           :name "flask-run"
                                           :buffer "*flask-run*"
                                           :command `(,flask-executable
                                                      "run"
                                                      "--host" ,flask-server-host
                                                      "--port" ,flask-server-port))
                                        (make-process
                                         :name "flask-run"
                                         :buffer "*flask-run*"
                                         :command `(,flask-executable
                                                    "run"
                                                    "--host" ,flask-server-host
                                                    "--port" ,flask-server-port
                                                    ,flask-run-command-line-args)))))))
      (progn
        (message "flask server already running. Restarting ...")
        (flask-kill-server)
        (flask-run-server))))


(defun flask-run-command (command)
  "Run flask with the given COMMAND.
The output will be piped in a buffer named *flask-COMMAND*."
  (let ((process-environment process-environment)
        (default-directory flask-default-directory)
        (buffer-name (concat "*flask-" command "*")))
    (progn
      (setenv "FLASK_APP" flask-app)
      (make-process
       :name (concat "flask-" command)
       :buffer buffer-name
       :command `("flask" ,command))
      (display-buffer-in-side-window (get-buffer buffer-name)
                                     '((side . bottom) (slot . 0))))))


;;;###autoload
(defun flask-run-tests ()
  "Run flask test command.
The output will be piped in a buffer named *flask-test*."
  (interactive)
  (flask-run-command "test"))

;;;###autoload
(defun flask-kill-server ()
  "Kill the flask server."
  (interactive)
  (if flask-run-process
      (progn
        (message "Killing flask server")
        (delete-process flask-run-process)
        (setq flask-run-process nil))
    (message "flask server is not running")))

(defun flask-search-project-root (directory)
  "Search for the root directory of the project.
Start in DIRECTORY and going down to /. If a file or directory named .git is
found, `flask-default-directory' gets set."
  (if (member ".git" (directory-files directory))
      (setq flask-default-directory directory)
    (progn
      (if (string= "/" directory)
          nil
        (flask-search-project-root (flask--go-back-one-dir directory))))))

(defun flask--go-back-one-dir (directory)
  "Go back one directory in DIRECTORY."
  (cond ((string= directory "/") "/")
        ((eq (aref directory (- (length directory) 1)) 47)
         (substring directory 0 (- (length directory) 1)))
        (t (flask--go-back-one-dir (substring directory 0 (- (length directory) 1))))))

(defun flask-try-set-flask-app (directory)
  "Try to set `flask-app' from DIRECTORY.
For example it the DIRECTORY is /foo/bar, this function checks if there exists
a file named bar.py in DIRECTORY. If it exists, `flask-app' will be set to this
 file."
  (let ((project-dir (car (last (split-string directory "/" t)))))
    (when (not (eq project-dir nil))
      (let ((filename (concat project-dir ".py")))
        (when (file-exists-p (concat directory "/" filename))
          (setq flask-app filename))))))

(defvar flask-minor-mode-map
  (let ((map (make-keymap)))
    map))

(defun flask-key(binding function)
  "Bind FUNCTION to BINDING in `flask-minor-mode-map'."
  (define-key flask-minor-mode-map binding function))

(flask-key (kbd "C-c , f r") 'flask-run-server)
(flask-key (kbd "C-c , f t") 'flask-run-tests)
(flask-key (kbd "C-c , f k") 'flask-kill-server)

(define-minor-mode flask-minor-mode
  "Flask minor mode"
  :init-value nil
  :lighter " Flask"
  :keymap flask-minor-mode-map)

(defun flask-mode ()
  "Initialize Flask mode."
  (interactive)
  (flask-minor-mode t)
  (run-hooks 'flask-minor-mode-hook)
  (flask-search-project-root (substring default-directory 0 (- (length default-directory) 1)))
  (flask-try-set-flask-app flask-default-directory))

(provide 'flask-mode)

;;; flask-mode.el ends here
