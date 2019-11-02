# flask-mode

A minor emacs mode to work with Flask projects. Emacs 25.1 or higher is required
to use this minor mode.

## Features

- Starting and killing flask server from emacs
- Running flask tests from emacs and seeing results
- Running flask cli commands from emacs

## Configuration

```lisp
(add-to-load-path "/path/to/flask-mode.el")
(require 'flask-mode)
```
Now you can enter `flask-mode` or if you want to start this minor mode every
time the python mode is started, add this to your configuration
```lisp
(add-hook 'python-mode-hook 'flask-mode)
```
There are two variables, that need to be set. `flask-app` this is the name of
the file which `flask run` should execute. `flask-mode` tries to set this
variable automatically upon start. Read the documentation for more. Also
`flask-default-directory` needs to be set to the root directory of the project.
`flask-mode` tries to automatically set this variable. Basically it tries to
find a .git directory. See the documentation of this variable for more
information. You could also set this variables in your configuration file with
for example
```lisp
(setq flask-app "awesomeApp.py"
      flask-default-directory "~/awesomeApp")
```.
See `customize-group flask` for more variables.

### Enabling and setting up flask-mode project wide
. Is it also possible to enable this mode project wide and set the variables
with a `.dir-locals.el` file. For that, create a file named `.dir-locals.el` in
the root directory of the project and put the following into it
```lisp
((nil . ((flask-minor-mode . t)
         (flask-default-directory . "/path/to/project")
         (flask-app . "awesomeApp.py"))))
```.
Replace the variable values with values that are suited to your project.

To add a custom cli command put something like this in your configuration
```lisp
(defun my-cli-command ()
    (interactive)
    (flask-run-command "myCommand"))
```.
Now you can `M-x` and run `my-cli-command`.

## Usage
Start a flask server with `flask-run-server`. If the server is already running,
it gets restarted. You can kill the server at any time with `flask-kill-server`. Tests
can be run with `flask-run-tests`.

### Default key bindings
| Binding      | Command name        |
| :--          | :--                 |
| `C-c , f r`  | `flask-run-server`  |
| `C-c , f k`  | `flask-kill-server` |
| `C-c , f t`  | `flask-run-tests`   |


## Development
This minor mode is at the moment really basic. Contributing would be welcome.
