1. C-u M-x apropos
   When apropos is invoked with a prefix argument, it not only reports Emacs functions and variables that match the search pattern, it also reports any existing keybindings for each command in the list. (This isn't the default because finding the keybindings can be slow.)

2. Another feature of Lisp Interaction mode is its ability to complete a partially typed Lisp symbol when you press M-TAB.

3. The prefix argument is a pseudo-argument that is automatically available for each Emacs command.

When you use a command, its behavior might differ, depending on the value of the prefix argument. You can use ‘C-u’ to set the prefix argument value to use, and thus affect the command’s behavior. ‘C-u’ is bound to command ‘universal-argument’, the universal argument command.

When you define a command, you can have it examine the current prefix argument and act differently, depending on its value.

Raw vs Numerical Prefix Arg

The prefix argument can have several kinds of value. It can be a list with a single integer element, ‘nil’, the symbol `-’, or an integer. The prefix argument is also called the raw prefix argument.

Whatever the raw value, the prefix argument is also said to have a numeric value that is derived from its raw value. This is also called the numeric prefix argument. The numeric prefix argument is 1 when the raw prefix argument is ‘nil’. It is -1 when the raw prefix argument is `-’. It is the integer list element when the raw prefix argument is a cons. Otherwise, it is the same integer as the raw prefix value.

If the user calls a command without using ‘C-u’, ‘C--’, ‘M--’, or `C-N’, where N is a positive or negative integer, then the raw prefix argument is ‘nil’, and we also speak of there being no prefix argument.

For more information, see the EmacsLisp manual, node Prefix Command Arguments. That node also describes which prefix argument is passed to a command when user calls the command in different ways (e.g. ‘C-u’, ‘C-u 3’).
Overloading Commands Using the Prefix Argument

When you define a command, you might want it to produce two or more different results, depending on what the user wants. In a sense, this is overloading the command name, giving the command two or more alternative behaviors.

A conventional way to do this is to make the behavior depend on the prefix argument. You could, for example, write a command that normally kills a buffer only if it has no unsaved changes, but kills a buffer even if it has unsaved changes when the user provides a ‘C-u’ prefix.

There are various ways that a command definition can examine the raw and numeric prefix arguments, but a typical way is to use ‘P’ for raw or ‘p’ for numeric in the command’s ‘interactive’ spec.

This command definition, for instance, binds the argument ‘arg’ to the raw prefix argument value. It prints the argument value.

    (defun foo (arg)
      "Print the current raw prefix argument value."
      (interactive "P")
      (message "raw prefix arg is %S" arg))

Try it:

‘M-x foo’ calls ‘foo’ with ‘arg’ bound to nil

‘C-u M-x foo’ calls ‘foo’ with ‘arg’ bound to (4)

‘C-u C-u M-x foo’ calls ‘foo’ with ‘arg’ bound to (16)

‘C-u 3 M-x foo’ calls ‘foo’ with ‘arg’ bound to 3

‘M-- M-x foo’ calls ‘foo’ with ‘arg’ bound to -
Multiple-Digit Numeric Prefix Argument

You can use ‘C-u’ to terminate a prefix argument that has more than one digit: ‘C-u 1 0 0 C-u’ sets the prefix argument to 100. See also InfiniteArgument.

4. A hook is an ordinary Lisp variable whose value is a list of functions that get executed under specific conditions. For instance, the variable write-file-hooks is a list of functions that Emacs executes whenever a buffer is saved, and post-command-hook is a list of functions to run after every interactive command. The hook that interests us most for this example is find-file-hooks, which Emacs runs every time a new file is visited. (There are many more hooks, some of which we'll be looking at later in the book. To discover what hooks are available try M-x apropos RET hook RET.)

(defun read-only-if-symlink ()
  (if (file-symlink-p buffer-file-name)
      (progn
	(setq buffer-read-only t)
	(message "File is a symlink"))))

5. To achieve this, we'll use advice. A piece of advice attached to a Lisp function is code that gets executed before or after the function each time the function is invoked. Before advice can affect the arguments before they're passed to the advised function. After advice can affect the return value that comes out of the advised function. Advice is a little bit like hook variables, but whereas Emacs defines only a few dozen hook variables for very particular circumstances, you get to choose which functions get "advised".

6. In addition to having a variable value and/or a function definition, every Emacs Lisp symbol may also have associated with it a property list. A property list is a mapping from names to values. Each name is yet another Lisp symbol, while each value may be any Lisp expression.

7. User Options and Docstrings
First we'll define the variables.
(defvar insert-time-format "%X"
  "*Format for \\[insert-time] (c.f. format-time-string').")
(defvar insert-date-format "%x"
  "*Format for \\[insert-date] (c.f. 'format-time-string').")
There are two new things to note about these docstrings.
  · First, each begins with an asterisk (*). A leading asterisk has special meaning in defvar docstrings. It means that the variable in question is a user option. A user option is just like any other Lisp variable except that it's treated specially in two cases:
— User options can be set interactively using set-variable, which prompts the user for a variable name (with completion of partially typed names) and a value. In some cases, the value can be entered in an intuitive way without having to dress it up in Lisp syntax; e.g., strings can be entered without their surrounding double-quotes.
To set variables interactively when they aren't user options, you must do something like
M-: (setq variable value) RET
(using Lisp syntax for value).
— User options, but not other variables, can be edited en masse using the option-editing mode available as M-x edit-options RET.

· The second new thing about these docstrings is that each contains the special construct \
[command]. (Yes, it's \[. . . ], but since it's written inside a Lisp string, the backslash has to
be doubled: \\[. . . ].) This syntax is magic. When the docstring is displayed to the user-such as when the user uses apropos or describe-variable—\ [command] is replaced with a representation of a keybinding that invokes command. For example, if C-x t invokes insert-time, then the docstring "*Format for \\[insert-time] (c.f. 'format-time-string')."
is displayed as *Format for C-x t (c.f. 'format-time-string').
If there is no keybinding for insert-time, then M-x insert-time is used. If there are two or more keybindings for insert-time, Emacs chooses one.
Suppose you want the string \ [insert-time] to appear literally in a docstring. How could you prevent its keybinding being substituted? For this purpose there is a special escape sequence: \=. When \= precedes \ [. . . , the magic replacement of \ [. . . ] doesn't happen. Of course, Lisp string syntax dictates that this be written as " . . . \\=\\ [. . . . . . ".
\= is also useful for escaping the asterisk at the beginning of a defvar docstring, if you don't want the variable to be a user option but you absolutely must have a docstring that begins with an asterisk.
All variables that are shared between two or more functions should be declared with defvar.
Which of those should be user options? A rule of thumb is that if the variable directly controls a user-visible feature that a user might want to change, and if setting that variable is straightforward (i.e., no complex data structures or specially coded values), then it should be a user option.

8. By now you know that interactive turns a function into a command and specifies how to obtain the function's arguments when invoked interactively. But we haven't seen * in the argument of interactive before, and besides, these functions take no arguments, so why does interactive have one? The asterisk, when it is the first character in an interactive argument, means "abort this function if the current buffer is read-only." It is better to detect a read-only buffer before a function begins its work than to let it get halfway through then die from a "Buffer is read-only" error.
