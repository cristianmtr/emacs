# Personal Emacs setup

```
git clone git@github.com:cristianmtr/emacs.git ~/.emacs.d

```

## Highlights 

- helm mode for M-x;
- company-mode for completions: C-/ ;
	- use "begin-backend" - "spell" to get dictionary-based word-completion;
- dabbrev with C-SPC;
- scroll down with C-M-n and up with C-M-p;
- undo-tree: editing history as a tree, with branches, rather than a stack: C-x u;
- file explorer with nav-mode: F8;
- markdown-mode editor;
- Control-Tab and Shift-Tab to jump between buffer windows;
- put full path to file in clipboard with M-c;
- comment-uncommnet region and line with C-';
- insert date and date-time with C-c . and C-c ,
- move to specific window with Shift-*direction key*;
- save current buffers and window setup with: C-x Shift-s to save current window layout and C-x Shift-f to return to saved window layout ;
- easier keybindings for copy (C-c), paste (C-v), cut (C-x);
- jump to beginning of word with ace-jump-mode: C-q then first letter of word;
- includes markdown previewer:
  - open markdown-editor\index.html in your browser and paste your markdown source on the left;
  - full credits go to jbt: [source here](https://github.com/jbt/markdown-editor)


## Notes on Windows setup

- checkout to new branch and make any changes there;
- maybe use another dir for default-directory
```emacs
(setq default-directory "C:/stuff" )
```
- make sure your Shift-Tab and Control-Tab keys are mapped correctly;
- use the .reg file and the vbs script in the setup/ dir to add emacs to the context menu;
- in WinSCP, you can add the following to the editor command:
```
path\to\emacs\bin\emacsclientw.exe --alternate-editor="path\to\emacs\bin\emacs.exe" -c !.!
```

## TODO

- have M-- and M-= insert 4 spaces (or delete 4 spaces, respectively) either on the region selected or at the beggining of the curr line;
- export from markdown-mode to html file in markdown-editor dir, open browser, and have some js on the index.html file to load that generated file into the correct div;
