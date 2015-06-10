#Personal Emacs setup#

```
git clone git@github.com:cristianmtr/emacs.git ~/.emacs.d

```

## Notes on Windows setup ##

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
