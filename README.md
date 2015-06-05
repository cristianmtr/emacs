#Personal Emacs setup#

```
git clone git@github.com:cristianmtr/emacs.git ~/.emacs.d

```

## Notes on Windows setup ##

- use the .reg file and the vbs script in the setup/ dir to add emacs to the context menu;
- in WinSCP, you can add the following to the editor command:
```
path\to\emacs\bin\emacsclientw.exe --alternate-editor="path\to\emacs\bin\emacs.exe" -c !.!
```
