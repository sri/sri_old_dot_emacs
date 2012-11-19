My old GNU Emacs init files.

I used Emacs heavily from 1998 - 2004 or so. But I haven't used it much since then.
But every now and then I get the urge to go back and give it a go.

Some interesting things you can find here:
 * My ~/.emacs loaded setup.el which then loaded all the other files
 * setup.el loaded the .elc files (.el files were first byte-compiled if necessary)
 * It was nice to spread the customizations across multiple files
 * I modified the default key bindings quite a bit. I defined a bunch of
   prefix keys (examples: C-l, C-x C-f, C-x C-b, C-j) with which I was able
   to organize the key-bindings. For example, C-x C-f handled file operations.
   So I would type C-x C-f v to view a file. In retrospect, this was probably a
   very bad idea!
 * I swap Shift & Control: http://defcraft.blogspot.com/2007/07/my-dot-emacs-shift-to-control.html. So uppercase A would be beginning-of-line and Ctrl-a would insert the letter A.

Released under the MIT License.
