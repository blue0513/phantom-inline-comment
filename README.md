# Phantom Inline Comment

Add visible inline-comments, which do not swear the source code as _phantom_.

## What's This

You can add inline-comments which can be visible but does not affect the source code, 
i.e. even if you write comments, no changes will be made in the source code.

This package is useful when leaving a little note on the source code for yourself.  
For example, when you write codes totally from scratch or when you read complex codes.

[![Image from Gyazo](https://i.gyazo.com/cefc091eb849b160901e221a23cc2885.gif)](https://gyazo.com/cefc091eb849b160901e221a23cc2885)

## Setup

`git clone` and edit your init.el as below.

```elisp
(require 'phantom-inline-comment)
(add-to-list 'load-path "YOUR PATH")
```

## Usage

### Add/Edit a phantom comment

+ `M-x phantom-inline-comment`

Add or Edit one phantom under a line of the cursor.  
After edit-buffer popups, you can write it.  
Then `C-c C-c` to apply changes and `C-g` to cancel.

### Delete a specific phantom comment

+ `M-x phantom-inline-comment-delete`

Delete one phantom under a line of the cursor.

### Delete All phantom comments

+ `M-x phantom-inline-comment-delete-all`

Delete all the phantoms.

### Show All phantom comments

+ `M-x phantom-inline-comment-show-all`

Show all the phantoms.  
Then select a file and `RET` to jump to the phantom comment.

### Fold/Unfold All the phantom comments

+ `M-x phantom-inline-comment-toggle-all`

Fold or Unfold all the phantoms.

## Current Limitations

Since this package uses `overlay`, the phantom inline comment is not persisted.  
After you quit emacs, the phantom inline comment you created will disappear.

To solve this problem, see Experimental section below.

### Experimental: Persistent Phantoms

**It's only experimental functions and not recommended.**

In init.el, you can add settings as bellow.

```elisp
;; Save phantom-inline-comments when quit Emacs
(phantom-inline-comment-auto-save-mode t)

;; Restore phantom-inline-comments when open files
(add-hook 'SOME-MODE-hook 'phantom-inline-comment-auto-restore-mode)
```

It allows you to save all the comments into the data-file
(default `~/.emacs.d/pahtnom-inline-comment` or `~/.phantom-inline-comment` if it exists), before you quit Emacs.  
And when you open the file you added comments, automatically the data-file will be loaded and the comments will be added again.

