# Phantom Inline Comment

Add visible inline-comments, which do not swear the source code as _phantom_.

## What's This

You can add inline-comments which can be visible but does not affect the source code, 
i.e. even if you write comments, no changes will be made in the source code.

This package is useful when leaving a little note on the source code for yourself.

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

## Current Limitations

Since this package uses `overlay`, the phantom inline comment is not persisted.  
After you quit emacs, the phantom inline comment you created will disappear.
