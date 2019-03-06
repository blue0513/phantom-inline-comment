# Phantom Inline Comment

Add visible inline-comments but they don't swear the code as _phantom_.

## What's This

You can add inline-comments which can be visible but does not affect the source code, 
i.e. even if you write comments, no changes will be made in the source code.

This package is useful when leaving a little note on the source code for yourself.

[![Image from Gyazo](https://i.gyazo.com/de667572d7f1e5d5acda58c675bc08fc.gif)](https://gyazo.com/de667572d7f1e5d5acda58c675bc08fc)

## Setup

`git clone` and edit your init.el as below.

```elisp
(require 'phantom-inline-comment)
(add-to-list 'load-path "YOUR PATH")
```

## Usage

### Add/Edit a phantom comment

+ `phantom-inline-comment`

Add one phantom under a line of the cursor.  
After popup edit-buffer, you can write it.

### Delete a specific phantom comment

+ `phantom-inline-comment-delete`

Delete one phantom under a line of the cursor.

### Delete All phantom comments

+ `phantom-inline-comment-delete-all`

Delete all the phantoms.
