# Intralisp

Do you hate PHP and also hate bash too much to use BHB?

Use intralisp!

## File format

Regular text is in a literal section. It is more or less printed as is, unless you embed a statement within a literal section.

Inside a literal section, the following syntax is recognised.

* `[% FUNCTION ARGS %]`: Evaluates an expression silently
* `[%~ FUNCTION ARGS %]`: the same as `%]`, but the output is printed out.
* `[%= VARIABLE %]`: prints out the value of a variable
* `[%=~ FORMAT-STRING VARIABLE* %]`: prints out the value of a variable and allows you to specify a format string to use
* `[%! "INTRALISP-FILE" %]` : Imports another intralisp file

Each of these start a statement.
Inside a statement the following syntax is recognised:

* `%]`: The token that ends the statement
* `[@ TEXT @]`: Starts a nested literal section.
* `[@^ TEXT @]`: Starts a literal section, but rather then printing to standard output, the section evaluates to a string.
E.g. you can do things such as `[% defvar header [@^Hello world@] %]`
Literal sections are ended by the `@]` token.

## Usage 

```shell
intralisp < file.intralisp
intralisp file.intralisp
echo 'Hello [%= "World" %]' | intralisp
```

## Dependencies

* Some lisp interpreter.
I have only tested things using SBCL, YMMV with other lisp interpreters.

## Examples

See examples folder.
