# Intralisp

Do you hate PHP, and also hate bash too much to use BHB?

Use intralisp!

## File format

Regular text is in a literal section. It is more or less printed as is, unless you embed a statement within a literal section.

`%` starts a statement, which will be evaluated as a block of lisp code.
Within a statement, two special characters are recognised:

* `%`: The character that ends the statement
* `@`: The character that starts a literal section.
Literal sections are ended by the `@` character.

You can also use `%= ... %` inside a literal section which is expands to `(FORMAT T ...)`.

## Usage 

```shell
intralisp < file.intralisp
intralisp file.intralisp
echo '%= "Hello World %' | intralisp
```

## Examples

### Basic
```
%loop for i from 1 to 10 do
@Hello for the %="~:r"i% time!
@%
```

### VHDL 😏
```
% dotimes (i 8)
@Load_TB <= %= "~3,'0b" i%;
@%
```
