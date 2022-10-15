# Intralisp

Do you hate PHP and also hate bash too much to use BHB?

Use intralisp!

## File format

Regular text is in a literal section. It is more or less printed as is, unless you embed a statement within a literal section.

Inside a literal section, the following syntax is recognised.

* `% FUNCTION ARGS %`: Evaluates an expression silently
* `%~ FUNCTION ARGS %`: the same as `%`, but the output is printed out.
* `%= VARIABLE %`: prints out the value of a variable
* `%=~ FORMAT-STRING VARIABLE* %`: prints out the value of a variable and allows you to specify a format string to use

Each of these start a statement.
Inside a statement the following syntax is recognised:

* `%`: The character that ends the statement
* `@ TEXT @`: Starts a nested literal section.
Literal sections are ended by the `@` character.

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
@Hello for the %=~"~:r"i% time!
@%
```

### VHDL 😏
```
% dotimes (i 8)
@Load_TB <= %=~ "~3,'0b" i%;
@%
```
