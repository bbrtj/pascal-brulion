# Brulion - a Trello clone in pas2js

This is a front-end application written in pas2js. It uses [this fork of pas2js-ws](https://github.com/bbrtj/Pas2JS_Widget).

# Development in Lazarus

Developed in Lazarus 4.0 and fpc 3.2.2.

- First off, pas2js-rtl (branch `fixes_3_0`) and pas2js-ws (link above, branch master) must be installed to Lazarus. This can be done with fpcupdeluxe, but git urls and branches must be changed manually in `fpcup.ini`
- WCL and WCLDsgn packages from pas2js-rtl must be compiled inside Lazarus
- Project should have a proper `BrulionApiUrl` set in the project options. API is currently not included
- Optionally, a different directory to output js can be set with `-o/my/directory/brulion.js`
- Compilation procedure currently does not copy elements from `assets` directory, so they need to be moved manually

