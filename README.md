# Brulion - a Trello clone in pas2js

This is a front-end application written in Pascal. It uses pas2js and [this fork of pas2js-ws](https://github.com/bbrtj/Pas2JS_Widget).

## API

A REST API server is developed in [a separate repository](https://github.com/bbrtj/perl-brulion-api).

## Development in Lazarus

Developed in Lazarus 4.0 and fpc 3.2.2.

- First off, pas2js-rtl (branch `fixes_3_0`) and pas2js-ws (link above, branch master) must be installed to Lazarus. This can be done with fpcupdeluxe, but git urls and branches must be changed manually in `fpcup.ini`
- WCL and WCLDsgn packages from pas2js-rtl must be compiled inside Lazarus
- Project should have a proper `BrulionApiUrl` set in the project options
- Optionally, a different directory to output js can be set with `-o/my/directory/brulion.js`
- `assets/brulion.html` can be opened to view the application, but the API HTTP requests will most likely get rejected because of cross origin policies. It is recommended to move everything from `assets` and `brulion.js` to a public server directory
- `assets/brulion.html` from need to have its paths to css/js documents adjusted manually after moving

