# PureScript30

A port of Wes Boss's JavaScript 30 Day Challenge ([Javascript30](https://github.com/wesbos/JavaScript30)) to [PureScript](http://www.purescript.org/) 

## Set Up

Be sure that you have downloaded the latest purescript, pulp and psc-package libraries.

`$ npm i -g purescript pulp psc-package`

You can also use `bower` as your package manager

`$ npm i -g bower`


These tutorials have been ported to the latest version of the PureScript compiler.  At the time of this commit, the compiler version is 0.12.0.  For an introduction to PureScript please see my tutorial series, [Make the leap from JavaScript to PureScript](https://github.com/adkelley/javascript-to-purescript/blob/master/README.md)


## Install, Build & Run

Navigate to the tutorial you're interested in running (e.g., `$ cd 01_Drum_Kit`) then execute the following steps:

1.  `$ npm run clean`
2.  `$ npm run install`
3.  `$ npm run build`
4.  `$ npm run exec`

Finally, navigate to `localhost:8000` on your favorite browser.  

**Options**: If you wish to install the library modules using Bower, then replace Step 2 with `bower install` and Step 3 with `pulp --psc-package browserify --main Clock --to output/bundle.js`.  If you want to default to this option permanently, then modify the `package.json` file accordingly.
