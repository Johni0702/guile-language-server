# guile-language-server

[Language Server Protocol](https://github.com/Microsoft/language-server-protocol) (LSP) server implemented in Guile Scheme and providing support for Guile programs (those which compile to `tree-il`, e.g. Scheme).
Very much WIP and barely working.

## Setup

To use guile-language-server, you need to have at least Guile 2.2 and [guile-json](https://savannah.nongnu.org/projects/guile-json/) installed.
Then, just point your LSP client/editor at the `wrapper` file.

If, for example, you're using [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim/), this will do it:
```vimscript
let g:LanguageClient_serverCommands = {
	\ 'scheme': ['path/to/wrapper'],
	\ }
```

Also make sure your project root is correctly configured (guile-language-server will add it to the load-path for compilation).

If your editor does not have paredit (parenthesis are always inserted/removed in pairs) builtin, it is highly recommended to install a plugin which does it.
If your code does not pass the parser (e.g. due to unbalanced parenthesis), then guile-language-server can only provide a very limited amount of information to you.

## Features

- Provides any warnings and errors which happen during macro expansion/compilation/running as diagnostics to your editor
- Tracks dependencies between mutliple files/modules and triggers re-compilation when necessary
- Jump to definition of top-level procedures
- Jump to definition of most other top-level variables (on a best effort basis)
- Jump to definition of local binding (WIP)

## Shortcomings

- Very few tests
- Single threaded (compilation will block all other interactions)
- LSP code is very much only implemented as far as required for it to work and not robust at all
- Litters your CWD with log files since at the moment there is no way to configure them to be somewhere else
- Jumping to local bindings tends to miss by quite a bit (hits some parent expression of the actual target). Not sure if this can really be reliably solved (see second paragraph [here](https://www.gnu.org/software/guile/manual/html_node/Source-Properties.html#Source-Properties)).
- Only tested with LanguageClient-neovim (which doesn't seem horribly spec-compliant itself)
- Has a tendency to break during dogfooding since both, the server itself and the to-be-compiled code, are loaded on the same VM (dogfooding is supposed to be possible, it's just subpar right now) 
- May break if you `set!` cross-module (this may include global registries like define-language). I see no way of fixing this short of restarting the whole VM and recompiling everything everytime and that's not happening.
- Something in the compilation process is rather slow (order of seconds for bigger files). Might just be that it's actually doing hard work. Not sure what/if something can be done about that.

## License
guile-language-server is provided under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
See `COPYING` for the full license text.
