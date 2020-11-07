
/*
---- WARNING: ----

The SurfingKeys settings interface is just like vimscript, i.e., it exhibits poor design and
makes it extremely easy to make mistakes since the config is not declarative and thus imposes
on the user the burden of keeping track of the order in which values are set.
Also it forces the user to map e.g. p to P rather than explicitly to the named function
that P denotes (since most built-in functions are anonymous).

- Remapping is automatic and not even optional: there is no "noremap" command (yikes).
    - E.g. we must set P=p before setting p=k if we want P to be the original p.
		- This means all 'unmap' commands should be done after all the 'map' commands.
		  E.g. if you first unmap k and then try to map p to k, the p binding will be invalid.
    - Also, setting e.g. both e=E and E=e will cause a logic loop rather than swapping
	      	them (double yikes).
	  - Errors can potentially be hard to track, since assignment is chained, e.g. p=P,e=p,j=e.
		  So avoid chained assignment. Instead do e.g. p=P,e=P,j=P.

---- WARNING ----
*/


// an example to create a new mapping `ctrl-y`
/*mapkey('<Ctrl-y>', 'Show me the money', function() {
    Front.showPopup('a well-known phrase uttered by characters in the 1996 film Jerry Maguire (Escape to close).');
});*/

// an example to replace `T` with `gt`, click `Default mappings` to see how `T` works.
//map('gt', 'T');

// an example to remove mapkey `Ctrl-i`
//unmap('<Ctrl-i>');


// set theme
settings.theme = `
.sk_theme {
    font-family: Input Sans Condensed, Charcoal, sans-serif;
    font-size: 11pt;
    background: #24272e;
    color: #abb2bf;
}
.sk_theme tbody {
    color: #fff;
}
.sk_theme input {
    color: #d0d0d0;
}
.sk_theme .url {
    color: #61afef;
}
.sk_theme .annotation {
    color: #56b6c2;
}
.sk_theme .omnibar_highlight {
    color: #528bff;
}
.sk_theme .omnibar_timestamp {
    color: #e5c07b;
}
.sk_theme .omnibar_visitcount {
    color: #98c379;
}
.sk_theme #sk_omnibarSearchResult ul li:nth-child(odd) {
    background: #303030;
}
.sk_theme #sk_omnibarSearchResult ul li.focused {
    background: #3e4452;
}
#sk_status, #sk_find {
    font-size: 20pt;
}`;




/*
-----------------------------------------------------------------
----------- start my settings -----------------------------------
-----------------------------------------------------------------
*/


//map('J', '10j'); // prefixes not yet supported in keybindings
//map('K', '10k'); // prefixes not yet supported in keybindings
//map('D', '99>>'); // prefixes not yet supported in keybindings
//map('U', '99<<'); // prefixes not yet supported in keybindings
//map('gh', 'g#');
//map(':D', 'ab');
//map('<', '<<');
//map('>', '>>');


// map keys for setting escape
map('fd', '<Esc>');
imap('fd', '<Esc>');
vmap('fd', '<Esc>');
cmap('<Ctrl-m>', '<Esc>');
// ctrl apparently cannot be used for vim visual mode (not documented!)
aceVimMap('fd', '<Esc>', 'insert');
aceVimMap('fd', '<Esc>', 'visual');
// ctrl has to be used for vim normal mode (not documented!)
aceVimMap('<Ctrl-m>', '<Esc>', 'normal');
aceVimMap('<C-h>','<Esc>h','insert');
aceVimMap('<C-j>','<Esc>j','insert');
aceVimMap('<C-k>','<Esc>k','insert');
aceVimMap('<C-l>','<Esc>l','insert'); 
// Alt can be used in regular field boxes to exit the input box
imap('<Alt-h>', '<Esc>');
imap('<Alt-j>', '<Esc>');
imap('<Alt-k>', '<Esc>');
imap('<Alt-l>', '<Esc>');
// Exiting visual / find mode (also seems to require the binding to include Ctrl, or at least some modifier key)
map('<Ctrl-]>', '<Esc>');
map('<Ctrl-m>', '<Esc>');

// escape keys for vim-map insert mode
/*
aceVimMap('<C-h>','<Esc>h','insert');
aceVimMap('<C-j>','<Esc>j','insert');
aceVimMap('<C-k>','<Esc>k','insert');
aceVimMap('<C-l>','<Esc>l','insert');
*/

// Alt can be used in regular vield boxes to exit the input box
imap('<Alt-h>', '<Esc>');
imap('<Alt-j>', '<Esc>');
imap('<Alt-k>', '<Esc>');
imap('<Alt-l>', '<Esc>');


// Misc 1
map('P', 'p'); // enter PassThrough mode to temporarily suppress SurfingKeys
iunmap(':'); // disable emoji suggestions


// omnibar controls
cmap('<Ctrl-j>', '<Tab>'); // up
cmap('<Ctrl-k>', '<Shift-Tab>'); // down


// scrolling
map('(', 'h');
map(')', 'l');
map('J', 'd');
map('K', 'u');
// map('J', '<Ctrl-d>');
// map('K', '<Ctrl-u>');
mapkey('J', '#3Move current tab to left', function() {
    RUNTIME('moveTab', {
        step: 99
    });
});


// navigate tabs
map('h', 'E'); // tab left
map('l', 'R'); // tab right
map('p', '<Alt-P>'); // pin this tab

// mapkey('ga', '#3Focus leftmost tab', function() {
//     RUNTIME('prevTab', {
//         step: 99
//     });
// });

// mapkey('gl', '#3Focus rightmost tab', function() {
//     RUNTIME('nextTab', {
//         step: 99
//     });
// });


// navigate history
map('H', 'S'); // back
map('L', 'D'); // forward
//map(';', '<Ctrl-6>'); // toggle prev tab (?)


// marks
map('ma', 'm'); // create mark


// closing tabs:
map('qh', 'gx0'); // close tabs to left
map('ql', 'gx$'); // close tabs to right
map('qH', 'gxt'); // close tab to left
map('qL', 'gxT'); // close tab to right


// open particular tabs
map('gj', 'gd'); // open downloads
mapkey('gE', '#12Open Chrome Extension Shortcuts', function() {
    tabOpenLink("chrome://extensions/shortcuts");
});
mapkey('gS', '#12Open Chrome Settings', function() {
    tabOpenLink("chrome://settings");
});
mapkey('<Ctrl-/>', '#12Open SurfingKeys Settings', function() {
    tabOpenLink("chrome-extension://gfbliohnnapiefjpjlpjnehglfpaknnc/pages/options.html#");
});
map('g/', '<Ctrl-/>'); // also open SurfingKeys Settings


// open links
map('F', 'f');
map('f', 'gf');
map(',', '[[');
map('.', ']]');
map('<', '[[');
map('>', ']]');


// move tab left/right
map('u', '<<');
map('d', '>>');
mapkey('U', '#3Move current tab to left', function() {
    RUNTIME('moveTab', {
        step: -99
    });
});
mapkey('D', '#3Move current tab to left', function() {
    RUNTIME('moveTab', {
        step: 99
    });
});


// visual mode mappings
vmap('mm', 'zz'); // center the display
vmap('J', '<Ctrl-d>'); // scroll 20 lines down
vmap('K', '<Ctrl-u>'); // scroll 20 lines up


// ACE-Vim-Map bindings
// move to beginning/end of line
aceVimMap('gl', '$', 'normal'); // line end
aceVimMap('gh', '^', 'normal'); // first non-whitespace on line
aceVimMap('a', '^', 'normal'); // first non-whitespace on line
aceVimMap('ga', '0', 'normal'); // line beginning
// word boundaries
aceVimMap('e', 'E', 'normal');
aceVimMap('w', 'W', 'normal');
aceVimMap('b', 'B', 'normal');




/* Search aliases */

mapkey('sw', '#8Open Search with Wikipedia', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "wi"});
});

mapkey('sg', '#8Open Search with Google', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "go"});
});

mapkey('sy', '#8Open Search with Youtube', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "yo"});
});

mapkey('sa', '#8Open Search with Amazon', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "az"});
});

mapkey('sm', '#8Open Search with MELPA', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "mel"});
});

mapkey('sh', '#8Open Search with Hoogle', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "ha"});
});

mapkey('sp', '#8Open Search with Pursuit', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "pur"});
});

mapkey('sn', '#8Open Search with Nixpkgs Search', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "nix"});
});

mapkey('st', '#8Open Search with Typed Racket Docs', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "tr"});
});

mapkey('sr', '#8Open Search with Racket Docs', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "ra"});
});

mapkey('sl', '#8Open Search with Nix Revision Search', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "laz"});
});

mapkey('su', '#8Open Search with Stack Overflow', function() {
  Front.openOmnibar({type: "SearchEngine", extra: "laz"});
});

// ------------------------------------------------------------------------
// ------------------------------------------------------------------------
// ------------------------------------------------------------------------
// ------------------------------------------------------------------------


addSearchAliasX('laz', 'Nix Revision', 'https://lazamar.co.uk/nix-versions/?channel=nixos-unstable&package=');

addSearchAliasX('nix', 'NixPkgs', 'https://search.nixos.org/packages?from=0&size=60&sort=relevance&channel=unstable&query=');

addSearchAliasX('mel', 'MELPA', 'https://melpa.org/#/?q=');

addSearchAliasX('ra', 'Racket Docs', 'https://docs.racket-lang.org/search/index.html?q=');

addSearchAliasX('tr', 'Typed-Racket Docs', 'https://docs.racket-lang.org/search/index.html?q=L%3Atyped%2Fracket%2Fbase%20');

addSearchAliasX('ha', 'Hoogle', 'https://hoogle.haskell.org/?hoogle=');

addSearchAliasX('pur', 'Pursuit', 'https://pursuit.purescript.org/search?q=');

addSearchAliasX('sp', 'StartPage', 'https://startpage.com/sp/search/?q=');

addSearchAliasX('gm', 'Google Maps', 'https://www.google.com/maps?q=');

addSearchAliasX('s', 'Stackoverflow', 'http://stackoverflow.com/search?q=');

addSearchAliasX('wi', 'Wikipedia', 'https://en.wikipedia.org/wiki/', 's', 'https://en.wikipedia.org/w/api.php?action=opensearch&format=json&formatversion=2&namespace=0&limit=40&search=', function(response) {
  return JSON.parse(response.text)[1];
});

addSearchAliasX('u', 'Github', 'https://github.com/search?q=', 's', 'https://api.github.com/search/repositories?order=desc&q=', function(response) {
  var res = JSON.parse(response.text)['items'];
  return res ? res.map(function(r){
    return {
      title: r.description,
      url: r.html_url
    };
  }) : [];
});

addSearchAliasX('y', 'Youtube', 'https://www.youtube.com/results?search_query=', 's',
								'https://clients1.google.com/complete/search?client=youtube&ds=yt&callback=cb&q=', function(response) {
									var res = JSON.parse(response.text.substr(9, response.text.length-10));
									return res[1].map(function(d) {
										return d[0];
									});
								});




/* Settings */

settings.scrollStepSize = 210;
settings.focusAfterClosed = "last"; // "right"|"left"|"last"
settings.digitForRepeat = false;
settings.prevLinkRegex = '/((back|older|<|‹|←|«|≪|<<|prev(ious)?)+)/i';
settings.nextLinkRegex = '/((more|newer|>|›|→|»|≫|>>|next)+)/i';
settings.hintShiftNonActive	= true;
settings.hintExplicit = true;
settings.omnibarPosition = "middle";
settings.focusOnSaved = false; // do not focus the text input after quitting from vim editor

Hints.characters = "asdfjhletowcmrnv";
Hints.scrollKeys = "0G$";

// Vimium-style link hints
Hints.style(
  `
  position: absolute;
  display: block;
  top: -1px;
  left: -1px;
  white-space: nowrap;
  overflow: hidden;
  font-size: 11px;
  padding: 1px 3px 0px 3px;
  background: linear-gradient(to bottom, #FFF785 0%,#FFC542 100%);
  border: solid 1px #C38A22;
  border-radius: 3px;
  box-shadow: 0px 3px 7px 0px rgba(0, 0, 0, 0.3);
  "text"
	`
);

/* To change style for link hints: */
// Hints.style('border: solid 3px #552a48; color:#efe1eb; background: initial; background-color: #552a48;');
/* To change style for link hints: */
// Hints.style("border: solid 8px #C38A22;padding: 1px;background: #e39913", "text");
// Visual.style('marks', 'background-color: #89a1e2;');
// Visual.style('cursor', 'background-color: #9065b7;');
// settings.theme = `
//     #sk_status, #sk_find {
//         font-size: 20pt;
//     }
// }`;




/* unmap unused bindings */
unmap('cc');
unmap('E');
unmap('R');
unmap('e');
unmap('m'); // so we can use 'ma' for 'mark'
unmap(';'); // for overriding from Vimium 'toggle-prev-tab'
// unmap('J'); // for overriding from Vimium
// unmap('K'); // for overriding from Vimium
// unmapAllExcept(['<Ctrl-m>', '<Ctrl-i>', 'yg', 'ZZ', 'ZR', 'ZQ', 'ab']);


