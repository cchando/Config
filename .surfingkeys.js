
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
		- Disabling settings.digitForRepeat will break any keys mapped to e.g. g0, g$, etc.,
		  if the mapping is done before the setting is disabled.
		- There is a bug where unmapping e.g. 'm' will also unmap any bindings for 'ma', 'mb', etc.
		- Furthermore, if you first map e.g. ';' to '<Ctrl-6>' and then map 'c' to ';j', the 2nd
		      mapping will be null because the default binding ';j' was nullified when its prefix
					';' was remapped. This is the case for any prefix, so you really have to be on your
					toes. I would definitely call this a bug, although technically you could argue that
					it's just really poor design. It blurs the line between both.
		- If you want to map e.g. "ma" to "d" and also unmap "m", you must unmap "m" first, since
		  unmapping "m" will also unmap any bindings prefixed by "m".

		- This means longer-keystroke bindings must always come before shorter ones.
		- A best-practice for dealing with the imperativeness:
        // save default key `t` to temp key `>_t`
   	  	map('>_t', 't');
				// create a new key `t` for default key `on`
				map('t', 'on');
				// create a new key `o` for saved temp key `>_t`
  		  map('o', '>_t');

---- END OF WARNING ----
*/



/*
   -----------------------------------------------------------------
   -----------------------------------------------------------------
   -----------------------------------------------------------------
   Start my settings:
   -----------------------------------------------------------------
   -----------------------------------------------------------------
   -----------------------------------------------------------------
*/


/* set temp keys*/
map('~t`', 't');
map('~q`', 'q');
map(':i`', 'q');
map('*', '.');
unmap('t');
unmap('q');
unmap(':');





// scrolling
map('(', 'h'); // scroll left
map(')', 'l'); // scroll right
map('zh', '0'); // scroll all the way left
map('zl', '$'); // scroll all the way right
map('J', 'd'); // scroll half-page down

map('K', 'u'); // scroll half-page down


// navigate tabs
map('o', '~t'); // open omnibar  // 't', 'go'
map('tt', 'on'); // open new tab
map('td', 'W'); // detach tab (new window w/ current tab)
map('h', 'E'); // tab left
map('l', 'R'); // tab right
map('p', '<Alt-p>'); // pin current tab
map('ga', 'g0'); // focus leftmost tab
map('gl', 'g$'); // focus rightmost tab


// navigate history
map('H', 'S'); // back
map('L', 'D'); // forward


// // marks
// map('ma', 'm'); // create mark -- replace with Vimium's marks


// closing tabs:
map('qH', 'gx0'); // close all tabs to left
map('qL', 'gx$'); // close all tabs to right
map('qO', 'gxx'); // close all tabs except current one
map('qh', 'gxt'); // close tab to left
map('ql', 'gxT'); // close tab to right
map('gxH', 'gx0'); // close all tabs to left
map('gxL', 'gx$'); // close all tabs to right
map('gxO', 'gxx'); // close all tabs except current one
map('gxh', 'gxt'); // close tab to left
map('gxl', 'gxT'); // close tab to right
map('cH', 'gx0'); // close all tabs to left
map('cL', 'gx$'); // close all tabs to right
map('cO', 'gxx'); // close all tabs except current one
map('ch', 'gxt'); // close tab to left
map('cl', 'gxT'); // close tab to right


// open particular tabs
map('gj', 'gd'); // open downloads
mapkey('gE', '#12Open Chrome Extension Shortcuts', function() {
    tabOpenLink("chrome://extensions/shortcuts");
});
mapkey('gS', '#12Open Chrome Settings', function() {
    tabOpenLink("chrome://settings");
});
mapkey('gb', '#12Open Chrome Settings', function() {
  tabOpenLink("chrome://bookmarks");
});
map('g/', ';e'); // open SurfingKeys settings
mapkey('<Ctrl-/>', '#12Open Vimium C Settings', function() {
  tabOpenLink("chrome-extension://hfjbmagddngcpeloejdejnfgbamkjaeg/pages/options.html");
});


/* move tab left/right */
map('u', '<<');
map('d', '>>');
mapkey('U', '#3Move current tab to leftmost', function() {
    RUNTIME('moveTab', {
        step: -99
    });
});
mapkey('D', '#3Move current tab to rightmost', function() {
    RUNTIME('moveTab', {
        step: 99
    });
});
map('gH', 'U'); // from above
map('gL', 'D'); // from above


/* open links */
map('F', 'cf'); // open multiple links in new tabs
map(',', '[[');
map('.', ']]');
map('<', '[[');
map('>', ']]');


/*
  -----------------------------------------------------------------
  Misc 1
  -----------------------------------------------------------------
*/

// map('mu', '<Alt-m>'); // mute current tab  -- use Vimium C's muteTab variants
map('P', 'p'); // enter PassThrough mode (refined version of Vimium's insert mode)
iunmap(':'); // disable emoji suggestions
map('e', 'cs'); // change scroll target
map('cd', ';j'); // close Downloads bar
map(':m', ';m'); // mouse-out last element
map(';', '<Ctrl-6>'); // toggle prev tab (must map AFTER any "map blah to ;_")
map('W', 'oh'); // open from history
map('gh', 'g#'); // open current url without the hash fragment
map('<Alt-p>', ';s'); // toggle pdf viewer
map('g/', ';e'); // open SurfingKeys settings
map(':D', 'ab'); // add bookmark
map('F', 'cf'); // open multiple links in new tabs
// map('I', 'i'); // enter insert mode
// map('O', 'ox'); // open recently-closed url  // TODO: not working somewhy. replaced by below
mapkey('O', '#8Open recently closed URL', function() {
  Front.openOmnibar({type: "URLs", extra: "getRecentlyClosed"});
});
// map('W', 'w'); // switch frames   // internal function seems to be broken

// map('w', 'oh'); // open from history  // TODO: not working somewhy. replaced by below
mapkey('w', '#8Open URL from history', function() {
  Front.openOmnibar({type: "History"});
});
// map('o', 'go'); // open omnibar  // TODO: not working somewhy. replaced by below
mapkey('o', '#8Open a URL in current tab', function() {
  Front.openOmnibar({type: "URLs", extra: "getAllSites", tabbed: false});
});





/* unmap unused bindings */
unmap("'"); // replaced by Vimium C's since it can use '' to toggle prev mark
unmap("m"); // replaced by Vimium C's since the popup looks better and we can use '' with it
unmap('ab');
unmap('af');
unmap('cf');
unmap('gr');
unmap(';s');
unmap(';j');
unmap(';e');
unmap(';dh');
unmap('oh');
unmap('go');
unmap('ss');
unmap('sb');
unmap('sd');
unmap('se');
unmap('gx$');
unmap('gx0');
unmap('gxT');
unmap('gxt');
unmap('gxx');
unmap('sso');
unmap('ssp');
unmap('S'); // go backward in history
/*
  unmap('D'); // go forward in history
	reserved as alias for ab (addBookmark)
*/
unmap('on');
unmap('g0');
unmap('g$');
unmap('@'); // Vimium_C toggleMuteTab all
unmap('$'); // Vimium_C toggleMuteTab other
// unmap('yf'); // copy form data in JSON on current page
unmap('E'); // go one tab left
unmap('R'); // Vimium_C reload hard; was "go one tab right"
unmap('ya'); // copy a link url to the clipboard
unmap('B'); // go one tab history back
/*
	unmap('F'); // go one tab history forward
	reserved for linksActivateInNewTab
 */
unmap('om'); // open url from marks
// unmap('oh'); // shadowed by 'o'; open from history -- already has alias 'W'
unmap('cc'); // shadowed by c (close downlaod bar); open selected link, or link from clipboard
unmap('cf'); // shadowed by c (close download bar); replaced by F
unmap('gf'); // redundant since <Shift> after 'f' does the same thing
unmap('C'); // same as 'gf' above
// unmap('go'); // replaced by 'o'
unmap('i'); // override with Vimium_C insertMode; go to edit box -- doesn't work well yet
unmap('I'); // override with Vimium_C insertMode; go to edit box -- doesn't work well yet
unmap('A'); // not sure if mapped; Vimium_C joinTabs
unmap('M'); // not sure if mapped; Vimium_C toggleMuteTab all
// unmap(';'); // for overriding from Vimium 'toggle-prev-tab'
unmap('<Ctrl-d>');
unmap('<Ctrl-u>');
unmap('<Shift-Tab>');
unmap('<Tab>');





// omnibar controls
cmap('<Ctrl-j>', '<Tab>'); // up
cmap('<Ctrl-k>', '<Shift-Tab>'); // down
cmap('<Ctrl-q>', '<Ctrl-d>'); // remove selected item from bookmarks
cmap('<Ctrl-h>', '<Backspace>'); // probably won't work since <Backspace> isn't explicitly mapped...
// cmap('<Ctrl-[>', '<Shift-,>'); // page up
// cmap('<Ctrl-]>', '<Shift-.>'); // page down



/* visual mode mappings */
vmap('mm', 'zz'); // center the display
vmap('J', '<Ctrl-d>'); // scroll 20 lines down
vmap('K', '<Ctrl-u>'); // scroll 20 lines up


/* map keys for setting escape */
cmap('<Ctrl-m>', '<Esc>');
cmap('<Ctrl-g>', '<Esc>');
/* ctrl apparently cannot be used for vim visual mode (not documented!) */
// aceVimMap('fd', '<Esc>', 'insert');
// aceVimMap('fd', '<Esc>', 'visual');
/* ctrl has to be used for vim normal mode (not documented!) */
aceVimMap('<Ctrl-m>', '<Esc>', 'normal');
aceVimMap('<Ctrl-g>', '<Esc>', 'normal');
aceVimMap('<C-h>','<Esc>h','insert');
aceVimMap('<C-j>','<Esc>j','insert');
aceVimMap('<C-k>','<Esc>k','insert');
aceVimMap('<C-l>','<Esc>l','insert');
/* Alt can be used in regular field boxes to exit the input box */
imap('<Alt-h>', '<Esc>');
imap('<Alt-j>', '<Esc>');
imap('<Alt-k>', '<Esc>');
imap('<Alt-l>', '<Esc>');
/* Exiting visual / find mode (also seems to require the binding to include Ctrl, or at least some modifier key) */
map('<Ctrl-]>', '<Esc>');
map('<Ctrl-m>', '<Esc>');
map('<Ctrl-g>', '<Esc>');
// escape keys for vim-map insert mode
/*
	aceVimMap('<C-h>','<Esc>h','insert');
	aceVimMap('<C-j>','<Esc>j','insert');
	aceVimMap('<C-k>','<Esc>k','insert');
	aceVimMap('<C-l>','<Esc>l','insert');
*/


/* ACE-Vim-Map bindings */
// move to beginning/end of line
aceVimMap('gl', '$', 'normal'); // line end
aceVimMap('gh', '^', 'normal'); // first non-whitespace on line
aceVimMap('a', '^', 'normal'); // first non-whitespace on line
aceVimMap('ga', '0', 'normal'); // line beginning
// aceVimMap('ygh', 'y0', 'normal'); // yank to first char on line
// aceVimMap('ygl', 'y$', 'normal'); // yank to line end
/* word boundaries */
aceVimMap('e', 'ea', 'normal');
aceVimMap('w', 'W', 'normal');
aceVimMap('b', 'B', 'normal');



/*
	----------------------------------------------------------------------
	----------------------------------------------------------------------
	Search alias keybindings:
	----------------------------------------------------------------------
	----------------------------------------------------------------------
*/


mapkey('sw', '#8Search Wikipedia for given term', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "wi"});
});

// map('sg', 'og');
unmap('og');
mapkey('sg', '#8Open Search with Google', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "go"});
});

mapkey('sy', '#8Search Youtube for given term', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "yo"});
});

mapkey('sa', '#8Search Amazon for given term', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "az"});
});

mapkey('sm', '#8Open Search MELPA for given package', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "mel"});
});

mapkey('sh', '#8Search Hoogle for given term', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "ha"});
});

mapkey('sp', '#8Open Search Pursuit for given term', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "pur"});
});

mapkey('so', '#8Search Stack Overflow', function() {
  Front.openOmnibar({type: "SearchEngine", extra: "so"});
});

mapkey('sz', '#8Open Search with StartPage', function() {
  Front.openOmnibar({type: "SearchEngine", extra: "sp"});
});

mapkey('sn', '#8Open Search Nixpkgs', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "nix"});
});

mapkey('st', '#8Open Search Typed Racket Docs for given term', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "tr"});
});

mapkey('sr', '#8Open Search Racket Docs for given term', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "ra"});
});

mapkey('sl', '#8Open Find Nix revision for given package', function() {
   Front.openOmnibar({type: "SearchEngine", extra: "laz"});
});

mapkey('su', '#8Search GitHub', function() {
  Front.openOmnibar({type: "SearchEngine", extra: "hub"});
});



/*
	----------------------------------------------------------------------
	----------------------------------------------------------------------
	Search alias definitions:
	----------------------------------------------------------------------
	----------------------------------------------------------------------
*/



addSearchAliasX('laz', 'Nix Revision', 'https://lazamar.co.uk/nix-versions/?channel=nixos-unstable&package=');

addSearchAliasX('nix', 'NixPkgs', 'https://search.nixos.org/packages?from=0&size=60&sort=relevance&channel=unstable&query=');

addSearchAliasX('mel', 'MELPA', 'https://melpa.org/#/?q=');

addSearchAliasX('ra', 'Racket Docs', 'https://docs.racket-lang.org/search/index.html?q=');

addSearchAliasX('tr', 'Typed-Racket Docs', 'https://docs.racket-lang.org/search/index.html?q=L%3Atyped%2Fracket%2Fbase%20');

addSearchAliasX('ha', 'Hoogle', 'https://hoogle.haskell.org/?hoogle=');

addSearchAliasX('pur', 'Pursuit', 'https://pursuit.purescript.org/search?q=');

addSearchAliasX('sp', 'StartPage', 'https://startpage.com/sp/search/?q=');

addSearchAliasX('gm', 'Google Maps', 'https://www.google.com/maps?q=');

addSearchAliasX('so', 'Stack Overflow', 'http://stackoverflow.com/search?q=');

addSearchAliasX('az', 'amazon', 'https://www.amazon.com/s/?field-keywords=');

addSearchAliasX('wi', 'Wikipedia', 'https://en.wikipedia.org/wiki/', 's', 'https://en.wikipedia.org/w/api.php?action=opensearch&format=json&formatversion=2&namespace=0&limit=40&search=', function(response) {
  return JSON.parse(response.text)[1];
});

addSearchAliasX('hub', 'Github', 'https://github.com/search?q=', 's', 'https://api.github.com/search/repositories?order=desc&q=', function(response) {
  var res = JSON.parse(response.text)['items'];
  return res ? res.map(function(r){
    return {
      title: r.description,
      url: r.html_url
    };
  }) : [];
});

addSearchAliasX('go', 'Google', 'https://www.google.com/search?q=', 's', 'https://www.google.com/complete/search?client=chrome-omni&gs_ri=chrome-ext&oit=1&cp=1&pgcl=7&q=', function(response) {
  var res = JSON.parse(response.text);
  return res[1];
});

addSearchAliasX('yo', 'Youtube', 'https://www.youtube.com/results?search_query=', 's',
								'https://clients1.google.com/complete/search?client=youtube&ds=yt&callback=cb&q=', function(response) {
									var res = JSON.parse(response.text.substr(9, response.text.length-10));
									return res[1].map(function(d) {
										return d[0];
									});
								});







/* the line-breaks in the below command are untested: */
// unmapAllExcept(['n', 'N', 'ox', 'b', ';j', 'g#', 'g?', 'oh', ';e', 'go', ';s', '<Tab>', '<Shift-Tab>',
// 								'<Ctrl-u>', '<Ctrl-d>', '<Ctrl-m>', 'yg', 'ZZ', 'ZR', 'ZQ',
// 								'ab', 'v', 'zz', '<Esc>', '?', 'f', 'cf', 'og', 'zr', 'zi',
// 								'zo', ';dh', 'i', 'T', 'w', '/']);




/*
	----------------------------------------------------------------------
	----------------------------------------------------------------------
	Settings:
	----------------------------------------------------------------------
	----------------------------------------------------------------------
*/


settings.scrollStepSize = 200;
settings.hintAlign = "left";
settings.focusAfterClosed = "left";
settings.prevLinkRegex = '/((back|older|<|‹|←|«|≪|<<|prev(ious)?)+)/i';
settings.nextLinkRegex = '/((more|newer|>|›|→|»|≫|>>|next)+)/i';
settings.hintShiftNonActive	= true;
settings.hintExplicit = true;
settings.omnibarMaxResults = 15;
settings.omnibarPosition = "middle";
settings.focusFirstCandidate = true; // for omnibar
settings.focusOnSaved = false; // do not focus the text input after quitting from vim editor
settings.tabsThreshold = 0; // threshold at/above which to show tabs in omnibar instead of in overlay
settings.interceptedErrors = ["*"]; // allow SurfingKeys on all error pages
Hints.numericHints = true; // type text to filter hints

// Vimium C bindings: (A, join tabs) (M, mute all tabs) (ma, create mark) (R, reload hard)
Hints.characters = "sdfghjkletncvbw";
// Hints.characters = "sdfgwhjkletncvb";
// Hints.characters = "sdfgwjhletncvb";
// Hints.characters = "asdfgqwertzxcvb"; // default value
Hints.scrollKeys = "0G$";

// Vimium-style link hints
Hints.style(
  `
  display: block;
  top: -1px;
  left: -1px;
  white-space: nowrap;
  overflow: hidden;
  font-family: Helvetica, Arial, sans-serif;
  font-style: normal;
  font-variant: normal;
  font-weight: bold;
  font-size: 11px;
  padding: 1px 3px 0px 3px;
  background: linear-gradient(to bottom, #FFF785 0%,#FFC542 100%);
  border: solid 1px #C38A22;
  border-radius: 3px;
  box-shadow: 0px 3px 7px 0px rgba(0, 0, 0, 0.3);
	`
);


//   text-shadow: 0 1px 0 rgba(255, 255, 255, 0.6);
// `
// );


// // Vimium-style visual-mode hints
// // the visual-mode CSS seems to have no effect for some reason
// Visual.style(
// 	'marks',
//   `
//   display: block;
//   top: -1px;
//   left: -1px;
//   white-space: nowrap;
//   overflow: hidden;
//   font-family: Helvetica, Arial, sans-serif;
//   font-style: normal;
//   font-variant: normal;
//   font-weight: bold;
//   font-size: 12px;
//   padding: 1px 3px 0px 3px;
//   background: linear-gradient(to bottom, #FFF785 0%,#FFC542 100%);
//   border: solid 1px #C38A22;
//   border-radius: 3px;
//   box-shadow: 0px 3px 7px 0px rgba(0, 0, 0, 0.3);
// 	`
// );



/*
  To change style for link hints:
  Hints.style('border: solid 3px #552a48; color:#efe1eb; background: initial; background-color: #552a48;');

  To change style for link hints:
  Hints.style("border: solid 8px #C38A22;padding: 1px;background: #e39913", "text");
  Visual.style('marks', 'background-color: #89a1e2;');
  Visual.style('cursor', 'background-color: #9065b7;');
  settings.theme = `
      #sk_status, #sk_find {
          font-size: 11pt;
      }
  }`;
*/


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
    font-size: 11pt;
}`;


// font-family: Input Sans Condensed, Charcoal, sans-serif;
// font-family: Helvetica, Arial, sans-serif;  // Vimium style

