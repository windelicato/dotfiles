	"set number 				" turn on line numbers and highlight colors
	set ruler 				" Always show current positions along the bottom
	set showcmd 				" show the command being typed
	set autoindent 				" autoindent spaces
	set cursorline
"	inoremap ;; <esc>			" fbind ;; to esecape key 
	set t_Co=256				" 256 colors 
	nmap <C-h> <C-w>h			" control h, j, k, l tab navigation
	nmap <C-j> <C-w>j
	nmap <C-k> <C-w>k
	nmap <C-l> <C-w>l
	syntax enable
	set background=dark
	"let g:solarized_termcolors=256
	"let g:solarized_termtrans=1
	colorscheme default
"	colors darkspectrum
"	set nowrap
"	set mouse=a 				" use mouse anywhere
	set autoread 				" Set to auto read when a file is changed from the outside

	filetype plugin indent on
	call pathogen#infect()
	call pathogen#helptags()
"}

" COLORS {
	" syntax highlighting groups
	hi Comment      ctermfg=12
	hi Constant     ctermfg=6 
	hi Identifier   ctermfg=4
	hi Statement    ctermfg=2
	hi PreProc      ctermfg=1
	hi Type         ctermfg=3
	hi Special      ctermfg=5
	hi Underlined   ctermfg=7
	hi Ignore       ctermfg=9
	hi Error        ctermfg=11
	hi Todo         ctermfg=1
	hi Normal ctermfg=none ctermbg=none
	hi NonText ctermfg=0 ctermbg=none
	hi Directory	ctermfg=12

	hi VertSplit	ctermfg=black
	hi StatusLine	ctermfg=white
	hi StatusLineNC	ctermfg=0 

	hi Pmenu ctermfg=10 ctermbg=0
	hi PmenuSel ctermfg=0 ctermbg=14
	hi LineNr ctermfg=0 ctermbg=none
	hi CursorLine ctermfg=none ctermbg=none cterm=none
	hi CursorLineNr ctermfg=none ctermbg=0 
	hi CursorColumn ctermfg=none ctermbg=0
"}

" Code Completion{

"set conceallevel=2
"set concealcursor=vin
"let g:clang_snippets=1
"let g:clang_conceal_snippets=1
"" The single one that works with clang_complete
"let g:clang_snippets_engine='clang_complete'
"
"" Complete options (disable preview scratch window, longest removed to aways
"" show menu)
"set completeopt=menu,menuone,longest
"
"" Limit popup menu height
"set pumheight=20
"
"" SuperTab completion fall-back 
"let g:SuperTabDefaultCompletionType='<c-x><c-u><c-p>'
"let g:clang_use_library=1


	"if v:version >= 600
	  "filetype plugin on
	  "filetype indent on
	"else
	  "filetype on
	"endif

	"if v:version >= 700
	  "set omnifunc=syntaxcomplete#Complete " override built-in C omnicomplete with C++ OmniCppComplete plugin
	  "let OmniCpp_GlobalScopeSearch   = 1
	  "let OmniCpp_DisplayMode         = 1
	  "let OmniCpp_ShowScopeInAbbr     = 0 "do not show namespace in pop-up
	  "let OmniCpp_ShowPrototypeInAbbr = 1 "show prototype in pop-up
	  "let OmniCpp_ShowAccess          = 1 "show access in pop-up
	  "let OmniCpp_SelectFirstItem     = 1 "select first item in pop-up
	  "set completeopt=menuone,menu,longest
	"endif

	""Update ctags
	"function! UpdateTags()
	 "execute ":!ctags -R --languages=C++ --c++-kinds=+p --fields=+iaS --extra=+q ./"
	  "execute ":!ctags -R --languages=C,C++ --c++-kinds=+p --fields=+iaS --extra=+q ./"
	  "echohl StatusLine | echo "C/C++ tag updated" | echohl None
	"endfunction
	"nnoremap <F4> :call UpdateTags()

"}

" WORD PROCESSING {
	set formatoptions=1
	set lbr
	set linebreak
	set wrap
	au BufRead,BufNewFile *.todo setfiletype todo


	cabbr wp call Wp()
	fun! Wp()
		set formatoptions=1
		set lbr
		set linebreak
		set wrap
		set nonumber
		nnoremap j gj
		nnoremap k gk
		nnoremap 0 g0
		nnoremap $ g$
		set comments=s1:/*,mb:*,ex:*/,://,b:#,:%,:XCOMM,n:>,fb:-,fb:[+],fb:[x],fb:[-]
		set comments +=fb:-
"		set spell spelllang=en_us

	endfu

	" Search for selected text, forwards or backwards.
	vnoremap <silent> * :<C-U>
	  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
	  \gvy/<C-R><C-R>=substitute(
	  \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
	  \gV:call setreg('"', old_reg, old_regtype)<CR>
	vnoremap <silent> # :<C-U>
	  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
	  \gvy?<C-R><C-R>=substitute(
	  \escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
	  \gV:call setreg('"', old_reg, old_regtype)<CR>
"}
