" Vim filetype plugin file
" Language:	SystemVerilog (superset extension of Verilog)
" Maintainer:	Amit Sethi <amitrajsethi@yahoo.com>
" Last Change:	Tue Jun 26 08:56:34 IST 2006
" Version: 1.0

" Only do this when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif


au BufRead,BufNewFile *.v,*.vh,*.sv		set filetype=verilog_systemverilog

" Behaves just like Verilog
runtime! ftplugin/verilog.vim
