" Vim Syntax File
"
" Language:    Prolog
" Maintainers: Aleksandar Dimitrov <aleks.dimitrov@googlemail.com>
" Created:     Jul 31st, 2008
" Changed:     Fri Aug  1 2008
" Remark:      This file mostly follows
"              http://www.sics.se/sicstus/docs/3.7.1/html/sicstus_45.html
"              but also features some SWI-specific enhancements.
"              The BNF cannot be followed strictly, but I tried to do my best.
"
" TODO:        - Difference Lists
"              - Constraint logic programming

if version < 600
   syn clear
elseif exists("b:current_syntax")
  finish
endif

syntax case match

syntax keyword prologISOBuiltIn   var nonvar integer float number atom string 
			\atomic compound unify_with_occurs_check fail false true repeat call once 
			\catch throw abolish retract asserta assertz current_predicate clause open 
			\close stream_property set_stream_position set_input set_output current_ouput 
			\nl put_byte put_char put_code flush_output get_byte get_code get_char 
			\peek_byte peek_code peek_char at_end_of_stream write_term write_canonical 
			\write writeq read read_term functor arg copy_term atom_codes atom_chars 
			\char_code number_chars number_codes atom_length sub_atom op current_op 
			\char_conversion current_char_conversion is mod rem div round float 
			\float_fractional_part float_integer_part truncate floor ceiling sqrt sin cos 
			\atan log findall bagof setof sub_atom

syntax keyword prologSWIBuiltIn   rational callable ground cyclic_term subsumes subsumes_chk 
			\unifiable use_module compare apply not ignore call_with_depth_limit call_cleanup 
			\print_message print_message_lines message_hook on_signal current_signal block exit 
			\term_hash redefine_system_predicate retractall assert recorda recordz recorded 
			\erase flag compile_predicates index current_atom 
			\current_blob current_functor current_flag current_key dwim_predicate nth_clause 
			\predicate_property open_null_stream current_stream is_stream stream_position_data 
			\seek set_stream see tell append seeing telling seen set_prolog_IO told 
			\wait_for_input byte_count character_count line_count line_position read_clause 
			\put tab ttyflush get0 get skip get_single_char copy_stream_data print portray 
			\read_history prompt setarg nb_setarg nb_linkarg duplicate_term numbervars 
			\term_variables atom_number name term_to_atom atom_to_term atom_concat 
			\concat_atom atom_prefix normalize_space collation_key char_type string_to_list
			\code_type downcase_atom upcase_atom collation_key locale_sort string_to_atom 
			\string_length string_concat sub_string between succ plus rdiv max min random 
			\integer rationalize ceil xor tan asin acos pi e cputime eval msb lsb popcount 
			\powm arithmetic_function current_arithmetic_function is_list memberchk length 
			\sort msort keysort predsort merge merge_set maplist forall writeln writef 
			\swritef format format_predicate current_format_predicate tty_get_capability 
			\tty_goto tty_put set_tty tty_size shell win_exec win_shell win_folder 
			\win_registry_get_value getenv setenv unsetenv setlocale unix date time 
			\get_time stamp_date_time date_time_stamp date_time_value format_time 
			\parse_time window_title win_window_pos win_has_menu win_insert_menu 
			\win_insert_menu_item access_file exists_file file_directory_name file_base_name 
			\same_file exists_directory delete_file rename_file size_file time_file 
			\absolute_file_name is_absolute_file_name file_name_extension expand_file_name 
			\prolog_to_os_filename read_link tmp_file make_directory working_directory chdir 
			\garbage_collect garbage_collect_atoms trim_stacks stack_parameter dwim_match 
			\wildcard_match sleep qcompile portray_clause acyclic_term clause_property 
			\setup_and_call_cleanup message_to_string phrase hash with_output_to fileerrors 
			\read_pending_input prompt1 same_term sub_string merge_set 

syntax cluster prologBuiltIn      contains=prologSWIBuiltIn,prologISOBuiltIn

syntax match   prologArithmetic   /\*\*\?\|+\|\/\/\?\|\/\\\|<<\|>>\|\\\/\?\|\^/ 
			\contained containedin=prologBody

syntax match   prologRelations    /=\.\.\|!\|=:=\|=\?<\|=@=\|=\\=\|>=\?\|@=\?<\|@>=\?\|\\+\|\\\?=\?=\|\\\?=@=\|=/ 
			\contained containedin=prologBody

syntax region  prologCComment     fold start=/\/\*/ end=/\*\// contains=prologTODO,@Spell
syntax match   prologComment      /%.*/ contains=prologTODO,@Spell
syntax region  prologCommentFold  fold start=/^\zs\s*%/ skip=/^\s*%/ end=/^\ze\s*\([^%]\|$\)/ contains=prologComment
syntax keyword prologTODO         FIXME TODO fixme todo Fixme FixMe Todo ToDo XXX xxx contained
syntax cluster prologComments     contains=prologCComment,prologComment,prologCommentFold

syntax region  prologBody         fold start=/\(:-\|?-\)/ end=/\./ 
			\contains=@prologAll,prologPredicateWithArity
syntax region  prologDCGBody      fold start=/-->/ end=/\./ 
			\contains=@prologAll,prologDCGSpecials

syntax match   prologNumber       /\<\d\+\>/ contained
syntax match   prologNumber       /\<\d\+\.\d\+\>/ contained
syntax match   prologAtom         /\<\l\w*\>\ze\([^(]\|$\)/ contained
syntax match   prologVariable     /\<\(_\|\u\)\w*\>/ contained

syntax match   prologHead         /\<\l\w*\>/ nextgroup=prologBody,prologDCGBody skipwhite
syntax region  prologHeadWithArgs start=/\<\l\w*\>(/ end=/)/ nextgroup=prologBody,prologDCGBody contains=@prologAll

syntax match  prologOpStatement   /indexed\|discontiguous\|dynamic\|module_transparent\|multifile\|volatile\|initialization/ 
			\containedin=prologBody contained

syntax region  prologDCGSpecials  start=/{/ end=/}/ contained contains=@prologAll

syntax region  prologTuple        fold start=/\W\zs(/ end=/)/ contained containedin=prologPredicate,prologBody contains=@prologAll
syntax region  prologPredicate    start=/\<\l\w*\>\ze(/ end=/)/ contains=@prologAll
syntax match   prologPredicateWithArity /\<\l\w*\>\/\d\+/ contains=@prologBuiltIn,prologArity
syntax match   prologArity        contained /\/\d\+/
syntax cluster prologPredicates   contains=prologPredicate,prologPredicateWithArity

syntax region  prologList         start=/\[/ end=/\]/ contains=prologListDelimiters,@prologAll,prologPredicateWithArity contained
syntax match   prologListDelimiters /[,|]/ contained

syntax cluster prologAll          contains=prologList,prologPredicate,prologTuple,@prologTerms,@prologComments,prologQuoted,@prologBuiltIn,prologRelations,prologArithmetic,prologDiffList
syntax cluster prologTerms        contains=prologVariable,prologAtom,prologList,
			\prologNumber,prologErrorTerm

syntax match   prologQuotedFormat /\~\(\d*[acd\~DeEgfGiknNpqrR@st\|+wW]\|`.t\)/ contained
syntax region  prologQuoted       start=/'/ end=/'/ contains=prologQuotedFormat,@Spell

syntax match   prologErrorVariable /\<\(_\|\u\)\w*\>/
syntax region  prologErrorTerm    start=/\<\(_\|\u\)\w*\>(/ end=/)/

"""" Highlights

highlight link prologErrorVariable Error
highlight link prologErrorTerm     Error

highlight link prologOpStatement  Preproc
highlight link prologComment      Comment
highlight link prologCComment     Comment
highlight link prologTODO         TODO

highlight link prologAtom         Constant
highlight link prologVariable     Identifier
highlight link prologNumber       Number

highlight link prologISOBuiltIn   Keyword
highlight link prologSWIBuiltIn   Keyword

highlight link prologRelations    Statement

highlight link prologQuotedFormat Special
highlight link prologQuoted       String

highlight link prologPredicate    Normal
highlight link prologPredicateWithArity Normal
highlight link prologHead         Constant
highlight link prologHeadWithArgs Normal

highlight link prologBody         Statement
highlight link prologDCGBody      Statement

highlight link prologList         Type
highlight link prologListDelimiters Type
highlight link prologArity        Type
highlight link prologDCGSpecials  Type
highlight link prologTuple        Type
highlight link prologDiffList     Type

syn sync minlines=20 maxlines=50

let b:current_syntax = "prolog"
