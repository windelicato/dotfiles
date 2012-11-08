" Vim syntax file
" Language:     Haskell
" Maintainer:   Rui Carlos A. Goncalves <rcgoncalves.pt@gmail.com>
" Last Change:  September 15, 2010
"
" Version:      2.0
" Url:          http://rcgoncalves.net/files/haskell.zip
"
" Original Author: John Williams <jrw@pobox.com>

" Remove any old syntax stuff hanging around
if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif


" (Qualified) identifiers (no default highlighting)
syn match ConId         "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=\<[A-Z][a-zA-Z0-9_']*\>"
syn match VarId         "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=\<[a-z][a-zA-Z0-9_']*\>"


" Infix operators--most punctuation characters and any (qualified) identifier
" enclosed in `backquotes`. An operator starting with : is a constructor,
" others are variables (e.g. functions).
syn match hsVarSym      "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[-!#$%&\*\+/<=>\?@\\^|~.][-!#$%&\*\+/<=>\?@\\^|~:.]*"
syn match hsConSym      "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=:[-!#$%&\*\+./<=>\?@\\^|~:]*"
syn match hsVarSym      "`\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[a-z][a-zA-Z0-9_']*`"
syn match hsConSym      "`\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[A-Z][a-zA-Z0-9_']*`"


" Reserved symbols--cannot be overloaded.
syn match hsDelimiter  "(\|)\|\[\|\]\|,\|;\|_\|{\|}"


" Strings and constants
syn match   hsSpecialChar	contained "\\\([0-9]\+\|o[0-7]\+\|x[0-9a-fA-F]\+\|[\"\\'&\\abfnrtv]\|^[A-Z^_\[\\\]]\)"
syn match   hsSpecialChar	contained "\\\(NUL\|SOH\|STX\|ETX\|EOT\|ENQ\|ACK\|BEL\|BS\|HT\|LF\|VT\|FF\|CR\|SO\|SI\|DLE\|DC1\|DC2\|DC3\|DC4\|NAK\|SYN\|ETB\|CAN\|EM\|SUB\|ESC\|FS\|GS\|RS\|US\|SP\|DEL\)"
syn match   hsSpecialCharError	contained "\\&\|'''\+"
syn region  hsString		start=+"+  skip=+\\\\\|\\"+  end=+"+  contains=hsSpecialChar
syn match   hsCharacter		"[^a-zA-Z0-9_']'\([^\\]\|\\[^']\+\|\\'\)'"lc=1 contains=hsSpecialChar,hsSpecialCharError
syn match   hsCharacter		"^'\([^\\]\|\\[^']\+\|\\'\)'" contains=hsSpecialChar,hsSpecialCharError
syn match   hsNumber		"\<[0-9]\+\>\|\<0[xX][0-9a-fA-F]\+\>\|\<0[oO][0-7]\+\>"
syn match   hsFloat		"\<[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\=\>"


" Keyword definitions. These must be patters instead of keywords
" because otherwise they would match as keywords at the start of a
" "literate" comment (see lhs.vim).
syn match hsModule	"\<module\>"
syn match hsImport	"\<import\>.*"he=s+6 contains=hsImportMod,hsString,hsType,hsFunction
syn match hsImportMod	contained "\<\(as\|qualified\|hiding\)\>"
syn match hsInfix	"\<\(infix\|infixl\|infixr\)\>"
syn match hsStructure	"\<\(class\|data\|deriving\|instance\|default\|where\)\>"
syn match hsTypedef	"\<\(type\|newtype\)\>"
syn match hsStatement	"\<\(do\|return\|case\|of\|let\|in\)\>"
syn match hsConditional	"\<\(if\|then\|else\)\>"
" Haskell 2010 keywords
syn match hsForeign	"\<foreign\>"
syn match hsForeignType "\<\(Int8\|Int16\|Int32\|Int64\|Word8\|Word16\|Word32\|Word64\|Ptr\|FunPtr\|StablePtr\)\>"

" Types from the standard prelude.
syn match hsType        "\<\(Bool\|Maybe\|Either\|Ordering\)\>"
syn match hsType        "\<\(Char\|String\|Int\|Integer\|Float\|Double\|Rational\|IO\)\>"
syn match hsType        "\<\(ReadS\|ShowS\)\>"
syn match hsType        "\<\(FilePath\|IOError\)\>"

" Classes from the standard prelude
syn match hsType        "\<\(Eq\|Ord\|Enum\|Bounded\|Num\|Real\|Integral\|Fractional\|Floating\|RealFrac\|RealFloat\|Monad\|Functor\)\>"
syn match hsType        "\<\(Show\|Read\)\>"


" Constants from the standard prelude.
syn match hsBoolean     "\<\(True\|False\)\>"
syn match hsMaybe       "\<\(Nothing\|Just\)\>"
syn match hsConstant    "\<\(Left\|Right\)\>"
syn match hsOrdering    "\<\(LT\|EQ\|GT\)\>"


" Functions from the standard prelude.
syn match hsFunction    "\<\(compare\|max\|min\)\>"
syn match hsFunction    "\<\(succ\|pred\|toEnum\|fromEnum\|enumFrom\|enumFromThen\|enumFromTo\|enumFromThenTo\)\>"
syn match hsFunction    "\<\(minBound\|maxBound\)\>"
syn match hsFunction    "\<\(negate\|abs\|signum\|fromInteger\)\>"
syn match hsFunction    "\<toRational\>"
syn match hsFunction    "\<\(quot\|rem\|div\|mod\|quotRem\|divMod\|toInteger\)\>"
syn match hsFunction    "\<\(recip\|fromRational\)\>"
syn match hsFunction    "\<\(pi\|exp\|log\|sqrt\|logBase\|sin\|cos\|tan\|asin\|acos\|atan\|sinh\|cosh\|tanh\|asinh\|acosh\|atanh\)\>"
syn match hsFunction    "\<\(properFraction\|truncate\|round\|ceiling\|floor\)\>"
syn match hsFunction    "\<\(floatRadix\|floatDigits\|floatRange\|decodeFloat\|encodeFloat\|exponent\|significand\|scaleFloat\|isNaN\|isInfinite\|isDenormalized\|isIEEE\|isNegativeZero\|atan2\)\>"
syn match hsFunction    "\<\(return\|fail\)\>"
syn match hsFunction    "\<\(fmap\)\>"
syn match hsFunction    "\<\(mapM\|mapM_\|sequence\|sequence_\)\>"
syn match hsFunction    "\<\(maybe\|either\)\>"
syn match hsFunction    "\<\(not\|otherwise\)\>"
syn match hsFunction    "\<\(subtract\|even\|odd\|gcd\|lcm\)\>"
syn match hsFunction    "\<\(fromIntegral\|realToFrac\)\>"
syn match hsFunction    "\<\(fst\|snd\|curry\|uncurry\|id\|const\|flip\|until\)\>"
syn match hsFunction    "\<\(asTypeOf\|error\|undefined\)\>"
syn match hsFunction    "\<\(seq\)\>"
syn match hsFunction    "\<\(map\|filter\|concat\|concatMap\)\>"
syn match hsFunction    "\<\(head\|last\|tail\|init\|null\|length\)\>"
syn match hsFunction    "\<\(foldl\|foldl1\|scanl\|scanl1\|foldr\|foldr1\|scanr\|scanr1\)\>"
syn match hsFunction    "\<\(iterate\|repeat\|replicate\|cycle\)\>"
syn match hsFunction    "\<\(take\|drop\|splitAt\|takeWhile\|dropWhile\|span\|break\)\>"
syn match hsFunction    "\<\(lines\|words\|unlines\|unwords\|reverse\|and\|or\)\>"
syn match hsFunction    "\<\(any\|all\|elem\|notElem\|lookup\)\>"
syn match hsFunction    "\<\(sum\|product\|maximum\|minimum\)\>"
syn match hsFunction    "\<\(zip\|zip3\|zipWith\|zipWith3\|unzip\|unzip3\)\>"
syn match hsFunction    "\<\(readsPrec\|readList\)\>"
syn match hsFunction    "\<\(showsPrec\|show\|showList\)\>"
syn match hsFunction    "\<\(reads\|shows\|read\|lex\)\>"
syn match hsFunction    "\<\(showChar\|showString\|readParen\|showParen\)\>"
syn match hsFunction    "\<\(ioError\|userError\|catch\)\>"
syn match hsFunction    "\<\(putChar\|putStr\|putStrLn\|print\)\>"
syn match hsFunction    "\<\(getChar\|getLine\|getContents\|interact\)\>"
syn match hsFunction    "\<\(readFile\|writeFile\|appendFile\|readIO\|readLn\)\>"


" Comments
syn match   hsLineComment      "--.*"
syn region  hsBlockComment     start="{-"  end="-}" contains=hsBlockComment
syn region  hsPragma	       start="{-#" end="#-}"

" Literate comments--any line not starting with '>' is a comment.
if exists("b:hs_literate_comments")
  syn region  hsLiterateComment   start="^" end="^>"
endif


if !exists("hs_minlines")
  let hs_minlines = 50
endif
exec "syn sync lines=" . hs_minlines

if version >= 508 || !exists("did_hs_syntax_inits")
  if version < 508
    let did_hs_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  hi link hsModule              hsStructure
  hi link hsImport              Include
  hi link hsImportMod           hsImport
  hi link hsForeign		hsImport
  hi link hsInfix               PreProc
  hi link hsStructure           Structure
  hi link hsStatement           Statement
  hi link hsConditional         Conditional
  hi link hsSpecialChar	        SpecialChar
  hi link hsTypedef             Typedef
  hi link hsVarSym              hsOperator
  hi link hsConSym              hsOperator
  hi link hsOperator            Operator
  hi link hsSpecialCharError    Error
  hi link hsString              String
  hi link hsCharacter           Character
  hi link hsNumber              Number
  hi link hsFloat               Float
  hi link hsConditional         Conditional
  hi link hsLiterateComment     hsComment
  hi link hsBlockComment        hsComment
  hi link hsLineComment         hsComment
  hi link hsComment             Comment
  hi link hsPragma              SpecialComment
  hi link hsBoolean             Boolean
  hi link hsType                Type
  hi link hsForeignType		Type
  hi link hsFunction            Function
  hi link hsMaybe               hsEnumConst
  hi link hsOrdering            hsEnumConst
  hi link hsEnumConst           Constant
  hi link hsConstant            Constant
  hi link hsDebug               Debug

  delcommand HiLink
endif

let b:current_syntax = "haskell"
