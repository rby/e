%% vim: syntax=erlang
%% Definition of the E lang grammar
%% License (we'll figure out this later just make it work)

%% add doc tag @ to Terminals

Nonterminals
seq_expr
%% won't use for now
%% literal_expr
noun
slot_expr
assign_expr
e_expr
match_bind_expr
pattern
define_expr
hide_expr
if_expr
escape_expr
catch_expr
finally_expr
call_expr
send_expr
e_exprs
object_expr
%% these 2 are related to object_expr
%% methodical_expr
%% plumbing_expr
patterns final_pattern var_pattern ignore_pattern
such_that_pattern
list_pattern
cdr_pattern
auditors
emethod emethods
verb
matcher escript behavior
texts
newlines
doc_comment.

Terminals
char float64 integer string

'&' ':=' '\n' ';' '=~' 'def' '{' '}' 'if' 'else' '(' ')' 'escape'
'catch' 'try' 'finally' '.' '<-' '_'

'/**' '*/'
':' 'var' '?' '[' ']' ',' '+'
'implements' 'method' 'match'
identifier
text
.

Rootsymbol seq_expr.

%% Expressions

%% Rules

seq_expr -> e_expr : ['$1'].
seq_expr -> e_expr newlines seq_expr : ['$1'|'$3'].
seq_expr -> e_expr ';'  seq_expr : ['$1'|'$3'].
e_expr -> char: '$1'.
e_expr -> integer: '$1'.
e_expr -> float64: '$1'.
e_expr -> string: '$1'.
e_expr -> noun : '$1'.
e_expr -> slot_expr: '$1'.
e_expr -> assign_expr: '$1'.
e_expr -> define_expr: '$1'.
e_expr -> hide_expr: '$1'.
e_expr -> if_expr: '$1'.
e_expr -> escape_expr: '$1'.
e_expr -> catch_expr: '$1'.
e_expr -> finally_expr: '$1'.
e_expr -> call_expr: '$1'.
e_expr -> send_expr: '$1'.
e_expr -> match_bind_expr: '$1'.
e_expr -> object_expr: '$1'.

noun -> identifier: '$1'.
verb -> identifier: '$1'.
slot_expr -> '&' identifier:  {slot, '$1'}.
assign_expr -> identifier ':=' e_expr : {':=', '$1', '$3'}.
match_bind_expr -> e_expr '=~' pattern : {'=~', '$1', '$3'}.
define_expr -> 'def' pattern ':=' e_expr: {def, '$2', '$4'}.
hide_expr -> '{' e_expr '}': {hide, '$2'}.
if_expr -> 'if' '(' e_expr ')' '{' e_expr '}' 'else' '{' e_expr '}': {if_statement, '$3', '$6', '$10'}.
escape_expr -> 'escape' pattern '{' e_expr '}' 'catch' pattern '{' e_expr '}': {escape, '$2', '$4', '$7', '$9'}.
catch_expr -> 'try' '{' e_expr '}' 'catch' pattern '{' e_expr '}': {catch_statement, '$3', '$6', '$8'}.
finally_expr -> 'try' '{' e_expr '}' 'finally' pattern '{' e_expr '}': {finally, '$3', '$6', '$8'}.
call_expr -> e_expr '.'  verb '(' e_exprs ')': {call, '$1', '$3', '$5'}.
send_expr -> e_expr '<-' verb '(' e_exprs ')': {send, '$1', '$3', '$5'}.
object_expr -> 'def' '_' auditors behavior: {object, "", underscore, '$3', '$4'}.
object_expr -> 'def' string auditors behavior: {object, "", '$2', '$3', '$4'}.
object_expr -> doc_comment object_expr : setelement(2, '$2', '$1').

behavior -> escript : '$1'.
behavior -> matcher : '$1'.

pattern -> ignore_pattern: '$1'.
pattern -> final_pattern: '$1'.
pattern -> var_pattern: '$1'.
pattern -> such_that_pattern: '$1'.
pattern -> list_pattern: '$1'.
pattern -> cdr_pattern: '$1'.

final_pattern -> noun ':' e_expr: {final_pattern, '$1', '$3'}.
var_pattern -> 'var' noun ':' e_expr: {var_pattern, '$2', '$4'}.
ignore_pattern -> '_' : ignore_pattern.
such_that_pattern -> pattern '?' e_expr: {such_that_pattern, '$1', '$3'}.
list_pattern -> '[' patterns ']':  {list_patterns, '$2'}.
cdr_pattern -> list_pattern '+' pattern: {cdr_pattern, '$1', '$3'}.


auditors -> '$empty': empty_auditors.
auditors -> 'implements' e_exprs: {implements, '$2'}.

emethod -> 'method' verb '(' patterns ')' ':' e_expr '{' seq_expr '}': {emethod, "", '$2', '$4', '$7', '$9'}.
emethod -> doc_comment emethod: setelement(2, '$2', '$1').

matcher -> 'match' pattern '{' seq_expr '}': {match, '$2', '$4'}.
escript -> '{'  '}' : {escript, [], nil}.
escript -> '{' matcher '}': {escript, [], '$2'}.
escript -> '{' emethods '}': {escript, '$2', nil}.
escript -> '{' emethods matcher '}': {escript, '$2', '$3' }.

%%% Helpers
emethods -> emethod : ['$1'].
emethods -> emethod ',' emethods: ['$1' | '$2'].

patterns -> pattern : ['$1'].
patterns -> pattern ',' patterns: ['$1' | '$2'].

e_exprs -> e_expr : ['$1'].
e_exprs -> e_expr ',' e_exprs: ['$1' | '$2'].

doc_comment -> '/**' '*/' : {comment, []}.
doc_comment -> '/**' texts '*/' : {comment, []}.
texts -> text : ['$1'].
texts -> text newlines texts: ['$1' | '$2'].

newlines -> '\n'.
newlines -> '\n' newlines.

Header
"%% This is the Elang compiler"
"".

Erlang code.
