#lang brag
program ::= stmt (/";" stmt /";"?)*
/stmt ::= @store_stmt | @del_stmt
/store_stmt ::= "store" expr expr
/del_stmt ::= "del" expr
/expr ::= @literal | ident | binop | /"(" @expr /")"
binop ::= expr BINOP expr
ident ::= STRING
/literal ::= STRING | INTEGER | BOOL
hole ::= "(??)"
