S -> SelectStatement
     | (SelectStatement ";")
     | (SelectStatement EmptySpace "LIMIT" EmptySpace Number)
     | (SelectStatement EmptySpace "LIMIT" EmptySpace Number ";").


SelectStatement ->
  ("WITH" EmptySpace Identifier EmptySpace "AS" EmptySpace "(" OptionalEmptyspace SelectStatement OptionalEmptyspace ")" OptionalEmptyspace SelectStatement)
  | ("," EmptySpace Identifier EmptySpace "AS" EmptySpace "(" OptionalEmptyspace SelectStatement OptionalEmptyspace ")" OptionalEmptyspace SelectStatement)
  | ("SELECT" EmptySpace Columns EmptySpace "FROM" EmptySpace Table)
  | ("SELECT" EmptySpace Columns EmptySpace "FROM" EmptySpace Table EmptySpace "WHERE" EmptySpace Expression).

Column ->
  Expression
  | Expression EmptySpace "AS" EmptySpace Identifier.
Columns -> 
  Column
  | (Column OptionalEmptyspace "," OptionalEmptyspace Column*).

Table -> 
  (Table EmptySpace "AS" EmptySpace Identifier)
  | Identifier
  | ("UNNEST([" (Expression "," OptionalEmptyspace)* Expression "])")
  | ("(" OptionalEmptyspace SelectStatement OptionalEmptyspace ")")
  | ("(" OptionalEmptyspace SelectStatement OptionalEmptyspace ")" EmptySpace "AS" EmptySpace Identifier)
  | (Table EmptySpace JoinExpr EmptySpace Table EmptySpace "ON" EmptySpace Expression).

JoinExpr ->
  "JOIN"
  | "LEFT JOIN"
  | "INNER JOIN"
  | "OUTER JOIN".

IdentifierChar -> (["a"-"z"] | ["A"-"Z"] | "_" | ["0"-"9"] | "-" | "*").
IdentifierChars -> 
  IdentifierChar
  | IdentifierChar IdentifierChar*.
EscapedIdentifierChar -> 
  ? IdentifierChar
  ? "." named PeriodQualifiedProjectName
  ? ":" named ColonQualifiedProjectName
  ? " ".
EscapedIdentifierChars ->
   EscapedIdentifierChar
  | EscapedIdentifierChar EscapedIdentifierChar*.
Identifier ->
  ? IdentifierChars
  ? Identifier "." Identifier
  ? "`" EscapedIdentifierChars "`" named BacktickIdentifier
  ? "[" EscapedIdentifierChars "]" named BracketIdentifier.

Expression -> 
  Number
  | String
  | Identifier 
  | (Expression OptionalEmptyspace Binop OptionalEmptyspace Expression) 
  | ("(" OptionalEmptyspace Expression OptionalEmptyspace ")")
  | ("[" (Expression ",")* Expression "]") 
  | Function "(" OptionalEmptyspace Expression OptionalEmptyspace ")".
Number -> ["0"-"9"] ["0"-"9"]*.
FunctionChar -> (["a"-"z"] | ["A"-"Z"] | "_").
Function -> FunctionChar | (FunctionChar FunctionChar*).
StringChar -> (["a"-"z"] | ["A"-"Z"] | "_" | ["0"-"9"] | "-" | "." | "*" | "[" | "]" | "(" | ")" | " ").
String -> ("\"" StringChar* "\"") | ("\'" StringChar* "\'").

Binop ->
  "+"
  | "-"
  | "*"
  | "/"
  | "="
  | ">"
  | ">="
  | "<"
  | "<="
  | EmptySpace "IS" EmptySpace
  | EmptySpace "IN" EmptySpace
  | EmptySpace "AND" EmptySpace
  | EmptySpace "OR" EmptySpace.


Newline -> "\n" | "\r\n".
Space -> " ".
Whitespace -> 
  Space
  | Space Space*.
EmptySpaceChar -> 
  Space | Newline.
EmptySpace -> EmptySpaceChar | (EmptySpaceChar EmptySpaceChar*).

OptionalEmptyspace ->
  ""
  | Whitespace
  | Newline.
start S