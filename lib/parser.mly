
%token <string> UID
%token <string> LID
%token <string> STR
%token <int> INT
%token <float> FLOAT

(*%token IN*)
(*%token REC*)

%token ARR
%token FATARR
%token DOT
%token STAR
%token QMARK
%token QQ
%token BAR
%token AMP
%token SLASH
%token THEN
%token IMPORT
%token SumOver
%token ELSE
%token SYMBOLIC
%token AT
%token LT
%token EQ
%token GT
%token COMMA
%token SEMI
%token CONSTRAINT
%token PREFERENCE
%token PRODUCTIONS
%token PREF
%token EMIT
%token TOSTR
%token WHERE
%token NAT
%token ANTIPREF
%token PARSIMONY
%token BUNDLE
%token BY
%token LPAREN
%token RPAREN
%token INFTY
%token LET
%token PLUS
%token OF
%token OPTIONS
%token IF
%token IFF
%token IN
%token TRUE
%token FALSE
%token FROM
%token OR
%token NOT
%token FOR
%token DASH
%token POSITIVE_EXAMPLES
%token NEGATIVE_EXAMPLES
%token START
%token NAMED
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET

%token EOF

%start problem
%type <Problem.t> problem
%start just_spaces
%type <Space.t list> just_spaces

%%

problem:
    | imports=imports d=decls START u=UID p=positives n=negatives EOF
      { Problem.make
          ~imports
          ~prods:(MyStdLib.fst5 d)
          ~constraints:(MyStdLib.snd5 d)
          ~start:(MyStdLib.Id.create u)
          ~positives:p
          ~negatives:n
          ~prefs:(MyStdLib.trd5 d)
          ~global_symbolics:(MyStdLib.for5 d)
          ~emits:(MyStdLib.fth5 d)
      }

imports:
    | IMPORT s=STR imps=imports
      { s::imps }
    | { [] }

decls:
    | d=decl DOT ds=decls
      { (MyStdLib.fst5 d @ MyStdLib.fst5 ds,MyStdLib.snd5 d @ MyStdLib.snd5 ds, MyStdLib.trd5 d @ MyStdLib.trd5 ds, MyStdLib.for5 d @ MyStdLib.for5 ds, MyStdLib.fth5 d @ MyStdLib.fth5 ds) }
    |
      { ([],[],[],[],[]) }

decl:
    | CONSTRAINT b=bool_exp
      { ([],[b],[],[],[]) }
    | PREFERENCE p=pholder
      { ([],[],[p],[],[]) }
    | SYMBOLIC l=LID IN r=range
      { ([],[],[],[(MyStdLib.Id.create l,r)],[]) }
    | EMIT e=eholder
      { ([],[],[],[],[e]) }
    | mp=mprod
      { ([mp],[],[],[],[]) }

mprod:
    | u=UID rbs=range_bindings ARR r=regex
      { MProd.make ~head:(MyStdLib.Id.create u,rbs) ~spaces:[Space.Singleton (false,Exp.True,None,r)] }
    | u=UID rbs=range_bindings ARR ss=spaces
      { MProd.make ~head:(MyStdLib.Id.create u,rbs) ~spaces:ss }

range_bindings:
    | rb=range_binding rbs=range_bindings
      { rb::rbs }
    |
      { [] }

range_binding:
    | LBRACE l=LID IN r=range RBRACE
      { (MyStdLib.Id.create l,r) }

range:
    | b1=left_bound i1=int_exp COMMA i2=int_exp b2=right_bound
      { (b1,i1,b2,i2) }
    | NAT
      { (Exp.Inclusive,Exp.Int 0,Exp.Exclusive,Exp.Infty(20,20)) }

left_bound:
    | LBRACKET
      { Exp.Inclusive }
    | LPAREN
      { Exp.Exclusive }

right_bound:
    | RBRACKET
      { Exp.Inclusive }
    | RPAREN
      { Exp.Exclusive }

indexed_id:
    | u=UID is=indices
      { (MyStdLib.Id.create u,is) }

indices:
    | LBRACE i=int_exp RBRACE is=indices
      { i::is }
    |
      { [] }

bool_exp:
    | iid=indexed_id
      { Exp.IndexedId iid }
    | c1=bool_exp AMP c2=bool_exp
      { Exp.Conjunct (c1,c2) }
    | NOT c=bool_exp
      { Exp.Not c }
    | FALSE
      { Exp.False }
    | TRUE
      { Exp.True }
    | c1=bool_exp BAR c2=bool_exp
      { Exp.Disjunct (c1,c2) }
    | c1=bool_exp IFF c2=bool_exp
      { Exp.Iff (c1,c2) }
    | c1=bool_exp FATARR c2=bool_exp
      { Exp.Implies (c1,c2) }
    | AMP AMP rb=range_binding b=bool_exp
      { Exp.AndOver (rb,b) }
    | BAR BAR rb=range_binding b=bool_exp
      { Exp.OrOver (rb,b) }
    | i1=int_exp EQ i2=int_exp
      { Exp.EQ (i1,i2) }
    | i1=int_exp LT i2=int_exp
      { Exp.LT (i1,i2) }
    | i1=int_exp GT i2=int_exp
      { Exp.GT (i1,i2) }
    | LPAREN c=bool_exp RPAREN
      { c }

string_exp:
    | s1=string_exp PLUS s2=string_exp
      { Exp.SConcat (s1,s2) }
    | LPAREN s=string_exp RPAREN
      { s }
    | s=STR
      { Exp.SBase s }
    | TOSTR LPAREN i=int_exp RPAREN
      { Exp.SInt i }

pholder:
    | p=preference
      { Pholder.Singleton p }
    | rb=range_binding p=pholder
      { Pholder.PrefBinding (rb,p) }

eholder:
    | se=string_exp IF b=bool_exp
      { Eholder.Singleton (se,b) }
    | se=string_exp
      { Eholder.Singleton (se,Exp.True) }
    | rb=range_binding e=eholder
      { Eholder.EmitBinding (rb,e) }

preference:
    | PREF f=FLOAT rs=rule_list_concs
      { Preferences.UPref (rs,f) }
    | PREF f=FLOAT LPAREN be=bool_exp RPAREN
      { Preferences.USATForm (be,f) }
    | ANTIPREF f=FLOAT rs=rule_list_concs
      { Preferences.UAntiPref (rs,f) }
    | PARSIMONY f=FLOAT rs=rule_list_concs
      { Preferences.UParsimony (rs,f) }
    | BUNDLE f=FLOAT rs=rule_list_concs
      { Preferences.UBundle (rs,f) }

rule_list_concs:
    | rl=rule_list AT rlc=rule_list_concs
      { rl::rlc }
    | rl=rule_list
      { [rl] }

rule_list:
    | PRODUCTIONS LPAREN iid=indexed_id RPAREN
      { Exp.Productions(iid) }
    | LBRACKET iids=iid_list RBRACKET
      { Exp.Concrete(iids) }
    | LBRACKET LBRACKET iid=indexed_id BAR rbs=range_bindings RBRACKET RBRACKET
      { Exp.Comprehension(rbs,iid) }

iid_list:
    | iid=indexed_id SEMI iids=iid_list
      { iid::iids }
    | iid=indexed_id
      { [iid] }

just_spaces:
    | s=spaces EOF
      { s }

spaces:
    | s=space ss=spaces
      { s::ss }
    |
      { [] }

space:
    | QQ rb=range_binding LPAREN ss=spaces RPAREN
      { Space.BigQ (rb,ss) }
    | LET SYMBOLIC l=LID IN r=range SEMI ss=spaces
      { Space.BigQ ((MyStdLib.Id.create l,r),ss) }
    | QMARK r=regex
      { Space.Singleton (false,Exp.True,None,r) }
    | QMARK r=regex NAMED iid=indexed_id
      { Space.Singleton (false,Exp.True,Some iid,r) }
    | QMARK IF LPAREN b=bool_exp RPAREN THEN r=regex
      { Space.Singleton (false,b,None,r) }
    | QMARK IF LPAREN b=bool_exp RPAREN THEN r=regex NAMED iid=indexed_id
      { Space.Singleton (false,b,Some iid,r) }

int_exp:
    | i1=int_exp PLUS i2=int_exp
      { Exp.Plus (i1,i2) }
    | i1=int_exp DASH i2=int_exp
      { Exp.Minus (i1,i2) }
    | l=LID
      { Exp.Var (MyStdLib.Id.create l) }
    | i=INT
      { Exp.Int i }
    | IF b=bool_exp THEN i1=int_exp ELSE i2=int_exp
      { Exp.ITE (b,i1,i2) }
    | LPAREN i=int_exp RPAREN
      { i }
    | SumOver rb=range_binding i=int_exp
      { Exp.SumOver(rb,i) }
    | i=infty
      { i }
    | BAR rlc=rule_list_concs BAR
      { Exp.SizeOf rlc }

infty:
    | INFTY
      { Exp.Infty(20,20) }
    | INFTY BY i=INT
      { Exp.Infty(20,i) }
    | INFTY FROM i=INT
      { Exp.Infty(i,20) }
    | INFTY FROM i1=INT BY i2=INT
      { Exp.Infty(i1,i2) }

regex:
    | LBRACKET l1=STR DASH l2=STR RBRACKET
      { let c1 = (String.get l1 0) in 
        let c2 = (String.get l2 0) in 
        Regex.CharSet [CharRange.CharRange.create_from_chars c1 c2]}
    | SLASH
      {
        Regex.CharSet [CharRange.CharRange.create_from_single '\\']
      }
    | LBRACKET i1=INT DASH i2=INT RBRACKET
        {Regex.CharSet [CharRange.CharRange.create_from_ints i1 i2]}
    | r1=regex BAR r2=regex
      { Regex.disjunct [r1;r2] }
    | r1=regex r2=regex
      { Regex.Conjunct [r1;r2] }
    | r1=regex STAR
      { Regex.Star r1 }
    | u=UID is=indices
      { Regex.Nonterminal (MyStdLib.Id.create u,is) }
    | s=STR
      { Regex.Conjunct (List.map (fun c -> Regex.CharSet [CharRange.CharRange.create_from_single c]) (Core.String.to_list (MyStdLib.undelimit_string s))) }
    | LPAREN r=regex RPAREN
      { r }

positives:
    | POSITIVE_EXAMPLES exs=examples
      { exs }
    | { [] }

negatives:
    | NEGATIVE_EXAMPLES exs=examples
      { exs }
    | { [] }

examples:
    | LBRACE n=nonempty_examples RBRACE
      { n }
    | LBRACE RBRACE
      { [] }

nonempty_examples:
    | e=example COMMA n=nonempty_examples
      { e::n }
    | e=example
      { [e] }

example:
    | DOT
      { "" }
    | s=STR
          { MyStdLib.undelimit_string s }


