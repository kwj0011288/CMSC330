open Types

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let rec tokenize input =
  let numre = Re.compile (Re.Perl.re "^(-?[0-9]+)") in
  let wsre = Re.compile (Re.Perl.re "^\\s+") in
  let token_bool = Re.compile (Re.Perl.re "^(true|false)") in
  let token_id = Re.compile (Re.Perl.re "^[a-zA-Z_][a-zA-Z0-9_]*") in
  let token_string = Re.compile (Re.Perl.re "^(\"[^\"]*\")") in
  let lparen = Re.compile (Re.Perl.re "^\\(") in
  let rparen = Re.compile (Re.Perl.re "^\\)") in
  let lcurly = Re.compile (Re.Perl.re "^\\{") in
  let rcurly = Re.compile (Re.Perl.re "^\\}") in
  let dot = Re.compile (Re.Perl.re "^\\.") in
  let equal = Re.compile (Re.Perl.re "^=") in
  let notequal = Re.compile (Re.Perl.re "^<>") in
  let greater = Re.compile (Re.Perl.re "^>") in
  let less = Re.compile (Re.Perl.re "^<") in
  let greaterequal = Re.compile (Re.Perl.re "^>=") in
  let lessequal = Re.compile (Re.Perl.re "^<=") in
  let token_or = Re.compile (Re.Perl.re "^\\|\\|") in
  let token_add = Re.compile (Re.Perl.re "^\\&\\&") in
  let token_not = Re.compile (Re.Perl.re "^(not)") in
  let token_if = Re.compile (Re.Perl.re "^(if)") in
  let token_then = Re.compile (Re.Perl.re "^(then)") in
  let token_else = Re.compile (Re.Perl.re "^(else)") in
  let add = Re.compile (Re.Perl.re "^\\+") in
  let sub = Re.compile (Re.Perl.re "^-") in
  let mult = Re.compile (Re.Perl.re "^\\*") in
  let div = Re.compile (Re.Perl.re "^/") in
  let concat = Re.compile (Re.Perl.re "^\\^") in
  let token_let = Re.compile (Re.Perl.re "(let)") in
  let token_def = Re.compile (Re.Perl.re "^(def)") in
  let token_in = Re.compile (Re.Perl.re "^(in)") in
  let token_rec = Re.compile (Re.Perl.re "^(rec)") in
  let token_fun = Re.compile (Re.Perl.re "^(fun)") in
  let arrow = Re.compile (Re.Perl.re "^->") in
  let doublesemi = Re.compile (Re.Perl.re "^;;") in
  let semi = Re.compile (Re.Perl.re "^;") in
  let neg_num_re = Re.compile (Re.Perl.re "^\\((-?[0-9]+)\\)") in
  let len = String.length input in
  if input = "" then [] 
  else
    if Re.execp token_bool input then 
    let boolgroup = Re.exec token_bool input in
    let boolmatch = Re.Group.get boolgroup 0 in
    let boollen = String.length boolmatch in
    let rest_len = len - boollen in match boolmatch with
    | "true" -> Tok_Bool true :: tokenize (String.sub input boollen rest_len)
    | "false" -> Tok_Bool false :: tokenize (String.sub input boollen rest_len)
    | _ -> raise (InvalidInputException "bool error")  
  else
    if Re.execp token_id input then
    let idgroup = Re.exec token_id input in
    let idmatch = Re.Group.get idgroup 0 in
    let idlen = String.length idmatch in
    let rest_len = len - idlen in
    match idmatch with
    | "let" -> Tok_Let :: tokenize (String.sub input idlen rest_len)
    | "def" -> Tok_Def :: tokenize (String.sub input idlen rest_len)
    | "in" -> Tok_In :: tokenize (String.sub input idlen rest_len)
    | "rec" -> Tok_Rec :: tokenize (String.sub input idlen rest_len)
    | "fun" -> Tok_Fun :: tokenize (String.sub input idlen rest_len)
    | "if" -> Tok_If :: tokenize (String.sub input idlen rest_len)
    | "then" -> Tok_Then :: tokenize (String.sub input idlen rest_len)
    | "else" -> Tok_Else :: tokenize (String.sub input idlen rest_len)
    | "not" -> Tok_Not :: tokenize (String.sub input idlen rest_len)
    | _ -> Tok_ID idmatch :: tokenize (String.sub input idlen rest_len)
  else
    if Re.execp neg_num_re input then
    let numgroup = Re.exec neg_num_re input in
    let nummatch = Re.Group.get numgroup 1 in
    let numlen = String.length (Re.Group.get numgroup 0) in 
    let rest_len = len - numlen in
    Tok_Int (int_of_string nummatch) :: tokenize (String.sub input numlen rest_len)
  else
    if Re.execp arrow input then 
      let arrowgroup = Re.exec arrow input in
      let arrowmatch = Re.Group.get arrowgroup 0 in
      let arrowlen = String.length arrowmatch in
      let rest_len = len - arrowlen in
      Tok_Arrow :: tokenize (String.sub input arrowlen rest_len)
  else
    if Re.execp sub input then 
    let subgroup = Re.exec sub input in
    let submatch = Re.Group.get subgroup 0 in
    let sublen = String.length submatch in
    let rest_len = len - sublen in
    Tok_Sub :: tokenize (String.sub input sublen rest_len)
  else 
    if Re.execp numre input then 
    let numgroup = Re.exec numre input in
    let nummatch = Re.Group.get numgroup 0 in
    let numlen = String.length nummatch in
    let rest_len = len - numlen in
    Tok_Int (int_of_string nummatch) :: tokenize (String.sub input numlen rest_len)
  else 
    if Re.execp wsre input then 
    let wsgroup = Re.exec wsre input in
    let wsmatch = Re.Group.get wsgroup 0 in
    let wslen = String.length wsmatch in
    let rest_len = len - wslen in
    tokenize (String.sub input wslen rest_len)
  else
    if Re.execp token_string input then 
    let stringgroup = Re.exec token_string input in
    let stringmatch = Re.Group.get stringgroup 0 in
    let stringlen = String.length stringmatch in
    let rest_len = len - stringlen in
    Tok_String (String.sub stringmatch 1 (stringlen - 2)) :: tokenize (String.sub input stringlen rest_len)
  else
    if Re.execp lparen input then 
    let lparengroup = Re.exec lparen input in
    let lparenmatch = Re.Group.get lparengroup 0 in
    let lparenlen = String.length lparenmatch in
    let rest_len = len - lparenlen in
    Tok_LParen :: tokenize (String.sub input lparenlen rest_len)
  else
    if Re.execp rparen input then 
    let rparengroup = Re.exec rparen input in
    let rparenmatch = Re.Group.get rparengroup 0 in
    let rparenlen = String.length rparenmatch in
    let rest_len = len - rparenlen in
    Tok_RParen :: tokenize (String.sub input rparenlen rest_len)
  else
    if Re.execp lcurly input then 
    let lcurlygroup = Re.exec lcurly input in
    let lcurlymatch = Re.Group.get lcurlygroup 0 in
    let lcurlylen = String.length lcurlymatch in
    let rest_len = len - lcurlylen in
    Tok_LCurly :: tokenize (String.sub input lcurlylen rest_len)
  else
    if Re.execp rcurly input then 
    let rcurlygroup = Re.exec rcurly input in
    let rcurlymatch = Re.Group.get rcurlygroup 0 in
    let rcurlylen = String.length rcurlymatch in
    let rest_len = len - rcurlylen in
    Tok_RCurly :: tokenize (String.sub input rcurlylen rest_len)
  else
    if Re.execp greaterequal input then 
    let greaterequalgroup = Re.exec greaterequal input in
    let greaterequalmatch = Re.Group.get greaterequalgroup 0 in
    let greaterequallen = String.length greaterequalmatch in
    let rest_len = len - greaterequallen in
    Tok_GreaterEqual :: tokenize (String.sub input greaterequallen rest_len)
  else
    if Re.execp lessequal input then 
    let lessequalgroup = Re.exec lessequal input in
    let lessequalmatch = Re.Group.get lessequalgroup 0 in
    let lessequallen = String.length lessequalmatch in
    let rest_len = len - lessequallen in
    Tok_LessEqual :: tokenize (String.sub input lessequallen rest_len)
  else
    if Re.execp dot input then 
    let dotgroup = Re.exec dot input in
    let dotmatch = Re.Group.get dotgroup 0 in
    let dotlen = String.length dotmatch in
    let rest_len = len - dotlen in
    Tok_Dot :: tokenize (String.sub input dotlen rest_len)
  else
    if Re.execp equal input then 
    let equalgroup = Re.exec equal input in
    let equalmatch = Re.Group.get equalgroup 0 in
    let equallen = String.length equalmatch in
    let rest_len = len - equallen in
    Tok_Equal :: tokenize (String.sub input equallen rest_len)
  else
    if Re.execp notequal input then 
    let notequalgroup = Re.exec notequal input in
    let notequalmatch = Re.Group.get notequalgroup 0 in
    let notequallen = String.length notequalmatch in
    let rest_len = len - notequallen in
    Tok_NotEqual :: tokenize (String.sub input notequallen rest_len)
  else
    if Re.execp greater input then 
    let greatergroup = Re.exec greater input in
    let greatermatch = Re.Group.get greatergroup 0 in
    let greaterlen = String.length greatermatch in
    let rest_len = len - greaterlen in
    Tok_Greater :: tokenize (String.sub input greaterlen rest_len)
  else
    if Re.execp less input then 
    let lessgroup = Re.exec less input in
    let lessmatch = Re.Group.get lessgroup 0 in
    let lesslen = String.length lessmatch in
    let rest_len = len - lesslen in
    Tok_Less :: tokenize (String.sub input lesslen rest_len)
  else
    if Re.execp token_or input then 
    let orgroup = Re.exec token_or input in
    let ormatch = Re.Group.get orgroup 0 in
    let orlen = String.length ormatch in
    let rest_len = len - orlen in
    Tok_Or :: tokenize (String.sub input orlen rest_len)
  else
    if Re.execp token_add input then 
    let andgroup = Re.exec token_add input in
    let andmatch = Re.Group.get andgroup 0 in
    let andlen = String.length andmatch in
    let rest_len = len - andlen in
    Tok_And :: tokenize (String.sub input andlen rest_len)
  else
    if Re.execp token_not input then 
    let notgroup = Re.exec token_not input in
    let notmatch = Re.Group.get notgroup 0 in
    let notlen = String.length notmatch in
    let rest_len = len - notlen in
    Tok_Not :: tokenize (String.sub input notlen rest_len)
  else
    if Re.execp token_if input then 
    let ifgroup = Re.exec token_if input in
    let ifmatch = Re.Group.get ifgroup 0 in
    let iflen = String.length ifmatch in
    let rest_len = len - iflen in
    Tok_If :: tokenize (String.sub input iflen rest_len)
  else
    if Re.execp token_then input then 
    let thengroup = Re.exec token_then input in
    let thenmatch = Re.Group.get thengroup 0 in
    let thenlen = String.length thenmatch in
    let rest_len = len - thenlen in
    Tok_Then :: tokenize (String.sub input thenlen rest_len)
  else
    if Re.execp token_else input then 
    let elsegroup = Re.exec token_else input in
    let elsematch = Re.Group.get elsegroup 0 in
    let elselen = String.length elsematch in
    let rest_len = len - elselen in
    Tok_Else :: tokenize (String.sub input elselen rest_len)
  else
    if Re.execp add input then
    let addgroup = Re.exec add input in
    let addmatch = Re.Group.get addgroup 0 in
    let addlen = String.length addmatch in
    let rest_len = len - addlen in
    Tok_Add :: tokenize (String.sub input addlen rest_len)

  else
    if Re.execp mult input then
    let multgroup = Re.exec mult input in
    let multmatch = Re.Group.get multgroup 0 in
    let multlen = String.length multmatch in
    let rest_len = len - multlen in
    Tok_Mult :: tokenize (String.sub input multlen rest_len)
  else
    if Re.execp div input then 
    let divgroup = Re.exec div input in
    let divmatch = Re.Group.get divgroup 0 in
    let divlen = String.length divmatch in
    let rest_len = len - divlen in
    Tok_Div :: tokenize (String.sub input divlen rest_len)
  else
    if Re.execp concat input then 
    let concatgroup = Re.exec concat input in
    let concatmatch = Re.Group.get concatgroup 0 in
    let concatlen = String.length concatmatch in
    let rest_len = len - concatlen in
    Tok_Concat :: tokenize (String.sub input concatlen rest_len)
  else
    if Re.execp token_let input then 
    let letgroup = Re.exec token_let input in
    let letmatch = Re.Group.get letgroup 0 in
    let letlen = String.length letmatch in
    let rest_len = len - letlen in
    Tok_Let :: tokenize (String.sub input letlen rest_len)
  else
    if Re.execp token_def input then 
    let defgroup = Re.exec token_def input in
    let defmatch = Re.Group.get defgroup 0 in
    let deflen = String.length defmatch in
    let rest_len = len - deflen in
    Tok_Def :: tokenize (String.sub input deflen rest_len)
  else 
    if Re.execp token_in input then 
    let ingroup = Re.exec token_in input in
    let inmatch = Re.Group.get ingroup 0 in
    let inlen = String.length inmatch in
    let rest_len = len - inlen in
    Tok_In :: tokenize (String.sub input inlen rest_len)
  else
    if Re.execp token_rec input then 
    let recgroup = Re.exec token_rec input in
    let recmatch = Re.Group.get recgroup 0 in
    let reclen = String.length recmatch in
    let rest_len = len - reclen in
    Tok_Rec :: tokenize (String.sub input reclen rest_len)
  else
    if Re.execp token_fun input then 
    let fungroup = Re.exec token_fun input in
    let funmatch = Re.Group.get fungroup 0 in
    let funlen = String.length funmatch in
    let rest_len = len - funlen in
    Tok_Fun :: tokenize (String.sub input funlen rest_len)
  else
    if Re.execp doublesemi input then 
    let doublesemigroup = Re.exec doublesemi input in
    let doublesemimatch = Re.Group.get doublesemigroup 0 in
    let doublesemilen = String.length doublesemimatch in
    let rest_len = len - doublesemilen in
    Tok_DoubleSemi :: tokenize (String.sub input doublesemilen rest_len)
  else
    if Re.execp semi input then 
    let semigroup = Re.exec semi input in
    let semimatch = Re.Group.get semigroup 0 in
    let semilen = String.length semimatch in
    let rest_len = len - semilen in
    Tok_Semi :: tokenize (String.sub input semilen rest_len)
  else 
    raise (InvalidInputException "nothing match")