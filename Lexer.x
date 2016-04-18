{
module Lexer where
import Token

}

%wrapper "posn"

@string     = \" ($printable # \")* \"

tokens :-

-- Parole Riservate al linguaggio
 
"import"		{\(AlexPn _ l c) s -> ((c,l,s),Import)}
"def"			{\(AlexPn _ l c) s -> ((c,l,s),Def)}
"val"			{\(AlexPn _ l c) s -> ((c,l,s),Val)}
"valres"		{\(AlexPn _ l c) s -> ((c,l,s),Valres)}
"cnst"			{\(AlexPn _ l c) s -> ((c,l,s),Cnst)}
"try"			{\(AlexPn _ l c) s -> ((c,l,s),Try)}
"except"		{\(AlexPn _ l c) s -> ((c,l,s),Except)}
"as"			{\(AlexPn _ l c) s -> ((c,l,s),As)}
"for"			{\(AlexPn _ l c) s -> ((c,l,s),For)}
"in"			{\(AlexPn _ l c) s -> ((c,l,s),In)}
"while"			{\(AlexPn _ l c) s -> ((c,l,s),While)}
"if"			{\(AlexPn _ l c) s -> ((c,l,s),If)}
"elif"			{\(AlexPn _ l c) s -> ((c,l,s),Elif)}
"else"			{\(AlexPn _ l c) s -> ((c,l,s),Else)}
"return"		{\(AlexPn _ l c) s -> ((c,l,s),Return)}
"continue"		{\(AlexPn _ l c) s -> ((c,l,s),Continue)}
"break"			{\(AlexPn _ l c) s -> ((c,l,s),Break)}


-- Caratteri usati dal linguaggio

[\r\n]+			{\(AlexPn _ l c) s -> ((c,l,s),End)}
"("				{\(AlexPn _ l c) s -> ((c,l,s),OpenR)}
")"				{\(AlexPn _ l c) s -> ((c,l,s),CloseR)}
"["				{\(AlexPn _ l c) s -> ((c,l,s),OpenS)}
"]" 			{\(AlexPn _ l c) s -> ((c,l,s),CloseS)}
-- VEDERE
"{"				{\(AlexPn _ l c) s -> ((c,l,s),OpenB)}
"}" 			{\(AlexPn _ l c) s -> ((c,l,s),CloseB)}
--
","				{\(AlexPn _ l c) s -> ((c,l,s),Comma)}
";"				{\(AlexPn _ l c) s -> ((c,l,s),Semicol)}
":"				{\(AlexPn _ l c) s -> ((c,l,s),Col)}
"."				{\(AlexPn _ l c) s -> ((c,l,s),Dot)}

-- Operatori per operazioni matematiche e booleane

"="				{\(AlexPn _ l c) s -> ((c,l,s), Equal)}
"+"				{\(AlexPn _ l c) s -> ((c,l,s), Plus)}
"-"				{\(AlexPn _ l c) s -> ((c,l,s), Minus)}
"/"				{\(AlexPn _ l c) s -> ((c,l,s), Div)}
"*"				{\(AlexPn _ l c) s -> ((c,l,s), Star)}
"not"			{\(AlexPn _ l c) s -> ((c,l,s), Not)}
"<" 			{\(AlexPn _ l c) s -> ((c,l,s), OpDiseqL)}
">"				{\(AlexPn _ l c) s -> ((c,l,s), OpDiseqG)}
"<="			{\(AlexPn _ l c) s -> ((c,l,s), OpDiseqLE)}
">="			{\(AlexPn _ l c) s -> ((c,l,s), OpDiseqGE)}

"=="			{\(AlexPn _ l c) s -> ((c,l,s), OpEq)}
"!="			{\(AlexPn _ l c) s -> ((c,l,s), OpNeq)}
"and"			{\(AlexPn _ l c) s -> ((c,l,s), And)}
"or"			{\(AlexPn _ l c) s -> ((c,l,s), Or)}

-- Carattere puntatori
"@"				{\(AlexPn _ l c) s -> ((c,l,s), DeRef)}
"^"				{\(AlexPn _ l c) s -> ((c,l,s), Ref)}

[\t]	        {\(AlexPn _ l c) s -> ((c,l,s), Tab)}  
--$white			{\(AlexPn _ l c) s -> ((c,l,s), Space)}
$white 			;

-- Tipi di dato del linguaggio
("none"|"NoneType")		{\(AlexPn _ l c) s -> ((c,l,s), TNoneType)}
"char"					{\(AlexPn _ l c) s -> ((c,l,s), TChar)}
"bool"					{\(AlexPn _ l c) s -> ((c,l,s), TBool)}
"int"					{\(AlexPn _ l c) s -> ((c,l,s), TInt)}
"string"				{\(AlexPn _ l c) s -> ((c,l,s), TStr)}
"float"					{\(AlexPn _ l c) s -> ((c,l,s), TFloat)}
"pointer"				{\(AlexPn _ l c) s -> ((c,l,s), TPtr)}
"true"|"false"			{\(AlexPn _ l c) s -> ((c,l,s), Boolean s)}
[a-zA-Z_][a-zA-Z0-9_]* 	{\(AlexPn _ l c) s -> ((c,l,s), Ident s)}
[0-9]+					{\(AlexPn _ l c) s -> ((c,l,s), Integer s)}
[0-9]+"."[0-9]+			{\(AlexPn _ l c) s -> ((c,l,s), Real s)}
@string					{\(AlexPn _ l c) s -> ((c,l,s), String (init (tail s)) )}
"'"$printable"'"		{\(AlexPn _ l c) s -> ((c,l,s), Char s)}


-- Commenti che vengono sopressi

"#".*[\n]+				;
"/*" (\*|.|\n)* "*/"	;

-- Buco nero 
.						{ \(AlexPn _ l c) s -> ((c,l,s), LexUndef) }

{

}

