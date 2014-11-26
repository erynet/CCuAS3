package MiniC.Parser;

import MiniC.Scanner.Scanner;
import MiniC.Scanner.Token;
import MiniC.Scanner.SourcePos;
import MiniC.Parser.SyntaxError;
import MiniC.ErrorReporter;
import MiniC.AstGen.*;


public class Parser {

    private Scanner scanner;
    private ErrorReporter errorReporter;
    private Token currentToken;
    private SourcePos previousTokenPosition;

    public Parser(Scanner lexer, ErrorReporter reporter) {
    	scanner = lexer;
    	errorReporter = reporter;
    }

    // accept() checks whether the current token matches tokenExpected.
    // If so, it fetches the next token.
    // If not, it reports a syntax error.
    void accept (int tokenExpected) throws SyntaxError {
		if (currentToken.kind == tokenExpected) {
			previousTokenPosition = currentToken.GetSourcePos();
			currentToken = scanner.scan();
		} else {
			syntaxError("\"%\" expected here", Token.spell(tokenExpected));
		}
    }

    // acceptIt() unconditionally accepts the current token
    // and fetches the next token from the scanner.
    void acceptIt() {
    	previousTokenPosition = currentToken.GetSourcePos();
    	currentToken = scanner.scan();
    }

    // start records the position of the start of a phrase.
    // This is defined to be the position of the first
    // character of the first token of the phrase.
    void start(SourcePos position) {
    	position.StartCol = currentToken.GetSourcePos().StartCol;
        position.StartLine = currentToken.GetSourcePos().StartLine;
    }

    // finish records the position of the end of a phrase.
    // This is defined to be the position of the last
    // character of the last token of the phrase.
    void finish(SourcePos position) {
    	position.EndCol = previousTokenPosition.EndCol;
    	position.EndLine = previousTokenPosition.EndLine;
    }

    void syntaxError(String messageTemplate, String tokenQuoted) throws SyntaxError {
    	SourcePos pos = currentToken.GetSourcePos();
    	errorReporter.reportError(messageTemplate, tokenQuoted, pos);
    	throw(new SyntaxError());
    }

    boolean isTypeSpecifier(int token) {
    	if(token == Token.VOID ||
    		token == Token.INT  ||
    		token == Token.BOOL ||
    		token == Token.FLOAT) {
    		return true;
		} else {
			return false;
		}
    }

    boolean isExprSpecifier(int token) {
		if(token == Token.PLUS ||	
			token == Token.MINUS ||	
			token == Token.NOT ||	
			token == Token.ID ||	
			token == Token.LEFTPAREN ||	
			token == Token.INTLITERAL ||	
			token == Token.BOOLLITERAL ||	
			token == Token.FLOATLITERAL ||	
			token == Token.STRINGLITERAL) {
			return true;
		} else	{
			return false;
		}
    }

    boolean isBinarySpecifier(int token) {
		if(token == Token.PLUS ||	
			token == Token.MINUS ||	
			token == Token.TIMES ||
			token == Token.DIV ||
			token == Token.EQ||
			token == Token.NOTEQ||
			token == Token.LESSEQ||
			token == Token.LESS||
			token == Token.GREATEREQ||
			token == Token.GREATER||
			token == Token.AND||
			token == Token.OR) {
			return true;
		} else {
			return false;
		}
    }


    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseArrayIndexDecl (Type T):
    //
    // Take [INTLITERAL] and generate an ArrayType
    //
    ///////////////////////////////////////////////////////////////////////////////

    public ArrayType parseArrayIndexDecl(Type T) throws SyntaxError {
		IntLiteral L;
		IntExpr IE;
		accept(Token.LEFTBRACKET);
		SourcePos pos = currentToken.GetSourcePos();
		L = new IntLiteral(currentToken.GetLexeme(), pos);
		accept(Token.INTLITERAL);
		accept(Token.RIGHTBRACKET);
		IE = new IntExpr (L, pos);
		return new ArrayType (T, IE, previousTokenPosition);
    }


    ///////////////////////////////////////////////////////////////////////////////
    //
    // toplevel parse() routine:
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Program parse() {
    	Program ProgramAST = null;
        previousTokenPosition = new SourcePos();
        previousTokenPosition.StartLine = 0;
        previousTokenPosition.StartCol = 0;
        previousTokenPosition.EndLine = 0;
        previousTokenPosition.EndCol = 0;

        currentToken = scanner.scan(); // get first token from scanner...

        try {
        	ProgramAST = parseProgram();
        	if (currentToken.kind != Token.EOF) {
        		syntaxError("\"%\" not expected after end of program",
        				currentToken.GetLexeme());
        	}
        } catch (SyntaxError s) {
        	return null;
        }
        return ProgramAST;
    }
    
    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseProgram():
    //
    // program ::= ( (VOID|INT|BOOL|FLOAT) ID ( FunPart | VarPart ) )* ";"
    //
    ///////////////////////////////////////////////////////////////////////////////

    // parseProgDecls: recursive helper function to facilitate AST construction.
    public Decl parseProgDecls () throws SyntaxError {
    	if (! isTypeSpecifier(currentToken.kind)) {
    		return new EmptyDecl (previousTokenPosition);
    	}
    	SourcePos pos = new SourcePos();
    	start(pos);
    	Type T = parseTypeSpec();
    	ID Ident = parseID();
    	if(currentToken.kind == Token.LEFTPAREN) {
    		Decl newD = parseFunPart(T, Ident, pos);
    		return new DeclSequence (newD, parseProgDecls(), previousTokenPosition);
    	} else {
    		DeclSequence Vars = parseVarPart(T, Ident);
    		DeclSequence VarsTail = Vars.GetRightmostDeclSequenceNode();
    		Decl RemainderDecls = parseProgDecls();
    		VarsTail.SetRightSubtree (RemainderDecls);
    		return Vars;
    	}
    }

    public Program parseProgram() throws SyntaxError {
    	SourcePos pos = new SourcePos();
    	start(pos);
    	Decl D = parseProgDecls();
    	finish(pos);
    	Program P = new Program (D, pos);
    	return P;
    }

    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseTypeSpec():
    //
    // TypeSpec ::= VOID|INT|BOOL|FLOAT
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Type parseTypeSpec() throws SyntaxError {
    	Type T = null;
    	switch(currentToken.kind) {
    	case Token.INT:
    		T = new IntType (currentToken.GetSourcePos());
    		break;
    	case Token.BOOL:
    		T = new BoolType (currentToken.GetSourcePos());
    		break;
    	case Token.FLOAT:
    		T = new FloatType (currentToken.GetSourcePos());
    		break;
    	case Token.VOID:
    		T = new VoidType (currentToken.GetSourcePos());
    		break;
    	default:
    		assert(false); // unknown type :(
    	}
    	acceptIt();
    	return T;
    }

    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseFunPart():
    //
    // FunPart ::= ( "(" ParamsList? ")" CompoundStmt )
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Decl parseFunPart(Type T, ID Ident, SourcePos pos) throws SyntaxError {

        // We already know that the current token is "(".
        // Otherwise use accept() !
    	acceptIt();
    	Decl PDecl = parseParamsList(); // can also be empty...
    	accept(Token.RIGHTPAREN);
    	CompoundStmt CStmt = parseCompoundStmt();
    	finish(pos);
    	return new FunDecl (T, Ident, PDecl, CStmt, pos);
    }

    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseParamsList():
    //
    // ParamsList ::= ParameterDecl ( "," ParameterDecl ) *
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Decl parseParamsList() throws SyntaxError {
    	if (!isTypeSpecifier(currentToken.kind)) {
    		return new EmptyFormalParamDecl(previousTokenPosition);
    	}
    	Decl PDecl = parseParameterDecl();
    	if (currentToken.kind == Token.COMMA) {
    		acceptIt();
    	}
    	return new FormalParamDeclSequence (PDecl,
    			parseParamsList(), previousTokenPosition);
    }

    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseParameterDecl():
    //
    // ParameterDecl ::= (VOID|INT|BOOL|FLOAT) Declarator
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Decl parseParameterDecl() throws SyntaxError {
    	Type T = null;
    	Decl D = null;

    	SourcePos pos = new SourcePos();
    	start(pos);
    	if (isTypeSpecifier(currentToken.kind)) {
    		T = parseTypeSpecifier();
    	} else {
    		syntaxError("Type specifier instead of % expected",
    				Token.spell(currentToken.kind));
    	}
    	D = parseDeclarator(T, pos);
    	return D;
    }

    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseDeclarator():
    //
    // Declarator ::= ID ( "[" INTLITERAL "]" )?
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Decl parseDeclarator(Type T, SourcePos pos) throws SyntaxError {
    	ID Ident = parseID();
    	if (currentToken.kind == Token.LEFTBRACKET) {
    		ArrayType ArrT = parseArrayIndexDecl(T);
    		finish(pos);
    		return new FormalParamDecl (ArrT, Ident, pos);
    	}
    	finish(pos);
    	return new FormalParamDecl (T, Ident, pos);
    }


    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseVarPart():
    //
    // VarPart ::= ( "[" INTLITERAL "]" )?  ( "=" initializer ) ? ( "," init_decl)* ";"
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Decl parseInitDecl(Type T) throws SyntaxError {
    	Decl D;
    	ID Ident = parseID();
    	ArrayType ArrT = null;
    	Expr E = null;
    	if(currentToken.kind==Token.LEFTBRACKET) {
    		ArrT = parseArrayIndexDecl(T);
    	}
    	if(currentToken.kind==Token.ASSIGN) {
    		acceptIt();
    		E = parseInitializer();
    	}
    	if(ArrT!=null) {
    		if(E!=null) {
    			D = new VarDecl(ArrT, Ident, E, previousTokenPosition);
    		} else {
    			D = new VarDecl(ArrT, Ident, new EmptyExpr(previousTokenPosition), 
    					previousTokenPosition);
    		}
    	} else {
    		if(E!=null) {
    			D = new VarDecl(T, Ident, E, previousTokenPosition);
    		} else {
    			D = new VarDecl(T, Ident, new EmptyExpr(previousTokenPosition), 
    					previousTokenPosition);
    		}
    	}
    	DeclSequence Seq = null;
    	if (currentToken.kind == Token.COMMA) {
    		acceptIt();
    		//Seq = new DeclSequence(D, parseInitDecl(T), previousTokenPosition);
    		return new DeclSequence(D, parseInitDecl(T), previousTokenPosition);
    	} else {
    		//Seq = new DeclSequence(D, new EmptyDecl (previousTokenPosition),
            //                         previousTokenPosition);
    		return new DeclSequence(D, new EmptyDecl (previousTokenPosition),
                                     previousTokenPosition);
    	}
    	//return Seq;
    }
    
    public DeclSequence parseVarPart(Type T, ID Ident) throws SyntaxError {
    	Type theType = T;
    	Decl D;
    	DeclSequence Seq = null;
    	Expr E = new EmptyExpr(previousTokenPosition);
    	if (currentToken.kind == Token.LEFTBRACKET) {
    		theType = parseArrayIndexDecl(T);
    	}
    	if (currentToken.kind == Token.ASSIGN) {
    		acceptIt();
    		E = parseInitializer();
    	}
    	D = new VarDecl (theType, Ident, E, previousTokenPosition);

    	if (currentToken.kind == Token.COMMA) {
    		acceptIt();
    		Seq = new DeclSequence (D, parseInitDecl(T), previousTokenPosition);
    	} else {
    		Seq = new DeclSequence (D, new EmptyDecl (previousTokenPosition),
    				previousTokenPosition);
    	}
    	accept (Token.SEMICOLON);
    	return Seq;
    }


	///////////////////////////////////////////////////////////////////////////////
	//
	// parseInitializer():
	//
	// Initializer ::= Expr 
    //				| "{" Expr ( "," Expr )* "}"
	//
	///////////////////////////////////////////////////////////////////////////////
    
    public Expr parseExprSequence() throws SyntaxError {
    	Expr lE, rE;
    	lE = parseExpr();
    	if(currentToken.kind == Token.COMMA) {
    		acceptIt();
    		rE = parseExprSequence();
    	} else {
    		rE = new EmptyExpr(previousTokenPosition);
    	}
    	return new ExprSequence(lE, rE, previousTokenPosition);
    }
    
    public Expr parseInitializer() throws SyntaxError {
    	Expr E;
    	if(currentToken.kind == Token.LEFTBRACE) {
    		acceptIt();
    		E = parseExprSequence();
    		accept(Token.RIGHTBRACE);
    	} else {
    		E = parseExpr();
    	}
    	return E;
    }

    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseExpr():
    //
    // Expr ::= OrExpr
    //
    ///////////////////////////////////////////////////////////////////////////////
    
    public Expr parseExpr() throws SyntaxError {
    	Expr E = parseOrExpr();
    	//while(isBinarySpecifier(currentToken.kind)) {
    	if(isBinarySpecifier(currentToken.kind)) {
    		Operator opAST = new Operator (currentToken.GetLexeme(),
    				previousTokenPosition);
    		acceptIt();
    		//Expr newE = new BinaryExpr(E, opAST, parseOrExpr(), 
    		//		previousTokenPosition);
    		return new BinaryExpr(E, opAST, parseOrExpr(), 
    				previousTokenPosition);
    		//E = newE;
    	}
    	return E;
    }
    
    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseOrExpr():
    //
    // OrExpr ::= AndExpr
    //		   | OrExpr "||" AndExpr
    //		  ::= ( OrExpr "||" )? AndExpr
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Expr parseOrExpr() throws SyntaxError {
    	Expr E;
    	E = parseAndExpr();
    	//while(currentToken.kind==Token.OR) {
    	if(currentToken.kind == Token.OR) {
    		Operator opAST = new Operator (currentToken.GetLexeme(),
    				previousTokenPosition);
    		acceptIt();
    		//Expr newE = new BinaryExpr(E, opAST, parseAndExpr(), 
    		//		previousTokenPosition);
    		return new BinaryExpr(E, opAST, parseAndExpr(), 
    				previousTokenPosition);
    		//E = newE;
    	}
    	return E;
    }
    
    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseAndExpr():
    //
    // AndExpr ::= RelationalExpr
    //		 	| AndExpr "&&" RelationalExpr
    //		   ::= ( AndExpr "&&" )? RelationalExpr
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Expr parseAndExpr() throws SyntaxError {
    	Expr E;
    	E = parseRelationalExpr();
    	//while(currentToken.kind==Token.AND) {
    	if(currentToken.kind == Token.AND) {
    		Operator opAST = new Operator (currentToken.GetLexeme(),
    				previousTokenPosition);
    		acceptIt();
    		//Expr newE = new BinaryExpr(E, opAST, parseRelationalExpr(), 
    		//		previousTokenPosition);
    		return new BinaryExpr(E, opAST, parseRelationalExpr(), 
    				previousTokenPosition);
    		//E = newE;
    	}
    	return E;
    }
    
    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseRelationalExpr():
    //
    // RelationalExpr ::= AddExpr ( ("=="|"!="|"<"|"<="|">"|">=") AddExpr )?
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Expr parseRelationalExpr() throws SyntaxError {
    	Expr E;
    	E = parseAddExpr();
    	//while(currentToken.kind==Token.EQ ||
    	if(currentToken.kind == Token.EQ ||
    			currentToken.kind == Token.NOTEQ ||
    			currentToken.kind == Token.LESSEQ ||
    			currentToken.kind == Token.LESS ||
    			currentToken.kind == Token.GREATEREQ ||
    			currentToken.kind == Token.GREATER ) {
    		Operator opAST = new Operator (currentToken.GetLexeme(),
    				previousTokenPosition);
    		acceptIt();
    		//Expr newE = new BinaryExpr(E, opAST, parseAddExpr(), 
    		//		previousTokenPosition);
    		return new BinaryExpr(E, opAST, parseAddExpr(), 
    				previousTokenPosition);
    		//E = newE;
    	}
    	return E;
    }
    
    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseAddExpr():
    //
    // AddExpr ::= MultExpr | AddExpr ("+"|"-") MultExpr
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Expr parseAddExpr() throws SyntaxError {
    	Expr E = parseMulExpr();
    	while(currentToken.kind == Token.PLUS || 
    	//if(currentToken.kind == Token.PLUS || 
    			currentToken.kind == Token.MINUS) {
    		Operator opAST = new Operator (currentToken.GetLexeme(),
    				previousTokenPosition);
    		acceptIt();
    		Expr newE = new BinaryExpr(E, opAST, parseMulExpr(), 
    				previousTokenPosition);
    		//return new BinaryExpr(E, opAST, parseMulExpr(), 
    		//		previousTokenPosition);
    		E = newE;
    	}
    	return E;
    }
    
    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseMulExpr():
    //
    // MulExpr ::= MultExpr | AddExpr ("+"|"-") MultExpr
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Expr parseMulExpr() throws SyntaxError {
    	Expr E;
    	E = parseUnaryExpr();
    	while(currentToken.kind==Token.TIMES || 
    	//if(currentToken.kind == Token.TIMES || 
    			currentToken.kind == Token.DIV) {
    		Operator opAST = new Operator (currentToken.GetLexeme(),
    				previousTokenPosition);
    		acceptIt();
    		Expr newE = new BinaryExpr(E, opAST, parseUnaryExpr(), 
    				previousTokenPosition);
    		//return new BinaryExpr(E, opAST, parseUnaryExpr(), 
    		//		previousTokenPosition);
    		E = newE;
    	}
    	return E;
    }

    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseUnaryExpr():
    //
    // UnaryExpr ::= ("+"|"-"|"!")* PrimaryExpr
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Expr parseUnaryExpr() throws SyntaxError {
    	while(currentToken.kind == Token.PLUS ||
    	//if(currentToken.kind == Token.PLUS ||
    			currentToken.kind == Token.MINUS ||
    			currentToken.kind == Token.NOT) {
    		Operator opAST = new Operator (currentToken.GetLexeme(),
    				previousTokenPosition);
    		acceptIt();
    		return new UnaryExpr (opAST, parseUnaryExpr(), 
    				previousTokenPosition);
    	}
    	return parsePrimaryExpr();
    }

    ///////////////////////////////////////////////////////////////////////////////
    //
    // parsePrimaryExpr():
    //
    // PrimaryExpr ::= ID arglist?
    //              |  ID "[" expr "]"
    //              |  "(" expr ")"
    //              |  INTLITERAL | BOOLLITERAL | FLOATLITERAL | STRINGLITERAL
    //			   ::= ID ( arglist? | "[" expr "]")
    //				|  "(" expr ")"
    //				| ( INTLITERAL | BOOLLITERAL | FLOATLITERAL | STRINGLITERAL )
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Expr parsePrimaryExpr() throws SyntaxError {
    	Expr returnExpr = null;
    	if(currentToken.kind == Token.ID) {
    		ID Ident = parseID();
    		Expr E = new VarExpr(Ident, previousTokenPosition);
    		if(currentToken.kind == Token.LEFTBRACKET) {
				acceptIt();
				Expr indexE=parseExpr();
				accept(Token.RIGHTBRACKET);
				
				returnExpr = new ArrayExpr(E, indexE, previousTokenPosition);
			} else if(currentToken.kind == Token.LEFTPAREN) {
				Expr paramE=parseArgList();
				returnExpr = new CallExpr(Ident, paramE, previousTokenPosition);
			} else {
				returnExpr = E;
			}
    	} else if(currentToken.kind == Token.LEFTPAREN) {
    		acceptIt();
    		Expr E = parseExpr();
    		accept(Token.RIGHTPAREN);
    		returnExpr = E;
    	} else if(currentToken.kind == Token.INTLITERAL) {
    		returnExpr = new IntExpr(new IntLiteral(currentToken.GetLexeme(), 
    				previousTokenPosition), 
    				previousTokenPosition);
    		acceptIt();
		} else if(currentToken.kind == Token.BOOLLITERAL) {
			returnExpr = new BoolExpr(new BoolLiteral(currentToken.GetLexeme(), 
					previousTokenPosition), 
					previousTokenPosition);
			acceptIt();
		} else if(currentToken.kind == Token.FLOATLITERAL) {
			returnExpr = new FloatExpr(new FloatLiteral(currentToken.GetLexeme(), 
					previousTokenPosition), 
					previousTokenPosition);
			acceptIt();
		} else if(currentToken.kind == Token.STRINGLITERAL) {
			returnExpr = new StringExpr(new StringLiteral(currentToken.GetLexeme(), 
					previousTokenPosition), 
					previousTokenPosition);
			accept(Token.STRINGLITERAL);
		} else {
			returnExpr = new EmptyExpr(previousTokenPosition);
		}
    	return returnExpr;
    }

    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseCompoundStmt():
    //
    // CompoundStmt ::= "{" VariableDef* Stmt* "}"
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Decl parseCompoundDecls () throws SyntaxError {
    	if (!isTypeSpecifier(currentToken.kind)) {
        	return new EmptyDecl (previousTokenPosition);
        }
    	//Type T = parseTypeSpec();
    	//ID Ident = parseID();
    	//DeclSequence Vars = parseVarPart(T, Ident);
    	DeclSequence Vars = parseVarPart(parseTypeSpec(), parseID());
    	DeclSequence VarsTail = Vars.GetRightmostDeclSequenceNode();
    	Decl RemainderDecls = parseCompoundDecls();
    	VarsTail.SetRightSubtree(RemainderDecls);
    	return Vars;       
    }

    public Stmt parseCompoundStmts () throws SyntaxError {
    	if (!(currentToken.kind == Token.LEFTBRACE ||
    			currentToken.kind == Token.IF ||
    			currentToken.kind == Token.WHILE ||
    			currentToken.kind == Token.FOR ||
    			currentToken.kind == Token.RETURN ||
    			currentToken.kind == Token.ID)) {
    		return new EmptyStmt(previousTokenPosition);
    	}
    	Stmt S = null;
    	S = parseStmt();
    	return new StmtSequence (S, parseCompoundStmts(), 
    			previousTokenPosition);
    }

    public Stmt parseStmt() throws SyntaxError {
		Stmt S;
		if(currentToken.kind==Token.LEFTBRACE) {
			S=parseCompoundStmt();
		} else if(currentToken.kind==Token.IF) {
			acceptIt();
			accept(Token.LEFTPAREN);
			Expr E = parseExpr();
			accept(Token.RIGHTPAREN);
			Stmt thenS = parseStmt();

			if(currentToken.kind==Token.ELSE) {
				acceptIt();
				Stmt elseS = parseStmt();
				S = new IfStmt(E, thenS, elseS, previousTokenPosition);
			} else {
				S = new IfStmt(E, thenS, previousTokenPosition);
			}
		} else if(currentToken.kind==Token.WHILE) {
			acceptIt();
			accept(Token.LEFTPAREN);
			Expr E = parseExpr();
			accept(Token.RIGHTPAREN);
			Stmt whileS = parseStmt();
			S = new WhileStmt(E, whileS, previousTokenPosition);
		} else if(currentToken.kind==Token.FOR) {
			acceptIt();
			accept(Token.LEFTPAREN);
			Expr E1=null, E2=null, E3=null;

			if(currentToken.kind == Token.ID) {
				//ID Ident = parseID();
				//Expr lE = new VarExpr(Ident, previousTokenPosition);
				Expr lE = new VarExpr(parseID(), previousTokenPosition);
				accept(Token.ASSIGN);			
				Expr rE = parseExpr();
				E1 = new AssignExpr(lE, rE, previousTokenPosition);
			} else { 
				E1 = new EmptyExpr(previousTokenPosition); 
			}
			accept(Token.SEMICOLON);		
			if(isExprSpecifier(currentToken.kind)) {
				E2 = parseExpr();
			} else {
				E2 = new EmptyExpr(previousTokenPosition);
			}
			accept(Token.SEMICOLON);
			if(currentToken.kind==Token.ID) {
				//ID Ident = parseID();
				//Expr lE = new VarExpr(Ident, previousTokenPosition);
				Expr lE = new VarExpr(parseID(), previousTokenPosition);
				accept(Token.ASSIGN);			
				Expr rE = parseExpr();
				E3 = new AssignExpr(lE, rE, previousTokenPosition);
			} else {
				E3 = new EmptyExpr(previousTokenPosition);
			}
			accept(Token.RIGHTPAREN);
			Stmt forS = parseStmt();
			
			S = new ForStmt(E1, E2, E3, forS, previousTokenPosition);		
		} else if(currentToken.kind==Token.RETURN) {
			acceptIt();
			Expr E;
			if(isExprSpecifier(currentToken.kind)) {
				E = parseExpr();
			} else {
				E = new EmptyExpr(previousTokenPosition);
			}
			accept(Token.SEMICOLON);
			S = new ReturnStmt(E, previousTokenPosition);
		} else if(currentToken.kind==Token.ID) {
			ID Ident = parseID();
			
			if(currentToken.kind == Token.ASSIGN) {
				//Expr idE = new VarExpr(Ident, previousTokenPosition);	
				accept(Token.ASSIGN);			
				//Expr rE = parseExpr();
				accept(Token.SEMICOLON);
				//S = new AssignStmt(idE, rE, previousTokenPosition);
				//S = new AssignStmt(new VarExpr(Ident, previousTokenPosition), rE, previousTokenPosition);
				S = new AssignStmt(new VarExpr(Ident, previousTokenPosition), parseExpr(), previousTokenPosition);
				
			} else if(currentToken.kind == Token.LEFTBRACKET) {
				//Expr idE = new VarExpr(Ident, previousTokenPosition);
				acceptIt();
				//Expr indexE = parseExpr();
				accept(Token.RIGHTBRACKET);

				//Expr arrayE = new ArrayExpr(idE, indexE, previousTokenPosition);
				//Expr arrayE = new ArrayExpr(new VarExpr(Ident, previousTokenPosition), indexE, previousTokenPosition);
				Expr arrayE = new ArrayExpr(new VarExpr(Ident, previousTokenPosition), parseExpr(), previousTokenPosition);

				accept(Token.ASSIGN);
				//Expr rE = parseExpr();
				accept(Token.SEMICOLON);
				//S = new AssignStmt(arrayE, rE, previousTokenPosition);
				S = new AssignStmt(arrayE, parseExpr(), previousTokenPosition);
			} else {
				//Expr E = parseArgList();
				//Expr callE = new CallExpr(Ident, E, previousTokenPosition);
				//Expr callE = new CallExpr(Ident, parseArgList(), previousTokenPosition);
				accept(Token.SEMICOLON);
				//S = new CallStmt(callE, previousTokenPosition);
				S = new CallStmt(new CallExpr(Ident, parseArgList(), previousTokenPosition), previousTokenPosition);
			}
		} else {
			S = new EmptyStmt(previousTokenPosition);
		}
	return S;
    }

    public CompoundStmt parseCompoundStmt() throws SyntaxError {
        SourcePos pos = new SourcePos();
        start(pos);
        accept(Token.LEFTBRACE);
        Decl D = parseCompoundDecls();
        Stmt S = parseCompoundStmts();
        accept(Token.RIGHTBRACE);
        finish(pos);
        if ( (D.getClass() == EmptyDecl.class) &&
        		(S.getClass() == EmptyStmt.class)) {
        	return new EmptyCompoundStmt (previousTokenPosition);
        } else {
        	return new CompoundStmt (D, S, pos);
        }
    }

    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseArgList():
    //
    // ArgList ::= "(" ( arg ( "," arg )* )? ")"
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Expr parseArgs() throws SyntaxError {
		if (currentToken.kind == Token.RIGHTPAREN) {
		    return new  EmptyActualParam (previousTokenPosition);
		}
		Expr Params = null;
		Params = new ActualParam (parseExpr(), previousTokenPosition);
		if (currentToken.kind == Token.COMMA) {
			acceptIt();
		}
		return new ActualParamSequence (Params, parseArgs(), previousTokenPosition);
    }

    public Expr parseArgList() throws SyntaxError {
    	accept(Token.LEFTPAREN);
    	Expr Params = parseArgs();
    	accept(Token.RIGHTPAREN);
    	return Params;
    }


    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseID():
    //
    // ID (terminal)
    //
    ///////////////////////////////////////////////////////////////////////////////

    public ID parseID() throws SyntaxError {
    	ID Ident = new ID(currentToken.GetLexeme(), currentToken.GetSourcePos());
    	accept(Token.ID);
    	return Ident;
    }


    ///////////////////////////////////////////////////////////////////////////////
    //
    // parseTypeSpecifier():
    //
    // VOID | INT | FLOAT | BOOL (all terminals)
    //
    ///////////////////////////////////////////////////////////////////////////////

    public Type parseTypeSpecifier() throws SyntaxError {
		Type T = null;
		switch (currentToken.kind) {
		case Token.INT:
		    T = new IntType(currentToken.GetSourcePos());
		    break;
		case Token.FLOAT:
		    T = new FloatType(currentToken.GetSourcePos());
		    break;
		case Token.BOOL:
		    T = new BoolType(currentToken.GetSourcePos());
		    break;
		case Token.VOID:
		    T = new VoidType(currentToken.GetSourcePos());
		    break;
		default:
		    syntaxError("Type specifier expected", "");
		}
		acceptIt();
		return T;
    }
}
