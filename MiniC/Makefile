# define a variable for compiler flags (JFLAGS)
# define a variable for the compiler (JC)  
#

JFLAGS = -g
JC = javac

# Clear any default targets for building .class files from .java files; we 
# will provide our own target entry to do this in this makefile.
# make has a set of default targets for different suffixes (like .c.o) 
# Currently, clearing the default for .java.class is not necessary since 
# make does not have a definition for this target, but later versions of 
# make may, so it doesn't hurt to make sure that we clear any default 
# definitions for these
#

.SUFFIXES: .java .class

# Here is our target entry for creating .class files from .java files 
# This is a target entry that uses the suffix rule syntax:
#	DSTS:
#		rule
# DSTS (Dependency Suffix Target Suffix)
# 'TS' is the suffix of the target file, 'DS' is the suffix of the dependency 
#  file, and 'rule'  is the rule for building a target	
# '$*' is a built-in macro that gets the basename of the current target 
# Remember that there must be a < tab > before the command line ('rule') 
#

.java.class:
	$(JC) $(JFLAGS) $*.java

# SRC is a macro consisting of several words (one for each java source file)
# the backslash "\" at the end of the line is a line continuation character
# so that the same line can continue over several lines 

SRC = \
    ErrorReporter.java \
    TreeDrawer/DrawerFrame.java \
    TreeDrawer/Polygon.java \
    TreeDrawer/Drawer.java \
    TreeDrawer/LayoutVisitor.java \
    TreeDrawer/DrawingTree.java \
    TreeDrawer/DrawerPanel.java \
    TreeDrawer/Polyline.java \
    TreePrinter/TreePrinterVisitor.java \
    TreePrinter/Printer.java \
    Parser/SyntaxError.java \
    Parser/Parser.java \
    Scanner/SourceFile.java \
    Scanner/Token.java \
    Scanner/Scanner.java \
    Scanner/SourcePos.java \
    Unparser/UnparseVisitor.java \
    Unparser/Unparser.java \
    MiniC.java \
    AstGen/StringExpr.java \
    AstGen/ExprSequence.java \
    AstGen/StmtSequence.java \
    AstGen/EmptyFormalParamDecl.java \
    AstGen/ForStmt.java \
    AstGen/VarDecl.java \
    AstGen/Expr.java \
    AstGen/IntType.java \
    AstGen/VoidType.java \
    AstGen/AssignExpr.java \
    AstGen/ReturnStmt.java \
    AstGen/FloatType.java \
    AstGen/DeclSequence.java \
    AstGen/Decl.java \
    AstGen/Visitor.java \
    AstGen/ActualParam.java \
    AstGen/CompoundStmt.java \
    AstGen/FloatLiteral.java \
    AstGen/BinaryExpr.java \
    AstGen/VarExpr.java \
    AstGen/FormalParamDeclSequence.java \
    AstGen/EmptyCompoundStmt.java \
    AstGen/ActualParamSequence.java \
    AstGen/BoolType.java \
    AstGen/UnaryExpr.java \
    AstGen/IntExpr.java \
    AstGen/EmptyStmt.java \
    AstGen/ID.java \
    AstGen/BoolLiteral.java \
    AstGen/EmptyActualParam.java \
    AstGen/Operator.java \
    AstGen/BoolExpr.java \
    AstGen/Terminal.java \
    AstGen/FloatExpr.java \
    AstGen/Stmt.java \
    AstGen/ArrayExpr.java \
    AstGen/IfStmt.java \
    AstGen/AssignStmt.java \
    AstGen/CallExpr.java \
    AstGen/Type.java \
    AstGen/FormalParamDecl.java \
    AstGen/EmptyDecl.java \
    AstGen/ErrorType.java \
    AstGen/IntLiteral.java \
    AstGen/AST.java \
    AstGen/Program.java \
    AstGen/FunDecl.java \
    AstGen/StringType.java \
    AstGen/StringLiteral.java \
    AstGen/EmptyExpr.java \
    AstGen/CallStmt.java \
    AstGen/ArrayType.java \
    AstGen/WhileStmt.java

# the default make target entry
# for this example it is the target classes

default: classes

# Next line is a macro that specifies the class files of the scanner.
# We use Suffix Replacement within a macro: 
# $(macroname:string1=string2)
# In the words in the macro named 'macroname' replace 'string1' with 'string2'
# Below we are replacing the suffix .java of all words in the macro SRC 
# with the .class suffix
#
CLS = $(SRC:.java=.class)

# Next line is a target dependency line. It says: target "classes" depends on the
# class files from the CLS macro.
classes: $(CLS)

# this line is to remove all unneeded files from
# the directory when we are finished executing(saves space)
# and "cleans up" the directory of unneeded .class files
# RM is a predefined macro in make (RM = rm -f)
#

clean:
	rm -f $(CLS)
#
