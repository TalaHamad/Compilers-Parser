#ifndef COMPILERPARSER_PARSER_H
#define COMPILERPARSER_PARSER_H

#include "Scanner.h"
#include "stable.h"
#include "ast.h"

class Parser
{
public:
    Parser(FileDescriptor*fd);
    ast_list* parse();

private:
    Scanner *scanner;
    STable *table;
    ast_list * programAST;
    int offset;
    ast_list* declList(TOKEN* currentToken);
    AST* varDecl(TOKEN* token);
    ste_list* varDeclList(TOKEN* token, ste_list* STE_list_x);
    bool checkFollowVarDeclList(TOKEN* token);
    AST* constantDecl(TOKEN* token);
    AST* functionDecl(TOKEN* token);
    AST* procedureDecl(TOKEN* token);
    ste_list* formals_list(TOKEN* token);
    ste_list* formals_list_tail(TOKEN* token);
    ste_list* formals(TOKEN* token, ste_list *STE_list_x);
    ste_list* formals_tail(TOKEN* token, ste_list *STE_list_x);
    symbol_table_entry* setSTEValues(ste_entry_type type, ...);
    STE_TYPE getTokenType(LEXEME_TYPE token_type);
    ste_list* addNodeToSteList(symbol_table_entry* ST_Entry, ste_list *STE_list_x);
    AST* statement(TOKEN* token);
    AST* ifStmnt(TOKEN* token);
    AST* ifTail(TOKEN* token, AST *ast_Exp, AST* ast_conseq);
    AST* whileStmnt(TOKEN* token);
    AST* forStmnt(TOKEN* token);
    AST* readStmnt(TOKEN* token);
    AST* writeStmnt(TOKEN* token);
    AST* returnStmnt(TOKEN* token);
    AST* blockStmnt(TOKEN* token);
    ast_list* statementlist(TOKEN* token, ast_list* AST_list_x);
    AST* assignStmnt(TOKEN* token);
    AST* callStmnt(TOKEN* token);
    AST* callAssignStmnt(TOKEN* token);
    ast_list* argsList(TOKEN* token);
    ast_list* argsListTail(TOKEN* token);
    ast_list* args(TOKEN* token, ast_list * AST_list_x);
    ast_list* argsTail(TOKEN* token, ast_list* AST_list_x);
    ast_list* addNodeToAstList(AST* ast, ast_list *AST_list_x);
    AST* eExpression(TOKEN* token);
    AST* extendedeExpression(AST* E, TOKEN* token);
    bool checkFollowExtE(TOKEN* token);
    AST* fExpression(TOKEN* token);
    AST* extendedfExpression(AST* F, TOKEN* token);
    AST* gExpression(TOKEN* token);
    AST* extendedgExpression(AST *G, TOKEN* token);
    AST* hExpression(TOKEN* token);
    AST* extendedhExpression(AST *H, TOKEN* token);
    AST* iExpression(TOKEN* token);
    AST* jExpression(TOKEN* token);
    bool matchLexeme(TOKEN* token, LEXEME_TYPE type);
    bool matchDataType(TOKEN* token);
    bool relconjOp(TOKEN* token);
    bool relOp(TOKEN* token);
    bool plusminOp(TOKEN* token);
    bool muldivOp(TOKEN* token);
    bool unaryOp(TOKEN* token);

    void reportError(string message);
    void print_AST(AST* ast, FILE*);
};
#endif //COMPILERPARSER_PARSER_H
