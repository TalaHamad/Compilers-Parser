#include "Parser.h"
#include <stdarg.h>

Parser::Parser(FileDescriptor *fileDescriptor)
{
    // Create a new scanner and symbol table
    scanner = new Scanner(fileDescriptor);
    table = new STable(1);

    // Initialize the programAST AST to NULL
    programAST = nullptr;
}

ast_list *Parser::parse()
{
    // Get the first token from the scanner
    TOKEN* token = scanner->Scan();

    // Initialize the programAST's AST list
    programAST = new ast_list();
    programAST->head = nullptr;
    programAST->tail = nullptr;

    // If the first token is NULL, return NULL
    if (token == nullptr)
        return nullptr;

    // Parse the declaration list
    programAST = declList(token);

    // Open the output file for writing the AST
    FILE* outputFile = std::fopen("C:\\Users\\AB\\CLionProjects\\CompilerParser\\output.txt", "w");

    // Iterate through the AST list and print each AST node
    ast_list* currentASTNode = programAST;
    while (currentASTNode != nullptr && currentASTNode->head != nullptr)
    {
        print_ast_node(outputFile, currentASTNode->head);
        currentASTNode = currentASTNode->tail;
    }

    // Close the output file
    fclose(outputFile);

    // Return the programAST's AST
    return programAST;
}

ast_list *Parser::declList(TOKEN *currentToken)
{
    char *tokenTypes[] = {/* Literals */
            "lx_identifier", "lx_integer", "lx_string", "lx_float",
            /* Keywords */
            "kw_program",	"kw_var", "kw_constant", "kw_integer", "kw_bool", "kw_string", "kw_float",
            "kw_true", "kw_false", "kw_if", "kw_fi", "kw_then", "kw_else",	"kw_while", "kw_do", "kw_od",
            "kw_and", "kw_or",	"kw_read", "kw_write",	"kw_for", "kw_from", "kw_to", "kw_by",
            "kw_function", "kw_procedure", "kw_return", "kw_not", "kw_begin", "kw_end",
            /* Operators */
            "lx_lparen", "lx_rparen", "lx_lbracket", "lx_rbracket", "lx_lsbracket", "lx_rsbracket",
            "Ix_colon", "lx_dot", "lx_semicolon", "lx_comma", "Ix_colon_eq",
            "lx_plus", "lx_minus", "lx_star", "lx_slash",
            "lx_eq", "lx_neq", "lx_lt", "lx_le", "lx_gt", "lx_ge", "lx_eof",
            /* Illegal */
            "illegal_token"};

    if(currentToken == NULL)
    {
        return NULL;
    }

    // Initialize the program's AST list
    AST* ast = nullptr;
  //  cout << "Current Token Type: " << tokenTypes[currentToken->type] <<endl;

    // Check the current token type and process accordingly
    switch (currentToken->type)
    {
        case kw_var:
            ast = varDecl(currentToken);   // Parse variable declaration
            if(ast == nullptr)
            {
                delete programAST;
                programAST = nullptr;
                return nullptr;
            }
            programAST = addNodeToAstList(ast, programAST);
            currentToken = scanner->Scan();
            break;

        case kw_constant:
            ast = constantDecl(currentToken);   // Parse constant declaration
            if(ast == nullptr)
            {
                delete programAST;
                programAST = nullptr;
                return nullptr;
            }
            programAST = addNodeToAstList(ast, programAST);
            currentToken = scanner->getLastToken();
            break;

        case kw_function:                     // Parse function declaration
            ast = functionDecl(currentToken);
            if(ast == nullptr)
            {
                delete programAST;
                programAST = nullptr;
                return nullptr;
            }
            programAST = addNodeToAstList(ast, programAST);
            currentToken = scanner->Scan();
            break;

        case kw_procedure:
            ast = procedureDecl(currentToken);  // Parse procedure declaration
            if(ast == nullptr)
            {
                delete programAST;
                programAST = nullptr;
                return nullptr;
            }
            programAST = addNodeToAstList(ast, programAST);
            currentToken = scanner->Scan();
            break;

        case lx_eof:                               // Parse EOF declaration
            ast = make_ast_node(ast_eof);
            if(ast == nullptr)
            {
                delete programAST;
                programAST = nullptr;
                return nullptr;
            }
            programAST = addNodeToAstList(ast, programAST);
            return programAST;

        default:
            return nullptr;
    }
    // Check if the token is followed by a semicolon
    if(!matchLexeme(currentToken, lx_semicolon))
    {
        reportError("Expected a semicolon (;) after the declaration.");
        return nullptr;
    }
    // Get the next token and continue parsing
    currentToken = scanner->Scan();
    return declList(currentToken);
}

AST *Parser::varDecl(TOKEN *token)
{
    // Initialize variables
    AST_type ast_type = ast_var_decl;
    TOKEN *identifier_token = scanner->Scan();
    int line_number = scanner->getLineNum();

    // Check if the next token is an identifier
    if (!matchLexeme(identifier_token, lx_identifier))
    {
        reportError("Please provide a valid identifier following the 'var' keyword.");
        return nullptr;
    }

    // Check for the presence of a colon
    TOKEN *colon_token = scanner->Scan();
    if (!matchLexeme(colon_token, Ix_colon))
    {
        reportError("You should include a colon (:) after the variable name in the 'var' declaration.");
        return nullptr;
    }

    // Check for the type token
    TOKEN *var_type_token = scanner->Scan();
    LEXEME_TYPE var_type = var_type_token->type;
    if (!matchDataType(var_type_token))
    {
        reportError("Please specify a data type after the colon in the 'var' declaration.");
        return nullptr;
    }

    // Get the symbol table entry for the identifier
    STEntry *ST_Entry = table->Get_entry(identifier_token->str_ptr);
    if(ST_Entry != nullptr)
    {
        reportError("Multiple Declaration for " + string(ST_Entry->Name));
        return nullptr;
    }


    // Create a new symbol table entry
    ST_Entry = table->Put_symbol(identifier_token->str_ptr);
    if(ST_Entry == nullptr)
    {
        reportError(
                "An error occurred while trying to add a new entry to the Symbol Table. There's no corresponding token available.");
        return nullptr;
    }
    // Set values for the symbol table entry
    ST_Entry = setSTEValues(ste_var, ST_Entry, line_number, getTokenType(var_type));
    if(ST_Entry->Name == nullptr)
    {
        return nullptr;
    }
    AST *ast = make_ast_node(ast_type, ST_Entry, ST_Entry->f.var.type);
    return ast;
}

ste_list* Parser::varDeclList(TOKEN* token, ste_list* STE_list_x)
{
    // If token is NULL, report an error
    if(token == nullptr)
    {
        reportError("Invalid syntax in variable declaration list.");
        return nullptr;
    }
    // If token matches 'kw_var', parse variable declaration
    else if(matchLexeme(token, kw_var))
    {
        // Parse var declaration
        AST* var_decl_ast = varDecl(token);
        if(var_decl_ast == nullptr)
        {
            return nullptr;
        }
        // Retrieve symbol table entry from the AST
        symbol_table_entry* ST_Entry = var_decl_ast->f.a_var_decl.name;
        // Add the symbol table entry to STE_list_x
        STE_list_x = addNodeToSteList(ST_Entry, STE_list_x);

        // Move to the next token (expecting ';')
        token = scanner->Scan();
        if(token == nullptr || !matchLexeme(token, lx_semicolon)){
            reportError("Expected a semicolon (;) after the variable declaration.");
            return nullptr;
        }
        token = scanner->Scan();

        // Recursively call varDeclList with the next token
        return varDeclList(token, STE_list_x);
    }

    // If token is in the FOLLOW set of varDeclList, return currentSTEList
    else if(checkFollowVarDeclList(token)){
        return STE_list_x;
    }

    // If token is not in FOLLOW set and not matching 'kw_var', report an error
    else if(!checkFollowVarDeclList(token)){
        reportError("The current token is not expected in the context of a variable declaration list.");
        return nullptr;
    }
    return nullptr;
}

// Check if the provided token is in the set of tokens that can follow a variable declaration list
bool Parser::checkFollowVarDeclList(TOKEN *token){
    if(token == nullptr){
        return false;
    }
    // Get the type of the token
    LEXEME_TYPE type = token->type;
    // Check if the token type matches any of the expected follow tokens for a variable declaration list
    return ((type == kw_read) || (type == kw_write) || (type == kw_begin) || (type == kw_if)
            ||(type == kw_for) || (type == kw_while) || (type == kw_return) || (type == kw_end)
            || (type == lx_identifier));
}


AST* Parser::constantDecl(TOKEN *token)
{
    // Initialize the AST type

    AST_type type = ast_const_decl;

    // Scan the next token as the identifier
    TOKEN* identifier_token = scanner->Scan();
    int lineNum = scanner->getLineNum();

    // Check if the token is an identifier
    if(identifier_token->type != lx_identifier)
    {
        reportError("Expect an identifier after the 'constant' keyword.");
        return nullptr;
    }

    // Check if the identifier already exists in the Symbol Table
    if(table->Get_entry(identifier_token->str_ptr) != nullptr)
    {
        reportError("Multiple declaration of " + string(identifier_token->str_ptr));
        return nullptr;
    }
    // Scan the next token as the equal sign
    TOKEN* Equal_token = scanner->Scan();

    // Check if Equal_token is null
    if(Equal_token == nullptr)
    {
        return nullptr;
    }
    // Check if Equal_token is the equal sign
    if(Equal_token->type != lx_eq)
    {
        reportError("Expect '=' after the constant identifier.");
        return nullptr;
    }
    // Scan the next token as the constant value
    TOKEN* constant_token = scanner->Scan();

    // Parse the expression and create AST
    AST* ast_exp = eExpression(constant_token);

    // Check if parsing the expression was successful
    if(ast_exp == nullptr)
    {
        return nullptr;
    }

    // Evaluate the AST expression to get the constant value
    int value = eval_ast_expr(scanner->Get_fd(), ast_exp);

    // Create a new entry in the Symbol Table for the constant
    STEntry *ST_Entry = table->Put_symbol(identifier_token->str_ptr);
    if(ST_Entry == nullptr)
    {
        return nullptr;
    }

    // Set values for the constant entry
    ST_Entry = setSTEValues(ste_const, ST_Entry, lineNum, value);

    // Create the AST node for the constant declaration
    AST *ast = make_ast_node(type, ST_Entry, ST_Entry->f.constant.value);

    return ast;
}

AST* Parser::functionDecl(TOKEN* token)
{
    // Expect the next token after 'function'
    token = scanner->Scan();

    // Check if the next token is an identifier
    if(token->type != lx_identifier)
    {
        reportError("An identifier is expected after the 'function' keyword.");
        return nullptr;
    }

    // Get the name of the function
    char* name = token->str_ptr;
    int line = scanner->getLineNum();

    // Check if the function name is already declared
    if(table->Get_entry(name) != nullptr)
    {
        reportError("The function or variable '" + string(name) + "' is already declared.");
        return nullptr;
    }

    // Get the next token
    token = scanner->Scan();

    // Parse the formal parameter list
    ste_list * formal_list = formals_list(token);

    // Get the last token of the formal parameter list
    token = scanner->getLastToken();

    // Check if the next token is ':'
    if(!matchLexeme(token, Ix_colon))
    {
       reportError("Expecting a colon (:) after the function formal list");
        return nullptr;
    }

    // Get the next token
    token = scanner->Scan();

    // Check if the next token is type
    if(!matchDataType(token)){
        reportError("Expecting a type after the colon (:)");
        return nullptr;
    }

    // Get the return type of the function
    LEXEME_TYPE type = token->type;

    // Create a symbol table entry for the function
    symbol_table_entry* ST_Entry = table->Put_symbol(name);
    ST_Entry = setSTEValues(ste_routine, ST_Entry, line, getTokenType(type));

    // Get the next token

    token = scanner->Scan();
    // Parse the function block
    AST* ast_block = blockStmnt(token);

    if(ast_block == nullptr)
        return nullptr ;

    // Create the AST node for the function declaration
    AST* func_ast = make_ast_node(ast_routine_decl, ST_Entry, formal_list, ST_Entry->f.routine.result_type, ast_block);
    //print_AST(func_ast, outputfile);
    return func_ast;
}


AST* Parser::procedureDecl(TOKEN* token)
{
    // Scan for the next token after 'procedure'
    token = scanner->Scan();

    // Check if the next token is an identifier
    if(token->type != lx_identifier)
    {
        reportError("Expect an identifier after 'procedure'");
        return nullptr;
    }

    // Get the name of the procedure and its lineNum number
    char* name = token->str_ptr;
    int lineNum = scanner->getLineNum();

    // Check if the procedure name is already declared
    if(table->Get_entry(name) != nullptr)
    {
        reportError("Multiple declaration for " + string(name));
        return nullptr;
    }

    // Create a symbol table entry for the procedure
    symbol_table_entry* st_entry = table->Put_symbol(name);
    st_entry = setSTEValues(ste_routine, st_entry, lineNum, STE_NONE);

    // Scan for the next token after the procedure name
    token = scanner->Scan();

    // Parse the formal parameter list
    ste_list * formal_list = formals_list(token);

    // Get the last token of the formal parameter list
    token = scanner->getLastToken();

    // Parse the block of the procedure
    AST* ast_block = blockStmnt(token);
    if(ast_block == nullptr)
        return nullptr;

    // Create an AST node for the procedure declaration
    AST* ast_procedure = make_ast_node(ast_routine_decl, st_entry, formal_list, st_entry->f.routine.result_type, ast_block);
    return ast_procedure;
}


//<formals_list>
ste_list* Parser::formals_list(TOKEN* token)
{
    // Enter a new scope in the symbol table for formal parameters
    table->enter_scope();
    // Check if the current token is a left parenthesis
    if(!matchLexeme(token, lx_lparen))
    {
        reportError("Expecting left parentheses");
        return nullptr;
    }

    // Move to the next token after the left parenthesis
    token = scanner->Scan();

    // Call the function to parse the list of identifiers
    return formals_list_tail(token);
}

//<formals_list_tail>
ste_list* Parser::formals_list_tail(TOKEN* token)
{
    // Check if the current token is a ')'
    if(matchLexeme(token, lx_rparen)){
        token = scanner->Scan();
        return nullptr;  // Return a null pointer to indicate the end of the list
    }
    else{
        // Create a new list to store Formals
        ste_list* formals_list = new ste_list();
        formals_list->head = nullptr;
        formals_list->tail = nullptr;

        // Call the function to parse Formals
        return formals(token, formals_list);
    }
}
//<formals>
ste_list* Parser::formals(TOKEN* token, ste_list* STE_list_x)
{
    // Check if the current token is an identifier
    if(!matchLexeme(token, lx_identifier))
    {
        reportError("Expect an identifier");
        return nullptr;
    }
    // Get the name of the identifier
    char* name  = token->str_ptr;

    // Move to the next token after the identifier
    token = scanner->Scan();

    // Check if the current token is a colon
    if(!matchLexeme(token, Ix_colon))
    {
        reportError("Expecting a colon (:) after identifier");
        return nullptr;
    }

    // Move to the next token after the colon
    token = scanner->Scan();

    // Check if the current token is a valid type
    if(!matchDataType(token))
    {
        reportError("Expecting a type after the colon (:)");
        return nullptr;
    }

    // Check if the identifier is already declared
    if(table->Get_entry(name) != nullptr)
    {
        reportError("Multiple declaration for " + string(name));
        return nullptr;
    }

    // Create a new symbol table entry for the identifier
    symbol_table_entry* ST_Entry = table->Put_symbol(name);
    int line = scanner->getLineNum();
    // Set the values for the symbol table entry
    ST_Entry = setSTEValues(ste_var, ST_Entry, line, getTokenType(token->type));

    // Add the symbol table entry to the list
    STE_list_x = addNodeToSteList(ST_Entry, STE_list_x);

    // Move to the next token after the type
    token = scanner->Scan();

    // Call the next step of parsing
    STE_list_x = formals_tail(token, STE_list_x);

    // Return the updated list of symbol table entries
    return STE_list_x;
}

// <formals_tail>
ste_list* Parser::formals_tail(TOKEN* token, ste_list* STE_list_x)
{
    // Check if the current token is a closing parentheses
    if(matchLexeme(token, lx_rparen))
    {
        token = scanner->Scan();
        return STE_list_x; // Return the updated list of symbol table entries
    }

    // Check if the current token is a comma
    else if(matchLexeme(token, lx_comma))
    {
        // Move to the next token after the comma
        token = scanner->Scan();

        // Call the next step of parsing and update the symbol table list
        STE_list_x = formals(token, STE_list_x);

        // Return the updated list of symbol table entries
        return STE_list_x;
    }
    else
    {
        reportError("Expect Right Parentheses or Comma");
        // Return the current list of symbol table entries as-is
        return STE_list_x;
    }
}


// A utility function to set values for various types of symbol table entries
symbol_table_entry* Parser::setSTEValues(ste_entry_type type, ...)
{
    // Start a variable argument list
    va_list ap;
    va_start(ap, type);

    // Get the symbol table entry pointer from the variable arguments
    symbol_table_entry* ST_Entry = va_arg(ap, symbol_table_entry*);
    ST_Entry->steType = type;

    // Based on the entry type, set the values accordingly
    switch(type)
    {
        case ste_const:
            ST_Entry->f.constant.line = va_arg(ap, int);
            ST_Entry->f.constant.value = va_arg(ap, int);
            break;
        case ste_var:
            ST_Entry->f.var.line = va_arg(ap, int);
            ST_Entry->f.var.type = (STE_TYPE)(va_arg(ap, int));
            break;
        case ste_routine:
            ST_Entry->f.routine.line = va_arg(ap, int);
            ST_Entry->f.routine.result_type = (STE_TYPE)(va_arg(ap, int));
            break;
        default:
            break;
    }

    // End the variable argument list
    va_end(ap);

    return ST_Entry;
}

// Convert a token's lexeme type to the corresponding symbol table entry type
STE_TYPE Parser::getTokenType(LEXEME_TYPE token_type)
{
    switch (token_type) {
        case kw_integer:
            return STE_INTEGER;
        case kw_float:
            return STE_FLOAT;
        case kw_string:
            return STE_STRING;
        case kw_bool:
            return STE_BOOLEAN;
        default:
            return STE_NONE;
    }
}

// Add a symbol table entry to a STE list
ste_list* Parser::addNodeToSteList(symbol_table_entry *ST_Entry, ste_list *STE_list_x){
    if(STE_list_x == nullptr || ST_Entry == nullptr){
        return STE_list_x;
    }
    ste_list *p = STE_list_x;

    // Traverse to the end of the STE list
    while(p->head != nullptr && p->tail != nullptr)
    {
        p = p->tail;
    }
    // If the list is empty, add the ST_Entry as the head
    if(p->head == nullptr)
    {
        p->head = ST_Entry;
    }
    else
    { // Create a new node and add the ST_Entry as the head of the new node
        p->tail = new ste_list();
        p->tail->head = ST_Entry;
        p->tail->tail = nullptr;
    }
    return STE_list_x;
}


AST* Parser::statement(TOKEN* token)
{
    AST* ast;

    // Check if the token is NULL
    if(token == nullptr)
    {
        return nullptr; // Return NULL if token is not valid
    }

    // Based on the token type, determine the type of statement and parse accordingly
    switch (token->type)
    {
        case kw_read:
            ast = readStmnt(token);
            break;
        case kw_write:
            ast = writeStmnt(token);
            break;
        case kw_return:
            ast = returnStmnt(token);
            break;
        case kw_for:
            ast = forStmnt(token);
            break;
        case kw_while:
            ast = whileStmnt(token);
            break;
        case kw_if:
            ast = ifStmnt(token);
            break;
        case kw_begin:
            ast = blockStmnt(token);
            break;
        case lx_identifier:
            ast = callAssignStmnt(token);
            break;
        default:
            reportError("Invalid Statement");
            return nullptr;
    }
    return ast;
}

AST* Parser::ifStmnt(TOKEN* token)
{
    token = scanner->Scan(); // Get the next token

    // Parse the expression for the 'if' condition
    AST* ast_Exp = eExpression(token);
    if(ast_Exp == nullptr)
    {
        return nullptr; // Return NULL if expression parsing fails
    }

    token = scanner->getLastToken(); // Get the last token read

    // Check if 'then' keyword is present after the expression
    if(!matchLexeme(token, kw_then))
    {
        reportError("Expecting then, 'then' keyword is missing"); // Report error if 'then' keyword is missing
        return nullptr;
    }

    token = scanner->Scan(); // Get the next token

    // Parse the consequent block of the 'if' statement
    AST* ast_conseq = statement(token);
    if(ast_conseq == nullptr)
    {
        return nullptr; // Return NULL if parsing of consequent block fails
    }

    token = scanner->Scan(); // Get the next token

    // Call the function to parse the optional 'else' part of the 'if' statement
    return ifTail(token, ast_Exp, ast_conseq);
}


AST* Parser::ifTail(TOKEN* token, AST* ast_Exp, AST* ast_conseq)
{
    AST  *if_ast, *ast_alter;

    // Check if 'fi' keyword is present, indicating the end of the if statement
    if(matchLexeme(token, kw_fi))
    {
        ast_alter = nullptr; // No alternative block (no 'else' part)
    }
    // Check if 'else' keyword is present, indicating the presence of an alternative block
    else if(matchLexeme(token, kw_else))
    {
        token = scanner->Scan(); // Get the next token
        ast_alter = statement(token); // Parse the alternative block
        if(ast_alter == nullptr)
        {
            return nullptr; // Return NULL if parsing of alternative block fails
        }

        token = scanner->Scan(); // Get the next token

        // Check if 'fi' keyword is present after the alternative block
        if(!matchLexeme(token, kw_fi))
        {
            reportError("Expecting fi keyword");
            return nullptr;
        }
    }
    else
    {
        reportError("Expecting fi keyword or else keyword");
        return nullptr;
    }

    // Create an AST node for the if statement, including its condition, consequent block, and alternative block
    if_ast = make_ast_node(ast_if, ast_Exp, ast_conseq, ast_alter);
    return if_ast; // Return the constructed AST node for the if statement
}

AST* Parser::whileStmnt(TOKEN* token)
{
    // Check if the token matches the 'while' keyword
    if(token == nullptr || !matchLexeme(token, kw_while))
    {
        return nullptr;
    }
    token = scanner->Scan();

    // Parse the loop condition expression
    AST* ast_Exp = eExpression(token);
    if(ast_Exp == nullptr)
    {
        return nullptr;
    }
    token = scanner->getLastToken();

    // Check if the next token is the 'do' keyword
    if(token == nullptr || !matchLexeme(token, kw_do))
    {
        reportError("Expect 'do' keyword");
        return nullptr;
    }
    token = scanner->Scan();

    // Parse the loop body statement
    AST* ast_stmnt = statement(token);
    if(ast_stmnt == nullptr)
    {
        return nullptr;
    }

    // Check for the 'od' keyword
    token = scanner->Scan();
    if(token == nullptr || !matchLexeme(token, kw_od))
    {
        reportError("Expect 'od' keyword. ");
        return nullptr;
    }

    // Create an AST node for the while loop
    AST* while_ast = make_ast_node(ast_while, ast_Exp, ast_stmnt);
    return while_ast;
}



AST* Parser::forStmnt(TOKEN* token)
{
    // Check if the token matches the 'for' keyword
    if(!matchLexeme(token, kw_for))
    {
        reportError("Invalid for loop");
        return nullptr;
    }
    delete token;
    token = scanner->Scan();

    // Check if the next token is an identifier
    if(token == nullptr || !matchLexeme(token, lx_identifier))
    {
        reportError("Expect an identifier. ");
        return nullptr;
    }

    // Parse the assignment statement for the loop variable
    AST* ast_assign = assignStmnt(token);
    token = scanner->getLastToken();
    if(ast_assign == nullptr){
        return nullptr;
    }

    // Check if the next token is the 'to' keyword
    if(token == nullptr || !matchLexeme(token, kw_to))
    {
        reportError("Expect 'to' keyword");
        return nullptr;
    }

    token = scanner->Scan();

    // Parse the loop condition expression
    AST* ast_Exp = eExpression(token);
    if(ast_Exp == nullptr){
        return nullptr;
    }
    token = scanner->getLastToken();

    // Check if the next token is the 'do' keyword
    if(token == nullptr || !matchLexeme(token, kw_do))
    {
        reportError("Expect 'do' keyword");
        return nullptr;
    }
    token = scanner->Scan();

    // Parse the loop body statement
    AST* ast_stmnt = statement(token);

    // Check for the 'od' keyword
    token = scanner->Scan();
    if(ast_stmnt == nullptr){
        return nullptr;
    }
    if(token == nullptr || !matchLexeme(token, kw_od)){
        reportError("Expected 'od' keyword ");
        return nullptr;
    }

    // Create an AST node for the for loop
    AST* for_ast = make_ast_node(ast_for, ast_assign->f.a_var.var, ast_assign, ast_Exp, ast_stmnt);
    return for_ast;
}

AST* Parser::readStmnt(TOKEN* token)
{
    // Check if the token matches the 'read' keyword
    if(!matchLexeme(token, kw_read))
    {
        return nullptr; // Return NULL if the token is not 'read'
    }
    delete token; // Delete the used token
    token = scanner->Scan(); // Get the next token

    // Check if the next token is '('
    if(!matchLexeme(token, lx_lparen))
    {
        reportError("Expect '(' after Read keyword");
        return nullptr; // Return NULL if '(' is not found
    }
    delete token; // Delete the used token
    token = scanner->Scan(); // Get the next token

    // Check if the next token is an identifier
    if(!matchLexeme(token, lx_identifier))
    {
        reportError("Expect identifier after '(' in Read statement");
        return nullptr; // Return NULL if an identifier is not found
    }
    char *name = token->str_ptr;

    // Look up the identifier in the symbol table
    symbol_table_entry* ST_Entry = table->Get_symbol(name);
    if(ST_Entry == nullptr)
    {
        reportError("Undefined Variable: " + string(name));
        return nullptr; // Return NULL if the variable is not found in the symbol table
    }

    token = scanner->Scan(); // Get the next token

    // Check if the next token is ')'
    if(!matchLexeme(token, lx_rparen))
    {
        reportError("Expect ')' after Read");
        return nullptr; // Return NULL if ')' is not found
    }

    // Create an AST node for the Read statement
    AST* ast = make_ast_node(ast_read, ST_Entry);
    return ast; // Return the constructed AST node
}
AST* Parser::writeStmnt(TOKEN* token)
{
    // Check if the token matches the 'write' keyword
    if(!matchLexeme(token, kw_write))
    {
        return nullptr; // Return nullptr if the token is not 'write'
    }
    delete token; // Delete the used token
    token = scanner->Scan(); // Get the next token

    // Check if the next token is '('
    if(!matchLexeme(token, lx_lparen))
    {
        reportError("Expect '(' in Write");
        return nullptr; // Return nullptr if '(' is not found
    }
    delete token; // Delete the used token
    token = scanner->Scan(); // Get the next token

    // Check if the next token is an identifier
    if(!matchLexeme(token, lx_identifier))
    {
        reportError("Expect identifier in Write");
        return nullptr; // Return nullptr if an identifier is not found
    }
    char *name = token->str_ptr;

    // Look up the identifier in the symbol table
    symbol_table_entry* ST_Entry = table->Get_symbol(name);
    if(ST_Entry == nullptr)
    {
        reportError("Undefined Variable " + string(name));
        return nullptr; // Return nullptr if the variable is not found in the symbol table
    }
    token = scanner->Scan(); // Get the next token

    // Check if the next token is ')'
    if(!matchLexeme(token, lx_rparen))
    {
        reportError("Expect ')' in Write");
        return nullptr; // Return nullptr if ')' is not found
    }

    // Create an AST node for the Write statement
    AST* ast = make_ast_node(ast_write, ST_Entry);
    return ast; // Return the constructed AST node
}

AST* Parser::returnStmnt(TOKEN* token)
{
    // Check if the token matches 'kw_return'
    if(!matchLexeme(token, kw_return)){
        return nullptr;
    }
    delete token;
    token = scanner->Scan();

    // Check for opening parenthesis after 'return'
    if(!matchLexeme(token, lx_lparen)){
        reportError("Expect '(' in Return");
        return nullptr;
    }
    delete token;
    token = scanner->Scan();

    // Parse the expression following 'return'
    AST* ast_Exp = eExpression(token);
    if(ast_Exp == nullptr){
        return nullptr;
    }
    token = scanner->getLastToken();

    // Check for closing parenthesis after the expression
    if(!matchLexeme(token, lx_rparen)){
        reportError("Expect ')' in Return");
        return nullptr;
    }

    // Create an AST node for the return statement
    AST* ast = make_ast_node(ast_return, ast_Exp);
    return ast;
}


AST* Parser::blockStmnt(TOKEN* token)
{
    // Initialize offset for variable declaration offsets
    offset = 0;


    // Check if the token matches the 'begin' keyword
    if(token == nullptr || !matchLexeme(token, kw_begin))
    {
        reportError("Expected begin");
        return nullptr;
    }
    token = scanner->Scan();

    // Initialize a linked list for variable declarations
    ste_list* var_dec_list;
    var_dec_list = new ste_list();
    var_dec_list->head = nullptr;
    var_dec_list->tail = nullptr;

    // Parse variable declaration list and assign offsets
    var_dec_list = varDeclList(token, var_dec_list);
    ste_list* p = var_dec_list;
    while(p != nullptr)
    {
        if(p->head == nullptr)
        {
            break;
        }
        p->head->offset = offset;
        offset++;
        p = p->tail;
    }

    // Return if no variable declarations are found
    if(var_dec_list == nullptr)
        return nullptr;

    token = scanner->getLastToken();

    // Initialize a linked list for statement parsing
    ast_list* stmnt_list;
    stmnt_list = new ast_list();
    stmnt_list->head = nullptr;
    stmnt_list->tail = nullptr;

    // Parse statement list
    stmnt_list = statementlist(token, stmnt_list);

    // Return if no statements are found
    if(stmnt_list == nullptr)
    {
        return nullptr;
    }

    token = scanner->getLastToken();

    // Check if the token matches the 'end' keyword
    if(token == nullptr || !matchLexeme(token, kw_end))
    {
        reportError("Expect 'end' keyword");
        return nullptr;
    }

    // Set lists to nullptr if they contain no elements
    if((var_dec_list != nullptr) && (var_dec_list->head == nullptr) && (var_dec_list->tail == nullptr))
    {
        var_dec_list = nullptr;
    }
    if((stmnt_list != nullptr) && (stmnt_list->head == nullptr) && (stmnt_list->tail == nullptr))
    {
        stmnt_list = nullptr;
    }

    // Create an AST node for the block
    AST* block_ast = make_ast_node(ast_block, var_dec_list, stmnt_list);
    return block_ast;
}



ast_list* Parser::statementlist(TOKEN* token, ast_list* AST_list_x)
{
    // Check if the token is NULL, indicating an invalid statement list
    if(token == nullptr)
    {
        reportError("Invalid Statement List");
        return nullptr;
    }
        // Check if the token matches the 'end' keyword, indicating the end of the statement list
    else if(matchLexeme(token, kw_end))
    {
        return AST_list_x;
    }
    else
    {
        // Parse the current statement
        AST* ast_stmnt = statement(token);
        if(ast_stmnt == nullptr)
        {
            return nullptr;
        }
        // Add the parsed statement to the AST list
        AST_list_x = addNodeToAstList(ast_stmnt, AST_list_x);

        // Update the token based on the type of the parsed statement
        if(ast_stmnt->type == ast_assign)
        {
            token = scanner->getLastToken();
        }
        else
        {
            token = scanner->Scan();
        }

        // Check if the token matches the ';' symbol
        if(token == nullptr || !matchLexeme(token, lx_semicolon))
        {
            reportError("Expects ';' in parse statement list");
            return nullptr;
        }
        token = scanner->Scan();

        // Recursively parse the next statement in the list
        return statementlist(token, AST_list_x);
    }
    return nullptr;
}


AST* Parser::assignStmnt(TOKEN* token)
{
    // Check if the token is nullptr or an identifier
    if(token == nullptr || !matchLexeme(token, lx_identifier))
    {
        reportError(" Expect an identifier");
        return nullptr; // Return nullptr if token is not an identifier
    }
    char* name = token->str_ptr;

    // Look up the identifier in the symbol table
    symbol_table_entry *ST_Entry = table->Get_symbol(name);
    if(ST_Entry == nullptr)
    {
        reportError("Undeclared Variable or Function");
        return nullptr; // Return nullptr if the identifier is not found in the symbol table
    }

    // Check if the symbol table entry represents a variable
    if(ST_Entry->steType != ste_var)
    {
        reportError("You can't assign non variables");
        return nullptr; // Return nullptr if the entry is not a variable
    }
    token = scanner->Scan(); // Get the next token

    // Check if the next token is ':='
    if(token == nullptr || !matchLexeme(token, Ix_colon_eq))
    {
        reportError("Expect := ");
        return nullptr; // Return nullptr if ':=' is not found
    }
    token = scanner->Scan(); // Get the next token

    // Parse the expression on the right-hand side of the assignment
    AST* ast_Exp = eExpression(token);
    if(ast_Exp == nullptr)
    {
        return nullptr; // Return nullptr if expression parsing fails
    }

    // Create an AST node for the assignment statement
    AST* assign_ast = make_ast_node(ast_assign, ST_Entry, ast_Exp);
    return assign_ast;
}

AST* Parser::callStmnt(TOKEN* token)
{
    // Check if the token is nullptr or an identifier
    if(token == nullptr || !matchLexeme(token, lx_identifier))
    {
        reportError("Expect an Identifier");
        return nullptr; // Return nullptr if token is not an identifier
    }
    char* name = token->str_ptr;

    // Look up the identifier in the symbol table
    symbol_table_entry *ST_Entry = table->Get_symbol(name);
    if(ST_Entry == nullptr)
    {
        reportError("Undeclared Variable or Function. ");
        return nullptr; // Return nullptr if the identifier is not found in the symbol table
    }

    // Check if the symbol table entry represents a routine (function or procedure)
    if(ST_Entry->steType != ste_routine)
    {
        reportError("You can't call anything other than routines ");
        return nullptr; // Return nullptr if the entry is not a routine
    }
    token = scanner->Scan(); // Get the next token

    // Parse the argument list for the function call
    ast_list* AST_list_x = argsList(token);

    // Create an AST node for the function call statement
    AST* call_ast = make_ast_node(ast_call, ST_Entry, AST_list_x);
    return call_ast;
}

AST* Parser::callAssignStmnt(TOKEN* token)
{
    // Check if the token is nullptr or an identifier
    if(token == nullptr || !matchLexeme(token, lx_identifier))
    {
        reportError("Expects an identifier");
        return nullptr; // Return nullptr if token is not an identifier
    }

    char* name  = token->str_ptr;

    // Look up the identifier in the symbol table
    symbol_table_entry* ST_Entry = table->Get_symbol(name);
    if(ST_Entry == nullptr)
    {
        reportError("Undeclared Variable or Function");
        return nullptr; // Return nullptr if the identifier is not found in the symbol table
    }

    // Check the type of the symbol table entry
    if(ST_Entry->steType == ste_var)
    {
        return assignStmnt(token); // Parse assignment statement if the entry is a variable
    }
    if(ST_Entry-> steType == ste_routine)
    {
        return callStmnt(token); // Parse function call statement if the entry is a routine
    }
    reportError("Invalid statement");
    return nullptr; // Return nullptr for any other cases
}



ast_list *Parser::argsList(TOKEN* token){
    // Check if the token is NULL or doesn't matchLexeme '('
    if(token == nullptr || !matchLexeme(token, lx_lparen))
    {
        reportError("Expect '(' ");
        return nullptr;
    }
    // Move to the next token
    token = scanner->Scan();

    // Call the function to parse the argument tail
    return argsListTail(token);
}

ast_list* Parser::argsListTail(TOKEN* token)
{
    // Check if the token matches ')'
    if(matchLexeme(token, lx_rparen)){
        // Move to the next token and return NULL indicating the end of arguments
        token = scanner->Scan();
        return nullptr;
    }
    else{
        // Create a new list to hold the arguments
        ast_list* args_list = new ast_list();
        args_list->head = nullptr;
        args_list->tail = nullptr;

        // Call the function to parse the actual arguments
        return args(token, args_list);
    }
}

ast_list* Parser::args(TOKEN* token, ast_list *AST_list_x)
{
    // Parse an expression as an argument
    AST* ast_Exp = eExpression(token);
    if(ast_Exp == nullptr){
        return nullptr;
    }

    // Add the parsed expression to the argument list
    AST_list_x = addNodeToAstList(ast_Exp, AST_list_x);

    // Get the last token from the scanner
    token = scanner->getLastToken();

    // Continue parsing the remaining arguments
    AST_list_x = argsTail(token, AST_list_x);

    return AST_list_x;
}

ast_list* Parser::argsTail(TOKEN* token, ast_list *AST_list_x)
{
    // Check for null token
    if(token == nullptr)
    {
        reportError("Null args_end token");
        return nullptr;
    }

    // If a comma is found, parse more arguments
    if(matchLexeme(token, lx_comma))
    {
        delete token;
        token = scanner->Scan();

        // Parse additional arguments and update the AST_list_x
        AST_list_x = args(token, AST_list_x);

        // Check if parsing arguments was successful
        if(AST_list_x == nullptr)
        {
            return nullptr;
        }

        // Get the last token from the scanner
        token = scanner->getLastToken();

        // Continue parsing remaining arguments
        AST_list_x = argsTail(token, AST_list_x);
        return AST_list_x;
    }
    // If a closing parenthesis is found, return the argument list
    if(matchLexeme(token, lx_rparen))
    {
        token = scanner->Scan();
        return AST_list_x;
    }

    // Return null for any other cases
    return nullptr;
}



// Add an AST node to an AST list
ast_list* Parser::addNodeToAstList(AST* ast, ast_list* AST_list_x)
{
    if (AST_list_x == nullptr || ast == nullptr)
    {
        return AST_list_x;
    }

    ast_list* p = AST_list_x;

    // Traverse to the end of the AST list
    while (p->head != nullptr && p->tail != nullptr)
    {
        p = p->tail;
    }

    // If the list is empty, add the AST node as the head
    if (p->head == nullptr)
    {
        p->head = ast;
    }
    else
    {
        // Create a new node and add the AST node as the head of the new node
        p->tail = new ast_list();
        p->tail->head = ast;
        p->tail->tail = nullptr;
    }
    return AST_list_x;
}

AST* Parser::eExpression(TOKEN* token)
{
    AST* E, *F;
    // Parse the term F
    F = fExpression(token);
    E = F;
    token = scanner->getLastToken();
    // Parse the rest of the expression using extendedeExpression
    E = extendedeExpression(E, token);
    return E;
}

AST* Parser::extendedeExpression(AST* E, TOKEN* token)
{
    AST* Eb, *F;
    Eb = new AST();
    // Check if either E or token is NULL
    if(token == nullptr || E == nullptr){
        return nullptr;
    }
    // Check if token matches any AND or OR operator
    if (relconjOp(token))
    {
        switch(token->type){
            case kw_and:
                Eb->type  = ast_and;
                Eb->f.a_binary_op.type = ast_and;
                break;
            case kw_or:
                Eb->type = ast_or;
                Eb->f.a_binary_op.type = ast_or;
                break;
            default:
                break;
        }
        token = scanner->Scan();
        // Parse the term F
        F = fExpression(token);
        // Set left and right arguments for the binary operation
        Eb->f.a_binary_op.larg = E;
        Eb->f.a_binary_op.rarg = F;
        token = scanner->getLastToken();
        // Recursively parse the rest of the expression using extendedeExpression
        E = extendedeExpression(Eb, token);
    }
        // Check if token is not in the FOLLOW set of extendedeExpression
    else if(checkFollowExtE(token) == false)
    {
        return nullptr;
    }
    return E;
}


// Check if the provided token is in the set of tokens that can follow an Extended E (E tail) production
bool Parser::checkFollowExtE(TOKEN *token)
{
    if(token == nullptr)
    {
        return false;
    }
    // Get the type of the token
    LEXEME_TYPE type = token->type;
    // Check if the token type matches any of the expected follow tokens for an Extended E (E tail) production
    return ((type == lx_rparen) || (type == kw_do) || (type == kw_then) || (type == kw_to)
            || (type == lx_semicolon) || (type == lx_comma));
}


AST* Parser::fExpression(TOKEN* token)
{
    AST* F, *G;
    // Parse the factor G
    G = gExpression(token);
    F = G;
    token = scanner->getLastToken();
    // Parse the rest of the factor using extendedfExpression
    F = extendedfExpression(F, token);
    return F;
}

AST* Parser::extendedfExpression(AST* F, TOKEN* token)
{
    AST* Fb, *G;
    Fb = new AST();
    // Check if token matches any Comparison Operations
    if(relOp(token)){
        switch(token->type){
            case lx_eq:
                Fb->type = ast_eq;
                Fb->f.a_binary_op.type = ast_eq;
                break;
            case lx_neq:
                Fb->type = ast_neq;
                Fb->f.a_binary_op.type = ast_neq;
                break;
            case lx_gt:
                Fb->type = ast_gt;
                Fb->f.a_binary_op.type = ast_gt;
                break;
            case lx_ge:
                Fb->type = ast_ge;
                Fb->f.a_binary_op.type = ast_ge;
                break;
            case lx_lt:
                Fb->type = ast_lt;
                Fb->f.a_binary_op.type = ast_lt;
                break;
            case lx_le:
                Fb->type = ast_le;
                Fb->f.a_binary_op.type = ast_le;
                break;
            default:
                break;
        }
        token = scanner->Scan();
        // Parse the factor G
        G = gExpression(token);
        // Set left and right arguments for the binary operation
        Fb->f.a_binary_op.larg = F;
        Fb->f.a_binary_op.rarg = G;
        token = scanner->getLastToken();
        // Recursively parse the rest of the factor using extendedfExpression
        F = extendedfExpression(Fb, token);
    }
    return F;
}

AST* Parser::gExpression(TOKEN* token)
{
    AST* G, *H;
    // Parse the term H
    H = hExpression(token);
    G = H;
    token = scanner->getLastToken();
    // Parse the rest of the term using extendedgExpression
    G = extendedgExpression(G, token);
    return G;
}


AST* Parser::extendedgExpression(AST * G, TOKEN* token)
{
    AST* Gb, *H;
    Gb = new AST();
    // Check if token matches any Arithmetic Operations Plus/Minus
    if(plusminOp(token))
    {
        switch(token->type)
        {
            case lx_plus:
                Gb->type = ast_plus;
                Gb->f.a_binary_op.type = ast_plus;
                break;
            case lx_minus:
                Gb->type = ast_minus;
                Gb->f.a_binary_op.type = ast_minus;
                break;
            default:
                break;
        }
        token = scanner->Scan();

        // Parse the term H
        H = hExpression(token);
        // Set left and right arguments for the binary operation
        Gb->f.a_binary_op.larg = G;
        Gb->f.a_binary_op.rarg = H;
        token = scanner->getLastToken();
        // Recursively parse the rest of the term using extendedgExpression
        G = extendedgExpression(Gb, token);
    }
    return G;
}

AST* Parser::hExpression(TOKEN* token)
{
    AST* H, *I;
    // Parse the factor I
    I = iExpression(token);
    H = I;
    token = scanner->getLastToken();
    // Parse the rest of the factor using extendedhExpression
    H = extendedhExpression(H, token);
    return H;
}

AST* Parser::extendedhExpression(AST* H, TOKEN* token)
{
    AST* Hb, *I;
    Hb = new AST();
    // Check if token matches any Arithmetic Operations MULT/DIV
    if(muldivOp(token))
    {
        switch(token->type){
            case lx_star:
                Hb->type = ast_times;
                Hb->f.a_binary_op.type = ast_times;
                break;
            case lx_slash:
                Hb->type = ast_divide;
                Hb->f.a_binary_op.type = ast_divide;
                break;
            default:
                break;
        }
        token = scanner->Scan();
        // Parse the factor I
        I = iExpression(token);
        // Set left and right arguments for the binary operation
        Hb->f.a_binary_op.larg = H;
        Hb->f.a_binary_op.rarg = I;
        token = scanner->getLastToken();
        // Recursively parse the rest of the factor using extendedhExpression
        H = extendedhExpression(Hb, token);
    }
    return H;
}
AST* Parser::iExpression(TOKEN* token)
{
    AST* I;
    I = new AST();

    // Check if token matches any UNARY Operations
    if(unaryOp(token))
    {
        switch(token->type){
            case kw_not:
                I->type = ast_not;
                break;
            case lx_minus:
                I->type = ast_uminus;
                break;
            default:
                break;
        }
        token = scanner->Scan();
        // Parse the argument I of the unary operation
        I->f.a_unary_op.arg = iExpression(token);
    }
        // Check if token matches Integer, Left parentheses , or Identifier
    else if(matchLexeme(token, lx_integer) || matchLexeme(token, lx_lparen) || matchLexeme(token, lx_identifier))
    {
        // Parse the expression J
        I = jExpression(token);
    }
    else
    {
        return nullptr; // Invalid token, return nullptr
    }
    return I;
}

AST* Parser::jExpression(TOKEN* token){
    AST* J;
    J = new AST();

    // Check if token matches '(' indicating the start of an expression
    if(matchLexeme(token, lx_lparen))
    {
        token = scanner->Scan();
        // Parse the expression E inside the parentheses
        J = eExpression(token);
        token = scanner->getLastToken();
        // Check if the closing ')' token is present
        if (token->type !=lx_rparen){
            reportError("Expecting ')' ");
            return nullptr;
        }
        token = scanner->Scan();
        return J;
    }
        // Check if token matches an integer literal
    else if(matchLexeme(token, lx_integer))
    {
        J->type = ast_integer;
        J->f.a_integer.value = token->value;
        token = scanner->Scan();
        return J;
    }
        // Check if token matches a floating-point literal
    else if(matchLexeme(token, lx_float)){
        J->type = ast_float;
        J->f.a_float.value = token->float_value;
        token = scanner->Scan();
        return J;
    }
        // Check if token matches a string literal
    else if(matchLexeme(token, lx_string)){
        J->type = ast_string;
        J->f.a_string.string = token->str_ptr;
        token = scanner->Scan();
        return J;
    }
        // Check if token matches the 'true' keyword
    else if(matchLexeme(token, kw_true)){
        J->type = ast_boolean;
        J->f.a_boolean.value = 1;
        token = scanner->Scan();
        return J;
    }
        // Check if token matches the 'false' keyword
    else if(matchLexeme(token, kw_false)){
        J->type = ast_boolean;
        J->f.a_boolean.value = 0;
        token = scanner->Scan();
        return J;
    }
        // Check if token matches an identifier
    else if(matchLexeme(token, lx_identifier)){
        symbol_table_entry* ST_Entry = table->Get_symbol(token->str_ptr);
        if(ST_Entry == nullptr){
            reportError("Undefined Variable");
            return nullptr;
        }
        if(ST_Entry->steType == ste_var){
            J->type = ast_var;
            J->f.a_var.var = table->Get_symbol(token->str_ptr);
            token = scanner->Scan();
            return J;
        }
        else if(ST_Entry->steType == ste_routine){
            // Parse the call statement and return the corresponding AST node
            J = callStmnt(token);
            token = scanner->Scan();
            return J;
        }
    }
    // None of the expected patterns matched, report an error
    {
        reportError(" Expecting ( or int ");
        return nullptr;
    }
}



// Check if the given token matches the expected type
bool Parser::matchLexeme(TOKEN* token, LEXEME_TYPE type)
{
    if(token==nullptr)
    {
        token = new TOKEN();
    }
    return (token->type == type);
}

// Check if the type of the provided token matches any of the valid data types
bool Parser::matchDataType(TOKEN* token)
{
    LEXEME_TYPE token_type = token->type;
    return ((token_type == kw_integer) || (token_type == kw_string) || (token_type == kw_float) || (token_type == kw_bool));
}

// Check if the type of the provided token matches any of the logical AND or OR operators
bool Parser::relconjOp(TOKEN* token)
{
    LEXEME_TYPE type = token->type;
    return ((type == kw_and) || (type == kw_or));
}


// Check if the type of the provided token matches any of the comparison operators
bool Parser::relOp(TOKEN* token)
{
    LEXEME_TYPE type = token->type;
    return((type == lx_eq) || (type == lx_neq) || (type == lx_lt) || (type == lx_le) || (type == lx_gt) || (type == lx_ge));
}

// Check if the type of the provided token matches any of the addition or subtraction operators
bool Parser::plusminOp(TOKEN* token)
{
    LEXEME_TYPE type = token->type;
    return ((type == lx_plus) || (type == lx_minus));
}


// Check if the type of the provided token matches any of the multiplication or division operators
bool Parser::muldivOp(TOKEN* token)
{
    LEXEME_TYPE type = token->type;
    return ((type == lx_star) || (type == lx_slash));
}

// Check if the type of the provided token matches any of the unary operators (NOT or negation)
bool Parser::unaryOp(TOKEN* token)
{
    LEXEME_TYPE type = token->type;
    return ((type == lx_minus) || (type == kw_not) );
}


void Parser::reportError(string message)
{
    scanner->Get_fd()->ReportError(message);

}

void Parser::print_AST(AST *ast, FILE* file)
{
    if(ast == nullptr)
    {
        return;
    }
    print_ast_node(file, ast);
}