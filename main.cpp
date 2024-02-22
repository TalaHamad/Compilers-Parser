#include <iostream>
#include "Parser.h"
using namespace std;

int main()
{
        FileDescriptor *fd = new FileDescriptor("C:\\Users\\AB\\CLionProjects\\CompilerParser\\input.txt");
        Parser *parser = new Parser(fd);
        ast_list* program = parser->parse();
        return 0;
}


