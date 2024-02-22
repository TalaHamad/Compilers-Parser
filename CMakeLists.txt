cmake_minimum_required(VERSION 3.23)
project(CompilerParser)

set(CMAKE_CXX_STANDARD 14)

add_executable(CompilerParser main.cpp FileDescriptor.h FileDescriptor.cpp Scanner.h Scanner.cpp stlist.h stentry.h stlist.cpp stable.h stable.cpp ast.h ast.cpp Parser.h Parser.cpp)
