#include "FileDescriptor.h"
int count =0;
FileDescriptor::FileDescriptor() {
    file = NULL;
    fileName = "";
    line_number = 0;
    char_number = 0;
}

FileDescriptor::FileDescriptor(char* fileName)
{
    // Open the file for reading
    fp.open(fileName, ios::in);
    line_number = 1;
    char_number = 0;
    buffer = new char[BUFFER_SIZE];
    // Read the first line from the file into the buffer
    fp.getline(buffer, BUFFER_SIZE);
    // Get the size of the line read

    lineSize = strlen(buffer);
}

FileDescriptor::FileDescriptor(string fileName)
{
    this->fileName = fileName;

    file = fopen(fileName.c_str(), "r");
        line_number = 0;
        char_number = 0;
        buffer = NULL;

}

std::string FileDescriptor::GetFileName()
{
    return fileName;
}

bool FileDescriptor::IsOpen()
{
    if(file == NULL)
        return false;

    else
        return true;
}

char* FileDescriptor::GetCurrLine()
{
    return buffer;
}

int FileDescriptor::GetLineNum()
{
    return line_number;
}

int FileDescriptor::GetCharNum()
{
    return char_number;
}

void FileDescriptor::Close()
{
    if (file != nullptr)
    {
        fclose(file);
        file = nullptr;
    }
}

char FileDescriptor::GetChar()
{
    char c = buffer[char_number];

    // If we reached the end of the line, we need to read the next line from the file
    if (lineSize == char_number)
    {
        // If we reached the end of the file, return EOF

        if (isEOF())
            return EOF;
        else
        {
            // Read the next line from the file into the buffer
            fp.getline(buffer, BUFFER_SIZE);

            // Update the line size and reset the character counter
            lineSize =  strlen(buffer);
            char_number = 0;

            // Increase the line number since we are moving to the next line
            line_number++;

            // Return a newline character to indicate the line change
            return '\n';
        }
    }
    char_number++;
    return c;
}


void FileDescriptor::ReportError(string message)
{
    // Initialize an empty string to store the line without newline characters
    string line;
    string errormsg;
    if (count ==0)
    {
        cout << "Report an Error: " << endl;
        cout << message << endl;
        count++;
    }
    else
        count=0;
/*
    // Iterate through bufferString to construct the line
    for (size_t i = 0; i < bufferString.size(); i++)
    {
        // Skip newline characters
        if (bufferString.at(i) != '\n')
        {
            line.push_back(bufferString.at(i));
        }
    }
    // Append the line to the error message
    errormsg += line ;

    // Create an arrow pointing to the error position
    string arrow(GetCharNum() - 2, ' ');
    arrow.push_back('^');

    // Append the arrow to the error message
    errormsg += arrow + "\n";

    // Append the error message parameter to the error message
    errormsg += "Line #" + to_string(GetLineNum()) +
                " Char #" + to_string(GetCharNum()-1) ;

    cout << errormsg << endl;*/
   // cout << message << endl;
    return;
}




void FileDescriptor::UngetChar (char c){
    char_number--;
}

FileDescriptor::~FileDescriptor()
{
    fp.close();
    delete[]buffer;
}
bool FileDescriptor::isEOF() {
    return fp.eof();
}

