#include "stable.h"
#include <iostream>
#include <cctype> // Include the cctype header for the tolower() function

using namespace std;

void STable::init(int size)
{
    Size = size;
    Table = new STList[size];
    fold_case = 0; // sensitive
}
/**
 * @brief STable::STable : Default constructor, call init with DEFAULT_SIZE
 */
STable::STable()
{
    init(DEFAULT_SIZE);
}
/**
 * @brief STable::STable  constructor with Number of slots = size, call init with size
 * @param size
 */
STable::STable(unsigned long size)
{
    init(size);
}
/**
 * @brief STable::Reset   Recreate the Table by calling clear then call init
 * @param size
 */
STable::STable(int fold_case_flag)
{

    // Initialize the hash table size and fold_case flag
    init(DEFAULT_SIZE);
    fold_case = fold_case_flag;
    entryLists = new STEntry *[Size];  // Allocate memory for the entryLists array (hash table)

    // Initialize each slot in the hash table to NULL
    for (int i = 0; i < Size; i++)
    {
        entryLists[i] = NULL;
    }
    // Set the head of the symbol table stack to this instance
    head = this;
    // Initialize the next pointer of this instance to NULL
    next = NULL;
}

void STable::Reset(unsigned long size)
{
    Clear();
    init(size);
}


/**
 * @brief STable::ElfHash : symbolTableHash Function
 * @param str  : Key which is the Variable Name to be added to the Symbol Table
 * @return
 */
unsigned long STable::ElfHash( char *str )
{
    if (fold_case)
    { // If fold_case is true, convert the string to lowercase before hashing
        for (int i = 0; str[i] != '\0'; i++)
        {
            str[i] = tolower(str[i]);
        }
    }

    unsigned long   h = 0, high; // h=0, g
    unsigned char *s = (unsigned char *) str;
    while ( *s )
    {
        h = ( h << 4 ) + *s++;  //1/4 of bits
        if ( high = h & 0xF0000000 )
            h ^= high >> 24; // h = h ^ (g >>24)
        h &= ~high; // in book this is h = h^g
    }
    return h%Size;
}
/**
 * @brief STable::AddEntry : Adds the Entry, call symbolTableHash to find index, then call Add to the List in Table[index] to add it
 * @param name : Name of Entry to be added
 * @param type : type of Entry. See the Main function for example how to find it
 * @return  true if added and false otherwise, the List Table[index] Already returns this for you
 */
bool STable::AddEntry(char *name, STE_TYPE type)
{
    // Convert name to lowercase if fold_case is true
    if (fold_case)
    {
        for (int i = 0; name[i] != '\0'; i++)
        {
            name[i] = tolower(name[i]);
        }
    }

    // Get the hash value (index) for the given name
    unsigned long index = ElfHash(name);

    // Check if the entry already exists in the current scope
    STEntry* entry = Table[index].FindEntry(name);
    if (entry != nullptr)
    {
       // cout << "Error: Entry " << name << " already exists in the current scope." << endl;
        return false;
    }

    // Create a new entry and add it to the linked list
    entry = new STEntry(name, type);
    entry->Next = entryLists[index]->Next;
    entryLists[index]->Next = entry;

    return true;
}
/**
 * @brief STable::FindAndPrintEntry Finds and prints the Entry if it is found
 *                if Not found print the Message Not found, see the example result
 * @param name  : name to search for
 * @param fp  : File pointer to print : See Example in reults
 */
void STable::FindAndPrintEntry(char *name, FILE *fp)
{
    // Convert name to lowercase if fold_case is true
    if (fold_case)
    {
        for (int i = 0; name[i] != '\0'; i++)
        {
            name[i] = tolower(name[i]);
        }
    }

    // Look for the entry in the current scope and its parent scopes
    STable* currentScope = head;
    while (currentScope != nullptr)
    {
        unsigned long index = currentScope->ElfHash(name);
        STEntry* entry = currentScope->Table[index].FindEntry(name);
        if (entry != nullptr)
        {
            fprintf(fp, "%s: Found = %s\n", name, entry->toString());
            return;
        }
        currentScope = currentScope->next; // Move to the parent scope
    }

    // Entry not found in any scope
    fprintf(fp, "%s: not found\n", name);
}
/**
 * @brief STable::PrintAll : Prints all Elements. Print the Count of each list and the Entries.
 *                            See the Sample Result (fout.txt) to format your output
 *                            Loop through the Slots (Lists) and Call Table[i].printAll
 * @param fp
 */

void STable::PrintAll(FILE *fp)
{
    unsigned long i;
    for (i = 0; i < Size; i++)
    {
        fprintf(fp, "T[%lu]: entries:\t", i);

        STEntry* entry = entryLists[i]->Next; // Skip the dummy node
        while (entry != nullptr)
        {
            fprintf(fp, "%s ", entry->toString());
            entry = entry->Next;
        }

        fprintf(fp, "\n");
    }
}

/**
 * @brief STable::Clear : Delete All Elements, use a loop and call the Table[i].clear. Then Delete the Array Table
 */
void STable::Clear()
{
    unsigned long i;
    for (i =0; i < Size; i++)
    {
        Table[i].Clear();
    }
}
STable::~STable()
{
    Clear();
    delete[] Table;

}

void STable::ClearSymbolTable()
{
    // Clear the entries in the current scope
    Clear();

    // Clear the entries in parent scopes
    STable* currentScope = head;
    while (currentScope != nullptr)
    {
        currentScope->Clear();
        currentScope = currentScope->next;
    }
}

void STable::PrintSymbolStats()
{
    // Print the number of entries in the symbol table
    int totalEntries = 0;
    for (unsigned long i = 0; i < Size; i++)
    {
        totalEntries += Table[i].Count();
    }
    printf("Total number of entries in the symbol table: %d\n", totalEntries);

    // Print the number of free slots in the symbol table array (entryLists)
    int freeSlots = 0;
    for (unsigned long i = 0; i < Size; i++)
    {
        if (entryLists[i] == nullptr)
        {
            freeSlots++;
        }
    }
    printf("Total number of free slots in the symbol table: %d\n", freeSlots);

    // Print the maximum scope depth
    int maxScopeDepth = 0;
    STable* currentScope = head;
    while (currentScope != nullptr)
    {
        maxScopeDepth++;
        currentScope = currentScope->next;
    }
    printf("Maximum scope depth in the symbol table: %d\n", maxScopeDepth);

    // Print the length of the search chains in each STList
    int maxLength = 0;
    for (unsigned long i = 0; i < Size; i++)
    {
        int chainLength = Table[i].Count();
        if (chainLength > maxLength)
        {
            maxLength = chainLength;
        }
    }
    printf("Maximum length of the search chains in the symbol table: %d\n", maxLength);
}

STEntry* STable::Put_symbol(char* name)
{
    // Convert the name to lowercase if fold_case is true
    if (fold_case) {
        for (size_t i = 0; name[i] != '\0'; ++i) {
            name[i] = tolower(name[i]);
        }
    }

    // Compute the hash index for the entry
    unsigned long index = ElfHash(name);

    // Get the current entry at the hash index
    STEntry* currentEntry = entryLists[index];

    // Check if the symbol already exists in the symbol table
    while (currentEntry != nullptr) {
        if (strcmp(currentEntry->Name, name) == 0) {
            // Symbol already exists, return the existing entry
          //  cout << "Symbol " << name << " already exists in the symbol table" << endl;
            return currentEntry;
        }
        currentEntry = currentEntry->Next;
    }

    // Symbol does not exist, create a new entry
    STEntry* newEntry = new STEntry();
    strcpy(newEntry->Name, name);
    // Add the new entry to the linked list at the hash index


    newEntry->Next = entryLists[index];
    entryLists[index] = newEntry;

  //  cout << "Added symbol " << name << " to the symbol table" <<endl;
    return newEntry;
}


STEntry* STable::Get_symbol(char* str)
{
    STable *currentScope = head;
    STEntry* entry=NULL;
    while(currentScope !=NULL &&   ( (entry = currentScope->Get_entry(str)) == NULL )  )
    {
        currentScope = currentScope->next;
    }
    return entry;

}

void STable::enter_scope()
{
    STable* newTable = new STable(fold_case); // Create a new symbol table with the same fold_case setting
    newTable->next = head;
    head = newTable;
}

void STable::exit_scope()
{
    head = head->next;
}

STEntry *STable::Get_entry(char *key)
{
    // Calculate the hash index using the ElfHash algorithm
    int index = ElfHash(key);

    // Traverse the linked list at the calculated index to find the entry with the given key
    STEntry *current_entry = entryLists[index];

    // Iterate through the linked list until the end or until a matching key is found
    while (current_entry != nullptr && (strcmp(current_entry->Name, key) != 0)) {
        current_entry = current_entry->Next;
    }

    // Return the found entry (or nullptr if not found)
    return current_entry;
}