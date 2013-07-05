// Michael Coulombe and Bryan Lee

#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

#include<vector>
#include<algorithm>
#include<set>

struct BranchInfo;

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   //List<CgenNode> *nodes;
   std::vector<CgenNodeP> nodes;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
   
   int maxClassTag;

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   
   void code_protos();
   void code_vtables();
   void code_nameTab_objTab_parentTab();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
   
   CgenNodeP find(Symbol c);
   
   void code_methods(Environment& env);
};

template<class F>
struct FeatTable {
    struct FeatDesc {
        Symbol className;
        F* impl;
    };
    std::vector<FeatDesc> list;
    
    // returns index of m in methods, or -1 if DNE
    int indexOf(Symbol f) const {
        for(int i = 0; i < (int) list.size(); i++) {
            if (list[i].impl->name == f) {
                return i;
            }
        }
        return -1;
    }
    
    // inserts or overrides (cn,i) into list
    // sets isFinal to false to an overridden method
    void insert(Symbol cn, F* i) {
        FeatDesc fd = {cn, i};
        
        int pos = indexOf(i->name);
        
        if (pos == -1) {
            list.push_back(fd);
        }
        else {
            list[pos].impl->isFinal = false;
            list[pos] = fd;
        }
    }
};
typedef FeatTable<method_class> VTable;
typedef FeatTable<attr_class> ATable;


class CgenNode : public class__class {
private: 
    CgenNodeP parentnd;              // Parent of class
    std::vector<CgenNodeP> children; // Children of class
    Basicness basic_status;          // `Basic' if class is basic, `NotBasic' otherwise
    int classTag;

    ATable atable;   // table of all attributes and their source class
    VTable vtable;   // table of all methods and their source class
   
public:
    CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

    void add_child(CgenNodeP child);
    std::vector<CgenNodeP>& get_children() { return children; }
    void set_parentnd(CgenNodeP p);
    CgenNodeP get_parentnd() { return parentnd; }
    int basic() { return (basic_status == Basic); }
    
    bool isSubclassOf(CgenNodeP other);
    VTable::FeatDesc lookupMethod(Symbol m);
    
    void createClassTag(int& tag); // recursively set tags such that A.tag > B.tag implies A not subclass B 

    int getClassTag() { return classTag; }
    
    void getClassTagRange(int& low, int& high); // B is an A iff low <= B.tag <= high
    
    int getSize() { return 3 + atable.list.size(); }

    void makeTables();   // initializes atable and vtable

    void code_proto(ostream& s);
    void code_vtable(ostream& s);

    int getAttrIndex(Symbol a);
    int getMethIndex(Symbol m);

    void code(Environment&, ostream&);
    void code_init(Environment& env, ostream& s);
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

class Environment {
private:
    CgenClassTable classTable;
    int labelCounter;
    
public:
    Environment(CgenClassTable ct);
    
    void codeMethods() { classTable.code_methods(*this); }
    
    CgenNode* curClass;
    
    SymbolTable<Symbol,int> localid_offset_table; // offsets from FP of stack variables
    
    int nextLabel();
    int get_attribute_offset(Symbol a);
    int get_method_offset(Symbol c, Symbol m);
    
    struct {
        int temps;
        void init() { temps = -3; }
        int getFreeOffset() { return temps--; }
        void releaseLastOffset() { ++temps; }
    } current_method;
    
    CgenNodeP getClassNode(Symbol c);
};
