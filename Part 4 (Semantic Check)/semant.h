#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class InheritanceNode;
class ClassTable;
typedef ClassTable *ClassTableP;


//
// The environment of a COOL  expression can be completely characterized
// by 
//    1. A symbol table describing method bindings.
//    2. A symbol table describing attribute/local variable bindings.
//    3. A symbol table of all classes.
//    4. The class in which the expression occurs (the class of SELF_TYPE)
//
class Environment
{
private:
  SymbolTable<Symbol, method_class> method_table;
  SymbolTable<Symbol, Entry> var_table;
  ClassTable *class_table;
  Class_ self_class;
public:
  Environment(ClassTable* ct, InheritanceNode *sn);
  Environment(SymbolTable<Symbol, method_class> mt,
	  SymbolTable<Symbol, Entry>  vt,
	  ClassTable *ct,
	  InheritanceNode *sn);
	  
	  Environment *clone_Environment(InheritanceNode *n);
	  
    // class table operations
    //
    // Standard ways to report errors.
    //
    ostream& semant_error();
    ostream& semant_error(tree_node *t);
    
    InheritanceNode *lookup_class(Symbol s);

    // method table operations 
    void method_add(Symbol s, method_class *m);
    method_class *method_lookup(Symbol s);
    method_class *method_probe(Symbol s);
    void method_enterscope();
    void method_exitscope();

    // attribute table operations
    void var_add(Symbol s, Symbol typ);
    Symbol var_lookup(Symbol s);
    Symbol var_probe(Symbol s);
    void var_enterscope();
    void var_exitscope();

    // type operations
    Symbol get_self_type();
    int type_leq(Symbol subtype, Symbol supertype);
    Symbol type_lub(Symbol t1, Symbol t2);
	InheritanceNode* get_attrs_class(Symbol,Symbol);
	InheritanceNode* get_methods_class(Symbol,Symbol);
	Feature method_is_in_class(InheritanceNode*, Symbol);
	Feature attr_is_in_class(InheritanceNode*, Symbol);
};


// A node of the inheritance graph is a Cool class with associated info:
//     1. parent in the inheritance graph
//     2. children " "       "        "
//     3. can inherit/can't inherit from this class
//     4. basic/not basic class
//     5. this class is reachable/unreachable from the Object class
//           via the "inherits from" relation
//     6. a type checking environment
//
class InheritanceNode : public class__class
{
private:
  InheritanceNode *parent;
  List<InheritanceNode> *children;
  bool inheritable;
  bool builtin;
  bool reachable;
  Environment *env;
  
public:
  InheritanceNode(Class_ c, bool inheritStatus, bool builtinStatus):
    class__class((const class__class &) *c), parent(NULL), children(NULL),
    inheritable(inheritStatus), builtin(builtinStatus), reachable(false), 
    env(NULL)
    { }
  bool is_inheritable() { return inheritable; }
  bool is_builtin() { return builtin; }
  bool is_reachable() { return reachable; }
  void mark_reachable() { reachable = true; }
  void add_child(InheritanceNode *c) { children = new List<InheritanceNode>(c, children); }
  List<InheritanceNode> *get_children() { return children; }
  void set_parent(InheritanceNode *p) { parent = p; }
  InheritanceNode *get_parent_node(){ return parent; }

  //
  // The type checking environment of class X is established by copying 
  // the environment of X's parent and setting setting the self class to be 
  // X.
  //
  void copy_env(Environment *e) { env = e->clone_Environment(this); }
  void build_feature_tables();
  
  //
  // For the root Object class, a fresh environment structure is created.
  //
  // Allocate new Environment structure.  Presently used only for the
  // root (Object) class; all other classes make a copy of their parent's
  // Environment.
  //
  void init_env(ClassTable *ct) { env = new Environment(ct,this); }
  void type_check_features();
  void check_main_method();
  method_class *method_lookup(Symbol s) { return env->method_lookup(s); }
  
};

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable : public SymbolTable<Symbol, InheritanceNode> {
private:
  	List<InheritanceNode> *classes;
  	int semant_errors;
  	void install_basic_classes();
  	void install_classes(Classes);
  	void install_class(InheritanceNode *);
  	void build_tree();
  	bool check_tree();
  	ostream& error_stream;
	InheritanceNode* get_features_class(InheritanceNode*, Symbol, bool);
	Feature feature_is_in_class(Features, Symbol, bool);

public:
  	ClassTable(Classes);
  	int errors() { return semant_errors; }
  	ostream& semant_error();
  	ostream& semant_error(Class_ c);
  	ostream& semant_error(Symbol filename, tree_node *t);
  
  	InheritanceNode *root();
  	InheritanceNode* get_class(Symbol name);
	InheritanceNode* get_attrs_class(Symbol, Symbol);
	InheritanceNode* get_methods_class(Symbol, Symbol);
	Feature method_is_in_class(InheritanceNode*, Symbol);
	Feature attr_is_in_class(InheritanceNode*, Symbol);
};


#endif

