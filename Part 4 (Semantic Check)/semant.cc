//Bryan Lee, 996417066

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

int curr_lineno;
extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}



InheritanceNode *ClassTable::root()
{
  return probe(Object);
}


ClassTable::ClassTable(Classes cs) : classes(NULL), semant_errors(0) , error_stream(cerr)
{
  enterscope();
  install_basic_classes();
  install_classes(cs);
  build_tree();             //check classes and construct the tree
  if(check_tree())             //checks inheritance for circles
  	for(int i = cs->first(); cs->more(i); i = cs->next(i)){
  		Environment* env = new Environment(this,get_class(cs->nth(i)->get_name()));
  		//cout << "Going to Install Class: " << cs->nth(i)->get_name() << endl;
		cs->nth(i)->tc(env);
  		delete env;
  	}
  exitscope();
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);
	  
    install_class(new InheritanceNode(Object_class, true, true)); // Object is built in, can be inherited from
    // fixme - need to add remaining basic classes as well

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  
	install_class(new InheritanceNode(IO_class, true, true));
    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);
	install_class(new InheritanceNode(Int_class, false, true));

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
	install_class(new InheritanceNode(Bool_class, false, true));
    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);
	install_class(new InheritanceNode(Str_class, false, true));
	
	//classes = append_Classes(classes,single_Classes(Object_class));
	//classes = append_Classes(classes,single_Classes(IO_class));
	//classes = append_Classes(classes,single_Classes(Int_class));
	//classes = append_Classes(classes,single_Classes(Bool_class));
	//classes = append_Classes(classes,single_Classes(Str_class));
}

void ClassTable::install_classes(Classes cs)
{
	bool main_found = false;
  	for(int i = cs->first(); cs->more(i); i = cs->next(i)){
		if(cs->nth(i)->get_name() == Main) main_found = true;
    	install_class(new InheritanceNode(cs->nth(i),true,false));
	}
	if(!main_found)
		cerr << "error: Class \"Main\" is not defined" << endl;
}

void ClassTable::install_class(InheritanceNode *nd)
{
	//cout << "Installing: " << nd->get_name() << endl;
	//cout << "Classes: ";
	//for(List<InheritanceNode>* l = classes; l;l = l->tl()){
	//	InheritanceNode* i = l->hd();
	//	cout << i->get_name() << ", ";
	//}
	//cout << endl;
  	Symbol name = nd->get_name();
  	for(List<InheritanceNode>* l = classes; l; l = l->tl()){
    	InheritanceNode* i = l->hd();
    	if(name == i->get_name()){
      		semant_error(nd) << " Class \"" << name << "\" is redefined." << endl;
      		//cerr << "error: class " << name << " is redefined." << endl;
      		return;
    	}
  	}
  // fixme - need to check if name already exists

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  	//cout << nd->get_name() << " Parent: " << nd->get_parent() << endl;
	//if(nd->get_name() != Object) 
	//	cout << "get_class: " << get_class(nd->get_parent())->get_name() << endl;
	if(nd->get_name() != Object)
		nd->set_parent(get_class(nd->get_parent()));
	classes = new List<InheritanceNode>(nd,classes);
 	//if(nd->get_name() != Object) 
	//	cout << nd->get_name() << " Parent (again): " << nd->get_parent_node()->get_name() << endl;
	addid(name,nd);
}

void ClassTable::build_tree(){
  for(List<InheritanceNode>* l = classes; l; l = l->tl()){
    InheritanceNode* i = l->hd();
    if(i->get_name() == Object) continue;
	//if(i->get_parent() == Object || i->get_parent() == IO) continue;
    if(i->get_parent() == Int || i->get_parent() == Bool || i->get_parent() == Str)
      semant_error(i) << " Class \"" << i->get_name() 
                        << "\" cannot inherit class " << i->get_parent() << endl;
    //look for non-basic parent
    bool foundParent = false;
    for(List<InheritanceNode>* k = classes; k; k = k->tl()){
      InheritanceNode* j = k->hd();
      if(i->get_parent() == j->get_name()) 
      { i->set_parent(j); j->add_child(i); foundParent = true; break; }
    }
    if(!foundParent) 
      semant_error(i) << " Class \"" << i->get_name() 
                        << " inherits from an undefined class " 
                        << i->get_parent() << endl;
  }
}

//check for circles
bool ClassTable::check_tree(){
 	bool circle = false;
  	for(List<InheritanceNode>* l = classes; l; l = l->tl()){
    	circle = false;
    	InheritanceNode* i = l->hd();
    	for(InheritanceNode* parent = i->get_parent_node();parent; parent = parent->get_parent_node()){
      		if(parent->get_name() == i->get_name()) { circle = true; break; }
    	}
    	if(circle) semant_error(i) << " Class \"" << i->get_name() << "\" has an inheritance cycle" << endl;
  	}
	return !circle;
}
InheritanceNode* ClassTable::get_class(Symbol name){
  for(List<InheritanceNode>* l = classes; l; l=l->tl()){
    InheritanceNode* i = l->hd();
    if(i->get_name() == name) return i;
  }
  return NULL;
}
InheritanceNode* ClassTable::get_attrs_class(Symbol current_class, Symbol attr)
{ return get_features_class(get_class(current_class),attr,false); }
InheritanceNode* ClassTable::get_methods_class(Symbol current_class, Symbol method)
{ return get_features_class(get_class(current_class),method,true); }
InheritanceNode* ClassTable::get_features_class(InheritanceNode* cl, Symbol feat, bool method){
	for(;cl && cl->get_name() != No_class; cl = cl->get_parent_node())
		if((!method && attr_is_in_class(cl,feat)) 
		|| (method && method_is_in_class(cl,feat))) return cl;
	return NULL;
}
Feature ClassTable::method_is_in_class(InheritanceNode* cl, Symbol method_)
{ return feature_is_in_class(cl->get_features(),method_,true); }
Feature ClassTable::attr_is_in_class(InheritanceNode* cl, Symbol attr_)
{ return feature_is_in_class(cl->get_features(),attr_,false); }
Feature ClassTable::feature_is_in_class(Features feats, Symbol feat, bool method){
	for(int i = feats->first(); feats->more(i); i = feats->next(i))
		if((feats->nth(i)->isMethod() == method) && (feats->nth(i)->get_name() == feat)) 
			return feats->nth(i);
	return NULL;
}
///////////////////////////////////////////////////////////////////////
//
//  Enviroment functions.
//
//  The Cool type checking rules require four structures to typecheck a 
//  class X.  These four items are encapsulated in an Environment:
//
//     a) a mapping from method names to method definitions for X
//     b) a mapping from variable (local and attribute) names to
//         definitions in X
//     c) a mapping from method names and class names to defintions
//         for methods of classes other than X
//     d) the self class (X)
//
//     c) is realized using a class_table, which contains a mapping
//        from class names to InheritanceNodes (and thereby to Environments)
//        for all classes.
//
//////////////////////////////////////////////////////////////////////

Environment::Environment(ClassTable *ct, InheritanceNode *sc): 
   method_table(*(new SymbolTable<Symbol, method_class>())),
   var_table(*(new SymbolTable<Symbol, Entry>())),
   class_table(ct),
   self_class(sc)
{ 
  method_table.enterscope();
  var_table.enterscope();
  var_table.addid(self,SELF_TYPE);  // self : SELF_TYPE in all environments
}

Environment::Environment(SymbolTable<Symbol, method_class> mt,
			 SymbolTable<Symbol, Entry>       vt,
			 ClassTable *ct,
			 InheritanceNode *sc):
		  method_table(mt), // copies the method_table
		  var_table(vt),    // copies the var_table
      class_table(ct),
		  self_class(sc)
{ 
  // push a new scope for each of the method/variable tables,
  // so that new methods/variables will not conflict with existing ones.
  method_table.enterscope();
  var_table.enterscope();
}

//
// Most "new" environments are duplicates of a parent class environment
// with the self class replaced.  An small but important point is that
// the method table and var table structures are copied, so that 
// additions to the new environment have no effect on the original.
//
Environment *Environment::clone_Environment(InheritanceNode *n) 
{ return new Environment(method_table,var_table,class_table,n); }

ostream& Environment::semant_error()
{ return class_table->semant_error(); }

ostream& Environment::semant_error(tree_node *t) 
{ return class_table->semant_error(self_class->get_filename(),t); }

InheritanceNode *Environment::lookup_class(Symbol s) 
{ return class_table->probe(s); }

void Environment::method_add(Symbol s, method_class *m) 
{ method_table.addid(s,m); }
method_class* Environment::method_lookup(Symbol s)
{ return method_table.lookup(s); }
method_class* Environment::method_probe(Symbol s)
{ return method_table.probe(s); }
void Environment::method_enterscope() { method_table.enterscope(); }
void Environment::method_exitscope() { method_table.exitscope(); }

void Environment::var_add(Symbol s, Symbol typ) { var_table.addid(s,typ); }
Symbol Environment::var_lookup(Symbol s) { return var_table.lookup(s); }
Symbol Environment::var_probe(Symbol s) { return var_table.probe(s); }
void Environment::var_enterscope() { var_table.enterscope(); }
void Environment::var_exitscope() { var_table.exitscope(); }

Symbol Environment::get_self_type() { return self_class->get_name(); }
int Environment::type_leq(Symbol subtype, Symbol supertype){
  	//cout << "Environment::type_leq enterred" << endl;
	if(subtype == SELF_TYPE) subtype = get_self_type();
	if(supertype == SELF_TYPE) supertype = get_self_type();
	InheritanceNode* sub = lookup_class(subtype);
  	InheritanceNode* sup = lookup_class(supertype);
  //InheritanceNode* node = sub;
  	//cout << "subtype: " << subtype << " | supertype: " << supertype << endl;
	int dif = 0;
	if(supertype == Object){
		while(sub->get_name() != Object) { sub = sub->get_parent_node(); ++dif; }
		return dif;
	}
	for(; sub->get_name() != Object; sub = sub->get_parent_node(), ++dif){
		//cout << "sub name: " << sub->get_name() << " | super name: " << sup->get_name() << endl;
		//cout << "sub == super" << (sub->get_name() == sup->get_name()) << endl;
		if(sub->get_name() == sup->get_name()) return dif;	
	}
	return -1;	  
	/*for(; node && node->get_name() != sup->get_name() && node->get_name() != No_class; ++dif){
    node = node->get_parent_node();
  }
  if(!node || node->get_name() == No_class) return -1;
  return dif;*/
}
Symbol Environment::type_lub(Symbol t1, Symbol t2){
  	//cout << "Environment::type_lub enterred" << endl;
	InheritanceNode* t1_node = lookup_class(t1);
	InheritanceNode* t2_node = lookup_class(t2);
  	Symbol lub;
  	if(type_leq(t1,t2) >= 0){
    	for(InheritanceNode* node = t2_node; node && node->get_name() != No_class; node = node->get_parent_node())
      		for(InheritanceNode* i = t1_node; i && i->get_name() != No_class; i = i->get_parent_node())
        		if(node->get_name() == i->get_name()) return node->get_name();
  	}
  	else
    	for(InheritanceNode* node = t1_node; node && node->get_name() != No_class; node = node->get_parent_node())
      		for(InheritanceNode* i = t2_node; i && i->get_name() != No_class; i = i->get_parent_node())
        		if(node->get_name() == i->get_name()) return node->get_name();
  	return Object;
}
InheritanceNode* Environment::get_attrs_class(Symbol current_class, Symbol attr)
{ return class_table->get_attrs_class(current_class,attr); }
InheritanceNode* Environment::get_methods_class(Symbol current_class, Symbol method)
{ return class_table->get_methods_class(current_class,method); }
Feature Environment::method_is_in_class(InheritanceNode* cl, Symbol method_)
{ return class_table->method_is_in_class(cl,method_); }
Feature Environment::attr_is_in_class(InheritanceNode* cl, Symbol attr_)
{ return class_table->attr_is_in_class(cl,attr_); }

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 


/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}
void class__class::checkAttrRedefine(Environment* env){
	//cout << "class__class::checkAttrRedefine enterred" << endl;
	InheritanceNode* node = env->lookup_class(name);
	std::set<Symbol> attrs;
	class__class* c = this;
	while(c && c->get_name() != Object){
		Features features = c->get_features();
		for(int i = features->first(); features->more(i); i = features->next(i)){
           	Feature f = features->nth(i);
           	if(!f->isMethod()){
            	if(attrs.count(f->get_name()) > 0){
                  	env->semant_error(this) << "error: Attribute " << f->get_name() 
                    	                  	<< " is previously defined" << endl;
                  	break;
               	}
               	attrs.insert(f->get_name());
            }
         }
         node = node->get_parent_node(); 
         c = node;
         //error, circle
         if(c->get_name() == this->get_name()) break;
    }
}
bool class__class::checkMethodRedefine(Environment* env){
  	//cout << "class__class::chechMethodRedefine enterred" << endl;
	InheritanceNode* current_class = env->lookup_class(name);
  	//cout << "checkMethodRedefine: Node: " << current_class->get_name() << endl;
	class__class* c = this;
  	Features features = c->get_features();
  	for(int i = features->first(); features->more(i); i = features->next(i)){
		Feature f = features->nth(i);
     	if(f->isMethod())
        	for(int j = features->next(i); features->more(j); j = features->next(j)){
           		Feature feat = features->nth(j);
           		if(feat->isMethod() && feat->get_name() == f->get_name())
              		env->semant_error(this) << "error: Method \"" << feat->get_name() 
                    			            << "\" is previouly defined in class \""
                                			<< this->get_name() << "\"" << endl;
        		}
  		}
  	//check parent methods with current methods
  	InheritanceNode* node = current_class;
  	for(int i = features->first(); features->more(i); i = features->next(i)){
     	Feature f = features->nth(i);
     	if(f->isMethod()){
        	method_class* m = dynamic_cast<method_class*>(f);
			node = current_class->get_parent_node();
        	c = node;
        while(c && c->get_name() != No_class){
           Features parent_features = c->get_features();
           for(int j = parent_features->first(); 
                 parent_features->more(j); 
                 j = parent_features->next(j)){
              Feature parent_f = parent_features->nth(j);
              if(parent_f->isMethod()){
                 method_class* parent_m = dynamic_cast<method_class*>(parent_f);
                 if(m->get_name() == parent_m->get_name()){
                    if(m->get_return_type() != parent_m->get_return_type()){
                       env->semant_error(this) << "error: Method \"" << m->get_name() 
                                         << "\" return type \"" 
                                         << m->get_return_type() 
                                         << "\" does not match parent class \""
                                         << parent_m->get_name() 
                                         << "\" return type \"" 
                                         << parent_m->get_return_type() << "\"" << endl;
						//cout << "checkMethodRedefine exit" << endl;
                       return false;
                    }
                    Formals forms = m->get_formals();
                    Formals parent_forms = parent_m->get_formals();
					formal_class* parent_form;
                    for(int k = forms->first(); forms->more(k); k = forms->next(k)){
                       formal_class* form = dynamic_cast<formal_class*>(forms->nth(k));
                       if(parent_forms->more(k)) parent_form = dynamic_cast<formal_class*>(parent_forms->nth(k));
                       else {
                          env->semant_error(this) << "error: Method \"" << m->get_name()
                                               << "\" does not have same number"
                                               << " of parameters as parent "
                                               << "class \"" << parent_m->get_name()
                                               << "\"" << endl;
							//cout << "checkMethodRedefine exit" << endl;
                          return false;
                       }
                       if(form->get_type_decl() != parent_form->get_type_decl()){
                          env->semant_error(this) << "error: Method \"" << m->get_name()
                                               << "\" parameter type \""
                                               << form->get_type_decl()
                                               << "\" differs from parent "
                                               << "parameter type \""
                                               << parent_form->get_type_decl()
                                               << "\"" << endl;
                          //cout << "checkMethodRedefine exit" << endl;
							return false;
                       }
                    }
                 }
              }
           }
			if(node->get_name() == Object) break;
           node = node->get_parent_node();
           c = node;
           if(c && c->get_name() == this->get_name()) break;
        }
    	method_class* method_ = dynamic_cast<method_class*>(features->nth(i));
		method_->tc(env);
		env->method_add(current_class->get_name(),method_);
  		}	
	}
	//cout << "checkMethodRedefine exit sucess" << endl;
 	 return true;
}
void class__class::checkFormalRedefine(Environment* env){
  //class__class* c = this;
  //cout << "class__class::checkFormalRedefine enterred" << endl;
  Features features = this->get_features();
  for(int i = features->first(); features->more(i); i = features->next(i)){
     Feature f = features->nth(i);
     if(f->isMethod()){
        method_class* m = dynamic_cast<method_class*>(f);
        if(m->checkFormalRedefine(env)) env->method_add(m->get_name(),m);
        
     }
  }
}
bool method_class::checkFormalRedefine(Environment* env){
	//cout << "method_class::checkFormalRedefine enterred" << endl;
	bool good = true;
  for(int i = formals->first(); formals->more(i); i = formals->next(i)){
     Formal f = formals->nth(i);
     for(int j = formals->next(i); formals->more(j); j = formals->next(j)){
        Formal f1 = formals->nth(j);
        if(f->get_name() == f1->get_name()){
           env->semant_error(this) << "error: Formal parameter \"" << f->get_name()
                             << "\" is defined mutiple times in Method \"" 
                             << this->name << "\"" << endl;
           good = false;
           break;
        }
     }
  }
  return good;
}
Symbol class__class::tc(Environment *env){
	//cout << "class__class::tc enterred" << endl;
	if(name == Object || name == IO || name == Bool || name == Int || name == Str){
		//type = name;
		return name;
	}
	if(!env->lookup_class(parent)){
		env->semant_error(this) << "error: inherited Parent \"" << parent 
														<< "\" does not exist" << endl;
	}
	env->method_enterscope();
	env->var_enterscope();
	env->var_add(self,SELF_TYPE);
	std::set<Symbol> class_features;
	bool main_method_found = false;
	method_class* main_m;
	for(int i = features->first(); features->more(i); i = features->next(i)){
		Feature feat = features->nth(i);
		/*if(class_features.count(feat->name) > 0){
			env->semant_error(this) << "error: Feature \"" << feat->name
															<< "\" has been previously defined" << endl;
			continue;
		}
		class_features.insert(feat->name);*/
		if(feat->isMethod() && feat->get_name() == main_meth){
			main_method_found = true;
			main_m = dynamic_cast<method_class*>(feat);
		}
		if(!feat->isMethod()){
			attr_class* attr_ = dynamic_cast<attr_class*>(features->nth(i));
			attr_->tc(env);
		}
	}
	if(!main_method_found && name == Main)
		env->semant_error(this) << "error: No \"main\" method in class Main" << endl;
	checkAttrRedefine(env);
	checkMethodRedefine(env);
	//checkFormalRedefine(env);
	/*std::set<Symbol> recheck;
	for(int i = features->first(); features->more(i); i = features->next(i)){
		Feature feat = features->nth(i);
		if(class_features.count(feat->name) > 0) continue;
		if(feat->isMethod()){
			method_class* method_ = dynamic_cast<method_class*>(features->nth(i));
			env->method_add(feat->name,method_->tc(env));
		}
		recheck.insert(feat->name);
	}*/
	env->var_exitscope();
	env->method_exitscope();
	//type = name;
	return name;
}
Symbol method_class::tc(Environment *env){
	//cout << "method_class::tc" << endl;
	env->method_enterscope();
	env->var_enterscope();
	//cout << "method::tc Self: " << env->get_self_type() << endl;
	std::set<Symbol> forms;
	//cout << "Line 663" << endl;
	if(name == main_meth && env->get_self_type() == Main && formals->more(0))
		env->semant_error(this) << "error: main method in class Main cannot take parameters" << endl;
	for(int i = formals->first(); formals->more(i); i = formals->next(i)){
		Formal form = formals->nth(i);
		if(forms.count(form->get_name()) > 0){
			env->semant_error(this) << "error: Formal \"" << name
									<< "\" is already declared" << endl;
		}
		else{
			forms.insert(form->get_name());
			form->tc(env);
		}
	}
	//cout << "Line 674" << endl;
	env->var_add(self,SELF_TYPE);
	//cout << "method::tc before expr-tc(env); in " << get_name() << endl;
	Symbol expr_type = expr->tc(env);
	//cout << "method::tc after expr-tc(env);" << endl;
	//cout << "Method: Expr Type: " << expr->get_type() << endl;
	//cout << "METHOD: Expr_type: " << expr_type << endl;
	Symbol current_return_type = return_type;
	//cout << "method::tc after current_return_type" << endl;
	if(expr_type == SELF_TYPE) expr_type = env->get_self_type();
	if(return_type == SELF_TYPE) current_return_type = env->get_self_type();
  	InheritanceNode* i = env->lookup_class(expr_type);
  	//cout << "Line: 683" << endl;
	//cout << "METHOD: " << get_name() << endl;
	if(!i){
    	env->semant_error(this) << "error: Type \"" << expr_type
        	                    << "\" does not exist within this scope" << endl;
    	env->method_exitscope();
		env->var_exitscope();
    	//type = Object;
		return Object;
  	}
	//cout << "Line: 684" << endl;
  	i = env->lookup_class(current_return_type);
  	if(!i){
    	env->semant_error(this) << "error: return Type \"" << current_return_type
     	 	                    << "\" does not exist within this scope" << endl;
    	env->method_exitscope();
		env->var_exitscope();
    	//type = Object;
    	return Object;
  	}
	//cout << "Line: 694" << endl;
  	if(env->type_leq(expr_type,current_return_type) < 0){
    	env->semant_error(this) << "error: Expression Type \"" << expr_type
        	                    << "\" does not conform to  return Type \""
            	                << current_return_type << "\"" << endl;
    	env->method_exitscope();
		env->var_exitscope();
    	//type = Object;
    	return Object;
  	}
  env->method_exitscope();
	env->var_exitscope();
  //type = return_type;
  //cout << "Leaving Method::tc" << endl;
  return current_return_type;
}
Symbol attr_class::tc(Environment *env){
	//cout << "attr_class::tc enterred" << endl;
  	init->tc(env);
  	InheritanceNode* i = env->lookup_class(type_decl);
  	if(!i){
    	env->semant_error(this) << "error: Type \"" << type_decl
       		                    << "\" does not exist within this scope" << endl;
    	//env->var_add(name,Object);
    	//type = Object;
    	return Object;
  	}
  	if(init->get_type() != No_type && init->get_type() != type_decl){
    	//cout << "Variable Name: " << name << endl;
		env->semant_error(this) << "error: Initialization Type \"" << init->get_type()
        	                    << "\" does not match Variable Type \""
            	                << type_decl << "\"" << endl;
    	env->var_add(name,type_decl);
    	//type = Object;
    	return Object;
  	}
  	env->var_add(name,type_decl);
  	//type = type_decl;
  	return type_decl;
}
Symbol formal_class::tc(Environment *env){
	//cout << "formal_class::tc enterred" << endl;
  	InheritanceNode* i = env->lookup_class(type_decl);
  	if(!i){
    	env->semant_error(this) << "error: Type \"" << type_decl
        	                    << "\" does not exist within this scope" << endl;
    	//env->var_add(name,Object);
    	//type = Object;
    	return Object;
  	}
  	if(name == self){
  		env->semant_error(this) << "error: Formal cannot be named self" << endl;
  		//type = Object;
    	return Object;
  	}
  	/*if(env->var_probe(name)){
  	env->semant_error(this) << "error: formal \"" << name 
  													<< "\" is already declared" << endl;
  	//type = Object;
    return Object;
  }*/
	//cout << "FORMAL: adding Var: " << name << " | Type: " << type_decl << endl;
  	env->var_add(name,type_decl);
	//cout << "FORMAL: env->var_lookup: " << env->var_lookup(name) << endl;
  //type = type_decl;
	return type_decl;
}
Symbol branch_class::tc(Environment *env){
	env->method_enterscope();
	env->var_enterscope();
  	InheritanceNode* i = env->lookup_class(type_decl);
	//Symbol expr_type = expr->get_type();
  	//if(type_decl == SELF_TYPE) type_decl = env->get_self_type();
	//if(expr->get_type() == SELF_TYPE) expr_type = env->get_self_type();
	if(!i){
    	env->semant_error(this) << "error: Type \"" << type_decl
        	                    << "\" does not exist within this scope" << endl;
    	expr->tc(env);
		return Object;	
  	}
	env->var_add(name,type_decl);
	expr->tc(env);
  	/*if(env->type_leq(expr_type, type_decl) < 0){
    	env->semant_error(this) << "error: Expression Type \"" << expr_type
        	                    << "\" does not match Variable Type \""
            	                << type_decl << "\"" << endl;
    	env->var_exitscope();
    	env->method_exitscope();
    	//type = Object;
    	return Object;
  	}*/
  	env->var_exitscope();
   	env->method_exitscope();
  	//type = type_decl;
  	return type_decl;
}
/* example of type-checking */
Symbol assign_class::tc(Environment *env){
  	//cout << "assign_class::tc enterred" << endl;
	expr->tc(env);
  	Symbol var_type = env->var_lookup(name);
 	//cout << "Assign Name: " << name << " = Type: " << var_type << endl;
	//cout << "Expr Type: " << expr->get_type() << endl;
	if(!var_type){
  		env->semant_error(this) << "error: Type \"" << var_type 
								<< "\" does not exist in current scope" << endl;
  		type = Object;
  		return Object;
  	}
	if(!env->lookup_class(expr->get_type())){
		env->semant_error(this) << "error: Expression Type \"" << expr->get_type()
								<< "\" does not exist in current scope" << endl;
		type = Object;
		return Object;
	}
  	if(env->type_leq(expr->get_type(), var_type) < 0){
    	env->semant_error(this) << "error: Expression Type \"" << expr->get_type()
 								<< "\" does not match Variable Type \""
  								<< var_type << "\"" << endl;
  		type = Object;
  		return Object;
  	}
  	type = var_type;
  	return var_type;
}
//Symbol static_dispatch_class::tc(Environment *env){
//	expr->tc(env);

Symbol static_dispatch_class::tc(Environment *env){
  	//cout << "static_dispatch_class::tc enterred" << endl;
	expr->tc(env);
	InheritanceNode* class_name = env->lookup_class(type_name);
  	if(!class_name){
  		env->semant_error(this) << "error: Type \"" << name
  								<< "\" Does not exist in current scope" << endl;
  		type = Object;
  		return Object;
  	}
	Feature method = env->method_is_in_class(class_name,name);
	Feature attr;
	if(!method) attr = env->attr_is_in_class(class_name,name);
	if(!method && !attr){
		env->semant_error(this) << "error: Class \"" << class_name->get_name()
								<< "\" does not contain feature \"" << name
								<< "\"" << endl;
		type = Object;
		return Object;
	}
  	for(int i = actual->first(); actual->more(i); i = actual->next(i)){
  		Expression e = actual->nth(i);
  		e->tc(env);
  	}
  	if(env->type_leq(expr->get_type(),type_name) < 0){
    	env->semant_error(this) << "error: Expression type \"" << expr->get_type()
        	                    << "\" does not conform to declared static dispatch type \""
            	                << type_name << endl;
    	type = Object;
    	return Object;
  	}
  	if(method){
    	method_class* m = dynamic_cast<method_class*>(method);
    	Formals formals = m->get_formals();
    	for(int i = formals->first(); formals->more(i); i = formals->next(i)){
      		Formal formal = formals->nth(i);
      		Expression expr_t = actual->nth(i);
      		if(!expr_t){
        		env->semant_error(this) << "error: Number of given arguments do not match "
                		                << "number of required parameters" << endl;
        		type = Object;
        		return Object;
      		}
      		if(env->type_leq(expr_t->get_type(),formal->get_type_decl()) < 0){
        		env->semant_error(this) << "error: Expression type \"" << expr_t->get_type()
										<< " does not conform to declared static dispatch type "
                        	        	<< formal->get_type_decl() << endl;
        		type = Object;
        		return Object;
      		}
    	}
  	}
  	if(method && method->get_type() == SELF_TYPE){
    	type = expr->get_type();
    	return expr->get_type();
  	}
	else if(method){
		type = method->get_type();
		return method->get_type();
	}
	else{	
    	type = attr->get_type();
    	return attr->get_type();
	}
}
Symbol dispatch_class::tc(Environment *env){
  	//cout << "dispatch_class::tc enterred" << endl;
	expr->tc(env);
	//cout << "Line 892" << endl;
  /*Symbol meth_type = env->method_lookup(name);
  if(!meth_type){
  	env->semant_error(this) << "error: Method \"" << name
  													<< "\" Does not exist in current scope" << endl;
  	type = Object;
  	return Object;
  }*/
  	Symbol expr_type = expr->get_type();
	//cout << "expr_type: " << expr_type << endl;
	InheritanceNode* class_name;
  	//cout << "Line 902" << endl;
	if(expr_type == SELF_TYPE) {
		//expr_type = self;
		//cout << "DISPATCH: Self Type: " << env->get_self_type() << endl;
		class_name = env->lookup_class(env->get_self_type());
	}
	else class_name = env->lookup_class(expr_type);
  	//cout << "Line 909" << endl;
	//cout << "class_name: " << class_name->get_name() << endl;
	//cout << "expr_type: " << expr_type;
	//cout << " | env->lookup_class(expr_type) = " << env->lookup_class(expr_type);
	if(!class_name){
  		cerr << "error: No Type found: " << expr_type << endl;
  	}
	//cout << "line 917" << endl;
	//cout << "expr_type: " << expr_type << endl;
	//cout << "CLASS_NAME: " << class_name->get_name() << " | Name: " << name << endl;
	InheritanceNode* dispatch_method_class = env->get_methods_class(class_name->get_name(),name);
  	//cout << "Line 920" << endl;
	//Feature feat;
  //bool foundMethod = false;
  	//cout << "Line 922" << endl;
  	//cout << "method_lookup: " << (env->method_probe(name)) << endl;
	//for(int i = feats->first(); feats->more(i); i = feats->next(i)){
  	//feat = feats->nth(i);
	//cout << "dispatch_class::tc feat->get_name(): " << feat->get_name() << " | name: " << name << endl;
  	//if(feat->isMethod() && feat->get_name() == name) { foundMethod = true; break; }
  //}
  	if(!dispatch_method_class){
  		env->semant_error(this) << "error: Dispatch to undefined Method \""
  								<< name << "\"" << endl;
  		type = Object;
  		return Object;
  	}
	Features feats = dispatch_method_class->get_features();
	Feature feat;
	for(int i = feats->first(); feats->more(i); i = feats->next(i))
		if(feats->nth(i)->get_name() == name) feat = feats->nth(i);
	if(!feat) {
		cerr << "Program error: Couldn't find Method \"" << name 
			<< "\" in class \"" << dispatch_method_class->get_name() << "\"" << endl;
		type = Object;
		return Object;
	}
  	for(int i = actual->first(); actual->more(i); i = actual->next(i))
  		actual->nth(i)->tc(env);

  	method_class* method_ = dynamic_cast<method_class*>(feat);
 	Formals forms = method_->get_formals();
	int i = forms->first();
  	for(; forms->more(i); i = forms->next(i)){
  		Formal form = forms->nth(i);
  		if(!actual->more(i)){
  			env->semant_error(this) 
  				<< "error: Given arguments do not match number of required parameters" << endl;
  			type = Object;
  			return Object;
  		}
  		Expression actual_expr = actual->nth(i);
	
  		if(env->type_leq(actual_expr->tc(env),form->tc(env)) < 0){
  			env->semant_error(this) 
  					<< "error: Given arguments do not match required parameters" << endl;
  			type = Object;
  			return Object;
  		}
  	}
	if(actual->more(i)){
		env->semant_error(this) << "error: Too many given arguments to the method \"" 
								<< method_->get_name() << "\"" << endl;
		type = Object;
		return Object;
	}
  	Symbol t = expr_type;
  	if(method_->get_return_type() == SELF_TYPE){
  		type = expr_type;
  		return expr_type;
  	}
  	else{
  		type = method_->get_return_type();
  		return method_->get_return_type();
  	}
}
//Symbol cond_class::get_type() { return }
Symbol cond_class::tc(Environment *env){
	//cout << "cond_class::tc enterred" << endl;
  	Symbol pred_type =  pred->tc(env);
  	Symbol then_type = then_exp->tc(env);
  	Symbol else_type = else_exp->tc(env);
	if(pred_type != Bool){
		env->semant_error(this) << "error: Condition is not of Type Bool" << endl;
		type = Object;
		return Object;
	}
	if(then_type == SELF_TYPE || then_type == self) then_type = env->get_self_type();
  	if(else_type == SELF_TYPE || else_type == self) else_type = env->get_self_type();
	Symbol lub = env->type_lub(then_type,else_type);
 	type = lub;
  	return lub;
}
//Symbol loop_class::get_type() { return Object; }
Symbol loop_class::tc(Environment *env){
	//cout << "loop_class::tc enterred" << endl;
  	pred->tc(env);
  	body->tc(env);
  	if(pred->get_type() != Bool){
    	env->semant_error(this) << "error: Predicate of loop is not of Type Bool" << endl;
    	type = Object;
    	return Object;
  	}
  	Symbol body_type = body->get_type();
  	InheritanceNode* body_class = env->lookup_class(body_type); //env->ct->get_class(body_type);
  	if(!body_class){
    	env->semant_error(this) << "error: Type \"" << body_type 
         	 	                << "\" of the Body of the loop is not a defined"
								<< endl;
    	type = Object;
    	return Object;
  	}
  	type = Object;
  	return Object;
}
//Symbol typecase_class::get_type() { return Object; }
Symbol typcase_class::tc(Environment *env){
  	//cout << "typcase_class::tc enterred" << endl;
	for(int i = cases->first(); cases->more(i); i = cases->next(i))
    	cases->nth(i)->tc(env);
  	for(int i = cases->first(); cases->more(i); i = cases->next(i)){
    	Case case_ = cases->nth(i);
    	for(int j = cases->next(i); cases->more(j); j = cases->next(j)){
      		Case case_j = cases->nth(j);
      		if(case_->get_type_decl() == case_j->get_type_decl()){
        		env->semant_error(this) << "error: Duplicate Branch of Type \""
            		                    << case_->get_type_decl() << "\"" << endl;
      		}
    	}
  	}
  	Symbol lub = cases->nth(cases->first())->get_type_decl();
  	if(!env->lookup_class(lub)) lub = Object;
	for(int i = cases->first(); cases->more(i); i = cases->next(i)){
    	Case case_ = cases->nth(i);
    	lub = env->type_lub(lub,case_->get_type_decl());
  	}
  	type = lub;
  	return lub;
}
/*Symbol block_class::get_type(){
  Symbol last_expr;
  for(int i = body->first(); body->more(i); i = body->next(i))
    last_expr = body->nth(i)->get_type();
  return last_expr;
}*/
Symbol block_class::tc(Environment *env){
	//cout << "block_class::tc enterred" << endl;
	Symbol last_expr;
	for(int i = body->first(); body->more(i); i = body->next(i)){
		Expression expr_ = body->nth(i);
		last_expr = expr_->tc(env);
	}
	type = last_expr;
	return last_expr;
}
//Symbol let_class::get_type() { return body->get_type(); }
Symbol let_class::tc(Environment *env){
	//cout << "let_class::tc enterred" << endl;
	//cout << "LET: " << type_decl << endl;
	InheritanceNode* type_decl_node = env->lookup_class(type_decl);
	if(!type_decl_node){
		env->semant_error(this) << "error: Type \"" << type_decl 
								<< "\" of Let-bound identifier \"" << identifier 
								<< "\" does not exist" << endl;
		type = Object;
		return Object;
	}
	Symbol t = type_decl == SELF_TYPE ? env->get_self_type() : type_decl;
	Symbol s = init->tc(env);
	env->var_add(identifier,type_decl);
	if(s == No_type) return body->tc(env);
	if(env->type_leq(t,s) >= 0){
		env->method_enterscope();
		env->var_enterscope();
		env->var_add(identifier,type_decl);
		Symbol b = body->tc(env);
		env->var_exitscope();
		env->method_exitscope();
		type = b;
		return b;
	}
	else{
		env->semant_error(this) << "error: Let Type \"" << type_decl 
														<< "\" is not conformed by initialization" << endl;
		type = Object;
		return Object;
	}
}
//Symbol plus_class::get_type() { return Int; }
Symbol plus_class::tc(Environment *env){
  	//cout << "plus_class::tc enterred" << endl;
	e1->tc(env);
  	e2->tc(env);
  	if((e1->get_type() != Int) || (e2->get_type() != Int))
    	env->semant_error(this) << "error: non-Int arguments: " << e1->get_type() << " + "
      							<< e2->get_type() << endl;
  	type = Int;
  	return Int;
}
//Symbol sub_class::get_type() { return Int; }
Symbol sub_class::tc(Environment *env){
  	//cout << "sub_class::tc enterred" << endl;
	e1->tc(env);
  	e2->tc(env);
  	if((e1->get_type() != Int) || (e2->get_type() != Int))
    	env->semant_error(this) << "error: non-Int arguments: " << e1->get_type() << " - "
      							<< e2->get_type() << endl;
  	type = Int;
  	return Int;
}
//Symbol mul_class::get_type() { return Int; }
Symbol mul_class::tc(Environment *env)
{
	//cout << "mul_class::tc enterred" << endl;
  	e1->tc(env);
  	e2->tc(env);

  	if ((e1->get_type() != Int) || (e2->get_type() != Int))
    	env->semant_error(this) << "error: non-Int arguments: " << e1->get_type() << " * "
      							<< e2->get_type() << endl;
  	type = Int;
  	return Int;
}
//Symbol divide_class::get_type() { return Int; }
Symbol divide_class::tc(Environment *env){
  	//cout << "divide_class::tc enterred" << endl;
	e1->tc(env);
  	e2->tc(env);
  	if((e1->get_type() != Int) || (e2->get_type() != Int))
    	env->semant_error(this) << "error: non-Int arguments: " << e1->get_type() << " / "
      							<< e2->get_type() << endl;
  	type = Int;
  	return Int;
}
//Symbol neg_class::get_type() { return Int; }
Symbol neg_class::tc(Environment *env){
  	//cout << "neg_class::tc enterred" << endl;
	e1->tc(env);
  	if(e1->get_type() != Int)
    	env->semant_error(this) << "error: non-Int arguments: -" << e1->get_type() << endl;
  	type = Int;
  	return Int;
}
//Symbol lt_class::get_type() { return Bool; }
Symbol lt_class::tc(Environment *env){
  	//cout << "lt_class::tc enterred" << endl;
	e1->tc(env);
  	e2->tc(env);
  	if((e1->get_type() != Int) || (e2->get_type() != Int))
    	env->semant_error(this) << "error: non-Int arguments: " << e1->get_type() << " < "
      							<< e2->get_type() << endl;
  	type = Bool;
  	return Bool;
}
//Symbol eq_class::get_type() { return Bool; }
Symbol eq_class::tc(Environment *env){
  	//cout << "eq_class::tc enterred" << endl;
	e1->tc(env);
  	e2->tc(env);
  	if(e1->get_type() != e2->get_type()){
		env->semant_error(this) << "error: not comparable arguments: " << e1->get_type() << " = "
     							<< e2->get_type() << endl;
  	}
  	type = Bool;
 	return Bool;
}
//Symbol leq_class::get_type() { return Bool; }
Symbol leq_class::tc(Environment *env){
  	//cout << "leq_class::tc enterred" << endl;
	e1->tc(env);
  	e2->tc(env);
  	if((e1->get_type() != Int) || (e2->get_type() != Int))
    	env->semant_error(this) << "error: non-Int arguments: " << e1->get_type() << " <= "
      							<< e2->get_type() << endl;
  	type = Bool;
  	return Bool;
}
//Symbol comp_class::get_type() { return Bool; }
Symbol comp_class::tc(Environment *env){
	//cout << "comp_class::tc enterred" << endl;
  	e1->tc(env);
  	if(e1->get_type() != Bool)
    	env->semant_error(this) << "error: non-Bool argument: " << e1->get_type() << endl;
  	type = Bool;
  	return Bool;
}
//Symbol int_const_class::get_type() { return Int; }
Symbol int_const_class::tc(Environment *env){
  	//cout << "int_const_class::tc enterred" << endl;
	type = Int;
  	return Int;
}
//Symbol bool_const_class::get_type() { return Bool; }
Symbol bool_const_class::tc(Environment *env){
  	//cout << "bool_const_class::tc enterred" << endl;
	type = Bool;
  	return Bool;
}
//Symbol string_const_class::get_type() { return Str; }
Symbol string_const_class::tc(Environment *env){
 	//cout << "string_const_class::tc enterred" << endl;
	type = Str;
  	return Str;
}
//Symbol new__class::get_type() { return type_name; }
Symbol new__class::tc(Environment *env){
  	//cout << "new__class::tc enterred" << endl;
	type = type_name;
  	return type_name;
}
//Symbol isvoid_class::get_type() { return Bool; }
Symbol isvoid_class::tc(Environment *env){
  	//cout << "isvoid_class::tc enterred" << endl;
	type = Bool;
  	return Bool;
}
//Symbol no_expr_class::get_type(){ return No_type; }
Symbol no_expr_class::tc(Environment *env){
	//cout << "no_expr_class::tc enterred" << endl;
  	type = No_type;
  	return No_type;
}
//Symbol object_class::get_type(){ return name; }
Symbol object_class::tc(Environment *env){
	//cout << "object_class::tc enterred" << endl;
	Symbol object_type = env->var_lookup(name);
	//cout << "Object Name: " << name << endl; //" | Type: " << object_type << endl;
	if(!object_type || object_type == No_type){
		env->semant_error(this) << "error: Undeclared Variable \"" << name << "\"" <<  endl;
		type = Object;
		return Object;
	}
	if(name == SELF_TYPE){
    	type = SELF_TYPE;
    	return SELF_TYPE;
  	}
  	type = object_type;
  	return object_type;
}
