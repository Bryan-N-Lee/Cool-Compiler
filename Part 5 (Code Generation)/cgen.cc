// Michael Coulombe and Bryan Lee

//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everything else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

int curr_lineno;

extern void emit_string_constant(ostream& str, const char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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

static const char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static const char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
    // spim wants comments to start with '#'
    if (cgen_debug) {
        os << "# start of generated code\n";
    }
    
    initialize_constants();
    //CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);
    Environment env(CgenClassTable(classes,os));
    
    env.codeMethods();
    
    if (cgen_debug) {
        os << "\n# end of generated code\n";
    }
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(const char *dest_reg, int offset, const char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(const char *source_reg, int offset, const char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(const char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(const char *dest_reg, const char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(const char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(const char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(const char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(const char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(const char *dest_reg, const char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(const char *dest, const char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(const char *dest, const char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(const char *dest, const char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(const char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(const char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_partial_jal(ostream &s)
{ s << JAL; }

static void emit_partial_jump(ostream &s)
{ s << JUMP; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(const char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(const char *src1, const char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(const char *src1, const char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(const char *src1, const char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(const char *src1, const char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(const char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(const char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(const char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

static void emit_pop(const char *reg, ostream& str)
{
  emit_addiu(SP,SP,4,str);
  emit_load(reg,0,SP,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(const char *dest, const char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(const char *source, const char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(const char *source, ostream &s)
{
  if (strcmp(source, A1) != 0) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
    IntEntryP lensym = inttable.add_int(len);

    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s  << LABEL                                             // label
       << WORD << stringclasstag << endl                                 // tag
       << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
       << WORD;


    /***** Add dispatch information for class String ******/
    emit_disptable_ref(Str, s);

    s << endl;                                              // dispatch table
    s << WORD;  lensym->code_ref(s);  s << endl;            // string length
    emit_string_constant(s,str);                                // ascii string
    s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

    /***** Add dispatch information for class Int ******/
    emit_disptable_ref(Int, s);

    s << endl;                                          // dispatch table
    s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/
    emit_disptable_ref(Bool, s);

      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

////////////////////////////////////////////////////////////////////////////////

void CgenNode::makeTables() {
    if (vtable.list.size() > 0) {
        // my tables are already computed
        return;
    }
    
    // otherwise copy parent's tables
    if (name != Object) {
        parentnd->makeTables();
        atable = parentnd->atable;
        vtable = parentnd->vtable;
    }
    
    // then insert my own features into them
    // also marks overridden methods as non-final
    for(int i = 0; i < features->len(); i++) {
        method_class* meth = features->nth(i)->isMethod();
        
        if (meth) {
            vtable.insert(name, meth);
            continue;
        }
        
        attr_class* attr = features->nth(i)->isAttr();
        
        if (attr) {
            atable.insert(name, attr);
            continue;
        }
    }
}

//------------------------------------------------------------------------------

void CgenNode::code_proto(ostream& s) {
    // ensure atable is calculated
    makeTables();
    
    s << WORD << "-1" << endl;
    
    emit_protobj_ref(name, s); s << ':' << endl;
    
    s << WORD << classTag; if (cgen_debug) { s << "\t# Class Tag"; } s << endl;
    s << WORD << getSize(); if (cgen_debug) { s << "\t# Size (words)"; } s << endl;
    s << WORD; emit_disptable_ref(name, s); s << endl;
    
    for(unsigned int i = 0; i < atable.list.size(); i++) {
        attr_class* attr = atable.list[i].impl;
        
        s << WORD;
        
        if (attr->type_decl == Int) {
            inttable.add_string("0")->code_ref(s);
            if (cgen_debug) { s << "\t# Zero Int"; }
        }
        else if (attr->type_decl == Str) {
            stringtable.add_string("")->code_ref(s);
            if (cgen_debug) { s << "\t# Empty String"; }
        }
        else if (attr->type_decl == Bool) {
            falsebool.code_ref(s);
            if (cgen_debug) { s << "\t# False Bool"; }
        }
        else {
            s << "0";
            if (cgen_debug) { s << "\t# Null " << attr->type_decl; }
        }
        
        s << endl;
    }
}

void CgenClassTable::code_protos() {
    for(unsigned int i = 0; i < nodes.size(); i++) {
        nodes[i]->code_proto(str);
    }
}

//------------------------------------------------------------------------------


void CgenNode::code_vtable(ostream& s) {
    emit_disptable_ref(name, s); s << ':' << endl;
    
    // ensure vtable is calculated
    makeTables();
    
    for(unsigned int i = 0; i < vtable.list.size(); i++) {
        VTable::FeatDesc& m = vtable.list[i];
        
        s << WORD << m.className << '.' << m.impl->name << endl;
    }
}

void CgenClassTable::code_vtables() {
    for(unsigned int i = 0; i < nodes.size(); i++) {
        nodes[i]->code_vtable(str);
    }
}

//------------------------------------------------------------------------------

#define CLASSPARENTTAB "class_parentTab"
#define SUBTYPELOOKUPFUN "isSubType"

void CgenClassTable::code_nameTab_objTab_parentTab() {
    std::vector<CgenNodeP> table;
    
    table.assign((size_t) maxClassTag, NULL);
    
    // sort nodes by classTag
    for(unsigned int i = 0; i < nodes.size(); i++) {
        CgenNodeP cn = nodes[i];
        
        int ct = cn->getClassTag();
        
        if ((int) table.size() <= ct) {
            table.resize(ct+1);
        }
        
        if (table[ct] != NULL) {
            cerr << "Error: table[" << ct << "] is taken" << endl;
        }
        
        table[ct] = cn;
    }
    
    // output table of pointers to class name string constants
    str << CLASSNAMETAB << ':' << endl;
    for(unsigned int i = 0; i < table.size(); i++) {
        if (table[i] == NULL) {
            cerr << "Error: table[" << i << "] is not taken" << endl;
            str << WORD << endl;
        }
        else {
            str << WORD;
            
            Symbol cn = table[i]->name;
            StringEntry* se = stringtable.lookup_string( cn->get_string() );
            
            se->code_ref(str); if (cgen_debug) { str << "\t# \"" << cn << '"'; }
            
            str << endl;
        }
        
    }
    
    // output table of pointers to object prototypes and init methods
    str << CLASSOBJTAB << ':' << endl;
    for(unsigned int i = 0; i < table.size(); i++) {
        if (table[i] == NULL) {
            str << WORD << endl << WORD << endl;
        }
        else {
            str << WORD; emit_protobj_ref(table[i]->name, str); str << endl;
            
            str << WORD; emit_init_ref(table[i]->name, str); str << endl;
        }
    }
}

//------------------------------------------------------------------------------

void CgenNode::createClassTag(int& tag) {
    classTag = tag++;
    
    for(unsigned int i = 0; i < children.size(); i++) {
        children[i]->createClassTag(tag);
    }
}

void CgenNode::getClassTagRange(int& low, int& high) {
    low = classTag;
    
    if (children.size() > 0) {
        // high = upper bound of last child's range
        int temp;
        children.back()->getClassTagRange(temp, high);
    }
    else {
        // no subclasses => range has just one element
        high = classTag;
    }
}

bool CgenNode::isSubclassOf(CgenNodeP other) {
    if (other == this) {
        return true;
    }
    else if (name == Object) {
        return false;
    }
    else {
        return parentnd->isSubclassOf(other);
    }
}

VTable::FeatDesc CgenNode::lookupMethod(Symbol m) {
    return vtable.list[ vtable.indexOf(m) ];
}

void CgenNode::code_init(Environment& env, ostream& s) {
    emit_init_ref(name, s); s << ':' << endl;
    
    if (cgen_debug) { s << "# "; emit_init_ref(name, s); s << " Begin" << endl; }
    
    env.localid_offset_table.enterscope();
    
    // allocate space in frame for old $fp, self, $ra, and let/case-bound variables
    unsigned int numSelfAttrs = 0;
    unsigned int numSelfInits = 0;
    unsigned int numParentInits = 0;
    int maxTemps = 0;
    for(unsigned int i = 0; i < atable.list.size(); ++i) {
        attr_class* attr = atable.list[i].impl;
        
        if (atable.list[i].className == name) {
            // this attr defined in this class
            maxTemps = std::max(maxTemps, attr->numLocals());
            
            numSelfAttrs++;
            
            if (! attr->init->is_no_expr()) {
                numSelfInits++;
            }
        }
        else {
            // this attr is inherited
            if (! attr->init->is_no_expr()) {
                numParentInits++;
            }
        }
    }
    env.current_method.init();
    
    /*
        OPT: if this class has no initializers for its noninherited attributes,
             then do not have to set up a stack frame
        OPT: if the parent of this class has no attributes to initialize,
             then do not have to call its init function
    */
    
    if (numSelfInits > 0) {
	    emit_store(FP, 0, SP, s);
	    emit_store(SELF, -1, SP, s);
        emit_store(RA, -2, SP, s);
        emit_move(FP, SP, s);
        emit_addiu(SP, SP, - WORD_SIZE * (3 + maxTemps), s);
    }
    
    if (numParentInits > 0) {
        if (numSelfInits > 0) {
            emit_partial_jal(s);
        }
        else {
            // no need to return to this function
            emit_partial_jump(s);
        }
        
        emit_init_ref(parentnd->name, s); s << endl;
    }
    
    if (numSelfInits > 0) {
        // put return value of parent's init into SELF
        emit_move(SELF, ACC, s);
        
        // run all attribute initializers
        for(unsigned int i = atable.list.size() - numSelfAttrs; i < atable.list.size(); i++) {
            attr_class* attr = atable.list[i].impl;
            
            if (cgen_debug) { s << "# Initializing " << name << "." << attr->name << endl; }
            
            attr->init->code(env, s);
            
            if (! attr->init->is_no_expr()) {
                emit_store(ACC, env.get_attribute_offset(attr->name), SELF,s);
            }
        }
        // put SELF back into ACC
	    emit_move(ACC, SELF, s);
    }
    
	if (numSelfInits > 0) {
        // unallocate frame then return
        emit_move(SP, FP, s);
        //emit_addiu(SP, SP, WORD_SIZE * (3 + maxTemps), s);
        emit_load(RA, -2, SP, s);
        emit_load(SELF, -1, SP, s);
        emit_load(FP, 0, SP, s);
	}
	
	if (! (numParentInits > 0 && numSelfInits == 0)) {
        emit_return(s);
    }
    
    env.localid_offset_table.exitscope();
    
    if (cgen_debug) { s << "# "; emit_init_ref(name, s); s << " End" << endl; }
}

void CgenNode::code(Environment& env, ostream& s) {
    env.curClass = this;
    
    // initializer function
    code_init(env, s);
    
    if (!basic()) {
        // non-inherited methods
        for(unsigned int i = 0; i < vtable.list.size(); i++) {
            if (vtable.list[i].className == name) {
                vtable.list[i].impl->code(env, s);
            }
        }
    }
    env.curClass = NULL;
}

void CgenClassTable::code_methods(Environment& env) {
    for(unsigned int i = 0; i < nodes.size(); i++) {
        nodes[i]->code(env, str);
    }
}



////////////////////////////////////////////////////////////////////////////////

CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nodes() , str(s)
{
    enterscope();

    install_basic_classes();
    install_classes(classes);
    build_inheritance_tree();

    // assign class tags from 0 up, and rearrange so nodes[i].classTag == i
    maxClassTag = 0;
    root()->createClassTag(maxClassTag);
    
    for(int i = 0; i < maxClassTag; i++) {
        std::swap(nodes[i], nodes[ nodes[i]->getClassTag() ]);
    }
    
    // output non-method assembly code
    stringclasstag = probe(Str)->getClassTag();
    intclasstag    = probe(Int)->getClassTag();
    boolclasstag   = probe(Bool)->getClassTag();
    
    code();
    exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nodes.push_back(nd);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(unsigned int i = 0; i < nodes.size(); i++) {
    set_relations(nodes[i]);
  }
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
    children.push_back(n);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}



void CgenClassTable::code()
{
    code_global_data();
    code_select_gc();
    code_constants();
    code_protos();
    code_nameTab_objTab_parentTab();
    code_vtables();
    code_global_text();
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}

CgenNodeP CgenClassTable::find(Symbol c) {
    for(unsigned int i = 0; i < nodes.size(); i++) {
        if (nodes[i]->name == c) {
            return nodes[i];
        }
    }
    return NULL;
}

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(),
   basic_status(bstatus),
   classTag(-1)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}

int CgenNode::getAttrIndex(Symbol a) {
    return atable.indexOf(a);
}
int CgenNode::getMethIndex(Symbol m) {
    return vtable.indexOf(m);
}

////////////////////////////////////////////////////////////////////////////////

Environment::Environment(CgenClassTable ct) : classTable(ct), labelCounter(0), curClass(NULL) {}

int Environment::nextLabel() { return labelCounter++; }

int Environment::get_attribute_offset(Symbol a) {
    return 1 * (3 + curClass->getAttrIndex(a));
}

int Environment::get_method_offset(Symbol c, Symbol m) {
    return 1 * classTable.find(c)->getMethIndex(m);
}

CgenNodeP Environment::getClassNode(Symbol c) {
    if (c == SELF_TYPE) {
        return curClass;
    }
    else {
        return classTable.find(c);
    }
}

////////////////////////////////////////////////////////////////////////////////

int attr_class::numLocals() {
    return init->numLocals();
}

int method_class::numLocals() {
    return expr->numLocals();
}

int assign_class::numLocals() {
    return expr->numLocals();
}

int static_dispatch_class::numLocals() {
    int n = expr->numLocals();
    
    for (int i = 0; i < actual->len(); i++) {
        n = std::max(n, actual->nth(i)->numLocals());
    }
    
    return n;
}

int dispatch_class::numLocals() {
    int n = expr->numLocals();
    
    for (int i = 0; i < actual->len(); i++) {
        n = std::max(n, actual->nth(i)->numLocals());
    }
    
    return n;
}

int cond_class::numLocals() {
    return std::max( std::max( pred->numLocals(), then_exp->numLocals() ), else_exp->numLocals() );
}

int loop_class::numLocals() {
    return std::max( pred->numLocals(), body->numLocals() );
}

int branch_class::numLocals() {
    return 1 + expr->numLocals();
}

int typcase_class::numLocals() {
    int n = expr->numLocals();
    
    for (int i = 0; i < cases->len(); i++) {
        n = std::max(n, cases->nth(i)->numLocals());
    }
    
    return n;
}

int block_class::numLocals() {
    int n = 0;
    for (int i = 0; i < body->len(); i++) {
        n = std::max(n, body->nth(i)->numLocals());
    }
    return n;
}

int let_class::numLocals() {
    return 1 + std::max( init->numLocals(), body->numLocals() );
}

int plus_class::numLocals() {
    return std::max( e1->numLocals(), e2->numLocals() );
}

int sub_class::numLocals() {
    return std::max( e1->numLocals(), e2->numLocals() );
}

int mul_class::numLocals() {
    return std::max( e1->numLocals(), e2->numLocals() );
}

int divide_class::numLocals() {
    return std::max( e1->numLocals(), e2->numLocals() );
}

int neg_class::numLocals() {
    return e1->numLocals();
}

int lt_class::numLocals() {
    return std::max( e1->numLocals(), e2->numLocals() );
}


int eq_class::numLocals() {
    return std::max( e1->numLocals(), e2->numLocals() );
}

int leq_class::numLocals() {
    return std::max( e1->numLocals(), e2->numLocals() );
}

int comp_class::numLocals() {
    return e1->numLocals();
}

int int_const_class::numLocals() {
    return 0;
}

int string_const_class::numLocals() {
    return 0;
}

int bool_const_class::numLocals() {
    return 0;
}

int new__class::numLocals() {
    return 0;
}

int isvoid_class::numLocals() {
    return e1->numLocals();
}

int no_expr_class::numLocals() {
    return 0;
}

int object_class::numLocals() {
    return 0;
}

//------------------------------------------------------------------------------

bool assign_class::is_never_null() {
    return expr->is_never_null();
}
bool static_dispatch_class::is_never_null() {
    // no easy way to know internals
    return (type == Int) || (type == Str) || (type == Bool);
}
bool dispatch_class::is_never_null() {
    // no easy way to know internals
    return (type == Int) || (type == Str) || (type == Bool);
}
bool cond_class::is_never_null() {
    return then_exp->is_never_null() && else_exp->is_never_null();
}
bool loop_class::is_never_null() {
    return false; // always null
}
bool branch_class::is_never_null() {
    return expr->is_never_null();
}
bool typcase_class::is_never_null() {
    for(int i = 0; i < cases->len(); i++) {
        if (! cases->nth(i)->is_never_null()) {
            return false;
        }
    }
    return true;
}
bool let_class::is_never_null() {
    return body->is_never_null();
}
bool block_class::is_never_null() {
    return body->nth( body->len() - 1 )->is_never_null();
}
bool plus_class::is_never_null() {
    return true; // type == Int
}
bool sub_class::is_never_null() {
    return true; // type == Int
}
bool mul_class::is_never_null() {
    return true; // type == Int
}
bool divide_class::is_never_null() {
    return true; // type == Int
}
bool neg_class::is_never_null() {
    return true; // type == Int
}
bool lt_class::is_never_null() {
    return true; // type == Bool
}
bool eq_class::is_never_null() {
    return true; // type == Bool
}
bool leq_class::is_never_null() {
    return true; // type == Bool
}
bool comp_class::is_never_null() {
    return true; // type == Bool
}
bool int_const_class::is_never_null() {
    return true; // type == Int
}
bool string_const_class::is_never_null() {
    return true; // type == String
}
bool bool_const_class::is_never_null() {
    return true; // type == Bool
}
bool new__class::is_never_null() {
    return true; // by construction
}
bool isvoid_class::is_never_null() {
    return true; // type == Bool
}
bool no_expr_class::is_never_null() {
    return false; // why not?
}
bool object_class::is_never_null() {
    // self is never null
    return (type == Int) || (type == Str) || (type == Bool) || (name == self);
}
//------------------------------------------------------------------------------

bool assign_class::is_unique() {
    return false; // value stored in a variable
}
bool static_dispatch_class::is_unique() {
    return false; // no easy way to know internals
}
bool dispatch_class::is_unique() {
    return false; // no easy way to know internals
}
bool cond_class::is_unique() {
    return then_exp->is_unique() && else_exp->is_unique();
}
bool loop_class::is_unique() {
    return false; // null is easy to create
}
bool branch_class::is_unique() {
    return expr->is_unique();
}
bool typcase_class::is_unique() {
    for(int i = 0; i < cases->len(); i++) {
        if (! cases->nth(i)->is_unique()) {
            return false;
        }
    }
    return true;
}
bool let_class::is_unique() {
    return body->is_unique();
}
bool block_class::is_unique() {
    return body->nth( body->len() - 1 )->is_unique();
}
bool plus_class::is_unique() {
    return true; // creates new Int
}
bool sub_class::is_unique() {
    return true; // creates new Int
}
bool mul_class::is_unique() {
    return true; // creates new Int
}
bool divide_class::is_unique() {
    return true; // creates new Int
}
bool neg_class::is_unique() {
    return true; // creates new Int
}
bool lt_class::is_unique() {
    return false; // returns global bool constant
}
bool eq_class::is_unique() {
    return false; // returns global bool constant
}
bool leq_class::is_unique() {
    return false; // returns global bool constant
}
bool comp_class::is_unique() {
    return false; // returns global bool constant
}
bool int_const_class::is_unique() {
    return false; // returns global int constant
}
bool string_const_class::is_unique() {
    return false; // returns global string constant
}
bool bool_const_class::is_unique() {
    return false; // return global bool constant
}
bool new__class::is_unique() {
    return true; // by construction
}
bool isvoid_class::is_unique() {
    return false; // returns global bool constant
}
bool no_expr_class::is_unique() {
    return false; // why not?
}
bool object_class::is_unique() {
    return false; // value stored in variable
}
//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void method_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << ".globl " << env.curClass->name << '.' << name << endl; }
    
    s << env.curClass->name << '.' << name << ':' << endl;
    
    if (cgen_debug) { s << "# Method Begin" << endl; }
	
    env.localid_offset_table.enterscope();
    
    // calculate offsets of parameters    
    std::vector<int> paramOffsets;
    paramOffsets.resize(formals->len());
    
    for(int i = 0; i < (int) paramOffsets.size(); i++) {
        // params are stored BEFORE where the frame pointer points
        
        paramOffsets[i] = paramOffsets.size() - i;
        //paramOffsets[i] = (1 + i);
         
        Symbol pname = ( (formal_class*) formals->nth(i) )->name;
        
        env.localid_offset_table.addid(pname, & paramOffsets[i]);
    }
    
    // allocate space in frame for old $fp, self, $ra, and let/case-bound variables
    int maxTemps = numLocals();
    env.current_method.init();
    
	emit_store(FP, 0, SP, s);
	emit_store(SELF, -1, SP, s);
    emit_store(RA, -2, SP, s);
    emit_move(FP, SP, s );
    emit_addiu(SP, SP, - WORD_SIZE * (3 + maxTemps), s);
    
    emit_move(SELF, ACC, s);
    // body
    expr->code(env, s);
    
    // unallocate frame and parameters then return
    emit_move(SP, FP, s);
    //emit_addiu(SP, SP, WORD_SIZE * (3 + maxTemps + paramOffsets.size()), s);
    emit_load(RA, -2, SP, s);
    emit_load(SELF, -1, SP, s);
    emit_load(FP, 0, SP, s);
    emit_addiu(SP, SP, WORD_SIZE * paramOffsets.size(), s);
    emit_return(s);
    
    env.localid_offset_table.exitscope();
    
    if (cgen_debug) { s << "# Method End" << endl; }
}


void assign_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# Assignment Begin" << endl; }

    expr->code(env,s);

    int* word_offset = env.localid_offset_table.lookup(name);

    if(word_offset == NULL) {
        int offset = env.get_attribute_offset(name);
        emit_store(ACC, offset, SELF, s);
        
        if(cgen_Memmgr != GC_NOGC) {
            emit_addiu(A1, SELF, WORD_SIZE * offset, s);
            emit_jal("_GenGC_Assign", s);
        }
    }
    else {
        emit_store(ACC, *word_offset, FP, s);
    }
    
    if (cgen_debug) { s << "# Assignment End" << endl; }
}

void static_dispatch_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# Static Dispatch Begin (" << type_name << "::" << name << ')' << endl; }
    
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        if (cgen_debug) { s << "# Static Dispatch Argument " << i << " Begin" << endl; }
        Expression exp = actual->nth(i);
        exp->code(env, s);
        emit_push(ACC, s);
    }
    if (cgen_debug) { s << "# Static Dispatch Subject Begin" << endl; }
    expr->code(env, s);
    if (cgen_debug) { s << "# Static Dispatch Subject End" << endl; }
    
    // OPT: static null check
    bool canDispatchToVoid = ! expr->is_never_null();
    
    if (canDispatchToVoid) {
        //Check if dispatching on a void/null
        int success_label = env.nextLabel();
        emit_bne(ACC, ZERO, success_label, s);
        emit_load_string(ACC, stringtable.lookup_string(env.curClass->filename->get_string()), s); 
        emit_load_imm(T1, get_line_number(), s);
        emit_jal("_dispatch_abort", s);
        emit_label_def(success_label, s);
    }
    else {
        if (cgen_debug) { s << "# Statically verified never null" << endl; }
    }

    //success
    emit_partial_jal(s); s << type_name << '.' << name << endl;
  
    if (cgen_debug) { s << "# Static Dispatch End" << endl; }
}

void dispatch_class::code(Environment& env, ostream &s) {
    CgenNodeP cn = env.getClassNode( expr->get_type() );
    
    // OPT: if calling a method that is not overriden by any child class,
    //      then can use a static call instead
    VTable::FeatDesc fd = cn->lookupMethod(name);
    if (fd.impl->isFinal) {
        static_dispatch_class sdc(this, fd.className);
        sdc.code(env, s);
    }
    else {
        if (cgen_debug) { s << "# Dispatch Begin (" << cn->name << "::" << name << ')' << endl; }
        
        for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
            if (cgen_debug) { s << "# Dispatch Argument " << i << " Begin" << endl; }
            
            Expression exp = actual->nth(i);
            exp->code(env, s);
            emit_push(ACC, s);
            
            if (cgen_debug) { s << "# Dispatch Argument " << i << " End" << endl; }
        }
        if (cgen_debug) { s << "# Dispatch Subject Begin" << endl; }
        expr->code(env, s);
        if (cgen_debug) { s << "# Dispatch Subject End" << endl; }

        // OPT: static null check
        bool canDispatchToVoid = ! expr->is_never_null();
        
        if (canDispatchToVoid) {
            int success_label = env.nextLabel();
            emit_bne(ACC, ZERO, success_label, s);
            
            // If it is here, calling dispatch abort.
            // filename in $a0, line number in $t1.

            emit_load_string(ACC, stringtable.lookup_string(env.curClass->filename->get_string()), s); 
            emit_load_imm(T1, get_line_number(), s);
            emit_jal("_dispatch_abort", s);

            //success
            emit_label_def(success_label, s);
        }
        else {
            if (cgen_debug) { s << "# Statically verified never null" << endl; }
        }
        emit_load(T1, DISPTABLE_OFFSET, ACC, s);

        int offset = env.get_method_offset(cn->name, name);

        emit_load(T1, offset, T1, s);

        emit_jalr(T1, s);
        
        if (cgen_debug) { s << "# Dispatch End" << endl; }
    }
}

void cond_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# If-Then-Else Begin" << endl; }
    
    env.localid_offset_table.enterscope();
    
    // OPT: true/false constant => only one branch is possible
    if (pred->is_bool_const()) {
        bool_const_class* b = (bool_const_class*) pred;
        
        if (b->val) {
            if (cgen_debug) { s << "# Pred is True Constant" << endl; }
            then_exp->code(env,s);
        }
        else {
            if (cgen_debug) { s << "# Pred is False Constant" << endl; }
            else_exp->code(env,s);
        }
    }
    else {
        if (cgen_debug) { s << "# Pred Begin" << endl; }
        pred->code(env,s);
        if (cgen_debug) { s << "# Pred End" << endl; }
        
        emit_load(T2,DEFAULT_OBJFIELDS,ACC,s);

        int false_label = env.nextLabel();
        emit_beqz(T2,false_label,s);
        
        if (cgen_debug) { s << "# True Branch Begin" << endl; }
        then_exp->code(env,s);
        if (cgen_debug) { s << "# True Branch End" << endl; }

        int end_label = env.nextLabel();
        emit_branch(end_label,s);

        //false
        emit_label_def(false_label,s);
        if (cgen_debug) { s << "# False Branch Begin" << endl; }
        else_exp->code(env,s);
        if (cgen_debug) { s << "# False Branch End" << endl; }

        emit_label_def(end_label,s);
    }
    
    env.localid_offset_table.exitscope();
    
    if (cgen_debug) { s << "# If-Then-Else End" << endl; }
}

void loop_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# Loop Begin" << endl; }
    
    env.localid_offset_table.enterscope();
    
    // OPT: true/false constant => only one branch is possible
    if (pred->is_bool_const()) {
        bool_const_class* b = (bool_const_class*) pred;
        
        if (b->val) {
            if (cgen_debug) { s << "# Pred is True Constant" << endl; }
            int start_label = env.nextLabel();
            
            emit_label_def(start_label,s);
            body->code(env,s);
            
            emit_branch(start_label,s);
        }
        else {
            if (cgen_debug) { s << "# Pred is False Constant" << endl; }
            // just return void
        }
    }
    else {
        int start_label = env.nextLabel();

        emit_label_def(start_label,s);
        if (cgen_debug) { s << "# Pred Begin" << endl; }
        pred->code(env,s);
        if (cgen_debug) { s << "# Pred End" << endl; }
        emit_load(T2,DEFAULT_OBJFIELDS,ACC,s);

        int end_label = env.nextLabel();
        emit_beqz(T2,end_label,s);
        
        if (cgen_debug) { s << "# Body Begin" << endl; }
        body->code(env,s);
        if (cgen_debug) { s << "# Body End" << endl; }

        emit_branch(start_label,s);
        emit_label_def(end_label,s);
    }
    
    emit_load_imm(ACC, 0, s);
    
    env.localid_offset_table.exitscope();
    
    if (cgen_debug) { s << "# Loop End" << endl; }
}

void branch_class::code(Environment& env, CgenNodeP cn, int exit_label, int offsetFromFP, bool guaranteed, ostream &s) {
    if (cgen_debug) { s << "# Case Branch Begin (" << cn->name << ')' << endl; }
    
    env.localid_offset_table.enterscope();
    env.localid_offset_table.addid(name, &offsetFromFP);
    
    if (guaranteed) {
        if (cgen_debug) { s << "# Guaranteed Branch" << endl; }
        
        expr->code(env, s);
    }
    else {
        int skip_label = env.nextLabel();
            
        int lowTag, highTag;
        cn->getClassTagRange(lowTag, highTag);
        
        // check that tag is in range
        emit_blti(T2,  lowTag, skip_label, s);
        emit_bgti(T2, highTag, skip_label, s);
        
        expr->code(env, s);
        
        emit_branch(exit_label, s);
        
        emit_label_def(skip_label, s);
	}
	
    env.localid_offset_table.exitscope();
    
    if (cgen_debug) { s << "# Case Branch End" << endl; }
}
struct BranchInfo {
    branch_class* branch;
    CgenNodeP classNode;
    
    bool operator<(const BranchInfo& rhs) const {
        return classNode->getClassTag() > rhs.classNode->getClassTag();
    }
};

void typcase_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# Case Expression Begin" << endl; }
    
    // code subject of case
    if (cgen_debug) { s << "# Subject Begin" << endl; }
    expr->code(env, s);
    if (cgen_debug) { s << "# Subject End" << endl; }
    
    // put result onto stack for use by branches
    int offsetFromFP = env.current_method.getFreeOffset();
	emit_store(ACC, offsetFromFP, FP, s);
    
    // OPT: if expr is never null, do not have to perform null check
    if (! expr->is_never_null()) {
        // call case abort 2 if switching on a void object
        int success_label = env.nextLabel();
        
        emit_bne(ACC, ZERO, success_label, s);
        // filename in $a0, line number in $t1.
        emit_load_string(ACC, stringtable.lookup_string(env.curClass->filename->get_string()), s); 
        emit_load_imm(T1, get_line_number(), s);
        emit_jal("_case_abort2", s);
        // add the code to check each branch
        emit_label_def(success_label, s);
    }
    else {
        if (cgen_debug) { s << "# Statically verified never null" << endl; }
    }
    // take each branch and put them in sorted order (class tag decreasing)
    
    std::set<BranchInfo> sortedCases;
    
    for(int i = 0; i < cases->len(); i++) {
        BranchInfo bi;
        bi.branch = (branch_class*) cases->nth(i);
        bi.classNode = env.getClassNode(bi.branch->type_decl);
        
        sortedCases.insert(bi);
    }
    
    // load class tag into T2
    emit_load(T2, 0, ACC, s);
    
    bool provenSafe = false;
    CgenNodeP expr_type = env.getClassNode(expr->type);
    int exit_label = env.nextLabel();
    
    for(std::set<BranchInfo>::iterator itr = sortedCases.begin(); itr != sortedCases.end(); ++itr) {
        if (expr_type->isSubclassOf(itr->classNode)) {
            // branch guaranteed to succeed, so no overhead
            itr->branch->code(env, itr->classNode, exit_label, offsetFromFP, true, s);
            provenSafe = true;
            break;
        }
        else {
            itr->branch->code(env, itr->classNode, exit_label, offsetFromFP, false, s);
        }
    }
    
    // OPT: if some branch is a superclass of expr->type, then it's impossible for a branch to fail
	if (!provenSafe) {
        // if no branch succeeded, then call case_abort (ACC still holds object)
	    emit_jal("_case_abort", s);
	}
	else {
	    if (cgen_debug) { s << "# Proven that case abort cannot happen, just fall-through" << endl; }
	}
    
    emit_label_def(exit_label, s);
    
	env.current_method.releaseLastOffset();
    
    if (cgen_debug) { s << "# Case Expression End" << endl; }
}

void block_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# Block Begin" << endl; }
    
	for(int i = body->first(); body->more(i); i = body->next(i)){
        if (cgen_debug) { s << "# Block Statement " << i << endl; }
        
		Expression e = body->nth(i);
		e->code(env, s);
    }
    
    if (cgen_debug) { s << "# Block End" << endl; }
}

void let_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# Let Begin" << endl; }
    
	env.localid_offset_table.enterscope();
	
    if (cgen_debug) { s << "# Create Variable Begin" << endl; }
	if(!init->is_no_expr()) {
	    init->code(env,s);
    }
	else{
		if(type_decl == Int || type_decl == Str || type_decl == Bool){
			emit_partial_load_address(ACC, s); emit_protobj_ref(type_decl,s); s << endl;
			emit_jal("Object.copy",s);
		}
		else { 
		    emit_load_imm(ACC,0,s);
	    }
	}
	int offsetFromFP = env.current_method.getFreeOffset();
	emit_store(ACC,offsetFromFP,FP,s);
    if (cgen_debug) { s << "# Create Variable End" << endl; }
	
	// it's ok to store this address because it is forgotten after exitscope below
	env.localid_offset_table.addid(identifier, &offsetFromFP);
	
    if (cgen_debug) { s << "# Body Begin" << endl; }
	body->code(env,s);
    if (cgen_debug) { s << "# Body End" << endl; }
	
	env.current_method.releaseLastOffset();
	
	env.localid_offset_table.exitscope();
	
    if (cgen_debug) { s << "# Let End" << endl; }
}

void plus_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# Plus Begin" << endl; }
    
    // run e1
	e1->code(env, s);
	
	// save result on stack
	emit_push(ACC,s);
	
	// run e2
	e2->code(env, s);
	
    // OPT: if e1 or e2 is a unique Int, then can just override its value rather than allocate
	bool u1 = e1->is_unique(), u2 = e2->is_unique();
	if (!u1 && !u2) {
	    // copy e2's result into a new Int
	    emit_jal("Object.copy",s);
	}
	else {
	    if (cgen_debug) { s << "# Statically verified that e" << ( u2 ? "2" : "1" ) << " can be reused." << endl; }
	}
	// fetch e2's result
	emit_fetch_int(T2,ACC,s);
	
	// fetch e1's result from stack
	emit_pop(T1, s);
	
	if (u1 && !u2) {
	    // if only e1 is unique, then move it's result into ACC to be modified and returned
	    // if e2 is unique, then reuse it by default
	    emit_move(ACC, T1, s);
	}
	
	emit_fetch_int(T1,T1,s);
	
	// perform operation
	emit_add(T3,T1,T2,s);
	
	// put final result in copy
	emit_store(T3,DEFAULT_OBJFIELDS,ACC,s);
	
    if (cgen_debug) { s << "# Plus End" << endl; }
}

void sub_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# Sub Begin" << endl; }
    
	// run e1
	e1->code(env, s);
	// save result on stack
	emit_push(ACC,s);
	
	// run e2
	e2->code(env, s);
	
	// OPT: if e1 or e2 is a unique Int, then can just override its value rather than allocate
	bool u1 = e1->is_unique(), u2 = e2->is_unique();
	if (!u1 && !u2) {
	    // copy e2's result into a new Int
	    emit_jal("Object.copy",s);
	}
	else {
	    if (cgen_debug) { s << "# Statically verified that e" << ( u2 ? "2" : "1" ) << " can be reused." << endl; }
	}
	
	// fetch e2's result
	emit_fetch_int(T2,ACC,s);
	
	// fetch e1's result from stack
	emit_pop(T1, s);
	
	if (u1 && !u2) {
	    // if only e1 is unique, then move it's result into ACC to be modified and returned
	    emit_move(ACC, T1, s);
	}
	
	emit_fetch_int(T1,T1,s);
	
	// perform operation
	emit_sub(T3,T1,T2,s);
	
	// put final result in copy
	emit_store(T3,DEFAULT_OBJFIELDS,ACC,s);
	
    if (cgen_debug) { s << "# Sub End" << endl; }
}

void mul_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# Mul Begin" << endl; }
    
	// run e1
	e1->code(env, s);
	// save result on stack
	emit_push(ACC,s);
	
	// run e2
	e2->code(env, s);
	
	// OPT: if e1 or e2 is a unique Int, then can just override its value rather than allocate
	bool u1 = e1->is_unique(), u2 = e2->is_unique();
	if (!u1 && !u2) {
	    // copy e2's result into a new Int
	    emit_jal("Object.copy",s);
	}
	else {
	    if (cgen_debug) { s << "# Statically verified that e" << ( u2 ? "2" : "1" ) << " can be reused." << endl; }
	}
	
	// fetch e2's result
	emit_fetch_int(T2,ACC,s);
	
	// fetch e1's result from stack
	emit_pop(T1, s);
	
	if (u1 && !u2) {
	    // if only e1 is unique, then move it's result into ACC to be modified and returned
	    emit_move(ACC, T1, s);
	}
	
	emit_fetch_int(T1,T1,s);
	
	// perform operation
	emit_mul(T3,T1,T2,s);
	
	// put final result in copy
	emit_store(T3,DEFAULT_OBJFIELDS,ACC,s);
	
    if (cgen_debug) { s << "# Mul End" << endl; }
}

void divide_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# Div Begin" << endl; }
    
	// run e1
	e1->code(env, s);
	// save result on stack
	emit_push(ACC,s);
	
	// run e2
	e2->code(env, s);
	
	// OPT: if e1 or e2 is a unique Int, then can just override its value rather than allocate
	bool u1 = e1->is_unique(), u2 = e2->is_unique();
	if (!u1 && !u2) {
	    // copy e2's result into a new Int
	    emit_jal("Object.copy",s);
	}
	else {
	    if (cgen_debug) { s << "# Statically verified that e" << ( u2 ? "2" : "1" ) << " can be reused." << endl; }
	}
	
	// fetch e2's result
	emit_fetch_int(T2,ACC,s);
	
	// fetch e1's result from stack
	emit_pop(T1, s);
	
	if (u1 && !u2) {
	    // if only e1 is unique, then move it's result into ACC to be modified and returned
	    emit_move(ACC, T1, s);
	}
	
	emit_fetch_int(T1,T1,s);
	
	// perform operation
	emit_div(T3,T1,T2,s);
	
	// put final result in copy
	emit_store(T3,DEFAULT_OBJFIELDS,ACC,s);
	
    if (cgen_debug) { s << "# Div End" << endl; }
}

void neg_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# Integer negation Begin" << endl; }
    
    // run e1
	e1->code(env, s);
	
	// OPT: if e1 is a unique Int, then can just override its value rather than allocate
	if (! e1->is_unique()) {
	    // copy it into a new Int
	    emit_jal("Object.copy",s);
	}
	else {
	    if (cgen_debug) { s << "# Statically verified that e1 can be reused." << endl; }
	}
	// fetch e1's result out of copy
	emit_fetch_int(T1,ACC,s);
	
	// perform operation
	emit_neg(T2,T1,s);
	
	// put final result in copy
	emit_store(T2,DEFAULT_OBJFIELDS,ACC,s);
	
    if (cgen_debug) { s << "# Integer negation End" << endl; }
}

void lt_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# Less Than Begin" << endl; }
    
	e1->code(env, s);
	emit_push(ACC,s);
	e2->code(env, s);
	
	emit_pop(T1, s);
	
	emit_fetch_int(ACC,ACC,s);
	emit_fetch_int(T1,T1,s);
	int true_label = env.nextLabel();
	emit_blt(T1,ACC,true_label,s);

	//false branch
	emit_load_bool(ACC,falsebool,s);
	int end_label = env.nextLabel();
	emit_branch(end_label,s);
	
	//true branch
	emit_label_def(true_label,s);
	emit_load_bool(ACC,truebool,s);
	
	emit_label_def(end_label,s);
	
    if (cgen_debug) { s << "# Less Than End" << endl; }
}


void eq_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# Equality Begin" << endl; }
    
    // OPT: if the types of e1 and e2 are incomparable, always returns false
    CgenNodeP cn1 = env.getClassNode(e1->type);
    CgenNodeP cn2 = env.getClassNode(e2->type);
    
    bool comparable = (cn1->isSubclassOf(cn2) || cn2->isSubclassOf(cn1));
    
	e1->code(env, s);
	
	if (comparable) {
	    emit_push(ACC,s);
	}
	
	e2->code(env, s);
	
	if (comparable) {
	    emit_pop(T1, s);
	
	    emit_move(T2,ACC,s);
	
	    int done_label = env.nextLabel();
	
	    emit_beq(T1,T2,done_label,s);
	    emit_load_bool(ACC,truebool,s);
	    emit_load_bool(A1,falsebool,s);
	    emit_jal("equality_test",s);
	    emit_label_def(done_label,s);
	}
	else {
	    if (cgen_debug) { s << "# Statically verified that the objects cannot be equal." << endl; }
	    emit_load_bool(ACC,falsebool,s);
	}
	
    if (cgen_debug) { s << "# Equality End" << endl; }
}

void leq_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# Less than or equal Begin" << endl; }
    
	e1->code(env,s);
	emit_push(ACC,s);
	e2->code(env,s);
	
	emit_pop(T1, s);
	
	int true_label = env.nextLabel();
	emit_fetch_int(ACC,ACC,s);
	emit_fetch_int(T1,T1,s);
	emit_bleq(T1,ACC,true_label,s);

	//false
	emit_load_bool(ACC,falsebool,s);
	int end_label = env.nextLabel();
	emit_branch(end_label,s);
	
	//true
	emit_label_def(true_label,s);
	emit_load_bool(ACC,truebool,s);
	
	emit_label_def(end_label,s);
	
    if (cgen_debug) { s << "# Less than or equal End" << endl; }
}

void comp_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# Boolean complement Begin" << endl; }
    
	e1->code(env,s);
	emit_load(T3,DEFAULT_OBJFIELDS,ACC,s);
	
    int false_label = env.nextLabel();
    int end_label = env.nextLabel();

    emit_beqz(T3,false_label,s);

    emit_load_bool(ACC,falsebool,s);
    emit_branch(end_label,s);

    emit_label_def(false_label,s);
    emit_load_bool(ACC,truebool, s);

    emit_label_def(end_label,s);
	
    if (cgen_debug) { s << "# Boolean complement End" << endl; }
}

void int_const_class::code(Environment& env, ostream& s) {
    if (cgen_debug) { s << "# Int constant Begin" << endl; }
    
    emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
  
    if (cgen_debug) { s << "# Int constant End" << endl; }
}

void string_const_class::code(Environment& env, ostream& s) {
    if (cgen_debug) { s << "# String constant Begin" << endl; }
    
    emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
  
    if (cgen_debug) { s << "# String constant End" << endl; }
}

void bool_const_class::code(Environment& env, ostream& s) {
    if (cgen_debug) { s << "# Bool constant Begin" << endl; }
    
    emit_load_bool(ACC, val ? truebool : falsebool , s);
  
    if (cgen_debug) { s << "# Bool constant End" << endl; }
}

void new__class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# New Begin" << endl; }
    
	if(type_name == SELF_TYPE){
		emit_load(T1, TAG_OFFSET, SELF,s); // get tag
		
		// multiply by 8 to get index into object table
		emit_sll(T1, T1, 3, s);
		emit_load_address(T2,CLASSOBJTAB,s);
		emit_addu(T2,T2,T1,s);
		
		// copy proto-object then dynamically jump to init
		emit_load(ACC,0,T2,s);
		emit_push(T2, s);
		emit_jal("Object.copy",s);
		emit_pop(T2, s);
		emit_load(T3,1,T2,s);
		emit_jalr(T3,s);
	}
	else {
	    // copy proto-object then statically jump to init
		emit_partial_load_address(ACC, s); emit_protobj_ref(type_name,s); s << endl;
	    emit_jal("Object.copy",s);
	    emit_partial_jal(s); emit_init_ref(type_name,s); s << endl;
	}
    if (cgen_debug) { s << "# New End" << endl; }
}

void isvoid_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# isvoid Begin" << endl; }
    
	e1->code(env,s);
	int void_label, end_label;
	
	// OPT: if never null, then always false
	bool canBeNull = ! e1->is_never_null();
	
	if (canBeNull) {
	    void_label = env.nextLabel();
	    end_label = env.nextLabel();

	    emit_beq(ACC,ZERO,void_label,s);
	}
	else {
	    if (cgen_debug) { s << "# Statically verified that object is never null." << endl; }
	}
	
    //not void
    emit_load_bool(ACC,falsebool,s);
    
    if (canBeNull) {
	    emit_branch(end_label,s);
	
	    //is void
	    emit_label_def(void_label,s);
	    emit_load_bool(ACC,truebool,s);

	    emit_label_def(end_label,s);
	}
    if (cgen_debug) { s << "# isvoid End" << endl; }
}

void no_expr_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# no expr" << endl; }
}

void object_class::code(Environment& env, ostream &s) {
    if (cgen_debug) { s << "# Load Identifier Begin" << endl; }
    
	if(name == self){
		emit_move(ACC,SELF,s);
	}
	else {
	    int* word_offset = env.localid_offset_table.lookup(name);
	    if(!word_offset)
	        emit_load(ACC, env.get_attribute_offset(name), SELF, s);
	    else
	        emit_load(ACC, *word_offset, FP, s);
	}
    
    if (cgen_debug) { s << "# Load Identifier End" << endl; }
}


