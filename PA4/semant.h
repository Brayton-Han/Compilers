#pragma once

#ifndef SEMANT_H
#define SEMANT_H

#include <assert.h>
#include <iostream>
#include <map>
#include <vector>
#include <string>
#include <algorithm>
#include <set>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;
  Class_ main_class;

public:
  Class_ cur_class;
  SymbolTable<Symbol, Symbol> st;
  std::map<Symbol, Class_> classTable;
  std::map<Class_, std::vector<method_class*>> methodTable;

  ClassTable();
  void install(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  ostream& semant_error(tree_node *t);
  void install_other_classes(Classes& classes);
  void install_methods();
  void check_classes();
  void check_features();
  std::vector<Class_> getInherit(Class_ cur);
  bool check(Symbol s1, Symbol s2, Symbol cur);
  Symbol LCA(Symbol s1, Symbol s2);
};

#endif