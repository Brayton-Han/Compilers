#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

extern int semant_debug;
extern char *curr_filename;

ClassTable ct;

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
static void initialize_constants(void) {
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

ClassTable::ClassTable() : semant_errors(0) , error_stream(cerr) {}

void ClassTable::install(Classes classes) {
    install_basic_classes();
    install_other_classes(classes);
    install_methods();
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
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

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

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
           
    classTable[Object_class->getName()] = Object_class;
    classTable[IO_class->getName()] = IO_class;
    classTable[Int_class->getName()] = Int_class;
    classTable[Bool_class->getName()] = Bool_class;
    classTable[Str_class->getName()] = Str_class;
}

void ClassTable::install_other_classes(Classes& classes) {
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        cur_class = classes->nth(i);
        Symbol parent = cur_class->getParentName();
        if(cur_class->getName() == Object)
            semant_error(cur_class) << "Redefinition of basic class Object." << endl;
        else if(cur_class->getName() == IO)
            semant_error(cur_class) << "Redefinition of basic class IO." << endl;
        else if(cur_class->getName() == Int)
            semant_error(cur_class) << "Redefinition of basic class Int." << endl;
        else if(cur_class->getName() == Bool)
            semant_error(cur_class) << "Redefinition of basic class Bool." << endl;
        else if(cur_class->getName() == Str)
            semant_error(cur_class) << "Redefinition of basic class Str." << endl;
        else if(cur_class->getName() == SELF_TYPE)
            semant_error(cur_class) << "Redefinition of basic class SELF_TYPE." << endl; 
        else if(classTable.find(cur_class->getName()) != classTable.end())
            semant_error(cur_class) << "Class " << cur_class->getName() << " was previously defined." << endl;
        else if(parent == Int || parent == Str || parent == Bool || parent == SELF_TYPE)
            semant_error(cur_class) << "Class " << cur_class->getName() << " cannot inherit class " << parent << "." << endl;
        else
            classTable[cur_class->getName()] = cur_class;
    }
    //cout << "right" << endl;
}

void ClassTable::install_methods() {
    for(auto it = classTable.begin(); it != classTable.end(); ++it) {
        //cout << it->first << ':' << endl;
        cur_class = it->second;
        Features features = cur_class->getFeatures();
        std::vector<method_class*> methods;
        for(int i = features->first(); features->more(i); i = features->next(i)) {
            if(features->nth(i)->isAttr()) continue;
            method_class* m = static_cast<method_class*>(features->nth(i));
            for(size_t j = 0; j < methods.size(); ++j)
                if(m->getName() == methods[j]->getName())
                    semant_error(cur_class) << "You redifined method " << m->getName() << "." << endl;
            methods.push_back(m);
            /*
            cout << m->getName() << endl;
            Formals formals = m->getFormals();
            for(int y = formals->first(); formals->more(y); y = formals->next(y))
                cout << formals->nth(y)->getType() << endl;
                */
        }
        methodTable[cur_class] = methods;
    }
}

// 此函数copy了助教给的文档中的代码
void ClassTable::check_classes() {
    // 从 class table 找 Main
    if(classTable.find(Main) == classTable.end()) {
        semant_error() << "Class Main is not defined." << endl;
        return;
    }
    main_class = classTable[Main];
    // 迭代遍历 features，检查是否存在 main 函数
    Features features = main_class->getFeatures();
    bool find_main = false;
    for(int i = features->first(); features->more(i); i = features->next(i))
        if(features->nth(i)->isMethod() && static_cast<method_class*>(features->nth(i))->getName() == main_meth)
            find_main = true;
    if(!find_main)
        semant_error(main_class) << "No 'main' method in class Main." << endl;
    // 检查类是否定义
    for(auto it = classTable.begin(); it != classTable.end(); it++) {
        cur_class = it->second;
        if(it->first != Object && classTable.find(cur_class->getParentName()) == classTable.end())
            semant_error(cur_class) << "Class " << cur_class->getName() << " inherits from an undefined class " << cur_class->getParentName() << "." << endl;
    }
    // 检查类是否构成循环依赖
    for(auto it = classTable.begin(); it != classTable.end(); it++) {
        if(it->first == Object) continue;
        cur_class = it->second;
        Symbol cname = it->first;
        Symbol pname = cur_class->getParentName();
        while(pname != Object) {
            if(pname == cname) {
                semant_error(cur_class) << "Class " << cur_class->getName() << ", or an ancestor of " << cur_class->getName() << ", is involved in an inheritance cycle." << endl;
                break;
            }
            if(classTable.find(pname) == classTable.end())
                break;
            pname = classTable[pname]->getParentName();
        }
    }
    //cout << "right" << endl;
}

void ClassTable::check_features() {
    for(auto it = classTable.begin(); it != classTable.end(); ++it) {
        cur_class = it->second;
        //基础类不用检查
        if(it->first == Object || it->first == IO || it->first == Int || it->first == Bool || it->first == Str) continue;
        //获取继承表
        std::vector<Class_> inherit = getInherit(cur_class);
        std::reverse(inherit.begin(), inherit.end());
        //获取当前类拥有的所有attr
        for(size_t i = 0; i < inherit.size(); ++i) {
            Features features = inherit[i]->getFeatures();
            st.enterscope();
            for(int j = features->first(); features->more(j); j = features->next(j))
                if(features->nth(j)->isAttr()) {
                    attr_class* a = static_cast<attr_class*>(features->nth(j));
                    if(st.lookup(a->getName()))
                        semant_error(a) << "Attribute " << a->getName() << " is an attribute of an inherited class." << endl;
                    Symbol* s = new Symbol(a->getType());
                    st.addid(a->getName(), s);
                }
        }
        std::reverse(inherit.begin(), inherit.end());
        //逐一检查当前类的所有属性
        Features features = cur_class->getFeatures();
        for(int i = features->first(); features->more(i); i = features->next(i))
            if(features->nth(i)->isAttr()) {
                attr_class* a = static_cast<attr_class*>(features->nth(i));
                std::string s = "self";
                if(!strcmp(a->getName()->get_string(), s.c_str()))
                    semant_error(a) << "'self' cannot be the name of an attribute." << endl;
                //检查属性的类型声明是否被定义，按照规范应该定义；
                if(classTable.find(a->getType()) == classTable.end())
                    semant_error(cur_class) << "Attribution " << a->getType() << " hasn't been defined yet." << endl;
                //检查属性初始化时被推导出的类型与声明是否符合，按照规范应该符合；
                Symbol exp = a->getInit()->checkType();
                if(classTable.find(exp) != classTable.end() && !check(exp, a->getType(), it->first))
                    semant_error(cur_class) << "The referred type and defined type are not same." << endl;
            } else {
                method_class* m = static_cast<method_class*>(features->nth(i));
                m->checkType();
                //当子类重载父类中定义的方法
                for(size_t j = 1; j < inherit.size(); ++j) {
                    auto methods = methodTable[inherit[j]];
                    method_class* method = NULL;
                    for(size_t k = 0; k < methods.size(); ++k)
                        if(methods[k]->getName() == m->getName()) {
                            method = methods[k];
                            break;
                        }
                    if(!method) continue;
                    //检查函数返回值是否与父类中的定义一致
                    if(method->getReturnType() != m->getReturnType())
                        semant_error(cur_class) << "Return type of method is wrong(cur and father)." << endl;
                    //检查函数参数数量，参数类型是否与父类中的定义一致
                    Formals formals = m->getFormals();
                    Formals father_formals = method->getFormals();
                    if(formals->len() != father_formals->len())
                        semant_error(m) << "Incompatible number of formal parameters in redefined method " << m->getName() << "." << endl;
                    for(int x = formals->first(), y = father_formals->first(); formals->more(x) && father_formals->more(y); x = formals->next(x), y = father_formals->next(y))
                        if(formals->nth(x)->getType() != father_formals->nth(y)->getType())
                            semant_error(m) << "In redefined method " << m->getName() << ", parameter type " << formals->nth(x)->getType() << " is different from original type " << father_formals->nth(y)->getType() << endl;
                }
            }
        for(size_t i = 0; i < inherit.size(); ++i)
            st.exitscope();
    }
}

std::vector<Class_> ClassTable::getInherit(Class_ cur) {
    std::vector<Class_> inherit;
    while(cur->getName() != Object) {
        inherit.push_back(cur);
        cur = classTable[cur->getParentName()];
    }
    return inherit;
}

bool ClassTable::check(Symbol s1, Symbol s2, Symbol cur) {
    //cout << s1 << ' ' << s2 << endl;
    if(s1 == s2) return true;
    if(s2 == SELF_TYPE) return false;
    if(ct.classTable.find(s2) == ct.classTable.end()) return true;
    if(s1 == SELF_TYPE) s1 = cur;
    std::vector<Class_> inherit = getInherit(classTable[s1]);
    inherit.push_back(classTable[Object]);
    for(size_t i = 0; i < inherit.size(); ++i)
        if(inherit[i]->getName() == s2)
            return true;
    return false;
}

Symbol ClassTable::LCA(Symbol s1, Symbol s2) {
    if(s1 == SELF_TYPE) s1 = ct.cur_class->getName();
    if(s2 == SELF_TYPE) s2 = ct.cur_class->getName();
    auto inherit1 = ct.getInherit(ct.classTable[s1]);
    auto inherit2 = ct.getInherit(ct.classTable[s2]);
    inherit1.push_back(ct.classTable[Object]);
    inherit2.push_back(ct.classTable[Object]);
    int i = inherit1.size() - 1, j = inherit2.size() - 1;
    while(i >= 0 && j >= 0) {
        if(inherit1[i] != inherit2[j])
            return inherit1[i + 1]->getName();
        --i; --j;
    }
    return inherit1[i + 1]->getName();
}

void method_class::checkType() {
    std::set<Symbol> set;
    ct.st.enterscope();
    for(int j = formals->first(); formals->more(j); j = formals->next(j)) {
        if(formals->nth(j)->getType() == SELF_TYPE)
            ct.semant_error(this) << "Formal parameter " << formals->nth(j)->getName() << " cannot have type SELF_TYPE." << endl;
        //检查形式参数的类型是否被定义，按照规范应该被定义；
        else if(ct.classTable.find(formals->nth(j)->getType()) == ct.classTable.end())
            ct.semant_error(this) << "Undefined return type " << formals->nth(j)->getType() << " in method " << this->getName() << "." << endl;
        //检查形式参数中是否包含 self，按照规范不应该包含；
        if(formals->nth(j)->getName() == self)
            ct.semant_error(this) << "'self' cannot be the name of a formal parameter." << endl;
        //检查形式参数是否被重复定义，按照规范不应该被重复定义；
        else if(set.find(formals->nth(j)->getName()) != set.end())
            ct.semant_error(this) << "Formal parameter " << formals->nth(j)->getName() << " is multiply defined." << endl;
        else {
            set.insert(formals->nth(j)->getName());
            Symbol* s = new Symbol(formals->nth(j)->getType());
            ct.st.addid(formals->nth(j)->getName(), s);
        }
    }
    //检查返回类型是否被定义，按照规范应该被定义；
    if(return_type != SELF_TYPE && ct.classTable.find(return_type) == ct.classTable.end())
        ct.semant_error(this) << "Undefined return type " << return_type << " in method " << this->getName() << "." << endl;
    //检查推导出来的返回类型和声明的返回类型是否一致，按照规范应该一致；
    Symbol exprType = expr->checkType();
    if(!ct.check(exprType, return_type, ct.cur_class->getName()))
        ct.semant_error(this) << "Inferred return type " << exprType << " of method " << this->getName() << " does not conform to declared return type " << return_type << '.' << endl;
    ct.st.exitscope();
}

Symbol assign_class::checkType() {
    Symbol returnType = expr->checkType();
    type = returnType;
    std::string s = "self";
    if(!strcmp(name->get_string(), s.c_str())) {
        ct.semant_error(this) << "Cannot assign to 'self'." << endl;
        return type;
    }
    else if(!ct.st.lookup(name)) {
        ct.semant_error(this) << "Assignment to undeclared variable " << name << "." << endl;
        return type;
    }
    Symbol decType = *ct.st.lookup(name);
    if(!ct.check(returnType, decType, ct.cur_class->getName())) {
        ct.semant_error(this) << "Type " << returnType << " of assigned expression does not conform to declared type " << decType << " of identifier " << name << "." << endl;
        type = decType;
        return type;
    }
    return type;
}

Symbol static_dispatch_class::checkType() {
    bool error = false;
    Symbol exprType = expr->checkType();
    //检查静态调用声明的类型是否被定义，仅在静态调用时检查，按照规范应该被定义
    if(this->type_name != SELF_TYPE && ct.classTable.find(this->type_name) == ct.classTable.end()) {
        ct.semant_error(this) << "Static dispatch to undefined class " << this->type_name << "." << endl;
        type = Object;
        return type;
    }
    //检查表达式类型是否被定义，按照规范应该被定义
    if(exprType != SELF_TYPE && ct.classTable.find(exprType) == ct.classTable.end()) {
        ct.semant_error(this) << "Dispatch on undefined class " << exprType << "." << endl;
        type = Object;
        return type;
    }
    //检查表达式类型与静态调用的声明是否符合，仅在静态调用时检查，按照规范应该符合
    if(!ct.check(exprType, this->type_name, ct.cur_class->getName())) {
        error = true;
        ct.semant_error(this) << "Expression type " << exprType << " does not conform to declared static dispatch type " << this->type_name << "." << endl;
    }
    //检查函数是否被定义，按照规范应该被定义
    method_class* method = NULL;
    auto inherit = ct.getInherit(ct.classTable[this->type_name]);
    for(size_t i = 0; i < inherit.size(); ++i) {
        auto methods = ct.methodTable[inherit[i]];
        for(size_t j = 0; j < methods.size(); ++j)
            if(methods[j]->getName() == name) {
                method = methods[j];
                break;
            }
        //if(method) break;
    }
    if(!method) {
        error = true;
        ct.semant_error(this) << "Static dispatch to undefined method " << name << "." << endl;
    } else {
        Formals formals = method->getFormals();
        //检查函数调用的参数数量与定义是否符合，按照规范应该符合
        if(actual->len() > formals->len()) {
            error = true;
            ct.semant_error(this) << "Method " << name << " called with wrong number of arguments." << endl;
        }
        //检查实参和形参的类型是否符合，按照规范应该符合
        for(int x = actual->first(), y = formals->first(); actual->more(x) && formals->more(y); x = actual->next(x), y = formals->next(y)) {
            Symbol actual_type = actual->nth(x)->checkType();
            Symbol formal_type = formals->nth(y)->getType();
            //cout << formal_type << endl;
            if(!ct.check(actual_type, formal_type, ct.cur_class->getName())) {
                error = true;
                ct.semant_error(this) << "In call of method " << name << ", type " << actual_type << " of parameter " << formals->nth(y)->getName() << " does not conform to declared type " << formal_type << "." << endl;
            }
        }
    }
    if(error) type = Object;
    else {
        type = method->getReturnType();
        if(type == SELF_TYPE) type = this->type_name;
    }
    return type;
}

Symbol dispatch_class::checkType() {
    bool error = false;
    //检查表达式类型是否被定义，按照规范应该被定义
    Symbol exprType = expr->checkType();
    if(exprType != SELF_TYPE && ct.classTable.find(exprType) == ct.classTable.end()) {
        ct.semant_error(this) << "Dispatch on undefined class " << exprType << "." << endl;
        type = Object;
        return type;
    }
    //检查函数是否被定义，按照规范应该被定义
    method_class* method = NULL;
    std::vector<Class_> inherit;
    if(exprType == SELF_TYPE) 
        inherit = ct.getInherit(ct.classTable[ct.cur_class->getName()]);
    else
        inherit = ct.getInherit(ct.classTable[exprType]);
    inherit.push_back(ct.classTable[Object]);
    for(size_t i = 0; i < inherit.size(); ++i) {
        auto methods = ct.methodTable[inherit[i]];
        for(size_t j = 0; j < methods.size(); ++j)
            if(methods[j]->getName() == name) {
                method = methods[j];
                break;
            }
        //if(method) break;
    }
    if(!method) {
        error = true;
        ct.semant_error(this) << "Dispatch to undefined method " << name << "." << endl;
    } else {
        Formals formals = method->getFormals();
        //检查函数调用的参数数量与定义是否符合，按照规范应该符合
        if(actual->len() > formals->len()) {
            error = true;
            ct.semant_error(this) << "Method " << name << " called with wrong number of arguments." << endl;
        }
        //检查实参和形参的类型是否符合，按照规范应该符合
        for(int x = actual->first(), y = formals->first(); actual->more(x) && formals->more(y); x = actual->next(x), y = formals->next(y)) {
            Symbol actual_type = actual->nth(x)->checkType();
            Symbol formal_type = formals->nth(y)->getType();
            //cout << formal_type << endl;
            if(!ct.check(actual_type, formal_type, ct.cur_class->getName())) {
                error = true;
                ct.semant_error(this) << "In call of method " << name << ", type " << actual_type << " of parameter " << formals->nth(y)->getName() << " does not conform to declared type " << formal_type << "." << endl;
            }
        }
    }
    if(error) type = Object;
    else {
        type = method->getReturnType();
        if(type == SELF_TYPE) type = exprType;
    }
    return type;
}

Symbol cond_class::checkType() {
    if(pred->checkType() != Bool)
        ct.semant_error(this) << "Predicate of 'if' does not have type Bool." << endl;
    Symbol thenType = then_exp->checkType();
    Symbol elseType = else_exp->checkType();
    if(thenType == SELF_TYPE && elseType == SELF_TYPE)
        type = SELF_TYPE;
    else
        type = ct.LCA(thenType, elseType);
    return type;
}

Symbol loop_class::checkType() {
    if(pred->checkType() != Bool)
        ct.semant_error(this) << "Loop condition does not have type Bool." << endl;
    body->checkType();
    type = Object;
    return type;
}

Symbol typcase_class::checkType() {
    expr->checkType();
    std::set<Symbol> branchs;
    for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
        branch_class* branch = static_cast<branch_class*>(cases->nth(i));
        if(branchs.find(branch->getType()) != branchs.end())
            ct.semant_error(branch) << "Duplicate branch " << branch->getType() << " in case statement." << endl;
        else
            branchs.insert(branch->getType());
        Symbol branchType = branch->checkType();
        if(i == cases->first())
            type = branchType;
        else if(type != SELF_TYPE || branchType != SELF_TYPE)
            type = ct.LCA(type, branchType);
    }
    return type;
}

Symbol branch_class::checkType() {
    ct.st.enterscope();
    Symbol* s = new Symbol(type_decl);
    ct.st.addid(name, s);
    type = expr->checkType();
    ct.st.exitscope();
    return type;
}

Symbol let_class::checkType() {
    ct.st.enterscope();
    std::string str = "self";
    if(!strcmp(this->getIdentifier()->get_string(), str.c_str()))
        ct.semant_error(this) << "'self' cannot be bound in a 'let' expression." << endl;
    Symbol initType = init->checkType();
    Symbol* s = new Symbol(type_decl);
    ct.st.addid(identifier, s);
    if(type_decl != SELF_TYPE && ct.classTable.find(type_decl) == ct.classTable.end())
        ct.semant_error(this) << "Class " << type_decl << " of let-bound identifier " << identifier << " is undefined." << endl;
    else if(initType != No_type && !ct.check(initType, type_decl, ct.cur_class->getName()))
        ct.semant_error(this) << "Inferred type " << initType << " of initialization of " << identifier << " does not conform to identifier's declared type " << type_decl << "." << endl;
    type = body->checkType();
    ct.st.exitscope();
    return type;
}

Symbol plus_class::checkType() {
    Symbol type1 = e1->checkType();
    Symbol type2 = e2->checkType();
    if(type1 != Int || type2 != Int)
        ct.semant_error(this) << "non-Int arguments: " << type1 << " + " << type2 << endl;
    type = Int;
    return type;
}

Symbol sub_class::checkType() {
    Symbol type1 = e1->checkType();
    Symbol type2 = e2->checkType();
    if(type1 != Int || type2 != Int)
        ct.semant_error(this) << "non-Int arguments: " << type1 << " - " << type2 << endl;
    type = Int;
    return type;
}

Symbol mul_class::checkType() {
    Symbol type1 = e1->checkType();
    Symbol type2 = e2->checkType();
    if(type1 != Int || type2 != Int)
        ct.semant_error(this) << "non-Int arguments: " << type1 << " * " << type2 << endl;
    type = Int;
    return type;
}

Symbol divide_class::checkType() {
    Symbol type1 = e1->checkType();
    Symbol type2 = e2->checkType();
    if(type1 != Int || type2 != Int)
        ct.semant_error(this) << "non-Int arguments: " << type1 << " / " << type2 << endl;
    type = Int;
    return type;
}

Symbol neg_class::checkType() {
    Symbol type1 = e1->checkType();
    if(type1 != Int)
        ct.semant_error(this) << "non-Int arguments: " << " - " << type1 << endl;
    type = Int;
    return type;
}

Symbol lt_class::checkType() {
    Symbol type1 = e1->checkType();
    Symbol type2 = e2->checkType();
    if(type1 != Int || type2 != Int)
        ct.semant_error(this) << "non-Int arguments: " << type1 << " < " << type2 << endl;
    type = Bool;
    return type;
}

Symbol leq_class::checkType() {
    Symbol type1 = e1->checkType();
    Symbol type2 = e2->checkType();
    if(type1 != Int || type2 != Int)
        ct.semant_error(this) << "non-Int arguments: " << type1 << " <= " << type2 << endl;
    type = Bool;
    return type;
}

Symbol eq_class::checkType() {
    Symbol type1 = e1->checkType();
    Symbol type2 = e2->checkType();
    if((type1 == Int || type1 == Bool || type1 == Str || type2 == Int || type2 == Bool || type2 == Str) && type1 != type2)
        ct.semant_error(this) << "Illegal comparison with a basic type." << endl;
    type = Bool;
    return type;
}

Symbol comp_class::checkType() {
    Symbol type1 = e1->checkType();
    if(type1 != Bool)
        ct.semant_error(this) << "non-Bool arguments: " << type1 << endl;
    type = Bool;
    return type;
}

Symbol new__class::checkType() {
    if(this->type_name != SELF_TYPE && ct.classTable.find(this->type_name) == ct.classTable.end()) {
        ct.semant_error(this) << "'new' used with undefined class " << this->type_name << "." << endl;
        this->type_name = Object;
    }
    type = this->type_name;
    return type;
}

Symbol object_class::checkType() {
    if(name == self)
        type = SELF_TYPE;
    else if(ct.st.lookup(name))
        type = *ct.st.lookup(name);
    else {
        ct.semant_error(this) << "Undeclared identifier " << name << "." << endl;
        type = Object;
    }
    return type;
}

Symbol no_expr_class::checkType() {
    type = No_type;
    return type;
}

Symbol isvoid_class::checkType() {
    e1->checkType();
    type = Bool;
    return type;
}

Symbol string_const_class::checkType() {
    type = Str;
    return type;
}

Symbol bool_const_class::checkType() {
    type = Bool;
    return type;
}

Symbol int_const_class::checkType() {
    type = Int;
    return type;
}

Symbol block_class::checkType() {
    for(int i = body->first(); body->more(i); i = body->next(i))
        type = body->nth(i)->checkType();
    return type;
}

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

ostream& ClassTable::semant_error(tree_node *t) {
    error_stream << cur_class->get_filename() << ":" << t->get_line_number() << ": ";
    return semant_error();
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
    ct.install(classes);
    if(ct.errors()) {
	    cerr << "Compilation halted due to static semantic errors." << endl;
	    exit(1);
    }

    /* some semantic analysis code may go here */
    ct.check_classes();
    if(ct.errors()) {
	    cerr << "Compilation halted due to static semantic errors." << endl;
	    exit(1);
    }

    ct.check_features();
    if(ct.errors()) {
	    cerr << "Compilation halted due to static semantic errors." << endl;
	    exit(1);
    }
}
