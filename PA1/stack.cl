(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class Stack inherits IO {

   s : String;

   push(tar : String): Object {
      {
         s <- s.concat("|");
         s <- s.concat(tar);
      }
   };

   show(): Object {
      {
         (let j : Int <- s.length() in {
            j <- j - 1;
            while 0 <= j loop {
               (let i : Int <- j in {
                  while (if 0 <= i then 1 else 0 fi + if s.substr(i, 1) = "|" then 0 else 1 fi = 2) loop {
                     i <- i - 1;
                  } pool;
                  out_string(s.substr(i + 1, j - i));
                  j <- i - 1;
                  out_string("\n");
               });
            } pool;
         });
      }
   };

   work(): Object {
      {
         (let len : Int <- s.length() in {
            if not len = 0 then {
               (let top : String <- s.substr(len - 1, 1) in {
                  if top = "+" then plus() 
                  else if top = "s" then swap() else 0 fi
                  fi;
               });
            } else 0 fi;
         });
      }
   };

   swap(): Object { {
      s <- s.substr(0, s.length() - 2);
      (let j : Int <- s.length() - 1, i : Int <- j in {
         while (if 0 <= i then 1 else 0 fi + if s.substr(i, 1) = "|" then 0 else 1 fi = 2) loop {
            i <- i - 1;
         } pool;
         if 0 <= i then {
            (let first : String <- s.substr(i + 1, j - i) in {
               j <- i - 1;
               i <- j;
               while (if 0 <= i then 1 else 0 fi + if s.substr(i, 1) = "|" then 0 else 1 fi = 2) loop {
                  i <- i - 1;
               } pool;
               if 0 <= i then {
                  (let second : String <- s.substr(i + 1, j - i) in {
                     s <- s.substr(0, i + 1);
                     s <- s.concat(first);
                     s <- s.concat("|");
                     s <- s.concat(second);
                  });
               } else 0 fi;
            });
         } else 0 fi;
      });
   }};

   plus(): Object {{
      s <- s.substr(0, s.length() - 2);
      (let j : Int <- s.length() - 1, i : Int <- j, top : String <- s.substr(i, 1) in {
         if (if top = "s" then 0 else 1 fi + if top = "+" then 0 else 1 fi = 2) then {
            while (if 0 <= i then 1 else 0 fi + if s.substr(i, 1) = "|" then 0 else 1 fi = 2) loop {
               i <- i - 1;
            } pool;
            if 0 <= i then {
               (let first : String <- s.substr(i + 1, j - i) in {
                  j <- i - 1;
                  i <- j;
                  top <- s.substr(i, 1);
                  if (if top = "s" then 0 else 1 fi + if top = "+" then 0 else 1 fi = 2) then {
                     while (if 0 <= i then 1 else 0 fi + if s.substr(i, 1) = "|" then 0 else 1 fi = 2) loop {
                        i <- i - 1;
                     } pool;
                     if 0 <= i then {
                        (let second : String <- s.substr(i + 1, j - i), cal : A2I <- new A2I, number : Int <- cal.a2i(first) + cal.a2i(second) in {
                           s <- s.substr(0, i + 1);
                           s <- s.concat(cal.i2a(number));
                        });
                     } else 0 fi;
                  } else 0 fi;
               });
            } else 0 fi;
         } else 0 fi;
      });
   }};

};

class Main inherits IO {

   main() : Object {{
      (let machine : Stack <- new Stack, operation : String <- "a" in {
         while not operation = "x" loop {
            out_string(">");
            operation <- in_string();
            if operation = "d" then machine.show()
            else if operation = "e" then machine.work()
            else if operation = "x" then 0
            else machine.push(operation)
            fi fi fi;
         } pool;
      });
   }};

};
