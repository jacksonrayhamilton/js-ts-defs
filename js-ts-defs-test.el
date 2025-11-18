;;; js-ts-defs-test.el --- Tests for js-ts-defs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jackson Ray Hamilton

;; Author: Jackson Ray Hamilton <jackson@jacksonrayhamilton.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for js-ts-defs package.

;;; Code:

(require 'cua-base)
(require 'ert)
(require 'js-ts-defs)

(defun js-ts-defs--deep-equal (obj1 obj2)
  "Deep equality comparison that handles hash tables.
Compare OBJ1 with OBJ2.
Like `equal' but also compares hash table contents."
  (cond
   ;; Both are hash tables
   ((and (hash-table-p obj1) (hash-table-p obj2))
    (and (= (hash-table-count obj1) (hash-table-count obj2))
         (eq (hash-table-test obj1) (hash-table-test obj2))
         (catch 'not-equal
           (maphash (lambda (key value1)
                      (let ((value2 (gethash key obj2 'js-ts-defs--not-found)))
                        (when (or (eq value2 'js-ts-defs--not-found)
                                  (not (js-ts-defs--deep-equal value1 value2)))
                          (throw 'not-equal nil))))
                    obj1)
           t)))

   ;; One is hash table, other is not
   ((or (hash-table-p obj1) (hash-table-p obj2))
    nil)

   ;; Both are lists
   ((and (listp obj1) (listp obj2))
    (and (= (length obj1) (length obj2))
         (catch 'not-equal
           (while (and obj1 obj2)
             (unless (js-ts-defs--deep-equal (car obj1) (car obj2))
               (throw 'not-equal nil))
             (setq obj1 (cdr obj1)
                   obj2 (cdr obj2)))
           t)))

   ;; One is list, other is not
   ((or (listp obj1) (listp obj2))
    nil)

   ;; Use regular equal for everything else
   (t
    (equal obj1 obj2))))

(ert-deftest js-ts-defs-test-build-scope ()
  "Test building scope object from buffer with var/let/const declarations."
  (with-temp-buffer
    (insert "var globalVar = 1;\n")
    (insert "let globalLet = 2;\n")
    (insert "function myFunc(param1) {\n")
    (insert "  var localVar = 3;\n")
    (insert "  let localLet = 4;\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 107     ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "globalVar" 5 variables)  ; position of "globalVar"
                               (puthash "globalLet" 24 variables) ; position of "globalLet"
                               (puthash "myFunc" 48 variables)    ; position of "myFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 39 ; start of function
                                   :end 106  ; end of function
                                   ;; Build expected function variables hash table
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "param1" 55 variables)   ; position of "param1"
                                                (puthash "localVar" 71 variables) ; position of "localVar"
                                                (puthash "localLet" 91 variables) ; position of "localLet"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-anonymous-function-expression-scope ()
  "Test that anonymous function expressions create proper scopes."
  (with-temp-buffer
    (insert "var fn1 = function(param1) {\n")
    (insert "  var localVar1 = param1;\n")
    (insert "};\n")
    (insert "\n")
    (insert "var fn2 = (param2) => {\n")
    (insert "  let localVar2 = param2;\n")
    (insert "};\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 113     ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "fn1" 5 variables)  ; position of "fn1"
                               (puthash "fn2" 64 variables) ; position of "fn2"
                               variables)
                  :children (list
                             ;; Build expected anonymous function expression scope
                             (list :type "function"
                                   :start 11 ; start of function expression
                                   :end 57   ; end of function expression
                                   ;; Build expected function variables hash table
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "param1" 20 variables)    ; position of "param1"
                                                (puthash "localVar1" 36 variables) ; position of "localVar1"
                                                variables)
                                   :children '())
                             ;; Build expected arrow function scope
                             (list :type "function"
                                   :start 70 ; start of arrow function
                                   :end 111  ; end of arrow function
                                   ;; Build expected function variables hash table
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "param2" 71 variables)    ; position of "param2"
                                                (puthash "localVar2" 90 variables) ; position of "localVar2"
                                                variables)
                                   :children '()
                                   ;; Mark as arrow function
                                   :is-arrow t)))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-named-function-expression-scope ()
  "Test that named functions are available in function expression scope."
  (with-temp-buffer
    (insert "var fn = function namedFunc(param1, param2) {\n")
    (insert "  return param1 + param2;\n")
    (insert "};\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 76      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "fn" 5 variables)  ; position of "fn"
                               variables)
                  :children (list
                             ;; Build expected function expression scope
                             (list :type "function"
                                   :start 10 ; start of function expression
                                   :end 74   ; end of function expression
                                   ;; Build expected function variables hash table
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "namedFunc" 19 variables) ; position of "namedFunc"
                                                (puthash "param1" 29 variables)    ; position of "param1"
                                                (puthash "param2" 37 variables)    ; position of "param2"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-var-vs-let-const-scoping ()
  "Test that var is added to function scope while let/const is added to block scope."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  {\n")
    (insert "    var varInBlock = 1;\n")
    (insert "    let letInBlock = 2;\n")
    (insert "    const constInBlock = 3;\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 109     ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 108  ; end of function
                                   ;; var should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "varInBlock" 35 variables) ; position of "varInBlock"
                                                variables)
                                   :children (list
                                              ;; Build expected block scope
                                              (list :type "block"
                                                    :start 25 ; start of block
                                                    :end 106  ; end of block
                                                    ;; let/const should be in block scope
                                                    :variables (let ((variables (make-hash-table :test 'equal)))
                                                                 (puthash "letInBlock" 59 variables)   ; position of "letInBlock"
                                                                 (puthash "constInBlock" 85 variables) ; position of "constInBlock"
                                                                 variables)
                                                    :children '())))))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-catch-block-scope ()
  "Test that catch variable and var declarations are scoped to catch block."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  try {\n")
    (insert "    throw new Error('test');\n")
    (insert "  } catch (error) {\n")
    (insert "    var varInCatch = 1;\n")
    (insert "    let letInCatch = 2;\n")
    (insert "    const constInCatch = 3;\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 162     ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 161  ; end of function
                                   ;; Function scope should be empty (var is scoped to catch block)
                                   :variables (make-hash-table :test 'equal)
                                   :children (list
                                              ;; Build expected catch block scope
                                              (list :type "block"
                                                    :start 64 ; start of catch clause
                                                    :end 159  ; end of catch clause
                                                    ;; All variables should be in catch block scope
                                                    :variables (let ((variables (make-hash-table :test 'equal)))
                                                                 (puthash "error" 71 variables)         ; position of "error"
                                                                 (puthash "varInCatch" 88 variables)    ; position of "varInCatch"
                                                                 (puthash "letInCatch" 112 variables)   ; position of "letInCatch"
                                                                 (puthash "constInCatch" 138 variables) ; position of "constInCatch"
                                                                 variables)
                                                    :children '())))))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-class-and-method-scope ()
  "Test that classes are added to block scope and methods create function scopes."
  (with-temp-buffer
    (insert "class MyClass {\n")
    (insert "  constructor(param1) {\n")
    (insert "    this.value = param1;\n")
    (insert "  }\n")
    (insert "  \n")
    (insert "  myMethod(param2) {\n")
    (insert "    let localVar = param2;\n")
    (insert "    return localVar;\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 148     ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "MyClass" 7 variables) ; position of "MyClass"
                               variables)
                  :children (list
                             ;; Build expected constructor method scope
                             (list :type "function"
                                   :start 19 ; start of constructor
                                   :end 69   ; end of constructor
                                   ;; Constructor parameters
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "param1" 31 variables) ; position of "param1"
                                                variables)
                                   :children '())
                             ;; Build expected myMethod scope
                             (list :type "function"
                                   :start 75 ; start of myMethod
                                   :end 145  ; end of myMethod
                                   ;; Method parameters and local variables
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "param2" 84 variables)    ; position of "param2"
                                                (puthash "localVar" 102 variables) ; position of "localVar"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-for-loop-var-scope ()
  "Test that var declarations in for loop are hoisted to function scope."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  for (var i = 0; i < 10; i++) {\n")
    (insert "    var innerVar = i;\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 84      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 83   ; end of function
                                   ;; var declarations should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "i" 34 variables)        ; position of "i"
                                                (puthash "innerVar" 64 variables) ; position of "innerVar"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-for-loop-let-scope ()
  "Test that let declarations in for loop create a new for scope."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  for (let i = 0; i < 10; i++) {\n")
    (insert "    let innerVar = i;\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 84      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 83   ; end of function
                                   ;; Function scope should be empty
                                   :variables (make-hash-table :test 'equal)
                                   :children (list
                                              ;; Build expected for scope
                                              (list :type "for"
                                                    :start 25 ; start of for statement
                                                    :end 81   ; end of for statement
                                                    ;; let declarations should be in for scope
                                                    :variables (let ((variables (make-hash-table :test 'equal)))
                                                                 (puthash "i" 34 variables)        ; position of "i"
                                                                 variables)
                                                    :children (list
                                                               ;; Build expected block scope
                                                               (list :type "block"
                                                                     :start 54 ; start of block
                                                                     :end 81   ; end of block
                                                                     ;; let declarations should be in for scope
                                                                     :variables (let ((variables (make-hash-table :test 'equal)))
                                                                                  (puthash "innerVar" 64 variables) ; position of "innerVar"
                                                                                  variables)
                                                                     :children '())))))))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-for-in-loop-var-scope ()
  "Test that var declarations in for-in loop are hoisted to function scope."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  for (var key in obj) {\n")
    (insert "    var value = obj[key];\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 80      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 79   ; end of function
                                   ;; var declarations should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "key" 34 variables)   ; position of "key"
                                                (puthash "value" 56 variables) ; position of "value"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-for-in-loop-let-scope ()
  "Test that let declarations in for-in loop create a new for scope."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  for (let key in obj) {\n")
    (insert "    let value = obj[key];\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 80      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 79   ; end of function
                                   ;; Function scope should be empty
                                   :variables (make-hash-table :test 'equal)
                                   :children (list
                                              ;; Build expected for scope
                                              (list :type "for"
                                                    :start 25 ; start of for-in statement
                                                    :end 77   ; end of for-in statement
                                                    ;; let declarations should be in for scope
                                                    :variables (let ((variables (make-hash-table :test 'equal)))
                                                                 (puthash "key" 34 variables)   ; position of "key"
                                                                 variables)
                                                    :children (list
                                                               ;; Build expected block scope
                                                               (list :type "block"
                                                                     :start 46 ; start of block
                                                                     :end 77   ; end of block
                                                                     ;; let declarations should be in for scope
                                                                     :variables (let ((variables (make-hash-table :test 'equal)))
                                                                                  (puthash "value" 56 variables) ; position of "value"
                                                                                  variables)
                                                                     :children '())))))))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-array-destructuring-var ()
  "Test that array destructuring in var declarations works correctly."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  var [a, b, ...rest] = [1, 2, 3, 4, 5];\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 66      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 65   ; end of function
                                   ;; var declarations should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "a" 30 variables)    ; position of "a"
                                                (puthash "b" 33 variables)    ; position of "b"
                                                (puthash "rest" 39 variables) ; position of "rest"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-object-destructuring-var ()
  "Test that object destructuring in var declarations works correctly."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  var {x, y: z, ...spread} = obj;\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 59      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 58   ; end of function
                                   ;; var declarations should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "x" 30 variables)      ; position of "x"
                                                (puthash "z" 36 variables)      ; position of "z"
                                                (puthash "spread" 42 variables) ; position of "spread"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-array-destructuring-let ()
  "Test that array destructuring in let declarations works correctly."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  {\n")
    (insert "    let [first, second] = arr;\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 64      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 63   ; end of function
                                   ;; Function scope should be empty
                                   :variables (make-hash-table :test 'equal)
                                   :children (list
                                              ;; Build expected block scope
                                              (list :type "block"
                                                    :start 25 ; start of block
                                                    :end 61   ; end of block
                                                    ;; let declarations should be in block scope
                                                    :variables (let ((variables (make-hash-table :test 'equal)))
                                                                 (puthash "first" 36 variables)  ; position of "first"
                                                                 (puthash "second" 43 variables) ; position of "second"
                                                                 variables)
                                                    :children '())))))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-function-param-destructuring ()
  "Test that destructuring in function parameters works correctly."
  (with-temp-buffer
    (insert "function testFunc({a, b: renamed}, [x, y]) {\n")
    (insert "  return a + renamed + x + y;\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 78      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 77   ; end of function
                                   ;; Function parameters should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "a" 20 variables)       ; position of "a"
                                                (puthash "renamed" 26 variables) ; position of "renamed"
                                                (puthash "x" 37 variables)       ; position of "x"
                                                (puthash "y" 40 variables)       ; position of "y"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-catch-param-destructuring ()
  "Test that destructuring in catch parameters works correctly."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  try {\n")
    (insert "    throw {code: 500, message: 'Error'};\n")
    (insert "  } catch ({code, message: msg}) {\n")
    (insert "    console.log(code, msg);\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 141     ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 140  ; end of function
                                   ;; Function scope should be empty
                                   :variables (make-hash-table :test 'equal)
                                   :children (list
                                              ;; Build expected catch block scope
                                              (list :type "block"
                                                    :start 76 ; start of catch clause
                                                    :end 138  ; end of catch clause
                                                    ;; Catch parameters should be in catch scope
                                                    :variables (let ((variables (make-hash-table :test 'equal)))
                                                                 (puthash "code" 84 variables) ; position of "code"
                                                                 (puthash "msg" 99 variables)  ; position of "msg"
                                                                 variables)
                                                    :children '())))))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-default-parameters ()
  "Test that default parameters work correctly."
  (with-temp-buffer
    (insert "function testFunc(a = 1, b = 2, c) {\n")
    (insert "  return a + b + c;\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 60      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 59   ; end of function
                                   ;; Function parameters should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "a" 19 variables) ; position of "a"
                                                (puthash "b" 26 variables) ; position of "b"
                                                (puthash "c" 33 variables) ; position of "c"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-rest-parameters ()
  "Test that rest parameters work correctly."
  (with-temp-buffer
    (insert "function testFunc(first, ...rest) {\n")
    (insert "  return first + rest.length;\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 69      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 68   ; end of function
                                   ;; Function parameters should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "first" 19 variables) ; position of "first"
                                                (puthash "rest" 29 variables)  ; position of "rest"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-nested-destructuring ()
  "Test that nested destructuring patterns work correctly."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  var {a: {b, c}, d: [e, f]} = obj;\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 61      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 60   ; end of function
                                   ;; var declarations should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "b" 34 variables) ; position of "b"
                                                (puthash "c" 37 variables) ; position of "c"
                                                (puthash "e" 45 variables) ; position of "e"
                                                (puthash "f" 48 variables) ; position of "f"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-destructuring-with-defaults ()
  "Test that destructuring with default values works correctly."
  (with-temp-buffer
    (insert "function testFunc({a = 1, b: renamed = 2} = {}) {\n")
    (insert "  return a + renamed;\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 75      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 74   ; end of function
                                   ;; Function parameters should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "a" 20 variables)       ; position of "a"
                                                (puthash "renamed" 30 variables) ; position of "renamed"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-default-import ()
  "Test that default imports are added to scope correctly."
  (with-temp-buffer
    (insert "import React from 'react';\n")
    (insert "import { useState } from 'react';\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 62      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "React" 8 variables)     ; position of "React"
                               (puthash "useState" 37 variables) ; position of "useState"
                               variables)
                  :children '())))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-named-imports ()
  "Test that named imports are added to scope correctly."
  (with-temp-buffer
    (insert "import { Component, Fragment } from 'react';\n")
    (insert "import { render as renderDOM } from 'react-dom';\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 95      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "Component" 10 variables) ; position of "Component"
                               (puthash "Fragment" 21 variables)  ; position of "Fragment"
                               (puthash "renderDOM" 65 variables) ; position of "renderDOM"
                               variables)
                  :children '())))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-namespace-import ()
  "Test that namespace imports are added to scope correctly."
  (with-temp-buffer
    (insert "import * as React from 'react';\n")
    (insert "import * as utils from './utils';\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 67      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "React" 13 variables) ; position of "React"
                               (puthash "utils" 45 variables) ; position of "utils"
                               variables)
                  :children '())))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-mixed-imports ()
  "Test that mixed import types work correctly together."
  (with-temp-buffer
    (insert "import React, { useState, useEffect as useEff } from 'react';\n")
    (insert "import * as lodash from 'lodash';\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 97      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "React" 8 variables)    ; position of "React"
                               (puthash "useState" 17 variables) ; position of "useState"
                               (puthash "useEff" 40 variables)  ; position of "useEff"
                               (puthash "lodash" 75 variables)  ; position of "lodash"
                               variables)
                  :children '())))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-export-function-declaration ()
  "Test that exported function declarations are added to scope."
  (with-temp-buffer
    (insert "export function myFunction(param) {\n")
    (insert "  return param * 2;\n")
    (insert "}\n")
    (insert "\n")
    (insert "export default function defaultFunc() {\n")
    (insert "  return 'default';\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 122     ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "myFunction" 17 variables)   ; position of "myFunction"
                               (puthash "defaultFunc" 84 variables) ; position of "defaultFunc"
                               variables)
                  :children (list
                             ;; Build expected myFunction scope
                             (list :type "function"
                                   :start 8  ; start of myFunction
                                   :end 58   ; end of myFunction
                                   ;; Function parameters
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "param" 28 variables) ; position of "param"
                                                variables)
                                   :children '())
                             ;; Build expected defaultFunc scope
                             (list :type "function"
                                   :start 75 ; start of defaultFunc
                                   :end 121  ; end of defaultFunc
                                   ;; Function has no parameters
                                   :variables (make-hash-table :test 'equal)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-export-variable-declaration ()
  "Test that exported variable declarations are added to scope."
  (with-temp-buffer
    (insert "export var exportedVar = 'hello';\n")
    (insert "export let exportedLet = 42;\n")
    (insert "export const exportedConst = true;\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 99      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "exportedVar" 12 variables)   ; position of "exportedVar"
                               (puthash "exportedLet" 46 variables)   ; position of "exportedLet"
                               (puthash "exportedConst" 77 variables) ; position of "exportedConst"
                               variables)
                  :children '())))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-export-class-declaration ()
  "Test that exported class declarations are added to scope."
  (with-temp-buffer
    (insert "export class MyClass {\n")
    (insert "  constructor() {}\n")
    (insert "}\n")
    (insert "\n")
    (insert "export default class DefaultClass {\n")
    (insert "  method() { return 'test'; }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 114     ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "MyClass" 14 variables)      ; position of "MyClass"
                               (puthash "DefaultClass" 67 variables) ; position of "DefaultClass"
                               variables)
                  :children (list
                             ;; Build expected constructor scope
                             (list :type "function"
                                   :start 26 ; start of constructor
                                   :end 42   ; end of constructor
                                   ;; Constructor has no parameters
                                   :variables (make-hash-table :test 'equal)
                                   :children '())
                             ;; Build expected method scope
                             (list :type "function"
                                   :start 84 ; start of method
                                   :end 111  ; end of method
                                   ;; Method has no parameters
                                   :variables (make-hash-table :test 'equal)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-export-named-specifiers ()
  "Test that export specifiers reference existing declarations."
  (with-temp-buffer
    (insert "function helper() { return 'help'; }\n")
    (insert "const value = 42;\n")
    (insert "export { helper, value as exportedValue };\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 99      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "helper" 10 variables) ; position of "helper"
                               (puthash "value" 44 variables)  ; position of "value"
                               variables)
                  :children (list
                             ;; Build expected helper function scope
                             (list :type "function"
                                   :start 1  ; start of helper function
                                   :end 37   ; end of helper function
                                   ;; Function has no parameters
                                   :variables (make-hash-table :test 'equal)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-arrow-function-scope ()
  "Test that arrow functions create proper scopes with parameters."
  (with-temp-buffer
    (insert "const myArrow = (param1, param2) => {\n")
    (insert "  let localVar = param1 + param2;\n")
    (insert "  return localVar;\n")
    (insert "};\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 95      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "myArrow" 7 variables) ; position of "myArrow"
                               variables)
                  :children (list
                             ;; Build expected arrow function scope
                             (list :type "function"
                                   :start 17   ; start of arrow function
                                   :end 93     ; end of arrow function
                                   ;; Arrow function parameters and local variables
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "param1" 18 variables)   ; position of "param1"
                                                (puthash "param2" 26 variables)   ; position of "param2"
                                                (puthash "localVar" 45 variables) ; position of "localVar"
                                                variables)
                                   :children '()
                                   ;; Mark as arrow function
                                   :is-arrow t)))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-duplicate-var-declarations ()
  "Test that when a var is declared twice in the same scope, only the first declaration counts."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  var x = 1;\n")
    (insert "  var x = 2;\n")
    (insert "  return x;\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 63      ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 62   ; end of function
                                   ;; Only first declaration should be recorded
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "x" 29 variables) ; position of first "x"
                                                variables)
                                   :children '())))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-variable-shadowing ()
  "Test that the same variable name can exist in outer and inner scopes (shadowing)."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  var x = 'outer';\n")
    (insert "  {\n")
    (insert "    let x = 'inner';\n")
    (insert "    console.log(x);\n")
    (insert "  }\n")
    (insert "  return x;\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node))
           (expected-scope
            ;; Build expected global scope
            (list :type "program"
                  :start 1     ; start of buffer
                  :end 105     ; end of buffer
                  ;; Build expected global variables hash table
                  :variables (let ((variables (make-hash-table :test 'equal)))
                               (puthash "testFunc" 10 variables) ; position of "testFunc"
                               variables)
                  :children (list
                             ;; Build expected function scope
                             (list :type "function"
                                   :start 1  ; start of function
                                   :end 104  ; end of function
                                   ;; Outer 'x' should be in function scope
                                   :variables (let ((variables (make-hash-table :test 'equal)))
                                                (puthash "x" 29 variables) ; position of outer "x"
                                                variables)
                                   :children (list
                                              ;; Build expected block scope
                                              (list :type "block"
                                                    :start 44 ; start of block
                                                    :end 90   ; end of block
                                                    ;; Inner 'x' should be in block scope
                                                    :variables (let ((variables (make-hash-table :test 'equal)))
                                                                 (puthash "x" 54 variables) ; position of inner "x"
                                                                 variables)
                                                    :children '())))))))

      ;; Assert that the built scope matches the expected structure
      (should (js-ts-defs--deep-equal scope expected-scope)))))

(ert-deftest js-ts-defs-test-find-definition ()
  "Test js-ts-defs-find-definition function directly."
  (with-temp-buffer
    (insert "var globalVar = 1;\n")
    (insert "function myFunc(param) {\n")
    (insert "  let localVar = param + globalVar;\n")
    (insert "  {\n")
    (insert "    const blockVar = localVar * 2;\n")
    (insert "    return blockVar;\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node)))

      ;; Test 1: Find global variable from global scope
      (let ((global-pos 5)
            (result (js-ts-defs-find-definition scope "globalVar" 1)))
        (should (= result global-pos)))

      ;; Test 2: Find function from global scope
      (let ((func-pos 29)
            (result (js-ts-defs-find-definition scope "myFunc" 1)))
        (should (= result func-pos)))

      ;; Test 3: Find parameter from within function
      (let* ((param-pos 36)
             (usage-pos 62)
             (result (js-ts-defs-find-definition scope "param" usage-pos)))
        (should (= result param-pos)))

      ;; Test 4: Find local variable from within function
      (let* ((local-pos 51)
             (usage-pos 106)
             (result (js-ts-defs-find-definition scope "localVar" usage-pos)))
        (should (= result local-pos)))

      ;; Test 5: Find block-scoped variable from within block
      (let* ((block-pos 95)
             (usage-pos 131)
             (result (js-ts-defs-find-definition scope "blockVar" usage-pos)))
        (should (= result block-pos)))

      ;; Test 6: Find global variable from within nested scope
      (let* ((global-pos 5)
             (usage-pos 70)
             (result (js-ts-defs-find-definition scope "globalVar" usage-pos)))
        (should (= result global-pos)))

      ;; Test 7: Variable not found should return nil
      (let ((result (js-ts-defs-find-definition scope "nonExistent" 1)))
        (should (null result)))

      ;; Test 8: Block variable not accessible from outside block
      (let* ((outside-pos 144)
             (result (js-ts-defs-find-definition scope "blockVar" outside-pos)))
        (should (null result))))))

(ert-deftest js-ts-defs-test-find-definition-with-shadowing ()
  "Test that js-ts-defs-find-definition finds the correct variable depending on lookup context with shadowing."
  (with-temp-buffer
    (insert "function testFunc() {\n")
    (insert "  var x = 'outer';\n")
    (insert "  {\n")
    (insert "    let x = 'inner';\n")
    (insert "    console.log(x); // should find inner x\n")
    (insert "  }\n")
    (insert "  return x; // should find outer x\n")
    (insert "}\n")

    ;; Enable js-ts-mode to get tree-sitter parser
    (js-ts-mode)

    ;; Build the scope
    (let* ((root-node (treesit-buffer-root-node))
           (scope (js-ts-defs-build-scope root-node)))

      ;; Test 1: Find inner 'x' from within the block scope
      (let* ((inner-usage-pos 83) ; Position on 'x' in console.log(x)
             (inner-def-pos 54)   ; Position of inner 'x' declaration
             (result (js-ts-defs-find-definition scope "x" inner-usage-pos)))
        (should (= result inner-def-pos)))

      ;; Test 2: Find outer 'x' from within the function scope (after block)
      (goto-char (point-min))
      (let* ((outer-usage-pos 123) ; Position on 'x' in return x
             (outer-def-pos 29)    ; Position of outer 'x' declaration
             (result (js-ts-defs-find-definition scope "x" outer-usage-pos)))
        (should (= result outer-def-pos))))))

(ert-deftest js-ts-defs-test-jump-to-definition ()
  "Test jumping to variable and function definitions."
  (with-temp-buffer
    (insert "function greet(name) {\n")
    (insert "  let message = 'Hello, ' + name;\n")
    (insert "  return message;\n")
    (insert "}\n")
    (insert "\n")
    (insert "let result = greet('World');\n")

    ;; Enable js-ts-mode
    (js-ts-mode)

    ;; Test 1: Jump from 'message' usage to its definition
    (goto-char (point-min))
    (search-forward "return message")
    (backward-word)                     ; Move to start of 'message'
    (let ((usage-pos (point)))
      (js-ts-defs-jump-to-definition)
      (should (< (point) usage-pos))    ; Should jump backward to definition
      (should (looking-at "message")))

    ;; Test 2: Jump from function call to function definition
    (goto-char (point-min))
    (search-forward "greet('World')")
    (backward-word 2)                   ; Move to start of 'greet'
    (let ((call-pos (point)))
      (js-ts-defs-jump-to-definition)
      (should (< (point) call-pos))     ; Should jump backward to definition
      (should (looking-at "greet")))

    ;; Test 3: Jump from parameter usage to parameter definition
    (goto-char (point-min))
    (search-forward "'Hello, ' + name")
    (backward-word)                     ; Move to start of 'name'
    (let ((usage-pos (point)))
      (js-ts-defs-jump-to-definition)
      (should (< (point) usage-pos))    ; Should jump backward to parameter
      (should (looking-at "name")))))

(ert-deftest js-ts-defs-test-no-tree-sitter-parser ()
  "Test error when no tree-sitter parser is available."
  (with-temp-buffer
    (insert "let x = 42;")
    ;; Don't enable js-ts-mode, so no parser will be available
    (let ((err (should-error (js-ts-defs-jump-to-definition)
                             :type 'user-error)))
      (should (string= (cadr err) "No tree-sitter parser available")))))

(ert-deftest js-ts-defs-test-no-identifier-at-point ()
  "Test error when point is not on an identifier."
  (with-temp-buffer
    (insert "let x = 42;")
    (js-ts-mode)
    ;; Position point on the '=' character
    (goto-char (point-min))
    (search-forward "=")
    (backward-char)
    (let ((err (should-error (js-ts-defs-jump-to-definition)
                             :type 'user-error)))
      (should (string= (cadr err) "No identifier at point")))))

(ert-deftest js-ts-defs-test-definition-not-found ()
  "Test error when definition cannot be found for an identifier."
  (with-temp-buffer
    (insert "console.log(notDefined);")
    (js-ts-mode)
    ;; Position point on 'notDefined'
    (goto-char (point-min))
    (search-forward "notDefined")
    (backward-word)
    (let ((err (should-error (js-ts-defs-jump-to-definition)
                             :type 'user-error)))
      (should (string= (cadr err)
                       (format-message "Definition not found for `notDefined'"))))))

(ert-deftest js-ts-defs-test-arguments-dynamic-scope ()
  "Test error for 'arguments' identifier in non-arrow function."
  (with-temp-buffer
    (insert "function test() {\n")
    (insert "  console.log(arguments);\n")
    (insert "}")
    (js-ts-mode)
    ;; Position point on 'arguments'
    (goto-char (point-min))
    (search-forward "arguments")
    (backward-word)
    (let ((err (should-error (js-ts-defs-jump-to-definition)
                             :type 'user-error)))
      (should (string= (cadr err)
                       (format-message "`arguments' has dynamic scope"))))))

(ert-deftest js-ts-defs-test-arguments-in-arrow-function ()
  "Test that 'arguments' in arrow function gives definition not found error."
  (with-temp-buffer
    (insert "const test = () => {\n")
    (insert "  console.log(arguments);\n")
    (insert "};")
    (js-ts-mode)
    ;; Position point on 'arguments'
    (goto-char (point-min))
    (search-forward "arguments")
    (backward-word)
    (let ((err (should-error (js-ts-defs-jump-to-definition)
                             :type 'user-error)))
      (should (string= (cadr err)
                       (format-message "Definition not found for `arguments'"))))))

(ert-deftest js-ts-defs-test-mark-ring-with-set-mark-command ()
  "Test that jumping to definition pushes mark so we can pop back with set-mark-command."
  (with-temp-buffer
    (insert "function greet(name) {\n")
    (insert "  let message = 'Hello, ' + name;\n")
    (insert "  return message;\n")
    (insert "}\n")

    ;; Enable js-ts-mode
    (js-ts-mode)

    ;; Position point on the usage of 'message' at the return statement
    (goto-char (point-min))
    (search-forward "return message")
    (backward-word)
    (let ((original-pos (point)))
      ;; Jump to the definition
      (js-ts-defs-jump-to-definition)
      (let ((definition-pos (point)))
        ;; Verify we moved to the definition
        (should (< definition-pos original-pos))
        (should (looking-at "message"))

        ;; Pop the mark using set-mark-command with prefix arg (simulating C-u C-SPC)
        (set-mark-command t)

        ;; Verify we're back at the original position
        (should (= (point) original-pos))))))

(ert-deftest js-ts-defs-test-mark-ring-with-cua-set-mark ()
  "Test that jumping to definition pushes mark so we can pop back with cua-set-mark."
  (with-temp-buffer
    (insert "function greet(name) {\n")
    (insert "  let message = 'Hello, ' + name;\n")
    (insert "  return message;\n")
    (insert "}\n")

    ;; Enable js-ts-mode
    (js-ts-mode)

    ;; Position point on the usage of 'message' at the return statement
    (goto-char (point-min))
    (search-forward "return message")
    (backward-word)
    (let ((original-pos (point)))
      ;; Jump to the definition
      (js-ts-defs-jump-to-definition)
      (let ((definition-pos (point)))
        ;; Verify we moved to the definition
        (should (< definition-pos original-pos))
        (should (looking-at "message"))

        ;; Pop the mark using cua-set-mark with prefix arg (simulating C-u C-SPC in CUA mode)
        (cua-set-mark t)

        ;; Verify we're back at the original position
        (should (= (point) original-pos))))))

(ert-deftest js-ts-defs-test-mark-ring-with-active-region ()
  "Test that jumping with an active region doesn't push a mark."
  (with-temp-buffer
    (insert "function greet(name) {\n")
    (insert "  let message = 'Hello, ' + name;\n")
    (insert "  return message;\n")
    (insert "}\n")

    ;; Enable js-ts-mode and transient-mark-mode
    (js-ts-mode)
    (transient-mark-mode 1)

    ;; Position point on the usage of 'message' at the return statement
    (goto-char (point-min))
    (search-forward "return message")
    (backward-word)

    ;; Activate the region
    (set-mark (point))       ; Set mark
    (activate-mark)          ; Explicitly activate it
    (forward-char 3)         ; Move point to create a region

    ;; Verify region is active
    (should (region-active-p))

    ;; Save the mark ring length before jumping
    (let ((mark-ring-length-before (length mark-ring)))
      ;; Jump to definition (should not push mark because region is active)
      (js-ts-defs-jump-to-definition)

      ;; Verify the mark ring length didn't change
      (should (= (length mark-ring) mark-ring-length-before)))))

(ert-deftest js-ts-defs-test-xref-marker-stack ()
  "Test that jumping to definition integrates with xref to allow popping back with M-,."
  (with-temp-buffer
    (insert "function greet(name) {\n")
    (insert "  let message = 'Hello, ' + name;\n")
    (insert "  return message;\n")
    (insert "}\n")

    ;; Enable js-ts-mode
    (js-ts-mode)

    ;; Position point on the usage of 'message' at the return statement
    (goto-char (point-min))
    (search-forward "return message")
    (backward-word)
    (let ((original-pos (point)))
      ;; Jump to the definition
      (js-ts-defs-jump-to-definition)
      (let ((definition-pos (point)))
        ;; Verify we moved to the definition
        (should (< definition-pos original-pos))
        (should (looking-at "message"))

        ;; Use xref-go-back (M-,) to go back
        (xref-go-back)

        ;; Verify we're back at the original position
        (should (= (point) original-pos))))))

(ert-deftest js-ts-defs-test-xref-marker-stack-multiple-jumps ()
  "Test that multiple jumps work correctly with xref stack (LIFO order)."
  (with-temp-buffer
    (insert "var globalVar = 42;\n")
    (insert "function outer() {\n")
    (insert "  let outerVar = globalVar;\n")
    (insert "  function inner() {\n")
    (insert "    return outerVar;\n")
    (insert "  }\n")
    (insert "}\n")

    ;; Enable js-ts-mode
    (js-ts-mode)

    ;; First jump: from 'outerVar' usage to its definition
    (goto-char (point-min))
    (search-forward "return outerVar")
    (backward-word)
    (let ((first-usage-pos (point)))
      (js-ts-defs-jump-to-definition)
      (let ((first-def-pos (point)))
        (should (< first-def-pos first-usage-pos))
        (should (looking-at "outerVar"))

        ;; Second jump: from 'globalVar' usage to its definition
        (goto-char (point-min))
        (search-forward "outerVar = globalVar")
        (backward-word)
        (let ((second-usage-pos (point)))
          (js-ts-defs-jump-to-definition)
          (let ((second-def-pos (point)))
            (should (< second-def-pos second-usage-pos))
            (should (looking-at "globalVar"))

            ;; Pop first marker (should go back to second usage)
            (xref-go-back)
            (should (= (point) second-usage-pos))

            ;; Pop second marker (should go back to first usage)
            (xref-go-back)
            (should (= (point) first-usage-pos))))))))

(ert-deftest js-ts-defs-test-xref-marker-stack-with-active-region ()
  "Test that xref allows jumping back even with active region."
  (with-temp-buffer
    (insert "function greet(name) {\n")
    (insert "  let message = 'Hello, ' + name;\n")
    (insert "  return message;\n")
    (insert "}\n")

    ;; Enable js-ts-mode and transient-mark-mode
    (js-ts-mode)
    (transient-mark-mode 1)

    ;; Position point on the usage of 'message' at the return statement
    (goto-char (point-min))
    (search-forward "return message")
    (backward-word)

    ;; Activate the region
    (set-mark (point))
    (activate-mark)
    (forward-char 3)

    ;; Verify region is active
    (should (region-active-p))

    (let ((original-pos (point)))
      ;; Jump to definition
      (js-ts-defs-jump-to-definition)

      ;; Even with active region, xref should allow jumping back
      (xref-go-back)

      ;; Verify we're back at the original position
      (should (= (point) original-pos)))))

(provide 'js-ts-defs-test)

;;; js-ts-defs-test.el ends here
