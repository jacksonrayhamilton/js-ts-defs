;;; js-ts-defs.el --- Find JavaScript variable definitions using tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jackson Ray Hamilton

;; Author: Jackson Ray Hamilton <jackson@jacksonrayhamilton.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, javascript, tree-sitter
;; URL: https://github.com/jacksonrayhamilton/js-ts-defs

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

;; Find JavaScript variable definitions using tree-sitter.
;;
;; This package provides `js-ts-defs-jump-to-definition' which jumps to the
;; definition of the JavaScript identifier at point.  It uses tree-sitter to
;; parse JavaScript code and build a scope structure to accurately resolve
;; variable definitions.
;;
;; These functions are designed to be used inside `js-ts-mode'.
;;
;; Prerequisites:
;;
;; 1. Install JavaScript tree-sitter grammar:
;;    (add-to-list
;;     'treesit-language-source-alist
;;     '(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1"))
;;    M-x treesit-install-language-grammar RET javascript RET
;;
;; 2. Enable `js-ts-mode' for JavaScript files:
;;    (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))
;;
;; Usage:
;;
;;   (add-hook 'js-ts-mode-hook
;;             (lambda ()
;;               (local-set-key (kbd "M-.") #'js-ts-defs-jump-to-definition)))
;;
;; This gives you:
;;   M-.   Jump to definition
;;   M-,   Jump back

;;; Code:

(require 'treesit)
(require 'xref)

;; Buffer-local variables for caching
(defvar-local js-ts-defs--cached-scope nil
  "Buffer-local cache for the scope structure.")

(defvar-local js-ts-defs--change-hook-setup nil
  "Buffer-local flag indicating if tree-sitter change hook is setup.")

;; Grammar compatibility detection
(defvar js-ts-defs--function-expression-node-type nil
  "Cached result of function expression node type detection.
Will be \"function_expression\" for newer grammars, \"function\" for older ones.")

(defun js-ts-defs--detect-function-expression-node-type ()
  "Detect the correct node type for function expressions.
Return \"function_expression\" for newer grammars (0.20.2+).
Return \"function\" for older ones.
Caches the result in `js-ts-defs--function-expression-node-type'."
  (unless js-ts-defs--function-expression-node-type
    (setq js-ts-defs--function-expression-node-type
          (condition-case nil
              (progn
                (treesit-query-capture 'javascript '((function_expression) @cap))
                "function_expression")
            (error "function"))))
  js-ts-defs--function-expression-node-type)

(defun js-ts-defs-build-scope (root-node)
  "Extract JavaScript definitions from ROOT-NODE using tree-sitter.
ROOT-NODE should be a tree-sitter root node.
Returns a nested scope structure with variable definitions."
  (let ((scope (js-ts-defs--build-scope "program"
                                        (treesit-node-start root-node)
                                        (treesit-node-end root-node))))
    (js-ts-defs--process-node root-node scope scope)
    scope))

(defun js-ts-defs--build-scope (scope-type start-pos end-pos &optional is-arrow)
  "Build a scope structure of SCOPE-TYPE with START-POS and END-POS.
SCOPE-TYPE is the type of scope to create.
START-POS is the starting position of the scope.
END-POS is the ending position of the scope.
If IS-ARROW is non-nil, marks this function scope as an arrow function."
  (let ((scope (list :type scope-type
                     :start start-pos
                     :end end-pos
                     :variables (make-hash-table :test 'equal)
                     :children '())))
    (when is-arrow
      (setq scope (append scope (list :is-arrow t))))
    scope))

(defun js-ts-defs--process-node (node function-scope block-scope)
  "Process NODE and add definitions to appropriate scopes.
FUNCTION-SCOPE is the enclosing function/program scope for var declarations.
BLOCK-SCOPE is the current block scope for lexical declarations."
  (let ((node-type (treesit-node-type node)))
    (cond
     ;; Function declarations create new scopes
     ((or (string= node-type "function_declaration")
          (string= node-type (js-ts-defs--detect-function-expression-node-type))
          (string= node-type "arrow_function")
          (string= node-type "method_definition"))
      (js-ts-defs--process-function node function-scope block-scope))

     ;; Class declarations add name to block scope but don't create function scope
     ((string= node-type "class_declaration")
      (js-ts-defs--process-class-declaration node function-scope block-scope))

     ;; Variable declarations go to function scope
     ((string= node-type "variable_declaration")
      (js-ts-defs--process-variable-declaration node function-scope block-scope))

     ;; Lexical declarations go to block scope
     ((string= node-type "lexical_declaration")
      (js-ts-defs--process-lexical-declaration node function-scope block-scope))

     ;; Import statements add variables to block scope
     ((string= node-type "import_statement")
      (js-ts-defs--process-import-statement node function-scope block-scope))

     ;; Statement blocks that may need block scopes
     ((string= node-type "statement_block")
      (js-ts-defs--process-statement-block node function-scope block-scope))

     ;; Catch clauses that define error variables
     ((string= node-type "catch_clause")
      (js-ts-defs--process-catch-clause node function-scope block-scope))

     ;; For statements that may need block scopes for lexical declarations
     ((string= node-type "for_statement")
      (js-ts-defs--process-for-statement node function-scope block-scope))

     ;; For in statements that may need block scopes for lexical declarations
     ((string= node-type "for_in_statement")
      (js-ts-defs--process-for-in-statement node function-scope block-scope))

     ;; For other nodes, just process children
     (t
      (js-ts-defs--process-children node function-scope block-scope)))))

(defun js-ts-defs--process-function (node _parent-function-scope parent-block-scope)
  "Process a function NODE, creating a new child scope.
NODE is the function node to process.
_PARENT-FUNCTION-SCOPE is the parent function scope (unused).
PARENT-BLOCK-SCOPE is the parent block scope."
  (let* ((node-type (treesit-node-type node))
         (is-arrow (string= node-type "arrow_function"))
         (function-scope (js-ts-defs--build-scope "function"
                                                  (treesit-node-start node)
                                                  (treesit-node-end node)
                                                  is-arrow))
         (parameters (js-ts-defs--get-function-parameters node)))

    ;; Handle function names based on node type
    (let ((name-node (treesit-node-child-by-field-name node "name")))
      (when (and name-node (string= (treesit-node-type name-node) "identifier"))
        (let ((name (substring-no-properties (treesit-node-text name-node)))
              (pos (treesit-node-start name-node)))
          (cond
           ;; For function_declaration, add name to parent block scope
           ((string= node-type "function_declaration")
            (js-ts-defs--add-variable parent-block-scope name pos))
           ;; For function_expression, add name to function's own scope
           ((string= node-type (js-ts-defs--detect-function-expression-node-type))
            (js-ts-defs--add-variable function-scope name pos))))))

    ;; Add parameters to the function scope
    (dolist (param parameters)
      (js-ts-defs--add-variable function-scope (car param) (cdr param)))

    ;; Process the function body in the new scope (function scope serves as both function and block scope)
    (let ((body (js-ts-defs--get-function-body node)))
      (when body
        (if (string= (treesit-node-type body) "statement_block")
            ;; If body is a statement block, process its children directly
            (let ((children (treesit-node-children body)))
              (dolist (child children)
                (js-ts-defs--process-node child function-scope function-scope)))
          ;; Otherwise process the body node normally
          (js-ts-defs--process-node body function-scope function-scope))))

    ;; Add the function scope as a child of the parent block scope
    (setf (plist-get parent-block-scope :children)
          (append (plist-get parent-block-scope :children) (list function-scope)))))

(defun js-ts-defs--process-variable-declaration (node function-scope block-scope)
  "Process a variable declaration NODE (var) and add variables to FUNCTION-SCOPE.
NODE is the variable declaration node to process.
FUNCTION-SCOPE is the function scope to add variables to.
BLOCK-SCOPE is the current block scope."
  (let ((declarators (treesit-node-children node)))
    (dolist (child declarators)
      (when (string= (treesit-node-type child) "variable_declarator")
        (let ((pattern (treesit-node-child-by-field-name child "name")))
          (when pattern
            (let ((identifiers (js-ts-defs--extract-identifiers-from-pattern pattern)))
              (dolist (identifier identifiers)
                (js-ts-defs--add-variable function-scope (car identifier) (cdr identifier))))))
        ;; Process the value part of the declaration if it exists
        (let ((value (treesit-node-child-by-field-name child "value")))
          (when value
            (js-ts-defs--process-node value function-scope block-scope)))))))

(defun js-ts-defs--process-lexical-declaration (node function-scope block-scope)
  "Process a lexical declaration NODE (let/const) and add variables to BLOCK-SCOPE.
NODE is the lexical declaration node to process.
FUNCTION-SCOPE is the current function scope.
BLOCK-SCOPE is the block scope to add variables to."
  (let ((declarators (treesit-node-children node)))
    (dolist (child declarators)
      (when (string= (treesit-node-type child) "variable_declarator")
        (let ((pattern (treesit-node-child-by-field-name child "name")))
          (when pattern
            (let ((identifiers (js-ts-defs--extract-identifiers-from-pattern pattern)))
              (dolist (identifier identifiers)
                (js-ts-defs--add-variable block-scope (car identifier) (cdr identifier))))))
        ;; Process the value part of the declaration if it exists
        (let ((value (treesit-node-child-by-field-name child "value")))
          (when value
            (js-ts-defs--process-node value function-scope block-scope)))))))

(defun js-ts-defs--get-function-parameters (node)
  "Extract parameter names and positions from function NODE."
  (let ((params '())
        (formal-params (treesit-node-child-by-field-name node "parameters")))
    (when formal-params
      (let ((param-nodes (treesit-node-children formal-params)))
        (dolist (param-node param-nodes)
          (setq params (append params (js-ts-defs--extract-identifiers-from-pattern param-node))))))
    params))

(defun js-ts-defs--get-function-body (node)
  "Get the body node of a function NODE."
  (treesit-node-child-by-field-name node "body"))

(defun js-ts-defs--add-variable (scope name position)
  "Add a variable NAME at POSITION to SCOPE if not already defined."
  (let ((variables (plist-get scope :variables)))
    (unless (gethash name variables)
      (puthash name position variables))))

(defun js-ts-defs--process-statement-block (node function-scope block-scope)
  "Process a statement block NODE, creating a block scope if needed.
NODE is the statement block node to process.
FUNCTION-SCOPE is the current function scope.
BLOCK-SCOPE is the current block scope."
  (let ((children (treesit-node-children node))
        (has-lexical-declaration nil))

    ;; Check if this block contains any lexical declarations
    (dolist (child children)
      (when (or (string= (treesit-node-type child) "lexical_declaration")
                (string= (treesit-node-type child) "function_declaration"))
        (setq has-lexical-declaration t)))

    (if has-lexical-declaration
        ;; Create a block scope and process children in it
        (let ((new-block-scope (js-ts-defs--build-scope "block"
                                                        (treesit-node-start node)
                                                        (treesit-node-end node))))
          (dolist (child children)
            (js-ts-defs--process-node child function-scope new-block-scope))
          (setf (plist-get block-scope :children)
                (append (plist-get block-scope :children) (list new-block-scope))))
      ;; No lexical declarations, just process children in current scopes
      (dolist (child children)
        (js-ts-defs--process-node child function-scope block-scope)))))

(defun js-ts-defs--process-catch-clause (node _function-scope block-scope)
  "Process a catch clause NODE, creating a new block scope.
NODE is the catch clause node to process.
_FUNCTION-SCOPE is the current function scope (unused).
BLOCK-SCOPE is the current block scope."
  (let ((parameter (treesit-node-child-by-field-name node "parameter"))
        (body (treesit-node-child-by-field-name node "body"))
        (catch-scope (js-ts-defs--build-scope "block"
                                              (treesit-node-start node)
                                              (treesit-node-end node))))

    ;; Add the error parameter to the catch scope if it exists
    (when parameter
      (let ((identifiers (js-ts-defs--extract-identifiers-from-pattern parameter)))
        (dolist (identifier identifiers)
          (js-ts-defs--add-variable catch-scope (car identifier) (cdr identifier)))))

    ;; Process the catch body in the catch scope
    (when body
      (if (string= (treesit-node-type body) "statement_block")
          ;; If body is a statement block, process its children directly
          (let ((children (treesit-node-children body)))
            (dolist (child children)
              (js-ts-defs--process-node child catch-scope catch-scope)))
        ;; Otherwise process the body node normally
        (js-ts-defs--process-node body catch-scope catch-scope)))

    (setf (plist-get block-scope :children)
          (append (plist-get block-scope :children) (list catch-scope)))))

(defun js-ts-defs--process-for-statement (node function-scope block-scope)
  "Process a for statement NODE, creating a block scope if needed.
NODE is the for statement node to process.
FUNCTION-SCOPE is the current function scope.
BLOCK-SCOPE is the current block scope."
  (let ((initializer (treesit-node-child-by-field-name node "initializer"))
        (condition (treesit-node-child-by-field-name node "condition"))
        (update (treesit-node-child-by-field-name node "update"))
        (body (treesit-node-child-by-field-name node "body"))
        (has-lexical-declaration nil))

    ;; Check if the initializer contains a lexical declaration
    (when (and initializer (string= (treesit-node-type initializer) "lexical_declaration"))
      (setq has-lexical-declaration t))

    (if has-lexical-declaration
        ;; Create a scope for the parenthesized part of the for loop
        (let ((for-scope (js-ts-defs--build-scope "for"
                                                  (treesit-node-start node)
                                                  (treesit-node-end node))))
          ;; Process the lexical declaration in the for scope
          (js-ts-defs--process-node initializer function-scope for-scope)

          ;; Process condition and update in the for scope
          (when condition
            (js-ts-defs--process-node condition function-scope for-scope))
          (when update
            (js-ts-defs--process-node update function-scope for-scope))

          ;; Process body in the for scope
          (when body
            (js-ts-defs--process-node body function-scope for-scope))

          (setf (plist-get block-scope :children)
                (append (plist-get block-scope :children) (list for-scope))))
      ;; No lexical declarations, just process children in current scopes
      (progn
        (when initializer
          (js-ts-defs--process-node initializer function-scope block-scope))
        (when condition
          (js-ts-defs--process-node condition function-scope block-scope))
        (when update
          (js-ts-defs--process-node update function-scope block-scope))
        (when body
          (js-ts-defs--process-node body function-scope block-scope))))))

(defun js-ts-defs--process-for-in-statement (node function-scope block-scope)
  "Process a for in statement NODE, creating a block scope if needed.
NODE is the for in statement node to process.
FUNCTION-SCOPE is the current function scope.
BLOCK-SCOPE is the current block scope."
  (let ((kind (treesit-node-child-by-field-name node "kind"))
        (left (treesit-node-child-by-field-name node "left"))
        (right (treesit-node-child-by-field-name node "right"))
        (body (treesit-node-child-by-field-name node "body"))
        (has-lexical-declaration nil))

    ;; Check if kind is const/let and left is an identifier
    (when (and kind left
               (string= (treesit-node-type left) "identifier")
               (let ((kind-text (substring-no-properties (treesit-node-text kind))))
                 (or (string= kind-text "const") (string= kind-text "let"))))
      (setq has-lexical-declaration t))

    (if has-lexical-declaration
        ;; Create a scope for the parenthesized part of the for in loop with a
        ;; lexical declaration
        (let ((for-scope (js-ts-defs--build-scope "for"
                                                  (treesit-node-start node)
                                                  (treesit-node-end node))))
          ;; Add the lexical variable to the for scope
          (let ((name (substring-no-properties (treesit-node-text left)))
                (pos (treesit-node-start left)))
            (js-ts-defs--add-variable for-scope name pos))

          ;; Process right side in the for scope
          (when right
            (js-ts-defs--process-node right function-scope for-scope))

          ;; Process body in the for scope
          (when body
            (js-ts-defs--process-node body function-scope for-scope))

          (setf (plist-get block-scope :children)
                (append (plist-get block-scope :children) (list for-scope))))
      ;; Handle var declaration or process children in current scopes
      (progn
        ;; If kind is var and left is identifier, add to function scope
        (when (and kind left
                   (string= (treesit-node-type left) "identifier")
                   (string= (substring-no-properties (treesit-node-text kind)) "var"))
          (let ((name (substring-no-properties (treesit-node-text left)))
                (pos (treesit-node-start left)))
            (js-ts-defs--add-variable function-scope name pos)))

        ;; Process other parts
        (when right
          (js-ts-defs--process-node right function-scope block-scope))
        (when body
          (js-ts-defs--process-node body function-scope block-scope))))))

(defun js-ts-defs--process-class-declaration (node function-scope block-scope)
  "Process a class declaration NODE, adding the class name to block scope.
NODE is the class declaration node to process.
FUNCTION-SCOPE is the current function scope.
BLOCK-SCOPE is the block scope to add the class name to."
  (let ((name-node (treesit-node-child-by-field-name node "name")))
    ;; Add class name to block scope
    (when (and name-node (string= (treesit-node-type name-node) "identifier"))
      (let ((name (substring-no-properties (treesit-node-text name-node)))
            (pos (treesit-node-start name-node)))
        (js-ts-defs--add-variable block-scope name pos))))

  ;; Process class body without creating a function scope
  (js-ts-defs--process-children node function-scope block-scope))

(defun js-ts-defs--process-import-statement (node function-scope block-scope)
  "Process an import statement NODE, adding imports to block scope.
NODE is the import statement node to process.
FUNCTION-SCOPE is the current function scope.
BLOCK-SCOPE is the block scope to add imports to."
  (let ((import-clause (treesit-node-child node 1)))
    (when import-clause
      (js-ts-defs--process-import-clause import-clause block-scope)))

  ;; Process other parts of the import statement
  (js-ts-defs--process-children node function-scope block-scope))

(defun js-ts-defs--process-import-clause (node block-scope)
  "Process an import clause NODE, adding imports to BLOCK-SCOPE.
NODE is the import clause node to process.
BLOCK-SCOPE is the block scope to add imports to."
  (let ((children (treesit-node-children node)))
    (dolist (child children)
      (let ((node-type (treesit-node-type child)))
        (cond
         ;; Default import: import foo from 'module'
         ((string= node-type "identifier")
          (let ((name (substring-no-properties (treesit-node-text child)))
                (pos (treesit-node-start child)))
            (js-ts-defs--add-variable block-scope name pos)))

         ;; Namespace import: import * as foo from 'module'
         ((string= node-type "namespace_import")
          (let* ((children (treesit-node-children child))
                 (identifier (car (last children))))
            (when (and identifier (string= (treesit-node-type identifier) "identifier"))
              (let ((name (substring-no-properties (treesit-node-text identifier)))
                    (pos (treesit-node-start identifier)))
                (js-ts-defs--add-variable block-scope name pos)))))

         ;; Named imports: import { foo, bar, baz as qux } from 'module'
         ((string= node-type "named_imports")
          (let ((grandchildren (treesit-node-children child)))
            (dolist (grandchild grandchildren)
              (when (string= (treesit-node-type grandchild) "import_specifier")
                (let* ((spec-children (treesit-node-children grandchild))
                       (name-node (car (last spec-children))))
                  (when (and name-node (string= (treesit-node-type name-node) "identifier"))
                    (let ((name (substring-no-properties (treesit-node-text name-node)))
                          (pos (treesit-node-start name-node)))
                      (js-ts-defs--add-variable block-scope name pos)))))))))))))

(defun js-ts-defs--extract-identifiers-from-pattern (node)
  "Extract identifier names and positions from pattern NODE."
  (let ((node-type (treesit-node-type node))
        (identifiers '()))
    (cond
     ;; Simple identifier
     ((string= node-type "identifier")
      (let ((name (substring-no-properties (treesit-node-text node)))
            (pos (treesit-node-start node)))
        (list (cons name pos))))

     ;; Rest pattern (...param)
     ((string= node-type "rest_pattern")
      (let ((pattern (treesit-node-child node 1)))
        (when pattern
          (js-ts-defs--extract-identifiers-from-pattern pattern))))

     ;; Object pattern ({a, b: c})
     ((or (string= node-type "object_pattern")
          (string= node-type "object_assignment_pattern"))
      (let ((children (treesit-node-children node)))
        (dolist (child children)
          (when (or (string= (treesit-node-type child) "pair_pattern")
                    (string= (treesit-node-type child) "shorthand_property_identifier_pattern")
                    (string= (treesit-node-type child) "object_assignment_pattern")
                    (string= (treesit-node-type child) "rest_pattern"))
            (cond
             ;; Shorthand property pattern {a}
             ((string= (treesit-node-type child) "shorthand_property_identifier_pattern")
              (let ((name (substring-no-properties (treesit-node-text child)))
                    (pos (treesit-node-start child)))
                (setq identifiers (append identifiers (list (cons name pos))))))
             ;; Property pair {a: b}
             ((string= (treesit-node-type child) "pair_pattern")
              (let ((value (treesit-node-child-by-field-name child "value")))
                (when value
                  (setq identifiers (append identifiers (js-ts-defs--extract-identifiers-from-pattern value))))))
             ;; Object assignment pattern {a = defaultValue}
             ((string= (treesit-node-type child) "object_assignment_pattern")
              (setq identifiers (append identifiers (js-ts-defs--extract-identifiers-from-pattern child))))
             ;; Rest pattern in object {...rest}
             ((string= (treesit-node-type child) "rest_pattern")
              (setq identifiers (append identifiers (js-ts-defs--extract-identifiers-from-pattern child)))))))
        identifiers))

     ;; Array pattern ([a, b])
     ((string= node-type "array_pattern")
      (let ((children (treesit-node-children node)))
        (dolist (child children)
          (when (not (string= (treesit-node-type child) ","))
            (setq identifiers (append identifiers (js-ts-defs--extract-identifiers-from-pattern child)))))
        identifiers))

     ;; Default parameter (param = value)
     ((string= node-type "assignment_pattern")
      (let ((left (treesit-node-child-by-field-name node "left")))
        (when left
          (js-ts-defs--extract-identifiers-from-pattern left))))

     ;; Unknown pattern type, return empty list
     (t '()))))

(defun js-ts-defs--process-children (node function-scope block-scope)
  "Process all children of NODE in the current scopes.
NODE is the parent node whose children to process.
FUNCTION-SCOPE is the current function scope.
BLOCK-SCOPE is the current block scope."
  (let ((children (treesit-node-children node)))
    (dolist (child children)
      (js-ts-defs--process-node child function-scope block-scope))))

(defun js-ts-defs-find-definition (scope identifier position)
  "Find definition of IDENTIFIER at POSITION within SCOPE.
Returns the position of the definition, or nil if not found.
Searches from innermost to outermost scope."
  (catch 'found
    ;; First check if we're inside any child scopes
    (dolist (child-scope (plist-get scope :children))
      (when (and (>= position (plist-get child-scope :start))
                 (< position (plist-get child-scope :end)))
        (let ((result (js-ts-defs-find-definition child-scope identifier position)))
          (when result
            (throw 'found result)))))

    ;; If not found in child scopes, check current scope
    (let ((variables (plist-get scope :variables)))
      (gethash identifier variables))))

(defun js-ts-defs--find-dynamic-function-scope (scope position)
  "Find if POSITION is within a non-arrow function scope in SCOPE.
SCOPE is the scope structure to search within.
POSITION is the buffer position to check.
Returns t if inside a non-arrow function scope, nil otherwise."
  (catch 'found
    ;; Check if we're inside any child scopes
    (dolist (child-scope (plist-get scope :children))
      (when (and (>= position (plist-get child-scope :start))
                 (< position (plist-get child-scope :end)))
        ;; If this child scope is a non-arrow function, we found one
        (when (and (string= (plist-get child-scope :type) "function")
                   (not (plist-get child-scope :is-arrow)))
          (throw 'found t))
        ;; Otherwise recurse into the child scope
        (let ((result (js-ts-defs--find-dynamic-function-scope child-scope position)))
          (when result
            (throw 'found result)))))

    ;; Check if current scope is a non-arrow function
    (when (and (string= (plist-get scope :type) "function")
               (not (plist-get scope :is-arrow)))
      t)))

(defun js-ts-defs--invalidate-cache (&rest _)
  "Invalidate the cached scope structure.
Argument _ is ignored (used for tree-sitter callback compatibility)."
  (setq js-ts-defs--cached-scope nil))

(defun js-ts-defs--setup-change-hook ()
  "Setup tree-sitter change hook to invalidate cache on syntax tree change."
  (unless js-ts-defs--change-hook-setup
    (when-let* ((parser (car (treesit-parser-list))))
      (treesit-parser-add-notifier parser #'js-ts-defs--invalidate-cache)
      (setq js-ts-defs--change-hook-setup t))))

(defun js-ts-defs--get-cached-scope ()
  "Get the cached scope structure, computing it if necessary."
  (unless js-ts-defs--cached-scope
    (when-let* ((parser (car (treesit-parser-list))))
      (let ((root-node (treesit-parser-root-node parser)))
        (setq js-ts-defs--cached-scope (js-ts-defs-build-scope root-node)))))
  js-ts-defs--cached-scope)

;;;###autoload
(defun js-ts-defs-jump-to-definition ()
  "Jump to the definition of the identifier at point."
  (interactive)
  (unless (treesit-parser-list)
    (user-error "No tree-sitter parser available"))

  ;; Setup change hook if not already done
  (js-ts-defs--setup-change-hook)

  (let* ((node (treesit-node-at (point)))
         (identifier-node nil)
         (identifier nil))

    ;; Find the identifier node at point
    (while (and node (not identifier-node))
      (when (string= (treesit-node-type node) "identifier")
        (setq identifier-node node))
      (setq node (treesit-node-parent node)))

    (unless identifier-node
      (user-error "No identifier at point"))

    (setq identifier (substring-no-properties (treesit-node-text identifier-node)))

    ;; Get the cached scope structure
    (let* ((scope (js-ts-defs--get-cached-scope))
           (definition-pos (js-ts-defs-find-definition scope identifier (point))))

      (if definition-pos
          (progn
            ;; Save the user's last position so it's easy to return with
            ;; `xref-go-back' (M-,), `set-mark-command' or `cua-set-mark'.
            (xref-push-marker-stack)
            (unless (region-active-p) (push-mark nil t))
            (goto-char definition-pos))
        ;; Special case for "arguments" - check if we're ultimately in a
        ;; non-arrow function scope
        (if (and (string= identifier "arguments")
                 (js-ts-defs--find-dynamic-function-scope scope (point)))
            (user-error "`arguments' has dynamic scope")
          (user-error "Definition not found for `%s'" identifier))))))

(provide 'js-ts-defs)

;;; js-ts-defs.el ends here
