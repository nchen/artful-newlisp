;; @module Mysql
;; @author Jeff Ober <jeffober@gmail.com>
;; @version 1.05 beta
;; @location http://static.artfulcode.net/newlisp/mysql.lsp
;; @package http://static.artfulcode.net/newlisp/mysql.qwerty
;; @description A new MySQL module to replace the distribution standard module (requires newlisp 10).
;; The Mysql module has been written from scratch utilizing some of the more
;; recent features of newLisp, such as FOOP and reference returns. One of its
;; major design goals was to simplify use as well as broaden the features of
;; the standard MySQL module, while at the same time allowing the creation of
;; new, anonymous instances at run-time.
;; 
;; The Mysql module differs from the distribution standard module in several
;; important ways. Most obviously, it uses FOOP wrappers for MySQL types. It
;; also requires clients to free results instances; in the standard module,
;; only the base MYSQL instance itself must be freed (using MySQL:close-db).
;; 
;; The significance of this is that it is much simpler to create multiple
;; connections (without having to duplicate the entire context at compile
;; time). Result sets are completely independent of each other, and several may
;; be maintained in any state at once. This also means that a spawned process
;; may be given its own Mysql instance to use without having to worry about
;; other processes' instances interfering. Using the standard module, the
;; entire context would need to be cloned at compile time and given a static
;; symbol reference (e.g., (new 'MySQL 'db)) in order to run multiple instances
;; or connections to a server.
;; 
;; Moreover, because this module uses unpack and MySQL C API accessor
;; functions, there is no need for the client to calculate member offsets in
;; MySQL compound types. So long as newLisp was compiled for the same target as
;; the libmysqlclient library (both are 32 bit or both are 64 bit), everything
;; should work out of the box. Additionally, MySQL errors are now checked in
;; the connect and query functions and re-thrown as interpreter errors. Instead
;; of checking for nil returns and a using MySQL:error to get the error
;; message, standard error  handling with the catch function may be used.
;; 
;; This module has been tested with MySQL version 5 and 5.1 and newLisp version
;; 10.0.1. It requires newLisp 10.0 or later.
;; 
;; <h3>Changelog</h3>
;; <b>1.05</b>
;; &bull; Mysql:query now checks if client mistakenly sent single, non-list, argument for format-args
;; 
;; <b>1.04</b>
;; &bull; fixed error in documentation example
;; &bull; changed Mysql:query to allow lists as format parameters
;; &bull; backward-incompatible change to Mysql:query parameter list
;; &bull; added Mysql:coerce-type as an independent function
;; 
;; <b>1.03</b>
;; &bull; fixed truncation bug when inserting binary data in Mysql:query
;; 
;; <b>1.02</b>
;; &bull; field types are now correctly distinguished when MySQL is compiled with 64-bit pointers
;; &bull; refactored MysqlResult:get-row
;; 
;; <b>1.01</b>
;; &bull; fixed invalid function in Mysql:tables, Mysql:fields, and Mysql:databases
;; 
;; <b>1.0</b>
;; &bull; initial release
;; 
;; <h3>Known bugs</h3>
;; &bull; None (at the moment); <i>please let me know if you find any!</i>
;; 
;; 
;; @example
;; &bull; Imperative usage
;; 
;; (setf db (Mysql)) ; initialize Mysql instance
;; (:connect db "localhost" "user" "secret" "my_database") ; connect to a server
;; (setf result (:query db "SELECT * FROM some_table")) ; evaluate a query
;; (setf rows (:fetch-all result)) ; generate a result
;; (:close-db db) ; free the database
;; 
;; &bull; Functional usage with the 'mysql context
;; 
;; (mysql:on-connect '("localhost" "user" "secret" "my_database")
;;   (lambda (db err)
;;     (if err (throw-error err))
;;     (mysql:row-iter db "SELECT * FROM some_table" nil
;;       (lambda (row)
;;         (println row)))))

;;;============================================================================
;;; MyCType: a base class providing a basic framework for working with
;;; MySQL C types and functions
;;;============================================================================

(setf MyCType:pack-format nil)

(define (MyCType:MyCType addr)
  (list (context) addr))

(define (MyCType:pointer inst)
  (inst 1))

(define (MyCType:members inst)
  (unpack MyCType:pack-format (:pointer inst)))

(define (MyCType:member inst n , unpacked)
  (nth n (:members inst)))

;;;============================================================================
;;; Utility functions and macros
;;;============================================================================

(unless if-not-zero
  (define-macro (if-not-zero)
    "If the first argument is not zero, evaluates the rest of the arguments.
    Useful for checking if the return argument of a C function is non-NULL."
    (letex ((ptr (eval (args 0))) (body (cons 'begin (rest (args)))))
      (if-not (zero? ptr)
        body
        nil)))

  (constant (global 'if-not-zero)))

;;;============================================================================
;;; Pre-declare classes and contexts to prevent circular dependencies
;;;============================================================================

(new 'MyCType 'Mysql)
(new 'MyCType 'MysqlField)
(new 'MyCType 'MysqlResult)

(sym "_mysql" '_MYSQL)

;;;============================================================================
;;; _MYSQL context stores API functions from libmysqlclient
;;;============================================================================

(context '_MYSQL)

;;; Find the libmysqlclient library on this system
(setf is-64-bit nil)
(let ((paths '("/usr/lib/libmysqlclient.so"
               "/usr/lib64/mysql/libmysqlclient.so"
               "/usr/local/mysql/lib/libmysqlclient.dylib"
               "/opt/local/lib/libmysqlclient.dylib"
               "/sw/lib/libmysqlclient.dylib")))
  (constant 'libmysqlclient
    (catch
      (dolist (path paths)
        (when (file? path)
          (if (find "lib64" path) ; some pack formats depend on this
            (setf is-64-bit true))
          (throw path))))))

;;; Import library functions
(import libmysqlclient "mysql_affected_rows")
(import libmysqlclient "mysql_close")
(import libmysqlclient "mysql_error")
(import libmysqlclient "mysql_free_result")
(import libmysqlclient "mysql_init")
(import libmysqlclient "mysql_insert_id")
(import libmysqlclient "mysql_real_connect")
(import libmysqlclient "mysql_real_query")
(import libmysqlclient "mysql_store_result")
(import libmysqlclient "mysql_num_fields")
(import libmysqlclient "mysql_fetch_field")
(import libmysqlclient "mysql_num_rows")
(import libmysqlclient "mysql_fetch_row")
(import libmysqlclient "mysql_fetch_lengths")
(import libmysqlclient "mysql_fetch_field_direct")
(import libmysqlclient "mysql_real_escape_string")

(context 'MAIN)

;;;============================================================================
;;; Mysql: An independent MySQL connection
;;;============================================================================

;; @syntax (Mysql)
;; <p>Returns a new Mysql instance that can safely be used in tandem with other
;; Mysql instances.</p>
(define (Mysql:Mysql , ptr)
  (setf ptr (_MYSQL:mysql_init 0))
  (if-not-zero ptr
    (list Mysql ptr)))

;; @syntax (:connect <Mysql-instance> <str-host> <str-user> <str-pass> <str-db> <int-port> <str-socket>)
;; @param <Mysql-instance> an instance of the Mysql class
;; @param <str-host> the hostname to connect to
;; @param <str-user> a MySQL username
;; @param <str-pass> <str-user>'s password
;; @param <str-db> the database to initially connect to
;; @param <int-port> (optional) port number of the MySQL server
;; @param <int-str> (optional) socket file to connect through
;; <p>Connects an initialized Mysql instance to a database. Returns <true> if
;; successful logging in, <nil> if not.</p>
;; @example
;; (setf db (Mysql))
;; (:connect db "localhost" "user" "secret" "my-database")
;; => true

(define (Mysql:connect inst host user pass db (port 0) (socket 0) , result)
  "Connects to a MySQL database. Throws an error on failure."
  (setf result (_MYSQL:mysql_real_connect (:pointer inst) host user pass db port socket 0))
  (if (zero? result)
    (throw-error (:error inst))
    true))

;; @syntax (:close <Mysql-instance>)
;; @param <Mysql-instance> an instance of the Mysql class
;; <p>Closes the connection and frees any memory used. This does <not> free the memory
;; used by results sets from this connection.</p>
(define (Mysql:close-db inst)
  (_MYSQL:mysql_close (:pointer inst)))

;; @syntax (:error <Mysql-instance>)
;; @param <Mysql-instance> an instance of the Mysql class
;; <p>Returns the last error message as a string or <nil> if there is none.</p>
(define (Mysql:error inst , ptr str)
  (setf ptr (_MYSQL:mysql_error (:pointer inst)))
  ; mysql_error always returns a valid string. If there is no error,
  ; the string will be empty.
  (setf str (get-string ptr))
  (if (= "" str) nil str))

;; @syntax (:coerce-type <Mysql-instance> <object>)
;; @param <Mysql-instance> an instance of the Mysql class
;; @param <object> a newLisp object
;; <p>Coerces <object> into something safe to use in a SQL statement. Lists are
;; converted into MySQL lists (e.g. '("foo" "bar" "baz") to
;; ('foo', 'bar', 'baz')) and string values are escaped. This is a helper
;; function for <Mysql:query>.</p>
(define (Mysql:coerce-type inst value)
  (cond
    ((nil? value) "NULL")
    ((or (= value "null") (= value "NULL")) value)
    ((number? value) value)
    ; Here the string must be packed to be sure that it is not truncated.
    ((string? value) (format "'%s'" (:escape inst (pack (format "s%d" (length value)) value))))
    ((list? value) (string "(" (join (map string (map (curry Mysql:coerce-type inst) value)) ", ") ")"))
    (true (format "'%s'" (:escape inst (string value))))))

;; @syntax (:query <Mysql-instance> <str-statement> [<lst-format-args>])
;; @param <Mysql-instance> an instance of the Mysql class
;; @param <str-statement> a SQL statement to execute
;; @param <lst-format-args> format arguments to the SQL statement
;; <p>Executes <str-statement>. Throws an error if the statement fails with the
;; reason. If the statement returns results, a <MysqlResult> class instance is
;; returned. Otherwise, returns the number of affected rows.</p>
;; <p>If <lst-format-args> is specified, all parameters are escaped (as
;; necessary) to generate safe, valid SQL. No quoting of values is required in
;; the format string; quotes are inserted as needed. To generate a
;; NULL in the SQL statement, pass <nil> or the string "NULL".</p>
;; @example
;; (:query db "SELECT name, employee_id FROM employees")
;; => (MysqlResult 1069216)
;; 
;; (:query db "DELETE FROM employees WHERE fired = 1")
;; => 14
;; 
;; (:query db '("SELECT id FROM employees WHERE name = %s" '("Johnson, John")))
;; ; SQL generated: SELECT id FROM employees WHERE name = 'Johnson, John'
;; => (MysqlResult 1069216)

(define (Mysql:query inst sql format-args , res ptr err params)
  (unless (or (null? format-args) (list? format-args))
    (throw-error "Format args must be passed to Mysql:query as a list!"))
  
  (when (list? format-args)
    (setf format-args (map (fn (v) (:coerce-type inst v)) format-args))
    (setf sql (format sql format-args)))
  
  (setf res (_MYSQL:mysql_real_query (:pointer inst) sql (+ 1 (length sql))))
  (if (zero? res)
    (begin
      ; Always attempt to store result firt. This does not degrade performance
      ; for non-result-returning queries (according to the MySQL C API docs).
      (setf ptr (_MYSQL:mysql_store_result (:pointer inst)))
      ; If mysql_store_result returns a null pointer, it may be an error or
      ; just mean that a query has no results (e.g. INSERT, DELETE, UPDATE).
      ; Error status requires a combination of a null pointer and a result
      ; from error.
      (when (and (zero? ptr) (setf err (:error inst)))
        (throw-error err))
      ; Otherwise, return an appropriate value. In the case of a non-result-
      ; returning query, return the number of affected rows. Otherwise, return
      ; a MysqlResult instance.
      (if (zero? ptr)
        (:affected-rows inst)
        (MysqlResult ptr)))
    ; mysql_real_query returns non-zero in case of an error.
    (throw-error (:error inst))))

;; @syntax (:insert-id <Mysql-instance>)
;; @param <Mysql-instance> an instance of the Mysql class
;; <p>Returns the id of the last inserted row when the target table contains
;; an AUTOINCREMENT field.</p>
(define (Mysql:insert-id inst)
  (_MYSQL:mysql_insert_id (:pointer inst)))

;; @syntax (:affected-rows <Mysql-instance>)
;; @param <Mysql-instance> an instance of the Mysql class
;; <p>Returns the number of rows affected by the most recent query.</p>
(define (Mysql:affected-rows inst)
  (_MYSQL:mysql_affected_rows (:pointer inst)))

;; @syntax (:escape <Mysql-instance> <str-value>)
;; @param <Mysql-instance> an instance of the Mysql class
;; @param <str-value> the string to escape
;; <p>Escapes a string to assure safety for use in a SQL statement.</p>
(define (Mysql:escape inst str , res) 
  (setf res (dup " " (+ 1 (* 2 (length str))))) 
  (_MYSQL:mysql_real_escape_string (:pointer inst) res str (length str)) 
  res)

;; @syntax (:databases <Mysql-instance>)
;; @param <Mysql-instance> an instance of the Mysql class
;; <p>Returns a list of the databases on this server.</p>
(define (Mysql:databases inst , res)
  (setf res (:query inst "SHOW DATABASES"))
  (map first (:fetch-rows res nil)))

;; @syntax (:tables <Mysql-instance> <str-database>)
;; @param <Mysql-instance> an instance of the Mysql class
;; @param <str-database> (optional) the database to query for tables
;; <p>Returns a list of tables available on this server. If <str-database> is
;; provided, the list of tables will be limited to that database.
(define (Mysql:tables inst db , sql res)
  (setf sql (if db (format "SHOW TABLES FROM `%s`" db) "SHOW TABLES"))
  (setf res (:query inst sql))
  (map first (:fetch-all res nil)))

;; @syntax (:fields <Mysql-instance> <str-table>)
;; @param <Mysql-instance> an instance of the Mysql class
;; @param <str-table> the table to display
;; <p>Returns metadata about the fields in <str-table>. The data is the result
;; of a 'SHOW FIELDS' query.</p>
(define (Mysql:fields inst table)
  (setf res (:query inst (format "SHOW FIELDS FROM `%s`" table)))
  (:fetch-rows res))

;;;============================================================================
;;; MysqlResult: The result of a MySQL query
;;;============================================================================

;; @syntax (MysqlResult <int-pointer>)
;; @param <int-pointer> a pointer to a MYSQL_RES struct
;; <p>Objects of this class are returned by Mysql:query as a result of queries
;; that generate result sets. This class is not generally instantiated directly
;; by the client.</p>

;; @syntax (:free <MysqlResult-instance>)
;; @param <MysqlResult-instance> an instance of the MysqlResult class
;; <p>Frees the memory used by a result. Must be called for each <MysqlResult>
;; generated, even if unused.</p>
(define (MysqlResult:free inst)
  (_MYSQL:mysql_free_result (:pointer inst)))

;; @syntax (:num-rows <MysqlResult-instance>)
;; @param <MysqlResult-instance> an instance of the MysqlResult class
;; <p>Returns the number of results in this result.</p>
(define (MysqlResult:num-rows inst)
  (_MYSQL:mysql_num_rows (:pointer inst)))

(define (MysqlResult:num-fields inst)
  (_MYSQL:mysql_num_fields (:pointer inst)))

(define (MysqlResult:column-lengths inst)
  (_MYSQL:mysql_fetch_lengths (:pointer inst)))

;; @syntax (:fields <MysqlResult-instance>)
;; @param <MysqlResult-instance> an instance of the MysqlResult class
;; <p>Returns a list of MysqlField instances corresponding to the columns in
;; this result.</p>
(define (MysqlResult:fields inst , n ptr fields)
  (setf fields '())
  (setf n (_MYSQL:mysql_num_fields (:pointer inst)))
  (until (zero? (setf ptr (_MYSQL:mysql_fetch_field (:pointer inst))))
    (push (MysqlField ptr) fields -1))
  fields)

;; @syntax (:fetch-row <MysqlResult-instance> <as-assoc>)
;; @param <MysqlResult-instance> an instance of the MysqlResult class
;; @param <as-assoc> (optional) whether to return results as a list or association list
;; <p>Returns one row from this result. If <as-assoc> is true, the results will
;; be returned as an association list (true by default). If this is the final row
;; in the result set, the MysqlResult instance is automatically freed.</p>
(define (MysqlResult:fetch-row inst (as-assoc true) , ptr num-fields cols lengths row)
  (setf ptr (_MYSQL:mysql_fetch_row (:pointer inst)))
  (if-not-zero ptr
    (setf num-fields (:num-fields inst))
    (setf cols (unpack (dup "lu" num-fields) ptr)) ; pointers to each column's start
    (setf lengths (unpack (dup "lu" num-fields) (:column-lengths inst))) ; the length of each column
    ; We must use the lengths because binary fields might contain null characters,
    ; which will fool get-string, which grabs chars until it hits a null.
    (setf row
      (map (lambda (len col i , value field result)
             (setf field (MysqlField (_MYSQL:mysql_fetch_field_direct (:pointer inst) i)))
             (setf value (first (unpack (format "s%d" len) col)))
             (setf value
               (case (:type field)
         				 ("bigint" (int value))
         				 ("bit" (int value 2)) ; untested
         				 ("date " (apply date-value (map int (parse value "-"))))
         				 ("datetime" (apply date-value (map int (parse value "[-: ]" 0))))
         				 ("decimal" (float value))
         				 ("double" (float value))
         				 ("float" (float value))
         				 ("integer" (int value))
         				 ("mediumint" (int value))
         				 ("null" nil)
         				 ("smallint" (int value))
         				 ("time" (map int (parse value ":"))) ; does not map to newlisp data type
         				 ("timestamp" (apply date-value (map int (parse value "[-: ]" 0))))
         				 ("tinyint" (int value))
         				 ("year" (int value))
                 (true value)))
             (if as-assoc (list (:name field) value) value))
           lengths
           cols
           (sequence 0 (- (length cols) 1)))))
  ; Either return the row value or free the result and return nil.
  (if (zero? ptr)
    (begin (:free inst) nil)
    row))

;; @syntax (:fetch-all <MysqlResult-instance> <as-assoc>)
;; @param <MysqlResult-instance> an instance of the MysqlResult class
;; @param <as-assoc> (optional) whether to return results as a list or association list
;; <p>Returns all rows from this result. If <as-assoc> is true, the results
;; will be returned as an association list (true by default).</p>
(define (MysqlResult:fetch-all inst (as-assoc true) , rows row)
  (setf rows '())
  (setf row (:fetch-row inst as-assoc))
  (while row
    (push row rows)
    (setf row (:fetch-row inst as-assoc)))
  rows)

;;;============================================================================
;;; MysqlField: A field in a MySQL result set
;;;============================================================================

;typedef struct st_mysql_field {
;  char *name;                 /* Name of column */
;  char *org_name;             /* Original column name, if an alias */
;  char *table;                /* Table of column if column was a field */
;  char *org_table;            /* Org table name, if table was an alias */
;  char *db;                   /* Database for table */
;  char *catalog;              /* Catalog for table */
;  char *def;                  /* Default value (set by mysql_list_fields) */
;  unsigned long length;       /* Width of column (create length) */
;  unsigned long max_length;   /* Max width for selected set */
;  unsigned int name_length;
;  unsigned int org_name_length;
;  unsigned int table_length;
;  unsigned int org_table_length;
;  unsigned int db_length;
;  unsigned int catalog_length;
;  unsigned int def_length;
;  unsigned int flags;         /* Div flags */
;  unsigned int decimals;      /* Number of decimals in field */
;  unsigned int charsetnr;     /* Character set */
;  enum enum_field_types type; /* Type of field. See mysql_com.h for types */
;} MYSQL_FIELD;

;; @syntax (MysqlField <int-pointer>)
;; @param <int-pointer> a pointer to a MYSQL_FIELD struct
;; <p>Objects of this class are returned by MysqlResult:fields.  It is used
;; internally in generating result rows. This class is not generally
;; instantiated directly by the client.</p>

(setf MysqlField:types ; see mysql_com.h for enum details
  (map list
    (append (sequence 0 16) (sequence 246 255))
    '("decimal" "tinyint" "smallint" "integer" "float" "double" "null" "timestamp"
      "bigint" "mediumint" "date " "time" "datetime" "year" "newdate" "varchar"
      "bit" "decimal" "enum" "set" "tiny blob" "medium blob" "long blob" "blob"
      "varchar" "char" "geometry")))

(if _MYSQL:is-64-bit
  (setf MysqlField:pack-format (append (dup "Lu" 9) (dup "lu" 11))) ; use 64-bit pointers
  (setf MysqlField:pack-format (append (dup "lu" 20))))

;; @syntax (:name <MysqlField-instance>)
;; @param <MysqlField-instance> an instance of the MysqlField class
;; <p>Returns the name of this field (or its alias).</p>
(define (MysqlField:name inst)
  (get-string (:member inst 0)))

;; @syntax (:table <MysqlField-instance>)
;; @param <MysqlField-instance> an instance of the MysqlField class
;; <p>Returns this field's table (or its alias).</p>
(define (MysqlField:table inst)
  (get-string (:member inst 2)))

;; @syntax (:type <MysqlField-instance>)
;; @param <MysqlField-instance> an instance of the MysqlField class
;; <p>Returns this field's type.</p>
(define (MysqlField:type inst)
  (lookup (:member inst 19) MysqlField:types))

;;;============================================================================
;;; mysql context contains convenience functions for working with MySQL
;;; databases
;;;============================================================================

(context 'mysql)

;; @syntax (mysql:on-connect <list-credentials> <fn-callback>)
;; @param <list-credentials> a list of parameters to pass to Mysql:connect
;; @param <fn-callback> a function to call with the database connection
;; <p>Connects to a MySQL server using <list-credentials> and calls
;; <fn-callback> using the Mysql instance as the first argument. If an
;; error occurred attempting connection, the error string is passed as the
;; second parameter. The minimum contents of <list-credentials> must be
;; '(<str-host> <str-username> <str-password> <str-database>).</p>
;; <p>The connection is automatically freed when mysql:on-connect returns.</p>
;; @example
;; (mysql:on-connect '("localhost" "user" "secret" "my_database")
;;   (lambda (db err)
;;     (if err
;;       (println "Error! " err)
;;       (println "Success! " db))))
(define (on-connect credentials func , db err success? result)
  (setf db (Mysql))
  (if (catch (eval (append '(:connect db) credentials)) 'err)
    (setf success? (catch (func db) 'result))
    (setf success? (catch (func db err) 'result)))
  (:close-db db)
  (if success? result (throw-error (replace {(ERR: user error : )+} result "" 0))))

;; @syntax (mysql:row-iter <Mysql-instance> <str-sql> <bool-as-assoc> <fn-callback>)
;; @param <Mysql-instance> a connect instance of the Mysql class
;; @param <str-sql> a sql statement
;; @param <bool-as-assoc> flags whether or not to pass rows as regular or association lists
;; @param <fn-callback> a function to call for each row returned by the query
;; <p>Iterates over the results of a query, passing a row at a time to
;; <fn-callback>. The MysqlResult is automatically freed. The return value
;; of mysql:row-iter is the result of the last call to <fn-callback>.</p>
;; <p>Note that each row is called with MysqlResult:fetch-row to avoid building
;; intermediate lists.</p>
;; @example
;; (mysql:on-connect '("localhost" "user" "secret" "my_database")
;;   (lambda (db err)
;;     (if err
;;       (println "Error! " err)
;;       (mysql:row-iter db "SELECT * FROM some_table" true
;;         (lambda (row) (println row))))))
(define (row-iter db sql as-assoc func , result row)
  (setf result (:query db sql))
  (while (setf row (:fetch-row result as-assoc))
    (func row)))

;; @syntax (mysql:row-map <Mysql-instance> <str-sql> <bool-as-assoc> <fn-callback>)
;; @param <Mysql-instance> a connect instance of the Mysql class
;; @param <str-sql> a sql statement
;; @param <bool-as-assoc> flags whether or not to pass rows as regular or association lists
;; @param <fn-callback> a function to apply to each row returned by the query
;; <p>Maps <fn-callback> over each row returned by querying <Mysql-instance>
;; with <str-sql>. Memory used by the MysqlResult is automatically freed.
;; Returns a list of the result of applying <fn-callback> to each row.</p>
;; @example
;; (mysql:on-connect '("localhost" "user" "secret" "my_database")
;;   (lambda (db err)
;;     (if err
;;       (println "Error! " err)
;;       (mysql:row-iter db "SELECT * FROM some_table" true first))))
(define (row-map db sql as-assoc func , res result rows)
  (setf result (:query db sql))
  (if (catch (:fetch-all result as-assoc) 'rows)
    (map func rows)))

;; @syntax (mysql:reduce-results <Mysql-instance> <str-sql> <bool-as-assoc> <fn-callback>)
;; @param <Mysql-instance> a connect instance of the Mysql class
;; @param <str-sql> a sql statement
;; @param <bool-as-assoc> flags whether or not to pass rows as regular or association lists
;; @param <fn-callback> a function to be applied in reducing the results of the query
;; <p>Reduces the results of the query by applying <fn-callback> successively
;; to slices of the list of rows from the left.  On the first call to
;; <fn-callback>, the arguments will be a number of rows equal to the number of
;; parameters that <fn-callback> accepts. On each subsequent call, the first
;; parameter will be replaced by the result of the previous call. See the
;; @link http://www.newlisp.org/newlisp_manual.html#apply apply&nbsp;function
;; for a more detailed description of the mechanics of apply/reduce. The return
;; value is the result of the final application of <fn-callback>.</p>
;; @example
;; (mysql:on-connect '("localhost" "user" "secret" "my_database")
;;   (lambda (db err)
;;     (if err
;;       (println "Error! " err)
;;       (mysql:row-reduce db "SELECT * FROM some_table" true
;;         (lambda (row-1 row-2)
;;           (+ (if (list? row-1) (first row-1) row-1) (first row-2)))))))
(define (row-reduce db sql as-assoc func , reduce-by rows arg-list)
  ; Determine the number of rows to reduce by on each call
  (setf arg-list (map name (first func)))
  (if (find "," arg-list)
    (setf reduce-by (length (rest (member "," (reverse arg-list)))))
    (setf reduce-by (length arg-list)))
  ; Perform reduction
  (setf result (:query db sql))
  (if (catch (:fetch-all result as-assoc) 'rows)
    (apply func rows reduce-by)))

(context 'MAIN)









