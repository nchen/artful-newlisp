;; @module qwerty
;; @author Jeff Ober <jeffober@gmail.com>
;; @version 1.0
;; @location http://static.artfulcode.net/newlisp/qwerty.lsp
;; @description Provides a dependency-managing library loading mechanism (requires newlisp 10)
;; <p>This module replaces nlmod, providing a simple mechanism for package
;; dependencies. qwerty supports packages with multiple files and directories.</p>
;; <p>All Artful Code modules (apart from qwerty itself, which must be loaded
;; manually) have a .qwerty package definition.</p>
;; <p>The provided macro, <define-package>, is used to register a package with
;; qwerty. This should be saved in a file named <module-name>.qwerty, located in
;; a directory in the list of registry paths (<qwerty:registry-path>).</p>
;; <p>A package must be located in one of the <qwerty:module-path> directories.
;; If the package was defined without any files, qwerty assumes that the file's
;; name is the same as the module's (e.g., <my-module>.lsp).</p>
;; <h4>Version history</h4>
;; <b>1.0</b>
;; &bull; initial release
;; 
;; @example
;; (load "/path/to/qwerty.lsp")
;; 
;; ; Load Artful Code libraries for a CGI application
;; (push "/path/to/artfulcode/modules" qwerty:module-path)
;; (push "/path/to/artfulcode/registry" qwerty:registry-path)
;; (qwerty:load-package "request" "response" "element" "mysql")

(define pkgdepends:pkgdepends)
(define pkgfiles:pkgfiles)
(define pkgloaded:pkgloaded)

(context 'qwerty)

(setf registry-path (list (append (env "NEWLISPDIR") "/registry")))
(setf module-path (list (append (env "NEWLISPDIR") "/modules")))

(define (register-package str-name files deps)
  (pkgfiles str-name files)
  (pkgdepends str-name deps))

(define (find-in-path str-search lst-paths , found)
  (setf found
    (catch
      (begin
        (dolist (path lst-paths)
          (when (file? (string path "/" str-search))
            (throw (string path "/" str-search))))
        (throw nil))))
  (or found (throw-error (format "Unable to locate %s; tried %s." str-search (string lst-paths)))))

(define (find-registry-path str-name)
  (find-in-path (string str-name ".qwerty") registry-path))

(define (find-module-path str-name)
  (find-in-path (string str-name ".lsp") module-path))

(define (find-package-path file-list , found)
  (setf found
    (catch
      (dolist (path module-path)
        (when (for-all (fn (f) (file? (string path "/" f))) file-list)
          (throw path))
          nil)))
  found)

;; @syntax (qwerty:load-package <pkg-name> [<pkg-name-2> ...])
;; @param <pkg-name> one or more package names to load
;; <p>Loads a package and its dependencies.  Throws an error if <pkg-name>, its
;; component files, or any of its dependencies fail to load.</p>
(define (load-package)
  (doargs (str-name)
    (unless (pkgloaded str-name)
      ;; First, try to load the package definition
      (local (file)
        (setf file (find-registry-path str-name))
        (load file))
      ;; Now that package is defined, load dependencies
      (dolist (dep (pkgdepends str-name))
        (load-package dep))
      ;; Now load package files
      (local (files file path)
        (setf files (pkgfiles str-name))
        ;; We want all package files to come from the same path
        (if (and (= 1 (length files)) (= str-name (first files)))
          ;; Case 1: single file package, named for itself
          (load (find-module-path (first files)))
          ;; Case 2: multi-file package; find a single directory will all files in it
          (begin
            (setf path (find-package-path files))
            (unless path (throw-error (format "Unable to locate package directory for %s" str-name)))
            (dolist (file files)
              (load (string path "/" file ".lsp"))))))
    ;; Prevent module reload
    (pkgloaded str-name true))))

(context 'MAIN)

;; @syntax (define-package <pkg-name> (files <file-1> <file-2> ...) (depends <dep-1> <dep-2> ...))
;; @param <files> a list of files that make up the package
;; @param <depends> a list of packages on which this package depends
;; <p>Defines a new package. The package will be named <pkg-name>. If a list of
;; files is not provided, it is assumed that the package contains only one file
;; that is named <pkg-name>.lsp. Otherwise, a list of file names may be provided
;; that are assumed to be in the same directory (the first found in the path).</p>
;; <p>The files may be preceded by a sub-path, e.g. (files "/foo-package/foo1.lsp"
;; "foo-package/foo2.lsp" "foo-package/sub-package/bar.lsp"), to permit an
;; arbitrarly deeply structured package. Files are listed in the order in which
;; they should be loaded.</p>
;; <p>Dependencies for the package are listed similarly, as a list beginning with
;; the symbol 'depends. Both component files and dependencies are optional. File
;; extensions are left off names in both lists.</p>
;; @example
;; (define-package "my-app"
;;    (files "my-app/lib" "my-app/macros" "my-app/main")
;;    (depends "cgi" "crypto"))
(define-macro (define-package)
  (letex ((str-name (args 0))
          (files (assoc 'files (rest (args))))
          (deps (assoc 'depends (rest (args)))))
    (qwerty:register-package str-name
      (if 'files (rest 'files) '(str-name))
      (if 'deps (rest 'deps) '()))))

(constant (global 'define-package))
