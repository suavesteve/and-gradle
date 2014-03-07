(require 'cl-lib)

;;(setq and-gradle-project-package-name "com.Bluejam")
(setq and-gradle-directory-seperator "/")
(setq and-gradle-compiled-file "master.dat")
(setq and-gradle-compile-error-regexp '("^:compileDebugJava\\([_[:alnum:]-/]*.java\\):\\([0-9]*\\)" 1 2))

(defmacro a-listp(a-list)
  "This function returns t if a list
is an a-list of type (SYMBOL . VALUE)
or nil if not, returns nil if an empty
list is passed"
  (list 'and (list 'listp a-list)
	(list 'symbolp (list 'car a-list)) 
	(list 'listp (list 'cdr a-list))))

(defun and-gradle-find-in-tree (xml-list value)
  "Walk the supplied xml-tree searching for key VALUE,
build a list of the activities containing this key"
  (when (consp xml-list) ;; When not null
    (append 
     (and (funcall (lambda(list key)
		     (if (a-listp list)
			 (progn
			   (find-if 
			    (lambda (p-list)
			      (if (consp p-list) 
				  (eq (car p-list) key)))
			    list)))) xml-list value) (list xml-list)) ;; Search the current 'root' node for the entry
     (and-gradle-find-in-tree (car xml-list) value) ;; Now search the 'left' branch
     (and-gradle-find-in-tree (cdr xml-list) value)))) ;; Now search the 'right' branch

(defun and-gradle-get-settings(project-path)
  (with-temp-buffer
    (insert-file-contents (concat project-path "/src/main/AndroidManifest.xml"))
    
    ;; XML parse returns a tree of format
    ;; (SYMBOL ((Attribute-Name . Value)) ((CHILD-NODES)))
    ;; Get the AndroidManifest.xml into a tree
    (let* ((main-node (xml-parse-tag))
	   ;; Get the child nodes the manifest, the activity nodes
	   (activities (xml-get-children (car (xml-get-children main-node 'application)) 'activity))
	   ;; Get the package value from the tree
	   (package (xml-get-attribute main-node 'package))
	   ;; Place-holder for the main-activity
	   (main-activity ()))

      ;; For each activity node in the Manifest->Application tree
      (cl-loop for current-activity in activities do
	       ;; Get the intent-filter section, if it exists
	       (let* ((intent (car (and-gradle-find-in-tree current-activity 'intent-filter))))
		 ;; For each intent-filter section (There may be more than one)
		 (cl-loop for current-intent in intent do
			  ;; If it's an a-list with the KEY 'intent-filter
			  (if (and (a-listp current-intent) (eq (car current-intent) 'intent-filter))
			      ;; Get the 'action child, and then it's attribute android:name
			      (let* ((action-string (xml-get-attribute 
						     (car (xml-get-children current-intent 'action)) 'android:name)))
				;; If this is android.intent.action.main, it's the main location for starting the app
				(if (and action-string (string= "android.intent.action.main" (downcase action-string)))
				    (setq main-activity (xml-get-attribute intent 'android:name))))))))
      
      (list package main-activity))))


(defun and-gradle-build-project(project-path)
  "Build the project according to the file build.gradle in
the supplied path - use the compilation buffer to allow
context highlighting of errors"
  (interactive "DProject Path: ")
  (add-to-list 'compilation-error-regexp-alist and-gradle-compile-error-regexp)
  (compile (concat "gradle -b " (expand-file-name project-path) "/build.gradle build")))

(defun and-gradle-install-project(project-path project-name)
  ""
  ;; Is adb running for us?
  (when (not (> (string-to-number 
	  (shell-command-to-string "ps -ax | grep -i adb | grep -v grep | wc -l")) 0))
    (progn
      (save-excursion
	(set-buffer "*compilation*")
	(goto-char (point-max))
	(insert "\nStarting ADB...")
	(shell-command "adb usb"))))

  (let* ((device-list
	  (split-string 
	   (shell-command-to-string "adb devices | tail -n +2 | head -n -1 | awk -F' ' '{print $1}'")
	   "\n" t)))

    ;; Output our ADB messages into the compilation buffer
    (save-excursion
      (set-buffer "*compilation*")
      (goto-char (point-max))
      (insert "\n\n** ADB Install **\n")
      
      (if (= 0 (length device-list))
	  (insert "Please connect an Android Device and try installing again")
	;; We have devices, allow the user to select which
	(let* ((device (completing-read "Device to install to: " device-list))
	       (name (car (last (split-string project-name "\\."))))
	       (activity (cadr (and-gradle-get-settings project-path)))
	       (install-cmd (concat "adb -s " device " install -r " 
			    project-path "/build/apk/" 
			    name "-debug-unaligned.apk"))
	       (run-cmd (concat "adb -s " device " shell am start -n " project-name "/" activity)))
	  (goto-char (point-max))
	  (insert (concat "\n" (shell-command-to-string install-cmd)))
	  (goto-char (point-min))
	  (while (re-search-forward "\r+$" nil t)
	    (replace-match "" t t))
	  (message run-cmd)
	  (shell-command run-cmd)
	  )))))

(defun and-gradle-build-and-install-project(project-path)
  (interactive "DProject Path: ")
  ""
  ; What to do when compilation finished
  (lexical-let ((name (car (and-gradle-get-settings project-path)))
		(path project-path))
    (setq compilation-finish-functions 
	  (lambda (buffer status)
	    ;; If the status isn't finished, exit with a status message
	    (if (string-match "exited abnormally" status)
		(message "Can not continue install, build errors!")
	      ;; Otherwise, call the install function
	      (funcall 'and-gradle-install-project path name)))))
			 
  (add-to-list 'compilation-error-regexp-alist and-gradle-compile-error-regexp)
  (compile (concat "gradle -b " (expand-file-name project-path) "/build.gradle build")))


(defun and-gradle-extract-files (path package-name project-name)
  "Read for the file master.dat, parse it for <<Replaceable>>
strings, replace as necessary, then generate the requied files
and populate the previously generated tree with the files"
  ;; The files as they appear in the master and the path they should be
  ;; inserted into
  (let* ((file-names (list `("AndroidManifest.xml" "AndroidManifest.xml" ,(concat path "src/main"))
			   `("ClassName.java" ,(concat project-name ".java") ,(concat path "src/main/java/"
										      (replace-regexp-in-string "\\." "\/" package-name) "/" project-name))
			   `("Build.gradle" "build.gradle" ,(concat path))
			   `("classname_layout.xml" ,(downcase (concat project-name "_layout.xml")) ,(concat path "src/main/res/layout"))
			   `("strings.xml" "strings.xml" ,(concat path "src/main/res/values")))))
    (with-temp-buffer
      ;; Insert the whole file contents
      (insert-file-contents "master.dat")

      ;; Parse the regions for the placeholders 
      ;; enclosed within << >>
      (let* ((replacements (list `("<<ClassName>>" ,(concat project-name))
				 `("<<PackageName>>" ,(concat package-name))
				 `("<<ContentLayout>>" ,(downcase (concat project-name "_layout"))))))
	(mapcar (lambda (rep-entry)
		  (goto-char (point-min))
		  (while (search-forward (nth 0 rep-entry) nil t)
		    (replace-match (nth 1 rep-entry) t t))) 
		replacements))

      ;; Now produce each file and place it in the correct
      ;; location in the tree
      (mapcar 
       (lambda (file)
	 (goto-char (point-min))
	 (let* ((start-st (concat "## " (nth 0 file) " ##"))
		(end-st (concat "## /" (nth 0 file) " ##"))
		(file-name (concat (nth 2 file) "/" (nth 1 file)))
		(start (progn		       
			 (search-forward start-st)
			 ;; Move the line forward, make sure the files don't
			 ;; have a blank line at the beginning (Causes
			 ;; fatal parsing errors in the build-system)
			 (forward-line 1)
			 (line-beginning-position)))
		(end (progn 
		       (search-forward end-st)
		       (line-beginning-position))))	   
	   (write-region start end file-name)))
       file-names))))

(defun and-gradle-build-dir (dir p-name)
  "Build the directory structure for the
gradle project system"
  (interactive "DPath: \nsProject Name: ")
  
  (let* ((dir-string (if (string= (substring dir (- (length dir) 1)) and-gradle-directory-seperator)
			 dir
		       (concat dir and-gradle-directory-seperator))))
    (if (not (file-directory-p (concat dir-string p-name)))
	(if (not (string= p-name ""))
	    (progn
	      ;; Build the directory tree	      
	      (let* ((sep and-gradle-directory-seperator)
		     (project-path (concat dir-string p-name sep))
		     ;; If the variable and-gradle-project-package-name hasn't been set
		     ;; get the package name from the user
		     (package-name (if (not (boundp 'and-gradle-project-package-name))
				       (read-from-minibuffer "Package Name: " "com.Example")
				     and-gradle-project-package-name)))

		(make-directory (concat project-path "build/") t)
		(make-directory (concat project-path "src/main/res/layout") t)
		(make-directory (concat project-path "src/main/res/values"))

		;; Produce the correct class-path from the custom set
		;; variable PROJECT-PACKAGE-NAME (com.CompanyName) and the project
		;; name P-NAME (com.CompanyName.ProjectName)
		(let* ((class-paths (replace-regexp-in-string "\\." "\/" package-name))
		       (java-path (concat project-path "src/main/java/" class-paths sep p-name)))
		  (make-directory java-path t)
		  ;; Now populate the tree with files
		  (and-gradle-extract-files project-path package-name p-name))))
	  (message "Error: A project name must be supplied"))
      (message (concat "Error: Project Directory " dir-string p-name " already exists")))))


