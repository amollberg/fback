#! /usr/local/bin/racket
#lang racket/base

(define (display-help)
  (displayln #<<EOF
Command-line syntax:

fback
 - Show this help screen

fback <directory>
 - Backup <directory> using default configuration

fback revisions <files>
 - List dates, checksums and file sizes of all revisions of each of <files>

fback restore <datestring> <file>
 - Restore <file> to the revision at <datestring>
EOF
 ))

;; Procedures:
;; If file changed from previous file version or snapshot:
;; 	Copy new file version
;; If time since last snapshot > threshold
;; 	Make new snapshot
;; Restore subfolder at destination to date:
;; 	For each file in subfolder:
;; 		Find newest version older than or equal to date
;; 		Copy that version to destination
;; If free space < threshold OR snapshot failed OR file version copy failed
;; 	# Merge snapshots
;; 	While number of snapshots > snapthreshold
;; 		OverwriteCopy second oldest snapshot to oldest snapshot
;; 	# Remove file versions
;; 	For each file:	
;; 		occupation=size*(revisions - revthreshold)
;; 	While files with positive occupation exist AND (free space < threshold OR snapshot failed OR file version copy failed)
;; 		Remove the oldest revision of the file with highest positive occupation
;; 	While free space < threshold OR snapshot failed OR file version copy failed
;; 		Remove a revision that is older than the newest snapshot from the file with highest occupation
;; 	
;; 
;; 
;; Pseudo-Predicates:
;; 	* file changed from previous file version or snapshot
;; 		Compare size
;; 		If equal size
;; 			Compare checksum
;; 	* time since last snapshot
;; 		Parse folder name of last folder in snapshot top folder
;; 	* Find newest version older than or equal to date
;; 		Traverse file revisions
;; 			If revision older than or equal to date
;; 				Save path of file
;; 		Traverse file revisions in snapshots
;; 			If revision older than or equal to date
;; 				Save path of file
;; 		Return the path that is newer of the two
;; 	* Copy that version to destination
;; 		FileCopy with force flag
;; 	* free space
;; 		DriveSpaceFree or procedure during testing
;; 	* snapshot failed
;; 		ErrorLevel during FileCopy
;; 	* file version copy failed
;; 		ErrorLevel during FileCopy
;; 	* number of snapshots
;; 		Traverse snapshot top folder
;; 			Increment a counter
;; 	* OverwriteCopy second oldest snapshot to oldest snapshot
;; 		Traverse snapshot top folder to find the snapshots
;; 	* files with positive occupation exist
;; 		Traverse file list 
;; 			Traverse file revisions
;; 				Sum file size
;; 			Write sum to file list
;; 	* Remove the oldest revision of the file with highest positive occupation
;; 	* Remove a revision that is older than the newest snapshot from the file with highest occupation
;; 
;; 
;; 
;; 	File structures:
;; 		U:\Snapshots\20150830111559\D\<path on source drive>
;; 		U:\FileRevisions\20150830111559\D\<path on source drive>
;; 	Data structures:
;; 		Map : file path -> occupation


(require "options.rkt")

(read-options "default.conf")
(define-options BACKUP-BASEPATH)
(define-options minimum-number-of-revisions)

(define MINIMUM-NUMBER-OF-REVISIONS (string->number minimum-number-of-revisions))

(when (not (directory-exists? BACKUP-BASEPATH))
    (make-directory* BACKUP-BASEPATH))

(define (tee val)
  (printf "~a\n" val)
  val)


(require racket/list)	
(define (get-drive-letter filepath)
  (substring (path->string (first (explode-path filepath))) 0 1))	


(require racket/string)	
(define (filepath-to-rev-regex fp)
  (pregexp
   (string-append 
    "(?i:^"
    (string-replace
     (path->string 
      (reroot-path fp
                   (string-append 
                    BACKUP-BASEPATH
                    "(Snapshots|FileRevisions)\\"
                    "[0-9]+")))
     "\\" "\\\\")
    "$)")))

(require racket/path)
(define (original-filepath rev-path)
  (let ((orig-fp-list (rest (rest (explode-path (find-relative-path BACKUP-BASEPATH rev-path))))))
    (set! orig-fp-list
	  (cons (string->path
		 (string-append
		  (path->string (first orig-fp-list))
		  ":")) ;; Add a : after the drive letter
		(rest orig-fp-list))) 
    (apply build-path orig-fp-list)))

	
(define (revision-datestring rev-path)
  (path->string (second (explode-path (find-relative-path BACKUP-BASEPATH rev-path)))))

(define (file-checksum filepath)
  (sha1 (open-input-file filepath)))


(define (revision-sequence fp)  
  (sequence-map 
   (lambda (rev-path)
     (list (revision-datestring rev-path)
	   (file-checksum rev-path)
	   (file-size rev-path)
	   rev-path))
   (sequence-filter 
    (lambda (rev-path) 	
      (regexp-match 
       (filepath-to-rev-regex fp)
       (path->string rev-path)))
    (in-directory BACKUP-BASEPATH))))

(define (newest-revision-tuple fp)
  (let* ((sorted-revs 
          (sort (sequence->list (revision-sequence fp))
                #:key car 
                string>?)))
    (if (null? sorted-revs)
        null
        (first sorted-revs))))



(define (revision-tuple-datestring rev-tuple)
  (if (null? rev-tuple)
      null
      (first rev-tuple)))

(define (revision-tuple-checksum rev-tuple)
  (if (null? rev-tuple)
      null
      (second rev-tuple)))

(define (revision-tuple-filesize rev-tuple)
  (if (null? rev-tuple)
      null
      (third rev-tuple)))


(define (revision-tuple-path rev-tuple)
  (if (null? rev-tuple)
      null
      (fourth rev-tuple)))


(require file/sha1)	
(define (file-changed? fp)	
  (let ((newest (revision-tuple-path (newest-revision-tuple fp))))
    (cond [(null? fp)
           #f]
          [(null? newest) 
           #t]
          [(not (= (file-size newest)
                   (file-size fp))) 
           #t]
          [(not (string=? (sha1 (open-input-file fp))
			  (sha1 (open-input-file fp))))
           #t]
          [#t #f])))


(require srfi/19)
(define (new-timestamp-string)
  (date->string (current-date) "~Y~m~d~k~M~S"))

 
(define (revision-filepath fp timestamp)
  (reroot-path fp
               (string-append 
                BACKUP-BASEPATH
                "FileRevisions\\"
                timestamp)))


;; Get amount of free space (overrides built-in for testing purposes)
(require math/base)	
(require racket/sequence)	
(define (get-free-space)
  (- 40000 
     (sequence-fold
      + 0
      (sequence-map
       file-size
       (in-directory (current-directory))))))

(define (list-revisions filepaths)
  (map (lambda (file)
	 (displayln file)
	 (sequence-for-each
	  (lambda (revision-tuple)
	    (printf "~a\t ~a\t ~a\n"
		    (revision-tuple-datestring revision-tuple)
		    (revision-tuple-checksum revision-tuple)
		    (revision-tuple-filesize revision-tuple)))
	  (revision-sequence file)))
       filepaths)
  (void))

(define (get-files-with-many-revisions)
  (sequence-filter
   (lambda (rev-path)
     (> 
      (sequence-length
       (revision-sequence
	(original-filepath
	 (path->string rev-path))))
      MINIMUM-NUMBER-OF-REVISIONS))
   (sequence-filter
    file-exists? ;; Is not directory
    (in-directory BACKUP-BASEPATH))))
  

(define (do-cleanup size)
  (when (> size 0)
	(let* ((files-to-remove (get-files-with-many-revisions))
	       (removed-size 0))
	  (for ([file-to-remove (get-files-with-many-revisions)])
	       #:break (> removed-size size)
	       ;;(delete-file file-to-remove)
	       (set! removed-size (+ removed-size (file-size file-to-remove)))
	       (printf "Removing ~a, ~a bytes removed total \n" file-to-remove removed-size)))))

(do-cleanup 20000)

(define (drive-full-error? e)
  (and (exn:fail:filesystem:errno? e) 
       (equal? (exn:fail:filesystem:errno-errno e)
	       '(112 . windows))))

(define (safe-copy-file . args)
  (with-handlers ([drive-full-error? (lambda (e)
				       (do-cleanup) ;; Clear up some space on backup drive
				       (apply safe-copy-file args))]) ;; Try to back up file again
		 (apply copy-file args)))

(define (restore-revision datestring filepath)
  (let ((copy-src (revision-filepath filepath datestring))
	(copy-dest filepath))
    (printf "Restoring from ~a\n" copy-src)
    (copy-file copy-src copy-dest #t)))

(require racket/file)
(define (backup-directory directorypath)
  (let ((ts (new-timestamp-string)))
    (for ([f (in-directory (current-directory))])
	 (let ((rev-path (revision-filepath f ts)))
	   (if (file-exists? f) ; If not directory
	       (if (file-changed? f)
		   (begin
		     (printf "  changed: ~a\n" rev-path)
		     (make-directory* (path-only rev-path))
		     (safe-copy-file f rev-path))
		   ;;(printf "unchanged: ~a\n" f)
		   (void)
		   )
	       (void))))))

(require racket/cmdline)
(let ((args (vector->list (current-command-line-arguments))))
  (if (null? args)
      (display-help)
      (case (first args)
	[("revisions")
	 (list-revisions (map
			  (lambda (path)
			    (path->complete-path (string->path path)))
			  (rest args)))]
	[("restore")
	 (restore-revision (second args)
			   (path->complete-path (string->path (third args))))]
	[else
	 (backup-directory (path->complete-path (string->path (first args))))])))



