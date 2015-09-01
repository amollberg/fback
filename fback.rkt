#! /usr/local/bin/racket
#lang racket/base

; Procedures:
; If file changed from previous file version or snapshot:
; 	Copy new file version
; If time since last snapshot > threshold
; 	Make new snapshot
; Restore subfolder at destination to date:
; 	For each file in subfolder:
; 		Find newest version older than or equal to date
; 		Copy that version to destination
; If free space < threshold OR snapshot failed OR file version copy failed
; 	# Merge snapshots
; 	While number of snapshots > snapthreshold
; 		OverwriteCopy second oldest snapshot to oldest snapshot
; 	# Remove file versions
; 	For each file:	
; 		occupation=size*(revisions - revthreshold)
; 	While files with positive occupation exist AND (free space < threshold OR snapshot failed OR file version copy failed)
; 		Remove the oldest revision of the file with highest positive occupation
; 	While free space < threshold OR snapshot failed OR file version copy failed
; 		Remove a revision that is older than the newest snapshot from the file with highest occupation
; 	
; 
; 
; Pseudo-Predicates:
; 	* file changed from previous file version or snapshot
; 		Compare size
; 		If equal size
; 			Compare checksum
; 	* time since last snapshot
; 		Parse folder name of last folder in snapshot top folder
; 	* Find newest version older than or equal to date
; 		Traverse file revisions
; 			If revision older than or equal to date
; 				Save path of file
; 		Traverse file revisions in snapshots
; 			If revision older than or equal to date
; 				Save path of file
; 		Return the path that is newer of the two
; 	* Copy that version to destination
; 		FileCopy with force flag
; 	* free space
; 		DriveSpaceFree or procedure during testing
; 	* snapshot failed
; 		ErrorLevel during FileCopy
; 	* file version copy failed
; 		ErrorLevel during FileCopy
; 	* number of snapshots
; 		Traverse snapshot top folder
; 			Increment a counter
; 	* OverwriteCopy second oldest snapshot to oldest snapshot
; 		Traverse snapshot top folder to find the snapshots
; 	* files with positive occupation exist
; 		Traverse file list 
; 			Traverse file revisions
; 				Sum file size
; 			Write sum to file list
; 	* Remove the oldest revision of the file with highest positive occupation
                                        ; 	* Remove a revision that is older than the newest snapshot from the file with highest occupation
                                        ; 
                                        ; 
                                        ; 
                                        ; 	File structures:
                                        ; 		U:\Snapshots\20150830111559\D\<path on source drive>
                                        ; 		U:\FileRevisions\20150830111559\D\<path on source drive>
                                        ; 	Data structures:
                                        ; 		Map : file path -> occupation
                                        ;		Map : 


(require "options.rkt")

(read-options "default.conf")
(define-options backup-basepath)

(define BACKUP-BASEPATH backup-basepath)


(define (tee val)
  (printf "~a\n" val)
  val)


(require racket/list)	
(define (get-drive-letter filepath)
  (substring (path->string (first (explode-path filepath))) 0 1))	

(require racket/string)	
(define (filepath-to-rev-regex fp)
                                        ;(printf "rr:~a\n" 
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
    "$)"))
                                        ;(pregexp "C:\\\\Users\\\\Alex\\\\Desktop\\\\testfback\\\\b\\\\(Snapshots|FileRevisions)\\\\\\d+\\\\c\\\\users\\\\alex\\\\desktop\\\\testfback\\\\"))
  )


(require racket/path)	
(define (revision-datestring rev-path)
  (path->string (second (explode-path (find-relative-path BACKUP-BASEPATH rev-path)))))

(define (revision-sequence fp)
  (sequence-map 
   (lambda (rev-path) 
     (cons (revision-datestring rev-path) rev-path))
   (sequence-filter 
    (lambda (rev-path) 	
                                        ;(printf " candidate: ~a\n" (path->string rev-path))
      (regexp-match 
       (filepath-to-rev-regex fp)
       (path->string rev-path)))
    (in-directory BACKUP-BASEPATH))))

(define (newest-revision-tuple fp)
                                        ;(printf "regex: ~a\n" (filepath-to-rev-regex fp))
  (let* ((sorted-revs 
          (sort (sequence->list (revision-sequence fp))
                #:key car 
                string>?)))
    (if (null? sorted-revs)
        null
        (first sorted-revs))))

(define (newest-revision-path rev-tuple)
  (if (null? rev-tuple)
      null
      (cdr rev-tuple)))



(require file/sha1)	
(define (file-changed? fp)	
  (let ((newest (newest-revision-path (newest-revision-tuple fp))))
;    (printf "~a, ~a, ~a\n" fp newest (null? newest))
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

(displayln (new-timestamp-string))

(require racket/file)
(for ([f (in-directory (current-directory))])
  (let* ((ts (new-timestamp-string))
         (rev-path (revision-filepath f ts)))
    (if (file-exists? f) ; If not directory
        (if (file-changed? f)
            (begin
              (printf "  changed: ~a\n" rev-path)
              (make-directory* (path-only rev-path))
              (copy-file f rev-path))
        (printf "unchanged: ~a\n" f))
    #f)))

                                        ; Get free space
(require math/base)	
(require racket/sequence)	
(define (get-free-space)
  (- 40000 
     (sequence-fold + 0
                    (sequence-map file-size
                                  (in-directory (current-directory))))))

(get-free-space)		
                                        ;(for ([f (in-directory (current-directory))]) 
                                        ;(printf "file:~a\n" f))
                                        ;)
