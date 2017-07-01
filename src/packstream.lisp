(in-package #:cl-neo4j)

;; MARKERS
;;
;; Every serialised value begins with a marker byte. The marker contains information on data type
;; as well as direct or indirect size information for those types that require it. How that size
;; information is encoded varies by marker type.
;; Some values, such as boolean true, can be encoded within a single marker byte. Many small
;; integers (specifically between -16 and +127 inclusive) are also encoded within a single byte.
;; A number of marker bytes are reserved for future expansion of the format itself. These bytes
;; should not be used, and encountering them in an incoming stream should treated as an error.
;;
;;
;; SIZED VALUES
;;
;; Some value types require variable length representations and, as such, have their size
;; explicitly encoded. These values generally begin with a single marker byte, followed by a size,
;; followed by the data content itself. Here, the marker denotes both type and scale and therefore
;; determines the number of bytes used to represent the size of the data. The size itself is
;; either an 8-bit, 16-bit or 32-bit unsigned integer. Sizes longer than this are not yet
;; supported.
;; The diagram below illustrates the general layout for a sized value, here with a 16-bit size:
;;   Marker Size          Content
;;     <>  <--->  <--------------------->
;;     XX  XX XX  XX XX XX XX .. .. .. XX
;;

;; FIXME

(setf *read-default-float-format* 'double-float)

(defun packed-hex (x)
  (format nil "~{~2,'0X~^:~}" (coerce (packed x) 'list)))

(defun rpack (thing type)
  (reverse (packet:pack thing type)))

(defun runpack (thing type)
  (packet:unpack thing type))

(defun aappend (array1 array2)
  (let* ((l1 (length array1))
         (l2 (length array2))
         (array1 (adjust-array array1 (+ l1 l2))))
    (loop
      for i from 0 below l2
      do
         (setf (aref array1 (+ l1 i))
               (aref array2 i)))
    array1))

(defun ainsert (item array)
  (let* ((l (length array))
         (array (adjust-array (reverse array) (1+ l))))
    (setf (aref array l)
          item)
    (reverse array)))

(defun apad (array size)
  (reverse (adjust-array (reverse array) size)))

(defun make-array1 (item)
  "Make one-element-length array"
  (make-array 1
              :element-type '(unsigned-byte 8)
              :initial-element item))

;; PACKED - pack LIST, VECTOR, NUMBER, BOOLEAN, :FALSE, STRING to PackStream

(defgeneric packed (thing)
  (:documentation "Pack `THING' to PackStream array of bytes"))

(defmethod packed ((thing (eql t)))
  (make-array1 #xc3))

(defmethod packed ((thing (eql :false)))
  (make-array1 #xc2))

(defmethod packed ((thing null))
  (make-array1 #xc0))

(defmethod packed ((value integer))
  (cond
    ((and (<= #x-10 value)
          (< value #x80))
     (packet:pack value :int8))
    ((and (<= #x-80 value)
          (< value #x80))
     (ainsert #xc8 (rpack value :int8)))
    ((and (<= #x-8000 value)
          (< value #x8000))
     (ainsert #xc9 (rpack value :int16)))
    ((and (<= #x-80000000 value)
          (< value #x80000000))
     (ainsert #xca (rpack value :int32)))
    ((and (<= #x-8000000000000000 value)
          (< value #x8000000000000000))
     (ainsert #xcb (rpack value :int64)))
    (t (error "Integer `~D' out of packable range" value))))

(defmethod packed ((value float))
  (ainsert #xc1 (rpack value :double)))

(defmethod packed ((s string))
  (let* ((utf-8 (babel:string-to-octets s))
         (size (length utf-8)))
    (aappend (cond
               ((< size #x10)
                (rpack (+ #x80 size) :uint8))
               ((< size #x100)
                (ainsert #xd0 (rpack size :uint8)))
               ((< size #x10000)
                (ainsert #xd1 (rpack size :uint16)))
               ((< size #x100000000)
                (ainsert #xd1 (rpack size :uint32)))
               (t (error "String `~A' too long to pack" s)))
            utf-8)))

;; VECTOR is mapped to Bolt structure
(defmethod packed ((v vector))
  (let* ((signature (elt v 0))
         (fields (subseq v 1))
         (size (length fields)))
    (aappend (cond
               ((< size #x10)
                (rpack (+ #xb0 size) :uint8))
               ((< size #x100)
                (ainsert #xdc (rpack size :uint8)))
               ((< size #x10000)
                (ainsert #xdd (rpack size :uint16)))
               (t (error "Vector (structure) `~A' too long to pack" v)))
             (aappend (rpack signature :uint8)
                      (reduce #'aappend
                              (loop
                                for i from 0 below size
                                collect (packed (elt fields i))))))))

(defmethod packed ((l list))
  (let* ((size (length l)))
    (aappend (cond
               ((< size #x10)
                (rpack (+ #x90 size) :uint8))
               ((< size #x100)
                (ainsert #xd4 (rpack size :uint8)))
               ((< size #x10000)
                (ainsert #xd5 (rpack size :uint16)))
               ((< size #x100000000)
                (ainsert #xd6 (rpack size :uint32)))
               (t (error "List `~A' too long to pack" l)))
             (reduce #'aappend
                     (mapcar #'packed l)))))

;; HASH-TABLE is mapped to Bolt map
(defmethod packed ((ht hash-table))
  (let* ((size (hash-table-count ht))
         (keys-and-values nil))
    (maphash (lambda (k v)
               (push (cons k v) keys-and-values))
             ht)
    (aappend (cond
               ((< size #x10)
                (rpack (+ #xA0 size) :uint8))
               ((< size #x100)
                (ainsert #xd8 (rpack size :uint8)))
               ((< size #x10000)
                (ainsert #xd9 (rpack size :uint16)))
               ((< size #x100000000)
                (ainsert #xda (rpack size :uint32)))
               (t (error "Hash-table `~A' too long to pack" ht)))
             (when keys-and-values
               (reduce #'aappend
                       (mapcar (lambda (x)
                                 (aappend (packed (car x)) (packed (cdr x))))
                               keys-and-values))))))



;; UNPACKED - unpack PackTream to LIST, VECTOR, NUMBER, BOOLEAN, :FALSE or STRING

(defparameter +offset+
  '(:int8 1
    :int16 2
    :int32 4
    :int64 8
    :uint8 1
    :uint16 2
    :uint32 4
    :double 8))

(defun offset (type)
  (getf +offset+ type))


(defun unpack-number (type data)
  (packet:unpack (reverse (subseq data 0 (offset type))) type))


(defun unpack-string (data length)
  "Unpack string with marker-encoded length"
  (babel:octets-to-string (subseq data 0 length)))

(defun unpack-string* (data size-specifier-type)
  "Unpack string with explicite length"
  (let ((size (unpack-number size-specifier-type (subseq data 0 (offset size-specifier-type)))))
    (list (unpack-string (subseq data (offset size-specifier-type))
                         size)
          ;; 1 for marker, type offset for size specifier, size for string
          (+ 1 (offset size-specifier-type) size))))


(defun unpack-list (data items)
  (let ((offset 0)
        (collected nil))
    (dotimes (i items)
      (progn i)
      (destructuring-bind (items items-offset) (unpack data)
        (alexandria:appendf collected (list items))
        (setf data (subseq data items-offset)
              offset (+ offset items-offset))))
    (list collected (1+ offset))))

(defun unpack-list* (data size-specifier-type)
  "Unpack list with explicite length"
  (let ((size (unpack-number size-specifier-type (subseq data 0 (offset size-specifier-type)))))
    (destructuring-bind (items offset)
        (unpack-list (subseq data (offset size-specifier-type))
                     size)
      ;; Add size marker to offset
      (list items
            (1+ offset)))))


(defun unpack-ht (data items)
  (let ((offset 0)
        (ht (make-hash-table :test 'equal)))
    (dotimes (i items)
      (progn i)
      (let ((key (destructuring-bind (items items-offset) (unpack data)
                   (setf data (subseq data items-offset)
                         offset (+ offset items-offset))
                   items))
            (value (destructuring-bind (items items-offset) (unpack data)
                   (setf data (subseq data items-offset)
                         offset (+ offset items-offset))
                     items)))
        (setf (gethash key ht)
              value)))
    ;; Add 1 for hash-table size marker
    (list ht (1+ offset))))

(defun unpack-ht* (data size-specifier-type)
  "Unpack hash-table with explicite length"
  (let ((size (unpack-number size-specifier-type (subseq data 0 (offset size-specifier-type)))))
    (destructuring-bind (items offset)
        (unpack-ht (subseq data (offset size-specifier-type))
                   size)
      ;; Add size marker to offset
      (list items
            (1+ offset)))))


(defun unpack-vector (data items)
  (let ((offset 0)
        (collected nil))
    (dotimes (i (1+ items))
      (progn i)
      (destructuring-bind (items items-offset) (unpack data)
        (alexandria:appendf collected (list items))
        (setf data (subseq data items-offset)
              offset (+ offset items-offset))))
    (list collected (1+ offset))))

(defun unpack-vector* (data size-specifier-type)
  "Unpack vector with explicite length"
  (let ((size (unpack-number size-specifier-type (subseq data 0 (offset size-specifier-type)))))
    (destructuring-bind (items offset)
        (unpack-vector (subseq data (offset size-specifier-type))
                       size)
      ;; Add size marker to offset
      (list (apply #'vector items)
            (1+ offset)))))


(defun unpack (bytes)
  "Unpack BYTES. Return '(UNPACKED-THING OFFSET)"
  (let ((marker (elt bytes 0))
        (rest (subseq bytes 1)))
    (cond
      ;; booleans
      ((= marker #xc0) '(nil 1))
      ((= marker #xc2) '(:false 1))
      ((= marker #xc3) '(t 1))
      ((< marker #x80) `(,marker 1))
      ;; numbers
      ((>= marker #xf0) (list (- marker #x100) 1))
      ((= marker #xc8) (list (unpack-number :int8 rest) (1+ (offset :int8))))
      ((= marker #xc9) (list (unpack-number :int16 rest) (1+ (offset :int16))))
      ((= marker #xca) (list (unpack-number :int32 rest) (1+ (offset :int32))))
      ((= marker #xcb) (list (unpack-number :int64 rest) (1+ (offset :int64))))
      ((= marker #xc1) (list (unpack-number :double rest) (1+ (offset :double))))
      ;; strings
      ;;; string with size encoded in marker
      ((and (<= #x80 marker)
            (< marker #x90))
       (let ((length (logand marker #x0f)))
         (list (unpack-string rest length) (1+ length))))
      ;;; string with explicit size, and the size specifier is of size uint{8,16,32}
      ((= marker #xd0) (unpack-string* rest :uint8))
      ((= marker #xd1) (unpack-string* rest :uint16))
      ((= marker #xd2) (unpack-string* rest :uint32))
      ;; lists
      ;;; size in marker
      ((and (<= #x90 marker)
            (< marker #xA0))
       (let ((length (logand marker #x0f)))
         (unpack-list rest length)))
      ;;; list with explicite size
      ((= marker #xd4) (unpack-list* rest :uint8))
      ((= marker #xd5) (unpack-list* rest :uint16))
      ((= marker #xd6) (unpack-list* rest :uint32))
      ;; hash-tables (maps)
      ;;; size in marker
      ((and (<= #xa0 marker)
            (< marker #xb0))
       (let ((length (logand marker #x0f)))
         (unpack-ht rest length)))
      ;;; ht with explicite size
      ((= marker #xd8) (unpack-ht* rest :uint8))
      ((= marker #xd9) (unpack-ht* rest :uint16))
      ((= marker #xda) (unpack-ht* rest :uint32))
      ;; vectors (structs)
      ;;; size in marker
      ((and (<= #xb0 marker)
            (< marker #xc0))
       (let ((length (logand marker #x0f)))
         (unpack-vector rest length)))
      ;;; vectors with explicite size
      ((= marker #xdc) (unpack-vector* rest :uint8))
      ((= marker #xdd) (unpack-vector* rest :uint16))





      )))

(defun unpacked (bytes)
  (loop
    with unpacked = nil
    while (and bytes (> (length bytes) 0))
    do
       (destructuring-bind (thing offset) (unpack bytes)
         (alexandria:appendf unpacked (list thing))
         (setf bytes (subseq bytes offset)))
    finally (return unpacked)))

