;;; Interop
;;; Functions to convert between Matlisp tensors and CL arrays/lists.

(in-package #:matlisp)

(defgeneric as-tensor (object &optional field-type)
  (:documentation "Returns OBJECT as a Matlisp tensor type. May share storage or copy. The field-type is coerced to FIELD-TYPE, if it is non-NIL."))

(defmethod as-tensor ((object tensor) &optional field-type)
  "If already a tensor then just return."
  (if (null field-type)
      object
      (copy object (tensor field-type))))

(defmethod as-tensor ((object simple-vector) &optional field-type)
  "Simple vectors can be used as store for a tensor. 
   The resulting tensor will share storage with the vector."
  (make-instance (tensor (or field-type t))
                 :dimensions  (coerce (array-dimensions object) '(simple-array index-type (*)))
                 :store object))

(defmethod as-tensor ((object array) &optional field-type)
  "Convert array OBJECT to tensor by copying contents."
  (copy object
        (tensor (or field-type (array-element-type object)))))

(defmethod as-tensor ((object list) &optional field-type)
  "Convert list OBJECT to tensor by copying contents."
  (copy object
	(tensor (or field-type t))))
;;;

(defgeneric as-array (object)
  (:documentation "Returns OBJECT as a CL array type. May share storage or copy."))

(defmethod as-array ((object array))
  "If already an array, just return."
  object)

(defmethod as-array ((object tensor))
  "Make an array of the same dimensions as OBJECT. 
   If 1D, returned array shares storage with OBJECT."
  ;; If 1D tensor, return the store directly
  (when (= 1 (length (dimensions object)))
    (slot-value object 'store))
  ;; If > 1D, copy into array. Note: can't create displaced array
  ;; since element ordering in tensor is column-major but arrays are row-major
  (copy object 'array))

