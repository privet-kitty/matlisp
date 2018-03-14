;;; Interop
;;; Functions to convert between Matlisp tensors and CL arrays

(in-package #:matlisp)

(defgeneric as-tensor (object)
  (:documentation "Returns OBJECT as a Matlisp tensor type. May share storage or copy."))

(defmethod as-tensor ((object tensor))
  "If already a tensor then just return"
  object)

(defmethod as-tensor ((object simple-vector))
  "Simple vectors can be used as store for a tensor. 
   The resulting tensor will share storage with the vector."
  (make-instance (tensor (array-element-type object)) 
                 :dimensions  (coerce (array-dimensions object) '(simple-array index-type (*)))
                 :store object))

(defmethod as-tensor ((object array))
  "Convert array OBJECT to tensor by copying contents"
  (copy object
        (tensor (array-element-type object))))

;;;

(defgeneric as-array (object)
  (:documentation "Returns OBJECT as a CL array type. May share storage or copy. "))

(defmethod as-array ((object array))
  "If already an array, just return"
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

