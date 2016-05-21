(in-package #:matlisp-tests)

(matlisp::generate-rand srandn single-float (matlisp::draw-standard-normal-single))
(matlisp::generate-rand crandn (complex single-float) (complex (matlisp::draw-standard-normal-single) (matlisp::draw-standard-normal-single)))
(matlisp::generate-rand zrandn (complex double-float) (complex (matlisp::draw-standard-normal) (matlisp::draw-standard-normal)))
