(library (srfi :194 random-data-generators)
  (export clamp-real-number
	  ;;
	  current-random-source
	  with-random-source
	  ;;
	  make-random-integer-generator
	  make-random-u1-generator
	  make-random-u8-generator make-random-s8-generator
	  make-random-u16-generator make-random-s16-generator
	  make-random-u32-generator make-random-s32-generator
	  make-random-u64-generator make-random-s64-generator
	  make-random-boolean-generator
	  make-random-char-generator
	  make-random-string-generator
	  make-random-real-generator
	  make-random-rectangular-generator
	  make-random-polar-generator
	  ;;
	  make-bernoulli-generator
	  make-binomial-generator
	  make-categorical-generator
	  make-normal-generator
	  make-exponential-generator
	  make-geometric-generator
	  make-poisson-generator
	  make-zipf-generator
	  make-sphere-generator
	  make-ellipsoid-generator
	  make-ball-generator
	  ;;
	  make-random-source-generator
	  gsampling)

  (import (rnrs)
          (srfi :27 random-bits)
	  (only (srfi :39 parameters) make-parameter parameterize)
	  (only (srfi :133 vectors) vector-copy! vector-copy vector-every vector-fold)
	  (srfi :158 generators-and-accumulators)
          (srfi private include)
          (srfi private define-values))

  (include/resolve ("srfi" "%3a194") "srfi-194-impl.scm")
  (include/resolve ("srfi" "%3a194") "zipf-zri.scm")
  (include/resolve ("srfi" "%3a194") "sphere.scm"))
