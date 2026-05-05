(defstruct bvh-node
  minx miny minz
  maxx maxy maxz
  left right
  objects)

(defun sphere-bbox (s)
  (let* ((c (sphere-center s))
	 (r (sphere-radius s)))
    (values (- (x c) r) (- (y c) r) (- (z c) r)
	    (+ (x c) r) (+ (y c) r) (+ (z c) r))))

(defun surface-bbox (s)
  (typecase s
    (sphere (sphere-bbox s))))

(defun bvh-merge-bbox (ax ay az bx by bz cx cy cz dx dy dz)
  (values (min ax bx cx dx)
	  (min ay by cy dy)
	  (min az bz cz dz)
	  (max ax bx cx dx)
	  (max ay by cy dy)
	  (max az bz cz dz)))

(defun bvh-bounds-of-objects (objects)
  (let (minx miny minz maxx maxy maxz)
    (dolist (s objects)
      (multiple-value-bind (a b c d e f) (surface-bbox s)
	(if (null minx)
	    (setf minx a miny b minz c maxx d maxy e maxz f)
	    (setf minx (min minx a)
		  miny (min miny b)
		  minz (min minz c)
		  maxx (max maxx d)
		  maxy (max maxy e)
		  maxz (max maxz f)))))
    (values minx miny minz maxx maxy maxz)))

(defun bvh-surface-center-axis (s axis)
  (let ((c (sphere-center s)))
    (ecase axis
      (0 (x c))
      (1 (y c))
      (2 (z c)))))

(defun build-bvh (objects)
  (multiple-value-bind (minx miny minz maxx maxy maxz)
      (bvh-bounds-of-objects objects)
    (let* ((count (length objects))
	   (ex (- maxx minx))
	   (ey (- maxy miny))
	   (ez (- maxz minz))
	   (axis (cond ((and (>= ex ey) (>= ex ez)) 0)
		       ((>= ey ez) 1)
		       (t 2))))
      (if (<= count 4)
	  (make-bvh-node :minx minx :miny miny :minz minz
			 :maxx maxx :maxy maxy :maxz maxz
			 :objects objects)
	  (let* ((sorted (stable-sort (copy-list objects) #'<
				     :key (lambda (s) (bvh-surface-center-axis s axis))))
		 (mid (ash count -1))
		 (left-objects (subseq sorted 0 mid))
		 (right-objects (subseq sorted mid))
		 (left (build-bvh left-objects))
		 (right (build-bvh right-objects)))
	    (make-bvh-node :minx minx :miny miny :minz minz
			   :maxx maxx :maxy maxy :maxz maxz
			   :left left :right right))))))

(defun rebuild-bvh ()
  (setf *bvh-world* *world*
	*bvh-root* (and *world* (build-bvh *world*))
	*surface-id* (make-hash-table :test #'eq))
  (let ((i 0))
    (dolist (s *world*)
      (setf (gethash s *surface-id*) i)
      (incf i)))
  *bvh-root*)

(defun ensure-bvh ()
  (when (not (eq *bvh-world* *world*))
    (rebuild-bvh))
  *bvh-root*)

(defun ray-aabb-range (pt xr yr zr node)
  (labels ((slab (p d min max)
	     (if (zerop d)
		 (if (and (>= p min) (<= p max))
		     (values most-negative-double-float most-positive-double-float)
		     (values nil nil))
		 (let* ((inv (/ 1.0d0 d))
			(t1 (* (- min p) inv))
			(t2 (* (- max p) inv)))
		   (if (< t1 t2)
		       (values t1 t2)
		       (values t2 t1))))))
    (multiple-value-bind (tx1 tx2) (slab (x pt) xr (bvh-node-minx node) (bvh-node-maxx node))
      (when tx1
      (multiple-value-bind (ty1 ty2) (slab (y pt) yr (bvh-node-miny node) (bvh-node-maxy node))
        (when ty1
        (multiple-value-bind (tz1 tz2) (slab (z pt) zr (bvh-node-minz node) (bvh-node-maxz node))
          (when tz1
            (let ((tmin (max tx1 ty1 tz1))
			  (tmax (min tx2 ty2 tz2)))
		      (when (<= tmin tmax)
			(values tmin tmax)))))))))))

(defun bvh-first-hit (pt xr yr zr)
  (let ((best-s nil)
	(best-t nil)
	(best-id nil)
	(stack (make-array 64 :adjustable t :fill-pointer 0)))
    (when *bvh-root*
      (vector-push-extend *bvh-root* stack))
    (loop while (> (fill-pointer stack) 0) do
      (let ((node (vector-pop stack)))
        (multiple-value-bind (tmin tmax) (ray-aabb-range pt xr yr zr node)
          (declare (ignore tmax))
          (when (and tmin (or (null best-t) (< tmin best-t)))
            (let ((objs (bvh-node-objects node)))
              (if objs
                  (dolist (s objs)
                    (let ((tt (intersect s pt xr yr zr)))
                      (when tt
                        (let ((sid (gethash s *surface-id*)))
                          (when (or (null best-t)
                                    (< tt best-t)
                                    (and best-id (= tt best-t) (< sid best-id)))
                            (setf best-s s
                                  best-t tt
                                  best-id sid))))))
                  (let ((l (bvh-node-left node))
                        (r (bvh-node-right node)))
                    (cond
                      ((and l r)
                       (multiple-value-bind (ltmin ltmax) (ray-aabb-range pt xr yr zr l)
                         (declare (ignore ltmax))
                         (multiple-value-bind (rtmin rtmax) (ray-aabb-range pt xr yr zr r)
                           (declare (ignore rtmax))
                           (cond
                             ((and ltmin rtmin)
                              (if (< ltmin rtmin)
                                  (progn (vector-push-extend r stack) (vector-push-extend l stack))
                                  (progn (vector-push-extend l stack) (vector-push-extend r stack))))
                             (ltmin (vector-push-extend l stack))
                             (rtmin (vector-push-extend r stack))))))
                      (l (vector-push-extend l stack))
                      (r (vector-push-extend r stack))))))))))
    (values best-s best-t)))

(defun bvh-blocked-to-light (pt xr yr zr light-dist ignore-surface)
  (let ((stack (make-array 64 :adjustable t :fill-pointer 0)))
    (when *bvh-root*
      (vector-push-extend *bvh-root* stack))
    (loop
	while (> (fill-pointer stack) 0)
	for node = (vector-pop stack)
	do
	  (multiple-value-bind (tmin tmax) (ray-aabb-range pt xr yr zr node)
	    (when (and tmin (< tmin light-dist) (> tmax 0.05))
	      (let ((objs (bvh-node-objects node)))
		(if objs
		    (dolist (s objs)
		      (unless (eq s ignore-surface)
			(let ((tt (intersect s pt xr yr zr)))
			  (when (and tt (> tt 0.05) (< tt light-dist))
			    (return-from bvh-blocked-to-light t)))))
		    (progn
		      (when (bvh-node-left node) (vector-push-extend (bvh-node-left node) stack))
		      (when (bvh-node-right node) (vector-push-extend (bvh-node-right node) stack))))))))
    nil))

(defun vogel-offsets (radius samples)
  (when (or (null *vogel-cache-dxs*)
	    (null *vogel-cache-dzs*)
	    (not (eql *vogel-cache-samples* samples))
	    (not (eql *vogel-cache-radius* radius)))
    (setf *vogel-cache-samples* samples
	  *vogel-cache-radius* radius
	  *vogel-cache-dxs* (make-array samples)
	  *vogel-cache-dzs* (make-array samples))
    (dotimes (i samples)
      (let* ((golden-angle 2.399963229728653)
	     (r (* radius (sqrt (/ (+ i 0.5) samples))))
	     (theta (* i golden-angle))
	     (dx (* r (cos theta)))
	     (dz (* r (sin theta))))
	(setf (aref *vogel-cache-dxs* i) dx
	      (aref *vogel-cache-dzs* i) dz))))
  (values *vogel-cache-dxs* *vogel-cache-dzs*))
