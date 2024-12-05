(defun C:ZCD (/ CURVE TLEN SS N SUMLEN)(vl-load-com)
	(setq
		SUMLEN 0
		SS (ssget '((0 . "CIRCLE,ELLIPSE,LINE,*POLYLINE,SPLINE,ARC")))
		N 0
	)

	(repeat
		( sslength SS)
		( setq
			CURVE (vlax-ename->vla-object (ssname SS N))
			TLEN (vlax-curve-getdistatparam CURVE (vlax-curve-getendparam CURVE))
			SUMLEN (+ SUMLEN TLEN)
			N (1+ N)
		)
	)

	(princ (strcat "\n共选择 " (itoa (sslength SS)) " 条线段。 线段总长： " (rtos SUMLEN 2 3) " 。"))

	(princ)
)
