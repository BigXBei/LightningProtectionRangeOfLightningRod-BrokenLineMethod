( princ "加载Lisp")

;直击雷保护范围图 Range map of direct lightning protection
( defun c:rdlp
	( / rinfo rresult);传入变量/局部变量

	;rinfo 存储避雷针信息，格式为：
	;（针数 保护高度 （（坐标） 高度 针号） （（坐标） 高度 针号） ...）
	( setq rinfo ( clr));选择避雷针

	( cdlpr rinfo);计算并绘制防雷保护范围
	( princ)
)

;选择避雷针 Choose lightning rod
( defun clr
	( / rnumber rinfo ritemp)

	( setq
		rnumber 1
		rinfo ( list ( list ( getpoint ( strcat "\n选取第" ( rtos rnumber 2 0) "根针（多针选择外围针）：")) ( getreal ( strcat "\n输入第" ( rtos rnumber 2 0) "根针高度（m）：")) ( getreal "输入此针编号：")))
	)

	( while
		( /= ritemp ( car ( last rinfo)))

		( setq
			rnumber ( 1+ rnumber)
			ritemp ( getpoint ( car ( car rinfo)) ( strcat "\n选取第" ( rtos rnumber 2 0) "根针（选取第1根针以结束）："))
		)

		( if
			( = ritemp ( car ( last rinfo)))
			( princ ( strcat "\n结束选取，共" ( rtos ( setq rnumber ( 1- rnumber)) 2 0) "根针。"))
			( setq rinfo ( cons ( cons ritemp ( cons ( getreal ( strcat "\n输入第" ( rtos rnumber 2 0) "根针高度（m）：")) ( list ( getreal "输入此针编号：")))) rinfo))
		)
	)

	( setq
		rinfo ( if ( dps rnumber rinfo) ( reverse rinfo) rinfo)
		rinfo ( cons ( getreal "输入被保护高度（m）：") rinfo)
		rinfo ( cons rnumber rinfo)
	)
)

;判断选点方向 Direction of point selection 
( defun dps
	( rnumber rinfo / sign rntemp atemp)

	( setq
		sign 0
		rntemp 0
		ritemp rinfo
	)

	( if
		( > rnumber 2);判断针数是否大于两根
		( progn
			( repeat
				rnumber
				( setq atemp ( - ( angle ( car ( nth rntemp ritemp)) ( if ( = ( 1+ rntemp) rnumber) ( car ( car ritemp)) ( car ( nth ( 1+ rntemp) ritemp)))) ( angle ( car ( nth rntemp ritemp)) ( if ( = rntemp 0) ( car ( car ( reverse ritemp))) ( car ( nth ( 1- rntemp) ritemp))))));计算当前点到下一点角度减当前点到上一点角度
				( if
					( or ( and ( > atemp ( * PI -1)) ( < atemp 0)) ( > atemp PI));角度差是否在（-π，0）∪（π，+∞）内
					( setq sign ( 1- sign));在范围内，逆时针,减一
					( setq sign ( 1+ sign));不在范围内，顺时针,加一
				)
				( setq rntemp ( 1+ rntemp))
			)

			( if
				( < sign 0);顺时针标志是否小于逆时针标志
				();输出逆时针
				( not ());输出顺时针
			)
		);大于两根针，判断方向
		( not ());小于三根针，输出顺时针
	)
)

;计算并绘制防雷保护范围 Calculate and draw the lightning protection range
( defun cdlpr
	( rinfo / temp scale rresult Dptemp DDptemp bxend pt1 pt2 x y z rx1temp rx2temp hxtemp rnumber rntemp ritemp h1temp P1temp h2temp P2temp Dtemp Dtemp atemp bxatemp bxlength bxtemp tangenta1 tangenta2 bxe1 bxe2 tpr1 tpr2 tpl1 tpl2 tptemp)
	
	( setq
		scale 1000
		scale ( getreal ( strcat "输入比例 1:<" ( itoa scale) ">："))
		scale ( if scale scale 1000)
		rnumber ( car rinfo)
	)

	( if
		( = rnumber 1);判断是否单针

		( progn
			( setq
				hxtemp ( cadr rinfo)
				h1temp ( cadr ( car ( cdr ( cdr rinfo))))
				tpr1 ( polar ( getpoint "选择计算表右上角点：") ( * ( / PI 2) 3) ( * scale 12))
				tpr2 ( polar tpr1 ( * ( / PI 2) 3) ( * scale 8))
				tpl1 ( polar tpr1 PI ( * scale 156))
				tpl2 ( polar tpl1 ( * ( / PI 2) 3) ( * scale 8))
			)

			( if
				( > h1temp 120)
				( setq P1temp 0.5)
				( if
					( > h1temp 30)
					( setq P1temp ( / 5.5 ( sqrt h1temp)))
					( setq P1temp 1)
				)
			);计算高度影响系数

			( if
				( < hxtemp ( * h1temp 0.5))
				( setq rx1temp ( * P1temp ( - ( * h1temp 1.5) ( * hxtemp 2))))
				( setq rx1temp ( * P1temp ( - h1temp hxtemp)))
			);计算保护范围半径

			( command "circle" "none" ( car ( caddr rinfo)) ( * rx1temp 1000));是单针，画圆

			( setq
				pt1 ( getpoint "选择保护范围上一点（引线标注点）：")
				pt2 ( getpoint pt1 "选择基线位置：")
			)

			( command "line" "none" pt1 "none" pt2 "");标注引线
			( command "line" "none" pt2 "none" ( polar pt2 ( if ( and ( < ( / PI 2) ( angle pt1 pt2)) ( < ( angle pt1 pt2) ( * ( / PI 2) 3))) PI 0) ( * scale ( if ( < hxtemp 10) 38 40))) "");标注引线
			( command "text" "j" ( if ( and ( < ( / PI 2) ( angle pt1 pt2)) ( < ( angle pt1 pt2) ( * ( / PI 2) 3))) "mr" "ml") "none" ( polar ( polar pt2 ( if ( and ( < ( / PI 2) ( angle pt1 pt2)) ( < ( angle pt1 pt2) ( * ( / PI 2) 3))) PI 0) ( * scale 5)) ( / PI 2) ( * scale 5)) ( * scale 5) 0 ( strcat ( rtos hxtemp 2 2) "m高度保护范围"));标注保护范围
		);单针，计算并画出范围

		( progn
			( setq
				temp 0
				rntemp 0
				hxtemp ( cadr rinfo)
				ritemp ( cdr ( cdr rinfo))
				bxend ( list)
				tpr1 ( polar ( getpoint "选择计算表右上角点：") ( * ( / PI 2) 3) ( * scale 12))
				tpr2 ( polar tpr1 ( * ( / PI 2) 3) ( * scale 8))
				tpl1 ( polar tpr1 PI ( * scale 156))
				tpl2 ( polar tpl1 ( * ( / PI 2) 3) ( * scale 8))
				tptemp ( polar tpl1 ( * ( / PI 2) 3) ( * scale 4))
			)

			( command "text" "j" "mc" "none" ( polar ( polar tpr1 PI ( * scale 73.5)) ( / PI 2) ( * scale 7)) ( * scale 5) 0 "多针保护计算结果（单位：m）");输入表格标题
			( command "line" "none" tpr1 "none" tpl1 "");画表头上方线
			( command "line" "none" tpr2 "none" tpl2 "");画表头下方线
			( repeat
				6
				( command "line" "none" ( polar tpr1 PI ( * scale 18 temp)) "none" ( polar tpr2 PI ( * scale 18 temp)) "")
				( setq temp ( 1+ temp))
			);画表头内侧线
			( setq temp 0)
			( repeat
				2
				( command "line" "none" ( polar ( polar tpr1 PI ( * scale 108)) PI ( * scale 9 temp)) "none" ( polar ( polar tpr2 PI ( * scale 108)) PI ( * scale 9 temp)) "")
				( setq temp ( 1+ temp))
			);画表头内侧线
			( setq temp 0)
			( repeat
				3
				( command "line" "none" ( polar ( polar tpr1 PI ( * scale 126)) PI ( * scale 15 temp)) "none" ( polar ( polar tpr2 PI ( * scale 126)) PI ( * scale 15 temp)) "")
				( setq temp ( 1+ temp))
			);画表头内侧线
			( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 7.5)) ( * scale 5) 0 "No.1");填写No.1
			( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 22.5)) ( * scale 5) 0 "No.2");填写No.2
			( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 34.5)) ( * scale 5) 0 "h1");填写h1
			( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 43.5)) ( * scale 5) 0 "h2");填写h2
			( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 57)) ( * scale 5) 0 "hx");填写hx
			( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 75)) ( * scale 5) 0 "rx1");填写rx1
			( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 93)) ( * scale 5) 0 "rx2");填写rx2
			( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 111)) ( * scale 5) 0 "D");填写D
			( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 129)) ( * scale 5) 0 "D'");填写D'
			( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 147)) ( * scale 5) 0 "bx");填写bx

			( repeat
				rnumber

				( setq
					pt1 ( car ( nth rntemp ritemp));避雷针1坐标
					h1temp ( cadr ( nth rntemp ritemp));避雷针1高度
					P1temp
						( if
							( <= h1temp 30)
							1
							( if
								( <= h1temp 120)
								( / 5.5 ( sqrt h1temp))
								0.5
							)
						);避雷针1高度影响系数
					rx1temp ( if
								( < hxtemp ( * h1temp 0.5))
								( * P1temp ( - ( * h1temp 1.5) ( * hxtemp 2)))
								( * P1temp ( - h1temp hxtemp))
							);避雷针1保护范围
					pt2
						( if
							( = rnumber ( 1+ rntemp));判断pt1是否是最后一点
							( car ( car ritemp));pt1是最后一点，pt2取第一点
							( car ( nth ( 1+ rntemp) ritemp));pt1不是最后一点，pt2取下一点
						);避雷针2坐标
					h2temp
						( if
							( = rnumber ( 1+ rntemp));判断pt1是否是最后一点
							( cadr ( car ritemp));pt1是最后一点，h2取第一点
							( cadr ( nth ( 1+ rntemp) ritemp));pt1不是最后一点，h2取下一点
						);避雷针2高度
					P2temp
						( if
							( <= h2temp 30)
							1
							( if
								( <= h2temp 120)
								( / 5.5 ( sqrt h2temp))
								0.5
							)
						);避雷针2高度影响系数
					rx2temp ( if
								( < hxtemp ( * h2temp 0.5))
								( * P2temp ( - ( * h2temp 1.5) ( * hxtemp 2)))
								( * P2temp ( - h2temp hxtemp))
							);避雷针2保护范围
					Dtemp
						( if
							( = h1temp h2temp);判断两针是否等高
							( distance pt1 pt2);等高，按两针间距
							( -
								( distance pt1 pt2)
								( if
									( < ( min h1temp h2temp) ( * ( max h1temp h2temp) 0.5))
									( * ( max P1temp P2temp) ( - ( * ( max h1temp h2temp) 1.5) ( * ( min h1temp h2temp) 2)) 1000)
									( * ( max P1temp P2temp) ( - ( max h1temp h2temp) ( min h1temp h2temp)) 1000)
								);计算保护范围半径
							);不等高,计算等效间距
						);两针等效间距
					x ( / ( / ( / Dtemp 1000) ( - ( min h1temp h2temp) hxtemp)) ( min P1temp P2temp));查表X
					y ( / hxtemp ( min h1temp h2temp));查表Y
					z ( fc x y);查表Z
					bxlength ( * z ( - ( min h1temp h2temp) hxtemp) ( min P1temp P2temp) 1000);求bx长度
					atemp ( angle pt1 pt2);求点到下一点角度
					bxatemp ( - atemp ( / PI 2));bx点的角度
					Dptemp ( polar pt1 atemp ( / ( distance pt1 pt2) 2));计算针间中点坐标
					DDptemp
						( if
							( = h1temp h2temp);判断两针是否等高
							( polar pt1 atemp ( / Dtemp 2));等高双针，求中点坐标
							( if
								( > h1temp h2temp);判断哪根针高
								( polar pt2 ( + atemp PI) ( / Dtemp 2));当前针高，取远点
								( polar pt1 atemp ( / Dtemp 2));当前针低，取近点
							);不等高双针，求等效间距中点
						);计算针间等效中点坐标
					bxtemp ( polar DDptemp bxatemp bxlength);求bx坐标
					tangenta1
						( +
							( atan
								( /
									( distance DDptemp bxtemp)
									( distance pt1 DDptemp)
								)
							)
							( atan
								( /
									( sqrt ( - ( expt ( distance pt1 bxtemp) 2) ( expt ( * rx1temp 1000) 2)))
									( * rx1temp 1000)
								)
							)
						);求pt1圆心-切点角度
					tangenta2
						( +
							( atan
								( /
									( distance DDptemp bxtemp)
									( distance pt2 DDptemp)
								)
							)
							( atan
								( /
									( sqrt ( - ( expt ( distance pt2 bxtemp) 2) ( expt ( * rx2temp 1000) 2)))
									( * rx2temp 1000)
								)
							)
						);求pt2圆心-切点角度
					bxe1 ( polar pt1 ( - atemp tangenta1) ( * rx1temp 1000));求bx在圆pt1上的切点坐标
					bxe2 ( polar pt2 ( + ( - atemp PI) tangenta2) ( * rx2temp 1000));求bx在圆pt2上的切点坐标
					bxend ( cons ( list bxe1 bxe2) bxend);存储bx结束点坐标
					rntemp ( 1+ rntemp);换下一针

					tpr1 tpr2
					tpr2 ( polar tpr1 ( * ( / PI 2) 3) ( * scale 8))
					tpl1 tpl2
					tpl2 ( polar tpl1 ( * ( / PI 2) 3) ( * scale 8))
					tptemp ( polar tpl1 ( * ( / PI 2) 3) ( * scale 4))
				)
				
				( command "line" "none" pt1 "none" pt2 "");画针间连线
				( command "line" "none" DDptemp "none" bxtemp "");画中点-bx连线
				( command "line" "none" bxtemp "none" bxe1 "");画切线1
				( command "line" "none" bxtemp "none" bxe2 "");画切线2
				( command "text" "j" "mc" "none" ( polar Dptemp ( + atemp ( / PI 2)) ( * scale 5)) ( * scale 5) ( * ( / ( if ( and ( < ( / PI 2) atemp) ( <= atemp ( * ( / PI 2) 3))) ( + atemp PI) atemp) PI) 180) ( strcat "D=" ( rtos ( / ( distance pt1 pt2) 1000) 2 2) "m" ));标注针间距离
				( command "text" "j" ( if ( and ( < ( / PI 2) bxatemp) ( <= bxatemp ( * ( / PI 2) 3))) "mr" "ml") "none" ( polar bxtemp bxatemp ( * scale 3.5)) ( * scale 5) ( * ( / ( if ( and ( < ( / PI 2) bxatemp) ( <= bxatemp ( * ( / PI 2) 3))) ( + bxatemp PI) bxatemp) PI) 180) ( strcat "bx=" ( rtos ( / bxlength 1000) 2 2) "m" ));标注bx
				
				( setq temp 0)
				( command "line" "none" tpr2 "none" tpl2 "");画表头下方线
				( repeat
					6
					( command "line" "none" ( polar tpr1 PI ( * scale 18 temp)) "none" ( polar tpr2 PI ( * scale 18 temp)) "")
					( setq temp ( 1+ temp))
				);画表头内侧线
				( setq temp 0)
				( repeat
					2
					( command "line" "none" ( polar ( polar tpr1 PI ( * scale 108)) PI ( * scale 9 temp)) "none" ( polar ( polar tpr2 PI ( * scale 108)) PI ( * scale 9 temp)) "")
					( setq temp ( 1+ temp))
				);画表头内侧线
				( setq temp 0)
				( repeat
					3
					( command "line" "none" ( polar ( polar tpr1 PI ( * scale 126)) PI ( * scale 15 temp)) "none" ( polar ( polar tpr2 PI ( * scale 126)) PI ( * scale 15 temp)) "")
					( setq temp ( 1+ temp))
				);画表头内侧线
				( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 7.5)) ( * scale 5) 0 ( rtos ( last ( nth ( 1- rntemp) ritemp)) 2 0));填写No.1
				( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 22.5)) ( * scale 5) 0 ( rtos ( if ( = rnumber rntemp) ( last ( car ritemp)) ( last ( nth rntemp ritemp))) 2 0));填写No.2
				( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 34.5)) ( * scale 5) 0 ( rtos h1temp 2 0));填写h1
				( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 43.5)) ( * scale 5) 0 ( rtos h2temp 2 0));填写h2
				( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 57)) ( * scale 5) 0 ( rtos hxtemp 2 2));填写hx
				( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 75)) ( * scale 5) 0 ( rtos rx1temp 2 2));填写rx1
				( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 93)) ( * scale 5) 0 ( rtos rx2temp 2 2));填写rx2
				( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 111)) ( * scale 5) 0 ( rtos ( / ( distance pt1 pt2) 1000) 2 2));填写D
				( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 129)) ( * scale 5) 0 ( rtos ( / Dtemp 1000) 2 2));填写D'
				( command "text" "j" "mc" "none" ( polar tptemp 0 ( * scale 147)) ( * scale 5) 0 ( rtos ( / bxlength 1000) 2 2));填写bx
			);画线段、标注、表格
			
			( setq
				rntemp 0
				bxend ( reverse bxend)
			)

			( repeat
				rnumber

				( command "arc" "none" ( cadr ( nth ( if ( = rntemp 0) ( 1- rnumber) ( 1- rntemp)) bxend)) "c" "none" ( car ( nth rntemp ritemp)) "none" ( car ( nth rntemp bxend)));画圆弧
				
				( setq rntemp ( 1+ rntemp))
			);画圆弧

			( setq
				pt1 ( getpoint "选择保护范围上一点（引线标注点）：")
				pt2 ( getpoint pt1 "选择基线位置：")
			)

			( command "line" "none" pt1 "none" pt2 "");标注引线
			( command "line" "none" pt2 "none" ( polar pt2 ( if ( and ( < ( / PI 2) ( angle pt1 pt2)) ( < ( angle pt1 pt2) ( * ( / PI 2) 3))) PI 0) ( * scale ( if ( < hxtemp 10) 38 40))) "");标注引线
			( command "text" "j" ( if ( and ( < ( / PI 2) ( angle pt1 pt2)) ( < ( angle pt1 pt2) ( * ( / PI 2) 3))) "mr" "ml") "none" ( polar ( polar pt2 ( if ( and ( < ( / PI 2) ( angle pt1 pt2)) ( < ( angle pt1 pt2) ( * ( / PI 2) 3))) PI 0) ( * scale 5)) ( / PI 2) ( * scale 5)) ( * scale 5) 0 ( strcat ( rtos hxtemp 2 2) "m高度保护范围"));标注保护范围
		);多针，计算并画出范围
	)

)

;MATLAB拟合曲线结果 Fit Curve
( defun fc
	( x y / z)

	( setq 
		p00 1.5893
		p10 -0.04888
		p01	-1.6186
		p20 -0.04591
		p11 -0.1346
		p02 1.988
		p30 0.008601
		p21 0.08857
		p12 0.2712
		p03 -3.2274
		p40 -0.0008377
		p31 -0.008016
		p22 -0.09213
		p13 -0.2109
		p04 4.4045
		p50 0.000002532
		p41 0.0003354
		p32 0.005813
		p23 0.01261
		p14 0.1045
		p05 -2.2725
		z ( +
			p00
			( * p10 x)
			( * p01 y)
			( * p20 ( expt x 2))
			( * p11 x y)
			( * p02 ( expt y 2))
			( * p30 ( expt x 3))
			( * p21 ( expt x 2) y)
			( * p12 x ( expt y 2))
			( * p03 ( expt y 3))
			( * p40 ( expt x 4))
			( * p31 ( expt x 3) y)
			( * p22 ( expt x 2) ( expt y 2))
			( * p13 x ( expt y 3))
			( * p04 ( expt y 4))
			( * p50 ( expt x 5))
			( * p41 ( expt x 4) y)
			( * p32 ( expt x 3) ( expt y 2))
			( * p23 ( expt x 2) ( expt y 3))
			( * p14 x ( expt y 4))
			( * p05 ( expt y 5))
		)
	)
)

( princ "成功。")
