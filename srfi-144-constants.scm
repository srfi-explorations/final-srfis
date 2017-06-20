;;;; mathh-constants.scm
;;; Common flonum constants in <math.h> + more
;;; The values are derived from the Chicken mathh.egg, but are not subject to copyright.


;;; Flonum Constants

;; The precision is greater than those found in <math.h>
;; but in practice not perfectly representable

(define-constant fl-e            2.7182818284590452353602874713526624977572)   ; e
(define-constant fl-1/e          0.3678794411714423215955237701614608674458)   ; 1/e
(define-constant fl-e-2          7.3890560989306502272304274605750078131803)   ; e^2
(define-constant fl-e-pi/4       2.1932800507380154565597696592787382234616)   ; e^(pi/4)
(define-constant fl-log2-e       1.44269504088896340735992468100189214)        ; log_2(e)
(define-constant fl-log10-e      0.434294481903251827651128918916605082)       ; log_10(e)
(define-constant fl-log-2        0.6931471805599453094172321214581765680755)   ; ln(2)
(define-constant fl-1/log-2      1.4426950408889634073599246810018921374266)   ; 1/ln(2)
(define-constant fl-log-3        1.0986122886681096913952452369225257046475)   ; ln(3)
(define-constant fl-log-pi       1.1447298858494001741434273513530587116473)   ; ln(pi)
(define-constant fl-log-10       2.3025850929940456840179914546843642076011)   ; ln(10)
(define-constant fl-1/log-10     0.4342944819032518276511289189166050822944)   ; 1/ln(10)
(define-constant fl-pi           3.1415926535897932384626433832795028841972)   ; pi
(define-constant fl-1/pi         0.3183098861837906715377675267450287240689)   ; 1/pi
(define-constant fl-2pi          6.2831853071795862319959269370883703231812)   ; pi * 2
(define-constant fl-pi/2         1.57079632679489661923132169163975144)        ; pi/2
(define-constant fl-2/pi         0.636619772367581343075535053490057448)       ; 2/pi
(define-constant fl-pi/4         0.785398163397448309615660845819875721)       ; pi/4
(define-constant fl-2/sqrt-pi    1.12837916709551257389615890312154517)        ; 2/sqrt(pi)
(define-constant fl-sqrt-pi      1.7724538509055160272981674833411451827975)   ; sqrt(pi)
(define-constant fl-pi-squared   9.8696044010893586188344909998761511353137)   ; pi^2
(define-constant fl-degree       0.0174532925199432957692369076848861271344)   ; pi/180
(define-constant fl-gamma-1/2    1.7724538509055160272981674833411451827975)   ; gamma(1/2)
(define-constant fl-gamma-1/3    2.6789385347077476336556929409746776441287)   ; gamma(1/3)
(define-constant fl-gamma-2/3    1.3541179394264004169452880281545137855193)   ; gamma(2/3)
(define-constant fl-sqrt-2       1.4142135623730950488016887242096980785697)   ; sqrt(2)
(define-constant fl-sqrt-3       1.7320508075688772935274463415058723669428)   ; sqrt(3)
(define-constant fl-sqrt-5       2.2360679774997896964091736687312762354406)   ; sqrt(5)
(define-constant fl-sqrt-10      3.1622776601683793319988935444327185337196)   ; sqrt(10)
(define-constant fl-cbrt-2       1.2599210498948731647672106072782283505703)   ; cubert(2)
(define-constant fl-cbrt-3       1.4422495703074083823216383107801095883919)   ; cubert(3)
(define-constant fl-4thrt-2      1.1892071150027210667174999705604759152930)   ; fourthrt(2)
(define-constant fl-1/sqrt-2     0.7071067811865475244008443621048490392848)   ; 1/sqrt(2)
(define-constant fl-phi          1.6180339887498948482045868343656381177203)   ; phi
(define-constant fl-log-phi      0.4812118250596034474977589134243684231352)   ; ln(phi)
(define-constant fl-1/log-phi    2.0780869212350275376013226061177957677422)   ; 1/ln(phi)
(define-constant fl-euler        0.5772156649015328606065120900824024310422)   ; euler
(define-constant fl-e-euler      1.7810724179901979852365041031071795491696)   ; e^euler
(define-constant fl-sin-1        0.8414709848078965066525023216302989996226)   ; sin(1)
(define-constant fl-cos-1        0.5403023058681397174009366074429766037323)   ; cos(1)
