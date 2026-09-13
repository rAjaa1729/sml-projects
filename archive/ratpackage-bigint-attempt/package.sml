use "rational.sig";

functor Rationalfunctor (BigInt : BIGINT) : RATIONAL =
struct

    exception rat_error
    type rational = BigInt.bigint * BigInt.bigint

    fun make_rat (x : BigInt.bigint, y : BigInt.bigint) =
        if (y="0") then NONE
        else
            let
                val check=BigInt.negative_check(x,y)
                val x_dup=BigInt.always_pos(x)
                val y_dup=BigInt.always_pos(y)
                val gd=BigInt.gcd(x_dup,y_dup)
                val p_x=BigInt.div_(x_dup,gd)
                val p_y=BigInt.div_(y_dup,gd)
                val xx= #1 p_x
                val yy= #1 p_y
            in
                if (check)
                then SOME (BigInt.to_neg(xx),yy)
                else SOME (xx,yy)
            end

    fun rat_make (x : BigInt.bigint, y : BigInt.bigint) =
        let
            val check=BigInt.negative_check(x,y)
            val x_dup=BigInt.always_pos(x)
            val y_dup=BigInt.always_pos(y)
            val gd=BigInt.gcd(x_dup,y_dup)
            val p_x=BigInt.div_(x_dup,gd)
            val p_y=BigInt.div_(y_dup,gd)
            val xx= #1 p_x
            val yy= #1 p_y
        in
            if (check)
            then (BigInt.to_neg(xx),yy)
            else (xx,yy)
        end

    fun rat (x : BigInt.bigint) = SOME (x,BigInt.to_bigint("1"))

    fun reci (x : BigInt.bigint) =
        if (x="0") then NONE
        else
            if (BigInt.negative_check(x,"1"))
            then SOME (BigInt.to_bigint("~1"),BigInt.to_pos(x))
            else SOME (BigInt.to_bigint("1"),x)

    fun neg (rt : rational) =
        let
            val p=rat_make(#1 rt,#2 rt)
            val x = #1 p
            val y = #2 p
        in
            if (BigInt.negative_check(x,y))
            then (BigInt.to_pos(x),y)
            else (BigInt.to_neg(x),y)
        end

    fun inverse (rt : rational) =
        let
            val p=rat_make(#1 rt,#2 rt)
            val x = #1 p
            val y = #2 p
        in
            if (x="0") then NONE
            else
                if (BigInt.negative_check(x,y))
                then SOME (BigInt.to_neg(y),BigInt.to_pos(x))
                else SOME (y,x)
        end

    fun inverse2 (rt : rational) =
        let
            val p=rat_make(#1 rt,#2 rt)
            val x = #1 p
            val y = #2 p
        in
            if (BigInt.negative_check(x,y))
            then (BigInt.to_neg(y),BigInt.to_pos(x))
            else (y,x)
        end

    fun equal (rt1 : rational, rt2 : rational) =
        let
            val rt1_dash=rat_make(#1 rt1,#2 rt1)
            val rt2_dash=rat_make(#1 rt2,#2 rt2)
            val x1= #1 rt1_dash
            val y1= #2 rt1_dash
            val x2= #1 rt2_dash
            val y2= #2 rt2_dash
        in
            if (x1=x2 andalso y1=y2)
            then true
            else false
        end

    fun less (rt1 : rational, rt2 : rational) =
        let
            val rt1_dash=rat_make(#1 rt1,#2 rt1)
            val rt2_dash=rat_make(#1 rt2,#2 rt2)
            val x1= #1 rt1_dash
            val y1= #2 rt1_dash
            val x2= #1 rt2_dash
            val y2= #2 rt2_dash
        in
            if equal(rt1,rt2)
            then false
            else if ((BigInt.is_neg2(x1) andalso not(BigInt.is_neg2(x2))))
            then true
            else if ((BigInt.is_neg2(x2) andalso not(BigInt.is_neg2(x1))))
            then false
            else if ((BigInt.is_neg2(x2) andalso (BigInt.is_neg2(x1))))
            then
                if (BigInt.great(BigInt.mult(BigInt.always_pos(x2),y1),BigInt.mult(BigInt.always_pos(x1),y2)))
                then false
                else true
            else
                if (BigInt.great(BigInt.mult(BigInt.always_pos(x2),y1),BigInt.mult(BigInt.always_pos(x1),y2)))
                then true
                else false
        end

    fun add (rt1 : rational, rt2 : rational) =
        let
            val x1= #1 rt1
            val y1= #2 rt1
            val x2= #1 rt2
            val y2= #2 rt2
            val den=BigInt.mult(y1,y2)
        in
            if ((BigInt.is_neg2(x1)) andalso (BigInt.is_neg2(x2)))
            then
                let
                    val xx1=BigInt.always_pos(x1)
                    val xx2=BigInt.always_pos(x2)
                    val num=BigInt.add(BigInt.mult(xx1,y2),BigInt.mult(xx2,y1))
                in
                    rat_make(BigInt.concat("~",num),den)
                end
            else if ((not(BigInt.is_neg2(x1)) andalso (not(BigInt.is_neg2(x2)))))
            then
                let
                    val num=BigInt.add(BigInt.mult(x1,y2),BigInt.mult(x2,y1))
                in
                    rat_make(num,den)
                end
            else if ((BigInt.is_neg2(x1) andalso (not(BigInt.is_neg2(x2)))))
            then
                let
                    val xx1=BigInt.always_pos(x1)
                    val num=BigInt.sub(BigInt.mult(x2,y1),BigInt.mult(xx1,y2))
                in
                    rat_make(num,den)
                end
            else
                let
                    val xx2=BigInt.always_pos(x2)
                    val num=BigInt.sub(BigInt.mult(x1,y2),BigInt.mult(xx2,y1))
                in
                    rat_make(num,den)
                end
        end

    fun subtract (rt1 : rational, rt2 : rational) = add(rt1,neg(rt2))

    fun multiply (rt1 : rational, rt2 : rational) =
        let
            val x1= #1 rt1
            val y1= #2 rt1
            val x2= #1 rt2
            val y2= #2 rt2
            val check=BigInt.negative_check(x1,x2)
            val num1=BigInt.mult(BigInt.always_pos(x1),BigInt.always_pos(x2))
            val num2=BigInt.mult(y1,y2)
        in
            if (check)
            then neg(rat_make(num1,num2))
            else rat_make(num1,num2)
        end

    fun divide (rt1 : rational, rt2 : rational) = 
        (rat_make(multiply(rt1,inverse2(rt2))))

    fun showRat (rt : rational) =
        let
            val p=rat_make(#1 rt,#2 rt)
            val x = #1 p
            val y = #2 p
        in
            BigInt.to_str(x)^"/"^BigInt.to_str(y)
        end

    fun showDecimal (rt : rational) =
        let
            val p=rat_make(#1 rt,#2 rt)
            val x = #1 p
            val y = #2 p
            val check=BigInt.negative_check(x,y)
            val xx=BigInt.always_pos(x)
            val yy=BigInt.always_pos(y)
        in
            if (check)
            then "~"^BigInt.fin_dec(xx,yy)
            else BigInt.fin_dec(xx,yy)
        end

    fun fromDecimal (s : string) =
        let
            val x=if substring(s,0,1)="." then "0"^s else s
        in
            if (hd(explode(x))= #"~" )
            then neg(rat_make(BigInt.fin_rat(implode(tl(explode(x))))))
            else rat_make(BigInt.fin_rat(x))
        end

    fun toDecimal (rt : rational) = showDecimal(rt)



    fun rat_funadd(s1:string,s2:string) =
        let
            val rt1=fromDecimal(s1)
            val rt2=fromDecimal(s2)
            val x=add(rt1,rt2)
            val y=showDecimal(x)
        in
            y
        end

    fun rat_subfun(s1:string,s2:string) =
        let
            val rt1=fromDecimal(s1)
            val rt2=fromDecimal(s2)
            val x=subtract(rt1,rt2)
            val y=showDecimal(x)
        in
            y
        end

    fun rat_mulfun(s1:string,s2:string) =
        let
            val rt1=fromDecimal(s1)
            val rt2=fromDecimal(s2)
            val x=multiply(rt1,rt2)
            val y=showDecimal(x)
        in
            y
        end

    fun rat_divfun(s1:string,s2:string) =
        let
            val rt1=fromDecimal(s1)
            val rt2=fromDecimal(s2)
            val x=divide(rt1,rt2)
            val y=showDecimal(x)
        in
            y
        end

    fun bigint_add(s1:string,s2:string) =
        let
            val rt1=fromDecimal(s1)
            val rt2=fromDecimal(s2)
            val x=add(rt1,rt2)
            val y=showDecimal(x)
        in
            y
        end

    fun sub_bigint(s1:string,s2:string) =
        let
            val rt1=fromDecimal(s1)
            val rt2=fromDecimal(s2)
            val x=subtract(rt1,rt2)
            val y=showDecimal(x)
        in
            y
        end

    fun mul_bigint(s1:string,s2:string) =
        let
            val rt1=fromDecimal(s1)
            val rt2=fromDecimal(s2)
            val x=multiply(rt1,rt2)
            val y=showDecimal(x)
        in
            y
        end

    fun divide_bigint(s1:string,s2:string) =
        BigInt.divd(s1,s2)

    fun modulus_bigint(s1:string,s2:string) =
        BigInt.modd(s1,s2)

    fun rat_lessthanfun(s1:string,s2:string) =
        let
            val x1=fromDecimal(s1)
            val x2=fromDecimal(s2)
        in
            if less(x1,x2)=true then "true" else "false"
        end

    fun rat_greaterthaneqfun(s1:string,s2:string) =
        if rat_lessthanfun(s1,s2)="false"
        then "true"
        else "false"

    fun rat_lessthaneqfun(s1:string,s2:string) =
        let
            val rt1=fromDecimal(s1)
            val rt2=fromDecimal(s2)
        in
            if equal(rt1,rt2) then "true"
            else if rat_lessthanfun(s1,s2)="true" then "true"
            else "false"
        end

    fun rat_greaterthanfun(s1:string,s2:string) =
        if rat_lessthaneqfun(s1,s2)="false"
        then "true"
        else "false"

    fun rat_noteqfun(s1:string,s2:string) =
        if s1="true" orelse s1="false" then if ((s1="true" andalso s2="true") orelse (s1="false" andalso s2="false")) then "true" else "false"
        else
            let
                val rt1=fromDecimal(s1)
                val rt2=fromDecimal(s2)
            in
                if equal(rt1,rt2) then "true"
                else "false"
            end

    fun rat_notfun(s1:string,s2:string) =
        if rat_noteqfun(s1,s2)="false"
        then "true"
        else "false"


end

structure Rational=Rationalfunctor(BigInt)