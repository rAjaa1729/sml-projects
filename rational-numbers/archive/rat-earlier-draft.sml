use "rat.sig";

functor Rational (BigInt : BIGINT ) : RATIONAL =
struct
    type rational = BigInt.bigint * BigInt.bigint
    exception rat_error
    (* in gcd we always take positive bigint *)

    fun Assign_sign(tup:string*string)=
        if((#1 tup)="~" andalso (#2 tup)="~") then ""
        else if((#1 tup)="+" andalso (#2 tup)="+") then ""
        else "~";


    fun gcd(a:BigInt.bigint,b:BigInt.bigint)=
        if (size (BigInt.trim_zero(a))=0 ) then b
        else if(size (BigInt.trim_zero(b))=0 ) then a
        else gcd(b,BigInt.rem(a,b))
    
    fun stan_rat(a:BigInt.bigint,b:BigInt.bigint)=
        let
            val a=BigInt.stan_bigint(a)
            val b=BigInt.stan_bigint(b)
            val tup = (substring(a,0,1),substring(b,0,1))
            val sign=Assign_sign(tup)
            val a=BigInt.abs(a)
            val b=BigInt.abs(b)
        in  
            (a,b,sign)
        end;


    fun make_rat(a:BigInt.bigint,b:BigInt.bigint)=
        if ((size(a)=0))
        then ("0","1")
        else if (size(a)=1 andalso (substring(a,0,1)= "~"))
        then ("~0","1")=
        else
        let
            val tup= stan_rat(a,b)
            val cf=gcd((#1 tup),(#2 tup))
            val a=BigInt.Divide((#1 tup),cf)
            val b=BigInt.Divide((#2 tup),cf)
        in
            ((#3 tup)^a,b)
        end;
    
    fun rat(a:BigInt.bigint)=(a,"1");

    fun reci(a:BigInt.bigint)=
        let
            val a=BigInt.stan_bigint(a)
            val sign=substring(a,0,1)
            val a = substring(a,1,(size a)-1)
            val sign= if(sign="~") then sign else ""

        in
            (sign^"1",a)
        end;
    
    fun neg((a:BigInt.bigint,b:BigInt.bigint))=
        let
            val tup=  stan_rat(a,b)
            val sign= if((#3 tup)="~") then "" else "~"
        in
            (sign^(#1 tup),(#2 tup))
        end;
    
    fun inverse((a:BigInt.bigint,b:BigInt.bigint))=
        let 
            val tup=  stan_rat(a,b)
        in
            ((#3 tup)^(#2 tup),(#1 tup))
        end;
    fun equal((a:BigInt.bigint,b:BigInt.bigint),(x:BigInt.bigint,y:BigInt.bigint))=
        let 
            val tup1= make_rat((a,b))
            val tup2= make_rat((x,y))
        in
            (((#1 tup1)=(#1 tup2)) andalso ((#2 tup1)=(#2 tup2)))
        end;
    fun less((a:BigInt.bigint,b:BigInt.bigint),(x:BigInt.bigint,y:BigInt.bigint))=
        let 
            val tup1= make_rat((a,b))
            val tup2= make_rat((x,y))
            val sign1= if(substring((#1 tup1),0,1)="~") then "~" else ""
            val sign2= if(substring((#1 tup2),0,1)="~") then "~" else ""
            fun  check(a,b,x,y)=
                let
                    val a=BigInt.abs(a)
                    val x=BigInt.abs(x)
                    val p=BigInt.mul(a,b)
                    val q=BigInt.mul(x,y)
                in
                    (not (BigInt.compare(q,p)))
                end;
        in
            if(sign1="~" andalso sign2="") then true
            else if(sign1="" andalso sign2="~") then false
            else check((#1 tup1),(#2 tup1),(#1 tup2),(#2 tup2))
        end;
    fun add((a:BigInt.bigint,b:BigInt.bigint),(x:BigInt.bigint,y:BigInt.bigint))=
        let
            val num1= BigInt.mul(a,y)
            val num2= BigInt.mul(b,x)
            val den=BigInt.mul(b,y)
            val num=BigInt.Add(num1,num2)
        in
            make_rat(num,den)
            (* (num1,"1") *)
        end;

    fun subtract((a:BigInt.bigint,b:BigInt.bigint),(x:BigInt.bigint,y:BigInt.bigint))=
        let
            val num1= BigInt.mul(a,y)
            val num2= BigInt.mul(b,x)
            val den=BigInt.mul(b,y)
            val num=BigInt.sub(num1,num2)
        in
            make_rat(num,den)
        end;
    
    fun multiply((a:BigInt.bigint,b:BigInt.bigint),(x:BigInt.bigint,y:BigInt.bigint))=
        let
            val den=BigInt.mul(b,y)
            val num=BigInt.mul(a,x)
        in
            make_rat(num,den)
        end;
    
    fun divide((a:BigInt.bigint,b:BigInt.bigint),(x:BigInt.bigint,y:BigInt.bigint))=
        multiply((a,b),(y,x))

    fun showRat((a:BigInt.bigint,b:BigInt.bigint))=
        let
            val tup=make_rat((a,b))
        in
            ((BigInt.inString(#1 tup))^"/"^(BigInt.inString(#2 tup)))
        end;


    fun producedecimal(decimalpart,pos)=
        let 
            val pre=substring(decimalpart,0,pos)
            val recur=substring(decimalpart,pos,(size decimalpart)-pos)
        in
            (pre^"("^recur^")")
        end;


    fun trackremainder(list_remainder,remainder,pos)=
        if(list_remainder=[]) then ("notrecur",pos)
        else if(BigInt.trim_zero(remainder) ="") then ("terminated",pos)
        else if(hd(list_remainder)=remainder) then ("recur",pos)
        else trackremainder(tl(list_remainder),remainder,pos+1)

    fun trackrecursion(remainder,divisor,list_remainder,decimalpart)=
        let
            val newq=BigInt.Divide(remainder^"0",divisor)
            val remainder=BigInt.rem(remainder^"0",divisor)
            val tup=trackremainder(list_remainder,remainder,0)
            val check = (#1 tup)
            val pos = (#2 tup)
            val decimalpart=decimalpart^newq
        in
            if(check="notrecur") then trackrecursion(remainder,divisor,(list_remainder@[remainder]),decimalpart)
            else if(check="terminated") then decimalpart
            else   producedecimal(decimalpart,pos)
        end;


    fun showDecimal((a:BigInt.bigint,b:BigInt.bigint))=
        let
            val tup=make_rat((a,b))
            val sign= if(substring((#1 tup),0,1)="~") then "~" else ""
            val a=BigInt.abs((#1 tup))
            val intpart=BigInt.trim_zero(BigInt.Divide(a,(#2 tup)))
            val remainder=BigInt.rem(a,(#2 tup))
            val decimal=trackrecursion(remainder,(#2 tup),[remainder],"")
        in
            (sign^intpart^"."^decimal)
        end;

    fun createdeno(l1,l2)=
        let
            fun mu1(l1,a)= if(l1=0) then a else mu1(l1-1,a^"0")
            fun mu2(l2,b)= if(l2=0) then b else mu2(l2-1,b^"9")
            val p= if(l1=0 andalso l2=0) then "1"
                    else if (l1=0) then mu2(l2,"")
                    else if(l2=0) then mu1(l1,"1")
                    else (mu2(l2,"")^mu1(l1,""))
        in
            p
        end;

    fun create_non_recur(a:string)=
        let
            fun intpart(a,b)=
                if(hd(a) = #".") then ((implode b),implode(tl(a)))
                else intpart(tl(a),b@[hd(a)])
            val tup=intpart(explode a,[])
            
            val integer=(#1 tup)
            val decimal=(#2 tup)
            fun mu1(l1,a)= if(l1=0) then a else mu1(l1-1,a^"0")
        in
            make_rat(integer^decimal,mu1(size decimal,"1"))

        end;

    fun create_recur(a)=
        let
            
            fun sep_rec_nonrec(a,b)=
                if(hd(a) = #"(") then (implode b,implode (tl(a)))
                else sep_rec_nonrec(tl(a),b@[hd(a)])
            
            fun sizl1(a,l1)=
                if(hd(a)= #".") then l1
                else sizl1(tl(a),l1+1)

            val tup= sep_rec_nonrec(explode a,[])
            val l2= (size (#2 tup))-1
            val l1= sizl1(rev(explode (#1 tup)),0)
            
            val part1=create_non_recur((#1 tup))
            val part2= substring((#2 tup),0,l2)
            val den= createdeno(l1,l2)

            val recur=make_rat(part2,den)

            val sign= if(substring((#1 part1),0,1)="~") then "~" 
                        else ""
        in
            if(sign = "~") then subtract(part1,recur)
            else add(part1,recur)
            (* part1 *)
        end;


    fun fromDecimal(a:string)=
        let
            val s=size a
        in
            if(substring(a,s-1,1)=")") then create_recur(a)
                else create_non_recur(a)
        end;


    
    fun toDecimal((a:BigInt.bigint,b:BigInt.bigint))=showDecimal((a,b))

end;

structure temp=Rational(BigInt)

(* 
val it10=temp.showDecimal("~1","3")
val it9=temp.showDecimal("~4567","234")
val k=temp.fromDecimal(temp.showDecimal("~465","1"))
val k2=temp.fromDecimal(temp.showDecimal("~4567","100"))
val k3=temp.fromDecimal(temp.showDecimal("~0","443267863")) *)
val ss=temp.showDecimal("~1","109")
val k4=temp.fromDecimal(temp.showDecimal("~1","109"))
(* val k5=temp.fromDecimal(temp.showDecimal("~235","978")) *)