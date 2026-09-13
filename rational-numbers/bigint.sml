use "BIGINT.sig";
(* structure of bigint *)

structure BigInt : BIGINT =
struct
    type bigint = string
    
    exception BigintDivideByZero

    fun inString(a:bigint)=a

    fun tobigint(a:string)=a
    
    fun Assign_sign(a,b)=
        if(a="~" andalso b="~") then ""
        else if(a="+" andalso b="+") then ""
        else "~"

    fun stan_bigint(a:bigint)=
        if(size a = 0) then "+0"
        else if(substring(a,0,1)= "+" orelse substring(a,0,1)="~") then a
        else ("+"^a)
    fun abs(a:bigint)=
        let 
            val a= stan_bigint(a)
        in 
            substring(a,1,(size a)-1)
        end;

    fun Add_zero (s:int,n:bigint)=
        if(s>0) then Add_zero(s-1,"0"^n)
        else n;

    fun trim_zero(a:bigint)=
        let 
            val p=size a
            fun trim(a,p)=
                if(p=0) then a
                else if(substring(a,0,1)= "0") then trim(substring(a,1,p-1),p-1)
                else a
        in 
            trim(a,p)
        end;

    (* compare only magnitude *)
    fun compare(a:bigint,b:bigint)=
        let  
            val a= if (substring(a,0,1)="+" orelse substring(a,0,1)="~") then substring(a,1,(size a)-1) else a
            val b= if (substring(b,0,1)="+" orelse substring(b,0,1)="~") then substring(b,1,(size b)-1) else b
            val a=trim_zero(a)
            val b=trim_zero(b)
        in 
            if( size a > size b) then true
            else if (size a  < size b) then false
            else ( a > b)
        end;



    (* helper function *)

    (* helper function of add *)
    fun Adder(a,b,c,ans)=
        if (length a=0) then ((Int.toString c)^ans)
        else
        let 
            val u= (Char.ord (hd(a)))+(Char.ord (hd(b)))-96+c
            val d= Int.toString (u mod 10)
            val c= (u div 10)
        in
            Adder(tl(a),tl(b),c,d^ans)
        end;
    
    fun addUnsigned(a:bigint,b:bigint)=
    let

        val p= size a
        val q= size b
        val a= if(p<q) then Add_zero(q-p,a) else a
        val b= if(q<p) then Add_zero(p-q,b) else b
    in 
        trim_zero(Adder(rev (explode a),rev (explode b),0,""))
    end;

    fun subtracter(l,m,c,ans)=
        if(length l=0) then  ans
        else
            let
                val u= (Char.ord (hd(l)))-(Char.ord (hd(m)))-c
                val c= if(u<0) then 1 else 0
                val u= if (c=0 ) then u else (u+10)
            in 
                subtracter(tl(l), tl(m), c,(Int.toString u)^ans)
            end;
    fun subUnsigned(a:bigint,b:bigint)=
        let 
            val p= size a
            val q= size b
            val a= if(p<q) then Add_zero(q-p,a) else a
            val b= if(q<p) then Add_zero(p-q,b) else b
        in 
            subtracter(rev (explode a),rev (explode b),0,"")
        end;
    

    fun Add(a,b)=
        let 
            val a= stan_bigint(a)
            val b= stan_bigint(b)
            val pa=abs(a)
            val pb=abs(b)
            val tuple = (substring(a,0,1),substring(b,0,1))
        in
            case tuple of 
             ("+","~") => if(compare (pb,pa)) then "~"^subUnsigned(pb,pa) else subUnsigned(pa,pb)
            |("+","+") => addUnsigned(pa,pb) 
            |("~","~") => "~"^addUnsigned(pa,pb)
            |("~","+") => if(compare (a,b)) then "~"^subUnsigned(pa,pb) else subUnsigned(pb,pa)
            | _ => "Invalid operands"
        end;
    fun sub(a:bigint,b:bigint)=
        if (a="" andalso b="") then "0" 
        else if (b="") then sub(a,"0")
        else if a= "" then sub("0",b)
        else
        let 
            val b= if (substring(b,0,1)="~") then (substring(b,1,(size b)-1))
                    else if (substring(b,0,1)="+") then ("~"^substring(b,1,(size b)-1))
                    else ("~"^b)
        in
            Add(a,b)
        end;

    fun multiplier(a,b,c,ans)=
        if(a=[]) then (Int.toString c)^ans
        else 
            let 
                val u= ((ord (hd(a)))-48)*b+c
                val d= Int.toString (u mod 10)
                val c= (u div 10)
            in  
                multiplier(tl(a),b,c,d^ans)
            end;
    fun Accumulator(a,b,ans,i)=
        if(b=[]) then ans
        else
            let 
                fun zeroes(p,s)=if(s=0) then p else zeroes(p^"0",s-1)
                val u=multiplier(a,(ord (hd(b)))-48,0,"")
                val u=zeroes(u,i)
                val ans=addUnsigned(ans,u)
            in
                Accumulator(a,tl(b),ans,i+1)
                (* ans *)
            end;

    fun mul(a:bigint,b:bigint)=
        if(size (trim_zero(a)) = 0 orelse size (trim_zero (b))= 0) then "0"
        else 
        let
            val a= stan_bigint(a)
            val b= stan_bigint(b)
            val sign = Assign_sign(substring(a,0,1),substring(b,0,1))
            val a= rev (tl(explode a))
            val b= rev (tl(explode b))
        in
            (sign^Accumulator(a,b,"",0))
            (* (implode(b)) *)
        end;

    fun find_q(a,d,b,fvalue) =
        if( compare(fvalue,a) ) then (d-1,subUnsigned(fvalue,b))
        else find_q(a,d+1,b,addUnsigned(fvalue,b));
    
    
    fun Divider(a,divisor,r,q,i)=
        if(i=0) then (q,r)
        else
            let
                val r=trim_zero(r)
                val r=r^substring(a,0,1)
                val tup= find_q(r,1,divisor,divisor)
                val q= q^(Int.toString (#1 tup))
                val r= if(compare(divisor,r)) then r else subUnsigned(r,(#2 tup)) 
            in 
                Divider(substring(a,1,i-1),divisor,r,q,i-1)
            end;


    fun div_rem(num:bigint,deno:bigint)=
        if(compare(deno,num)) then ("0",num)
        else 
            let 
                val num=stan_bigint(num)
                val deno=stan_bigint(deno)
                val sign =  Assign_sign (substring(num,0,1),substring(deno,0,1))
                
                val a=abs(num)
                val b=abs(deno)
                val quotient=Divider(a,b,"0","",size a)
            in
                (sign^(trim_zero(#1 quotient)),trim_zero(#2 quotient))
            end;
    
    fun Divide(a:bigint,b:bigint)=
        let
            val tup=div_rem(a,b)
        in
            (#1 tup)
        end;
    fun rem(a:bigint,b:bigint)=
        let
            val tup=div_rem(a,b)
        in
            (#2 tup)
        end;
end;
