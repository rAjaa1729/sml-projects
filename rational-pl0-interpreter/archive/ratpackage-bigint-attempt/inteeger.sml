use "BIGINT.sig";

structure BigInt : BIGINT =
struct

    type bigint = string

    fun to_bigint(s : string) = s
    fun to_str(s : bigint) = s

    fun concat(s1 : string, s2) = s1^s2

    fun negative_check(s1,s2) =
        let
            val x=explode(s1)
            val y=explode(s2)
        in
            if ((hd(x) = #"~" andalso hd(y) = #"~") orelse (hd(x) <> #"~" andalso hd(y) <> #"~"))
            then false
            else true
        end

    fun is_neg2(s) = (hd(explode(s)) = #"~")
    fun to_pos(s) = implode(tl(explode(s)))
    fun to_neg(s) = "~"^s

    fun always_pos(s) =
        let
            val x=explode(s)
        in
            if (hd(x) = #"~")
            then implode(tl(x))
            else s
        end

    fun trim1(s,sz) =
        if ((size(s))=sz)
        then
            s
        else
            trim1("0"^s,sz)

    fun trim2(s,sz) =
        if (sz=1)
        then s
        else
            let
                val n=valOf(Int.fromString(substring(s,0,1)))
            in
                if (n=0)
                then trim2(substring(s,1,sz-1),sz-1)
                else s
            end

    fun great(s11,s22) =
    let
        val s1=trim2(s11,size(s11))
        val s2=trim2(s22,size(s22))
        val sz1=size(s1)
        val sz2=size(s2)
    in
        if (sz1<>sz2)
        then
            sz1>sz2
        else
            let
                val num1=valOf(Int.fromString(substring(s1,0,1)))
                val num2=valOf(Int.fromString(substring(s2,0,1)))
            in
                if (sz1=1)
                then
                    num1>num2
                else
                    if (num1<>num2)
                    then
                        num1>num2
                    else
                        great(substring(s1,1,sz1-1),substring(s2,1,sz2-1))
            end
    end

    fun add_help(s1,s2,rem) =
        let
            val sz=size(s1)
        in
            if (sz=1)
            then
                Int.toString(valOf(Int.fromString(s1))+valOf(Int.fromString(s2))+rem)
            else
                let
                    val f_s=substring(s1,0,sz-1)
                    val f_s1=substring(s1,sz-1,1)
                    val s_s=substring(s2,0,sz-1)
                    val s_s1=substring(s2,sz-1,1)
                    val n=valOf(Int.fromString(f_s1))+valOf(Int.fromString(s_s1))+rem
                in
                    add_help(f_s,s_s,n div 10)^Int.toString(n mod 10)
                end
        end

    fun mx(a,b) =
        if (a>b) then a else b

    fun add(s1,s2) =
        let
            val m=mx(size(s1),size(s2))
            val s_1=trim1(s1,m+1)
            val s_2=trim1(s2,m+1)
            val s=add_help(s_1,s_2,0)
        in
            trim2(s,size(s))
        end

    fun sub_help(s1,s2,rem) =
        if (size(s1)=1)
        then
            Int.toString(valOf(Int.fromString(s1))-valOf(Int.fromString(s2))-rem)
        else
            let
                val f_s=substring(s1,0,size(s1)-1)
                val f_s1=substring(s1,size(s1)-1,1)
                val s_s=substring(s2,0,size(s1)-1)
                val s_s1=substring(s2,size(s1)-1,1)
                val n=valOf(Int.fromString(f_s1))-valOf(Int.fromString(s_s1))-rem
            in
                if (n>=0)
                then sub_help(f_s,s_s,0)^Int.toString(n)
                else sub_help(f_s,s_s,1)^Int.toString(n+10)
            end

    fun sub(s1,s2) =
        if (great(add(s1,"1"),s2))
        then
            let
                val m=mx(size(s1),size(s2))
                val s_1=trim1(s1,m+1)
                val s_2=trim1(s2,m+1)
                val s=sub_help(s_1,s_2,0)
            in
                trim2(s,size(s))
                (* s_1 *)
            end
        else
            let
                val m=mx(size(s1),size(s2))
                val s_1=trim1(s1,m+1)
                val s_2=trim1(s2,m+1)
                val s=sub_help(s_2,s_1,0)
            in
                "~"^trim2(s,size(s))
            end

    fun mult_help(srt,rem,num) =
        let
            val sz=size(srt)
        in
            if (sz=1)
            then 
                let 
                    val n=valOf (Int.fromString (srt))
                    val nn=n*num+rem
                in
                    Int.toString(nn)
                end
            else
                let
                    val s1=substring(srt,0,sz-1)
                    val s2=substring(srt,sz-1,1)
                    val n=((valOf (Int.fromString (s2)))*num)+rem
                    val call=mult_help(s1,n div 10,num)
                in
                    call^(Int.toString(n mod 10))
                end
        end

    fun add_zero(s,zer) =
        if (zer=0)
        then s
        else add_zero(s^"0",zer-1)

    fun mult_help2(s1,s2,zer) =
        if (size(s2)=1)
        then
            add_zero(mult_help(s1,0,valOf(Int.fromString(s2))),zer)
        else
            let
                val s=substring(s2,0,size(s2)-1)
                val n=valOf(Int.fromString(substring(s2,size(s2)-1,1)))

            in
                add(add_zero(mult_help(s1,0,n),zer),mult_help2(s1,s,zer+1))
            end

    fun mult(s1,s2) =
        let
            val n=mult_help2(s1,s2,0)
        in
            trim2(n,size(n))
        end

    fun suitable(num,divisor,mul) =
        if (mul=10) then 9
        else
            if (great(mult(divisor,Int.toString(mul)),num)) then mul-1
            else suitable(num,divisor,mul+1)

    fun div_help(divisor,rem,quot,srt) =
        if (size(srt)=1)
        then (quot,rem)
        else
            let
                val num=trim2(rem^substring(srt,0,1),size(rem^substring(srt,0,1)))
                val n=suitable(num,divisor,1)
                val n_rem=sub(num,mult(divisor,Int.toString(n)))
                val n_quot=quot^Int.toString(n)
            in
                div_help(divisor,n_rem,n_quot,substring(srt,1,size(srt)-1))
            end

    fun div_(s1,s2) =
        let
            val call=div_help(s2,"","",s1^"0")
        in
            (trim2(#1 call,size(#1 call)),trim2(#2 call,size(#2 call)))
        end
    
    fun gcd(s1,s2) =
        if (size(s2)=1 andalso valOf(Int.fromString(s2))=0)
        then
            s1
        else
            let val k=div_(s1,s2) in gcd(s2,#2 k) end

    fun find_lst(lst,pos,key) =
        if (length(lst)=1)
        then
            if (hd(lst)=key) then pos else ~1
        else
            if (hd(lst)=key) then pos else find_lst(tl(lst),pos+1,key)

    fun dec_part(divisor,divi,lst,quot) =
        let
            val n_div=divi^"0"
            val n=suitable(n_div,divisor,1)
            val temp=sub(n_div,mult(divisor,Int.toString(n)))
            val n_rem=trim2(temp,size(temp))
            val n_quot=quot^Int.toString(n)
            val x=find_lst(lst,0,n_rem)
            val n_lst=lst @ [n_rem]
            fun searchingtype(adj_list: (int * int list) list, start_check: int) =
            let
                  val visited = Array.tabulate(length adj_list, fn _ => false)
            in
                  visited
            end;
        in
            if ((size(n_rem)=1) andalso (valOf(Int.fromString(n_rem))=0))
            then (n_quot,~1)
            else if (x<>(~1))
            then (n_quot,x)
            else 
                dec_part(divisor,n_rem,n_lst,n_quot)
        end
    
    fun fin_dec(s1,s2) =
        let
            val p=div_(s1,s2)
            val frt= #1 p
            val rem= #2 p
        in
            if ((size(rem)=1) andalso (valOf(Int.fromString(rem))=0))
            then frt
            else 
                let
                    val np=dec_part(s2,rem,[rem],"")
                    val x= #1 np
                    val y= #2 np
                in
                    if (y=(~1)) then frt^"."^x
                    else if ((size(frt)=1) andalso (valOf(Int.fromString(frt))=0))
                    then "."^substring(x,0,y)^"("^substring(x,y,(size(x)-y))^")"
                    else frt^"."^substring(x,0,y)^"("^substring(x,y,(size(x)-y))^")"
                end
        end

    fun lgh(srt,pos,ch,ans,ans2,num) =
        if (size(srt)=pos)
        then (ans,ans2,num)
        else
            if (substring(srt,pos,1)= "." )
            then lgh(srt,pos+1,true,ans,ans2,num)
            else if (ch)
            then
                if ((substring(srt,pos,1)= "(" ) orelse (substring(srt,pos,1)= ")" ))
                then lgh(srt,pos+1,true,ans,ans2,num)
                else lgh(srt,pos+1,true,ans^substring(srt,pos,1),ans2,num+1)
            else lgh(srt,pos+1,false,ans^substring(srt,pos,1),ans2^substring(srt,pos,1),num)

    fun zer_num(num) =
        if (num=0)
        then ""
        else "0"^zer_num(num-1)

    fun pos1(srt,pos) =
        if (substring(srt,pos,1)= "." )
        then pos
        else pos1(srt,pos+1)

    fun pos2(srt,pos) =
        if (substring(srt,pos,1)= "(" )
        then pos
        else pos2(srt,pos+1)

    fun to_rat(srt) =
        let
            val p=lgh(srt,0,false,"","",0)
            val s= #1 p
            val s1= #2 p
            val num= #3 p
        in
            if (substring(srt,size(srt)-1,1)= ")")
            then
                (sub(s,s1),sub(("1"^zer_num(num)),"1"))
            else
                (s,("1"^zer_num(num)))
        end

    fun fin_rat(srt) =
        if (substring(srt,size(srt)-1,1)= ")")
        then
            let
                val p1=pos1(srt,0)
                val p2=pos2(srt,0)
                val lh=p2-p1
                val nw=substring(srt,0,p1)^substring(srt,p1+1,lh-1)^"."^substring(srt,p2,size(srt)-p2)
                val dv="1"^zer_num(lh-1)
                val call=to_rat(nw)
            in
                (#1 call, mult(#2 call, dv))
                (* (nw,"1") *)
            end
        else
            to_rat(srt)

        fun divd(s1,s2) =
            let
                val x=div_(s1,s2)
            in
                (#1 (x))
            end

        fun modd(s1,s2) =
            let
                val x=div_(s1,s2)
            in
                (#2 (x))
            end

end

val t=BigInt.to_rat("000001.(0)");

(* val it1=BigInt.dec_part("7","2",["2"],"") *)
(* val it2=BigInt.dec_part("15","2",["2"],"")
val it3=BigInt.dec_part("75","2",["2"],"")
val it4=BigInt.dec_part("375","2",["2"],"")
val it5=BigInt.dec_part("4","2",["2"],"")
val it6=BigInt.dec_part("6","2",["2"],"") *)
(* val i1=BigInt.fin_dec("35","32")
val i2=BigInt.fin_dec("7","34")
val i3=BigInt.fin_dec("2","11")
val i4=BigInt.fin_dec("2","23")
val i5=BigInt.fin_dec("2","375")
val i6=BigInt.fin_dec("96","25") *)
(* val k1=BigInt.to_rat("1.33") *)


    