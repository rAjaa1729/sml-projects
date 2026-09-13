(* -----------------------------incase no use ---------------open *)
val outputfile="Answer.html";
val x=TextIO.openOut outputfile;
val close=TextIO.closeOut x;
(* -----------------------------incase no use ---------------close *)

    
fun writeToFile(filename: string, data: string) =
    let
        val file_down = TextIO.openAppend filename
        val Add_data= TextIO.output (file_down, data)
    in
        TextIO.closeOut file_down
    end;

fun bold_check(outputfile,bold)=
    if (bold=0) then writeToFile(outputfile,"<b>")
    else  writeToFile(outputfile,"</b>");
fun italics_check(outputfile,italics)=
    if (italics=0) then writeToFile(outputfile,"<i>")
    else  writeToFile(outputfile,"</i>");

fun underline_check(contents,outputfile,index,underline)=
    if(substring(contents,index,1)=" " orelse substring(contents,index,1)="\n" ) 
    then 
        let 
            val write=writeToFile(outputfile,underline^"</u>")
        in
            index
        end
    else if(substring(contents,index,1)="_") then underline_check(contents,outputfile,index+1,underline^" ")
    else underline_check(contents,outputfile,index+1,underline^substring(contents,index,1))
fun directlink_check(contents,outputfile,index,text,link)=
    if(substring(contents,index,1)= ")")
        then
            let 
                val write=writeToFile(outputfile,"<a href="^link^">"^text^"</a>")
            in
                index+1
            end
    else
        directlink_check(contents,outputfile,index+1,text,link^substring(contents,index,1))

fun indirect_link(contents,outputfile,index,link)=
    if(substring(contents,index,1)=">")
    then
        let 
            val write=writeToFile(outputfile,"<a href="^link^">"^link^"</a>")
        in
            index+1
        end
    else indirect_link(contents,outputfile,index+1,link^substring(contents,index,1));

fun table_make(contents,outputfile,index,table)=
    if(substring(contents,index,2)=">>") 
        then
            let
                fun remove_extra(table,le)=
                    if(substring(table,le-1,1)=" ") then remove_extra(substring(table,0,le-1),le-1)
                    else substring(table,0,le-12)
                val table=remove_extra(table,(size table))
                val write=writeToFile(outputfile,table^"</TABLE></CENTER>")
            in
                index+2
            end
    else if(substring(contents,index,1)="\n") then table_make(contents,outputfile,index+1,table^"</TD></TR>\n<TR><TD>")
    else if(substring(contents,index,1)="|") then table_make(contents,outputfile,index+1,table^"</TD><TD>")
    else table_make(contents,outputfile,index+1,table^substring(contents,index,1));
            
fun unorderedlist_check(contents,outputfile,index)=
    if(substring(contents,index,1)="-")
        then
            let 
                fun line_end(contents,index,line)=
                    if(substring(contents,index,2)="\n\n") then (line,index)
                    else line_end(contents,index+1,line^substring(contents,index,1));
                
                val tuple=line_end(contents,index+1,"<li><p>")
                val line= #1 tuple
                val index= #2 tuple
                val write=writeToFile(outputfile,line^"</p></li>\n")
            in 
                unorderedlist_check(contents,outputfile,index+1)
            end
    else index
fun codeblock_check(contents,outputfile,index,block_write)=
    if(substring(contents,index,1)="\n" andalso substring(contents,index+1,8)<>"        ") 
    then 
        let 
            val write=writeToFile(outputfile,block_write^"</code></pre>")
        in
            index
        end
    else codeblock_check(contents,outputfile,index+1,block_write^substring(contents,index,1))





(* -----Starting html file with initilization functions------------ *)
fun start_html (outputfile)=
    let
        val A="<!DOCTYPE html> \n"
        val B="<html> \n"
        val C="<body> \n"
    in
        writeToFile(outputfile,A^B^C)
    end;
(* -----closing html file functions------------ *)
fun end_html (outputfile)=
    let 
        val D="</body> \n"
        val E="</html> \n"
    in 
        writeToFile(outputfile,(D^E))
    end;

(* starting converter--------- *)


(* -----------------------------incase no use ---------------open *)

start_html (outputfile); 
(* ---------started working to convert file *)

val file_in = TextIO.openIn "file.txt";
val contents = TextIO.inputAll file_in;
val close=TextIO.closeIn file_in;
val len_contents=String.size(contents);

(* -----------------------------incase no use ---------------close *)

fun DFA (contents:string) (len_contents:int) (index:int)  (bold:int) (italics:int) (quotes:int) (heads:int) (stack:string) (space:int)  (str_write:string) (outputfile:string)=
    
    if(index=len_contents) then writeToFile(outputfile,str_write^"\n")
    else if(substring(contents,index,1)="\")
        then DFA contents len_contents (index+2) bold italics quotes heads stack space (str_write^substring(contents,index,2)) outputfile 
    else if (substring(contents,index,9)="\n        " andalso stack="" )
        then 
            let
                val write=writeToFile(outputfile,str_write^"\n")
                val index=codeblock_check(contents,outputfile,index+9,"<pre><code>        ")
            in
                DFA contents len_contents index bold italics quotes heads stack space "" outputfile
            end
    
    else if(substring(contents,index,1)="#") 
    then 
        let
            val write=writeToFile(outputfile,str_write)
            fun nheads(contents,index,heads)=
                if(substring(contents,index,1)="#") then nheads(contents,index+1,heads+1)
                else if(substring(contents,index,1)=" ") then nheads(contents,index+1,heads)
                else (index,heads)
            val tuple=nheads(contents,index,0)
            val heads= #2 tuple
            val index=  #1 tuple
            val write=writeToFile(outputfile,("<h"^(Int.toString heads)^">"))
        in
            DFA contents len_contents index bold italics quotes heads stack space "" outputfile
        end
    
    else if(substring(contents,index,2)="**") 
    then 
        let 
            val write=writeToFile(outputfile,str_write)
            val execute=bold_check(outputfile,bold)
        in
            DFA contents len_contents (index+2) ((bold+1) mod 2) italics quotes heads stack space "" outputfile
        end
    
    else if(substring(contents,index,1)="*")
    then
        let 
            val write=writeToFile(outputfile,str_write)
            val execute=italics_check(outputfile,italics)
        in
            DFA contents len_contents (index+1) bold ((italics+1) mod 2) quotes heads stack space "" outputfile
        end
    
    else if(substring(contents,index,1)="_") 
        then
            let
                val write=writeToFile(outputfile,str_write)
                val index =underline_check(contents,outputfile,index+1,"<u>")
            in
                DFA contents len_contents index bold italics quotes heads stack space "" outputfile
            end
    else if(substring(contents,index,3)="---")
        then
            let 
                val write=writeToFile(outputfile,str_write^"<hr>")
            in
                DFA contents len_contents (index+3) bold italics quotes heads stack space "" outputfile
            end
    else if(substring(contents,index,1)="[")
        then
            let 
                val write=writeToFile(outputfile,str_write)
                fun check(contents,index,text)=
                    if(substring(contents,index,1)="]") 
                    then if(substring(contents,index+1,1)="(")
                            then
                                let 
                                    val index=directlink_check(contents,outputfile,index+2,text,"")
                                in 
                                    (index,"")
                                end 
                        else (index+1,"["^text^"]")
                    else check(contents,index+1,text^substring(contents,index,1))
                val tuple= check(contents,index+1,"")
                val index= #1 tuple
                val text= #2 tuple
            in
                DFA contents len_contents index bold italics quotes heads stack space text outputfile
            end
    else if(substring(contents,index,4)="<http")
        then
            let 
                val write=writeToFile(outputfile,str_write)
                val index=indirect_link(contents,outputfile,index+1,"")
            in
                DFA contents len_contents index bold italics quotes heads stack space "" outputfile
            end
    else if(substring(contents,index,2)="<<")
        then
            let 
                val write=writeToFile(outputfile,str_write)
                val index=table_make(contents,outputfile,index+3,"<CENTER><TABLE border=\"1\">\n<TR><TD>")
            in
                DFA contents len_contents index bold italics quotes heads stack space "" outputfile
            end
    else if(substring(contents,index,1)="-")
        then
            let 
                val write=writeToFile(outputfile,str_write^"<ul>")
                val index=unorderedlist_check(contents,outputfile,index)
            in
                DFA contents len_contents index bold italics quotes heads stack space "</ul>" outputfile
            end
    else if(substring(contents,index,1)="\n") 
        then
            let
                fun manageheads(nheads)=
                    if(nheads>0) then writeToFile(outputfile,str_write^"</h"^(Int.toString nheads)^">")
                    else writeToFile(outputfile,str_write)

                val write=manageheads(heads)

                fun cal_block(contents,index,n_quotes)=
                    if(substring(contents,index,1)=">") then cal_block(contents,index+1,n_quotes+1)
                    else if(substring(contents,index,1)=" ") then cal_block(contents,index+1,n_quotes)
                    else (index,n_quotes)

                val tuple=cal_block(contents,index+1,0)
                val index= #1 tuple
                val n_quotes= #2 tuple

                fun blockquotes_check(quotes,n_quotes,block_write)=
                    if(quotes>n_quotes) then blockquotes_check(quotes-1,n_quotes,block_write^"</blockquote>")
                    else if(quotes<n_quotes) then blockquotes_check(quotes,n_quotes-1,block_write^"<blockquote>")
                    else block_write
                fun write_quotes(quotes,n_quotes,block_write)=
                    if(quotes>n_quotes) then writeToFile(outputfile,block_write^"\n")
                    else writeToFile(outputfile,"\n"^block_write)

                val block_write=blockquotes_check(quotes,n_quotes,"")
                val write=write_quotes(quotes,n_quotes,block_write)

            in
                DFA contents len_contents (index) bold italics n_quotes 0 stack space "" outputfile
            end
    else
        DFA contents len_contents (index+1) bold italics quotes heads stack space (str_write^substring(contents,index,1)) outputfile;






(* fun md2html(filename)=
    let
        val file_in = TextIO.openIn filename
        val contents = TextIO.inputAll file_in
        val close=TextIO.closeIn file_in
        val len_contents=String.size(contents)
        (* val z=change_file_name(filename,substring(filename,0,(size filename) -3)^"html") *)
        val outputfile=substring(filename,0,(size filename) -3)^"html"
        val new=OS.FileSys.rename(filename, outputfile)
        val open=TextIO.openOut outputfile
        val close=TextIO.closeOut x
        val start=start_html (outputfile)
        val DFA (contents^"Raja") len_contents 0 0 0 0 0 "" outputfile
    in
        end_html(outputfile)
    end;

md2html("file.txt");


 *)







(* -----------------------------incase no use ---------------open *)


DFA (contents^"RajDepartment of Computer Science and Engineering") len_contents 0 0 0 0 0 "" 0 "" "Answer.html";

(* closing html file *)
end_html (outputfile);


(* -----------------------------incase no use ---------------close *)