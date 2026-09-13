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

fun head_check(contents,outputfile,nheads,str_write,i)=
    if substring(contents,i,1)="#" then head_check(contents,outputfile,nheads+1,str_write,i+1)
    else if(substring(contents,i,1)="\n") 
    then 
        let 
            val tmp=writeToFile(outputfile,("<h"^(Int.toString nheads)^">")^(str_write)^("</h"^(Int.toString nheads)^">")^("\n"))
        in 
            i+1
        end
    else head_check(contents,outputfile,nheads,str_write^substring(contents,i,1),i+1);

fun bold_check(outputfile,bold)=
    if (bold=0) then writeToFile(outputfile,"<b>")
    else  writeToFile(outputfile,"</b>");
fun italics_check(outputfile,italics)=
    if (italics=0) then writeToFile(outputfile,"<i>")
    else  writeToFile(outputfile,"</i>");

fun underline_check(outputfile,inspect)=
    if(inspect="\"\\_") then writeToFile(outputfile,"<u>")
    else writeToFile(outputfile,"</u>")
fun directlink_check(contents,outputfile,index,text)=
    if(substring(contents,index,4)= "\\]\\(")
        then
            let 
                fun link_text(contents,index,link)=
                    if(substring(contents,index,3)="\\)\"") then (link,index)
                    else link_text(contents,index+1,link^substring(contents,index,1));
                val tuple=link_text(contents,index+4,"")
                val link= #1 tuple
                val write=writeToFile(outputfile,"<a href="^link^">"^text^"</a>")
            in
                ((#2 tuple)+3)
            end
    else
        directlink_check(contents,outputfile,index+1,text^substring(contents,index,1))

fun indirect_link(contents,outputfile,index,link)=
    if(substring(contents,index,1)=">")
    then
        let 
            val write=writeToFile(outputfile,"<a href="^link^">"^link^"</a>")
        in
            index+1
        end
    else indirect_link(contents,outputfile,index+1,link^substring(contents,index,1));

(* fun table_make(contents,outputfile,index,table,flip)=
    if(substring(contents,index,2)=">>") 
        then
            let
                val write=writeToFile(outputfile,table^"</TABLE></CENTER>")
            in
                index+2
            end
    else if(substring(contents,index,1)="\n") then table_make(contents,outputfile,index+1,table^"</TD></TR>\n</TD><TD>",1)
    else if(substring(contents,index,1)="|") then table_make(contents,outputfile,index+1,table^"</TD><TD>",0)
    else if((substring(contents,index,1)<>" ") andalso flip=1) then table_make(contents,outputfile,index+1,table^"<TR><TD>"^substring(contents,index,1),0)
    else table_make(contents,outputfile,index+1,table^substring(contents,index,1),0); *)
            
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


(* fun unorderedlist_check(contents,outputfile,index)=
    if(substring(contents,index,1)="-")
        then
            let 
                fun line_end(contents,index,line)=
                    if(substring(contents,index,1)="\n") then (line,index)
                    else line_end(contents,index+1,line^substring(contents,index,1));
                
                val tuple=line_end(contents,index+1,"<li><p>")
                val line= #1 tuple
                val index= #2 tuple
                val write=writeToFile(outputfile,line^"</p></li>\n")
            in 
                unorderedlist_check(contents,outputfile,index+1)
            end
    else index *)

(* fun blockquotes_check(contents,outputfile,index,block,blockquotes)=
    if(substring(contents,index,1)="\n")
    then
        let 
            fun cal_block(contents,index,n_block)=
                if(substring(contents,index,1)=">") then cal_block(contents,index+1,n_block+1)
                else if(substring(contents,index,1)=" ") then cal_block(contents,index+1,n_block)
                else (index,n_block);
            val tuple=cal_block(contents,index+1,0)
            val index= #1 tuple
            val n_block=#2 tuple *)

(* -----Starting html file with initilization functions------------ *)
fun start_html ()=
    let
        val A="<!DOCTYPE html> \n"
        val B="<html> \n"
        val C="<body> \n"
    in
        writeToFile(outputfile,A^B^C)
    end;
(* -----closing html file functions------------ *)
fun end_html ()=
    let 
        val D="</body> \n"
        val E="</html> \n"
    in 
        writeToFile(outputfile,D^E)
    end;

(* starting converter--------- *)


(* -----------------------------incase no use ---------------open *)

start_html (); 
(* ---------started working to convert file *)

val file_in = TextIO.openIn "mdtab-2023.md";
val contents = TextIO.inputAll file_in;
val close=TextIO.closeIn file_in;
val len_contents=String.size(contents);

(* -----------------------------incase no use ---------------close *)

fun DFA (contents:string) (len_contents:int) (index:int)  (bold:int) (italics:int)  (str_write:string) (outputfile:string)=
    
    if(index=len_contents) then writeToFile(outputfile,str_write^"\n")
    
    else if(substring(contents,index,1)="#") 
    then 
        let 
            val write=writeToFile(outputfile,str_write)
            val index=head_check(contents,outputfile,0,"",index)
        in
            DFA contents len_contents index bold italics  "" outputfile
        end
    
    else if(substring(contents,index,2)="**") 
    then 
        let 
            val write=writeToFile(outputfile,str_write)
            val execute=bold_check(outputfile,bold)
        in
            DFA contents len_contents (index+2) ((bold+1) mod 2) italics  "" outputfile
        end
    
    else if(substring(contents,index,1)="*")
    then
        let 
            val write=writeToFile(outputfile,str_write)
            val execute=italics_check(outputfile,italics)
        in
            DFA contents len_contents (index+1) bold ((italics+1) mod 2)  "" outputfile
        end
    
    else if(substring(contents,index,3)="\"\\_" orelse substring(contents,index,3)="\\_\"") 
        then
            let
                val write=writeToFile(outputfile,str_write)
                val execute=underline_check(outputfile,substring(contents,index,3))
            in
                DFA contents len_contents (index+3) bold italics  "" outputfile
            end
    
    else if(substring(contents,index,2)="\\_")
        then DFA contents len_contents (index+2) bold italics  (str_write^" ") outputfile
    
    else if(substring(contents,index,3)="---")
        then
            let 
                val write=writeToFile(outputfile,str_write^"<hr>")
            in
                DFA contents len_contents (index+3) bold italics  "" outputfile
            end
    else if(substring(contents,index,3)="\"\\[")
        then
            let 
                val write=writeToFile(outputfile,str_write)
                val index=directlink_check(contents,outputfile,index+3,"")
            in
                DFA contents len_contents index bold italics  "" outputfile
            end
    else if(substring(contents,index,4)="<http")
        then
            let 
                val write=writeToFile(outputfile,str_write)
                val index=indirect_link(contents,outputfile,index+1,"")
            in
                DFA contents len_contents index bold italics  "" outputfile
            end
    else if(substring(contents,index,2)="<<")
        then
            let 
                val write=writeToFile(outputfile,str_write)
                val index=table_make(contents,outputfile,index+3,"<CENTER><TABLE border=\"1\">\n<TR><TD>")
            in
                DFA contents len_contents index bold italics  "" outputfile
            end
    (* else if(substring(contents,index,1)="-")
        then
            let 
                val write=writeToFile(outputfile,str_write^"<ul>")
                val index=unorderedlist_check(contents,outputfile,index)
            in
                DFA contents len_contents index bold italics  "</ul>" outputfile
            end *)
    (* else if(substring(contents,index,1)="<")=
        then DFA contents len_contents (index+1) bold italics (#1 ,(#2 )+1) (str_write^substring(contents,index,1)) outputfile
    else if(substring(contents,index,1)=">")=
        then if(#2 >0) then DFA contents len_contents (index+1) bold italics (#1 ,if (#2 )-1) (str_write^substring(contents,index,1)) outputfile
            else 
                let
                    val write=writeToFile(outputfile,str_write)
                    val index=block_check(contents,outputfiel,index+1,1,"")
                in
                   DFA contents len_contents (index+1) bold italics  "" outputfile
                end *)
    else
        DFA contents len_contents (index+1) bold italics  (str_write^substring(contents,index,1)) outputfile;



(* fun md2html(filename)=
    let
        val file_in = TextIO.openIn "mdtab-2023.md"
        val contents = TextIO.inputAll file_in
        val close=TextIO.closeIn file_in
        val len_contents=String.size(contents)
        val outputfile=substring(filename,0,(size filename) -3)^"html"
        val new=OS.FileSys.rename(filename, outputfile)
        val open=TextIO.openOut outputfile
        val close=TextIO.closeOut x
        val start=start_html ()
        val DFA (contents^"Raja") len_contents 0 0 0 (0,0) "" outputfile;
    in
        end_html(outputfile)
    end;

md2html("inputfile.txt"); *)










(* -----------------------------incase no use ---------------open *)


DFA (contents^"Raja") len_contents 0 0 0 "" "Answer.html";

(* closing html file *)
end_html ();


(* -----------------------------incase no use ---------------close *)