
Builder := Object clone

Builder forward := method(
  writeln("<",  call message name, ">")
  call message arguments foreach(
    arg,
    content := self doMessage(arg);
    if(content type == "Sequence", writeln(content)))
writeln("</", call message name, ">"))

Builder ul(
       li("Io"), 
       li("Lua"),
       li("JavaScript"))

# --------------------- Builder2 handles indent
writeln("-----------------------")
Builder2 := Object clone
Builder2 ind ::= 0
Builder2 forward := method(
    ind_str := (".." repeated(self ind))
    writeln (ind_str, "<", call message name, ">")
    self setInd(self ind + 1)
    call message arguments foreach(arg,
        content := self doMessage( arg);
        if(content type == "Sequence", writeln(ind_str, "..", content)))
    self setInd(self ind - 1)
    writeln(ind_str , "</", call message name, ">")
)


Builder2 ul(
       li("Io"), 
       li("Lua"),
       li("JavaScript"))
