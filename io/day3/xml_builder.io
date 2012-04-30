
XML_Builder := Object clone

XML_Builder forward := method(
  writeln("<",  call message name, ">")
  call message arguments foreach(
    arg,
    content := self doMessage(arg);
    if(content type == "Sequence", writeln(content)))
writeln("</", call message name, ">"))

XML_Builder ul(
       li("Io"), 
       li("Lua"),
       li("JavaScript"))

# --------------------- Builder2 handles indent
writeln("-----------------------")
XML_Builder2 := Object clone
XML_Builder2 ind ::= 0
XML_Builder2 forward := method(
    ind_proto := ". "
    ind_str := (ind_proto repeated(self ind))
    writeln (ind_str, "<", call message name, ">")
    self setInd(self ind + 1)
    call message arguments foreach(arg,
        content := self doMessage( arg);
        if(content type == "Sequence", writeln(ind_str, ind_proto, content)))
    self setInd(self ind - 1)
    writeln(ind_str , "</", call message name, ">")
)


XML_Builder2 ul(
       li("Io"), 
       li("Lua"),
       li("JavaScript"))

# --------------------- Builder3 handles attributes
#--------For instance book({author:'tate'}) would print <book author='tate'>
writeln("-----------------------")


XML_Builder3 := Object clone
XML_Builder3 ind ::= 0

XML_Builder3 curlyBrackets := method(
  r := Map clone
  s := ""
  is_first := true
  call message arguments foreach(arg,
    toks := arg asString split(":")
    if(is_first not, s := s .. ",")
    s := s .. toks at(0)
    s := s .. "="
    s := s .. toks at(1)
    is_first := false)
  s
)

XML_Builder3 forward := method(
    ind_proto := ". "
    ind_str := (ind_proto repeated(self ind))
    msg_name := call message name
    msg_attrbs := ""
    args := call message arguments
    msg_attrbs_call := "{}"
    if(args size > 0,
      first_arg := args at(0)
      if(first_arg name == "curlyBrackets",
        msg_attrbs_call := args removeAt(0)
        msg_attrbs := self doMessage(msg_attrbs_call)
        r := Map clone
        r asList asString
      )
    )
    writeln (ind_str, "<", msg_name, " ",  msg_attrbs, ">")
    self setInd(self ind + 1)
    args foreach(arg,
        content := self doMessage( arg);
        if(content type == "Sequence", writeln(ind_str, ind_proto, content)))
    self setInd(self ind - 1)
    writeln(ind_str , "</", msg_name, ">")
)


XML_Builder3 ul(
       li({"count":"12", "color":"brown"}, "eggs"), 
       li({"size":"gallon"}, "milk"),
       li("oranges"))

