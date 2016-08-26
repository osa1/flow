-- $Id: main.lua,v 1.62 2015/05/15 12:28:08 roberto Exp $

-- most (all?) tests here assume a reasonable "Unix-like" shell
if _port then return end

-- use only "double quotes" inside shell scripts (better change to
-- run on Windows)


print ("testing stand-alone interpreter")

assert(os.execute())   -- machine has a system command

local arg = arg or _ARG

local prog = os.tmpname()
local otherprog = os.tmpname()
local out = os.tmpname()

local progname
do
  local i = 0
  while arg[i] do i=i-1 end
  progname = arg[i+1]
end
print("progname: "..progname)

local prepfile = function (s, p)
  p = p or prog
  io.output(p)
  io.write(s)
  assert(io.close())
end

local function getoutput ()
  io.input(out)
  local t = io.read("a")
  io.input():close()
  assert(os.remove(out))
  return t
end

local function checkprogout (s)
  local t = getoutput()
  for line in string.gmatch(s, ".-\n") do
    assert(string.find(t, line, 1, true))
  end
end

local function checkout (s)
  local t = getoutput()
  if s ~= t then print(string.format("'%s' - '%s'\n", s, t)) end
  assert(s == t)
  return t
end


local function RUN (p, ...)
  p = string.gsub(p, "lua", '"'..progname..'"', 1)
  local s = string.format(p, ...)
  assert(os.execute(s))
end

local function NoRun (msg, p, ...)
  p = string.gsub(p, "lua", '"'..progname..'"', 1)
  local s = string.format(p, ...)
  s = string.format("%s 2> %s", s, out)  -- will send error to 'out'
  assert(not os.execute(s))
  assert(string.find(getoutput(), msg, 1, true))  -- check error message
end

RUN('lua -v')

print(string.format("(temporary program file used in these tests: %s)", prog))

-- running stdin as a file
prepfile""
RUN('lua - < %s > %s', prog, out)
checkout("")

prepfile[[
  print(
1, a
)
]]
RUN('lua - < %s > %s', prog, out)
checkout("1\tnil\n")

RUN('echo "print(10)\nprint(2)\n" | lua > %s', out)
checkout("10\n2\n")


-- test option '-'
RUN('echo "print(arg[1])" | lua - -h > %s', out)
checkout("-h\n")

-- test environment variables used by Lua

prepfile("print(package.path)")

-- test LUA_PATH
RUN('env LUA_INIT= LUA_PATH=x lua %s > %s', prog, out)
checkout("x\n")

-- test LUA_PATH_version
RUN('env LUA_INIT= LUA_PATH_5_3=y LUA_PATH=x lua %s > %s', prog, out)
checkout("y\n")

-- test LUA_CPATH
prepfile("print(package.cpath)")
RUN('env LUA_INIT= LUA_CPATH=xuxu lua %s > %s', prog, out)
checkout("xuxu\n")

-- test LUA_CPATH_version
RUN('env LUA_INIT= LUA_CPATH_5_3=yacc LUA_CPATH=x lua %s > %s', prog, out)
checkout("yacc\n")

-- test LUA_INIT (and its access to 'arg' table)
prepfile("print(X)")
RUN('env LUA_INIT="X=tonumber(arg[1])" lua %s 3.2 > %s', prog, out)
checkout("3.2\n")

-- test LUA_INIT_version
prepfile("print(X)")
RUN('env LUA_INIT_5_3="X=10" LUA_INIT="X=3" lua %s > %s', prog, out)
checkout("10\n")

-- test LUA_INIT for files
prepfile("x = x or 10; print(x); x = x + 1")
RUN('env LUA_INIT="@%s" lua %s > %s', prog, prog, out)
checkout("10\n11\n")

-- test errors in LUA_INIT
NoRun('LUA_INIT:1: 10', 'env LUA_INIT="error(10)" lua')

-- test option '-E'
local defaultpath, defaultCpath

do
  prepfile("print(package.path, package.cpath)")
  RUN('env LUA_INIT="error(10)" LUA_PATH=xxx LUA_CPATH=xxx lua -E %s > %s',
       prog, out)
  local out = getoutput()
  defaultpath = string.match(out, "^(.-)\t")
  defaultCpath = string.match(out, "\t(.-)$")
end

-- paths did not changed
assert(not string.find(defaultpath, "xxx") and
       string.find(defaultpath, "lua") and
       not string.find(defaultCpath, "xxx") and
       string.find(defaultCpath, "lua"))


-- test replacement of ';;' to default path
local function convert (p)
  prepfile("print(package.path)")
  RUN('env LUA_PATH="%s" lua %s > %s', p, prog, out)
  local expected = getoutput()
  expected = string.sub(expected, 1, -2)   -- cut final end of line
  assert(string.gsub(p, ";;", ";"..defaultpath..";") == expected)
end

convert(";")
convert(";;")
convert(";;;")
convert(";;;;")
convert(";;;;;")
convert(";;a;;;bc")


-- test -l over multiple libraries
prepfile("print(1); a=2; return {x=15}")
prepfile(("print(a); print(_G['%s'].x)"):format(prog), otherprog)
RUN('env LUA_PATH="?;;" lua -l %s -l%s -lstring -l io %s > %s', prog, otherprog, otherprog, out)
checkout("1\n2\n15\n2\n15\n")

-- test 'arg' table
local a = [[
  assert(#arg == 3 and arg[1] == 'a' and
         arg[2] == 'b' and arg[3] == 'c')
  assert(arg[-1] == '--' and arg[-2] == "-e " and arg[-3] == '%s')
  assert(arg[4] == nil and arg[-4] == nil)
  local a, b, c = ...
  assert(... == 'a' and a == 'a' and b == 'b' and c == 'c')
]]
a = string.format(a, progname)
prepfile(a)
RUN('lua "-e " -- %s a b c', prog)   -- "-e " runs an empty command

-- test 'arg' availability in libraries
prepfile"assert(arg)"
prepfile("assert(arg)", otherprog)
RUN('env LUA_PATH="?;;" lua -l%s - < %s', prog, otherprog)

-- test messing up the 'arg' table
RUN('echo "print(...)" | lua -e "arg[1] = 100" - > %s', out)
checkout("100\n")
NoRun("'arg' is not a table", 'echo "" | lua -e "arg = 1" -')

-- test error in 'print'
RUN('echo 10 | lua -e "print=nil" -i > /dev/null 2> %s', out)
assert(string.find(getoutput(), "error calling 'print'"))

-- test 'debug.debug'
RUN('echo "io.stderr:write(1000)\ncont" | lua -e "require\'debug\'.debug()" 2> %s', out)
checkout("lua_debug> 1000lua_debug> ")

-- test many arguments
prepfile[[print(({...})[30])]]
RUN('lua %s %s > %s', prog, string.rep(" a", 30), out)
checkout("a\n")

RUN([[lua "-eprint(1)" -ea=3 -e "print(a)" > %s]], out)
checkout("1\n3\n")

-- test iteractive mode
prepfile[[
(6*2-6) -- ===
a =
10
print(a)
a]]
RUN([[lua -e"_PROMPT='' _PROMPT2=''" -i < %s > %s]], prog, out)
checkprogout("6\n10\n10\n\n")

prepfile("a = [[b\nc\nd\ne]]\n=a")
RUN([[lua -e"_PROMPT='' _PROMPT2=''" -i < %s > %s]], prog, out)
checkprogout("b\nc\nd\ne\n\n")

prompt = "alo"
prepfile[[ --
a = 2
]]
RUN([[lua "-e_PROMPT='%s'" -i < %s > %s]], prompt, prog, out)
local t = getoutput()
assert(string.find(t, prompt .. ".*" .. prompt .. ".*" .. prompt))

-- test for error objects
prepfile[[
debug = require "debug"
m = {x=0}
setmetatable(m, {__tostring = function(x)
  return tostring(debug.getinfo(4).currentline + x.x)
end})
error(m)
]]
NoRun(progname .. ": 6\n", [[lua %s]], prog)

prepfile("error{}")
NoRun("error object is a table value", [[lua %s]], prog)


-- chunk broken in many lines
s = [=[ -- 
function f ( x ) 
  local a = [[
xuxu
]]
  local b = "\
xuxu\n"
  if x == 11 then return 1 + 12 , 2 + 20 end  --[[ test multiple returns ]]
  return x + 1 
  --\\
end
return( f( 100 ) )
assert( a == b )
do return f( 11 ) end  ]=]
s = string.gsub(s, ' ', '\n\n')   -- change all spaces for newlines
prepfile(s)
RUN([[lua -e"_PROMPT='' _PROMPT2=''" -i < %s > %s]], prog, out)
checkprogout("101\n13\t22\n\n")
  
prepfile[[#comment in 1st line without \n at the end]]
RUN('lua %s', prog)
  
prepfile[[#test line number when file starts with comment line
debug = require"debug"
print(debug.getinfo(1).currentline)
]]
RUN('lua %s > %s', prog, out)
checkprogout('3')

-- close Lua with an open file
prepfile(string.format([[io.output(%q); io.write('alo')]], out))
RUN('lua %s', prog)
checkout('alo')

-- bug in 5.2 beta (extra \0 after version line)
RUN([[lua -v  -e"print'hello'" > %s]], out)
t = getoutput()
assert(string.find(t, "PUC%-Rio\nhello"))


-- testing os.exit
prepfile("os.exit(nil, true)")
RUN('lua %s', prog)
prepfile("os.exit(0, true)")
RUN('lua %s', prog)
prepfile("os.exit(true, true)")
RUN('lua %s', prog)
prepfile("os.exit(1, true)")
NoRun("", "lua %s", prog)   -- no message
prepfile("os.exit(false, true)")
NoRun("", "lua %s", prog)   -- no message

-- remove temporary files
assert(os.remove(prog))
assert(os.remove(otherprog))
assert(not os.remove(out))

-- invalid options
NoRun("unrecognized option '-h'", "lua -h")
NoRun("unrecognized option '---'", "lua ---")
NoRun("unrecognized option '-Ex'", "lua -Ex")
NoRun("unrecognized option '-vv'", "lua -vv")
NoRun("unrecognized option '-iv'", "lua -iv")
NoRun("'-e' needs argument", "lua -e")
NoRun("syntax error", "lua -e a")
NoRun("'-l' needs argument", "lua -l")


if T then   -- auxiliary library?
  print("testing 'not enough memory' to create a state")
  NoRun("not enough memory", "env MEMLIMIT=100 lua")
end
print('+')

print('testing Ctrl C')
do
  -- interrupt a script
  local function kill (pid)
    return os.execute(string.format('kill -INT %d 2> /dev/null', pid))
  end

  -- function to run a script in background, returning its output file
  -- descriptor and its pid
  local function runback (luaprg)
    -- shell script to run 'luaprg' in background and echo its pid
    local shellprg = string.format('%s -e "%s" & echo $!', progname, luaprg)
    local f = io.popen(shellprg, "r")   -- run shell script
    local pid = f:read()   -- get pid for Lua script
    print("(if test fails now, it may leave a Lua script running in \z
            background, pid " .. pid .. ")")
    return f, pid
  end

  -- Lua script that runs protected infinite loop and then prints '42'
  local f, pid = runback[[
    pcall(function () print(12); while true do end end); print(42)]]
  -- wait until script is inside 'pcall'
  assert(f:read() == "12")
  kill(pid)  -- send INT signal to Lua script
  -- check that 'pcall' captured the exception and script continued running
  assert(f:read() == "42")  -- expected output
  assert(f:close())
  print("done")

  -- Lua script in a looooog unbreakable search
  local f, pid = runback[[
    print(15); string.find(string.rep('a', 1000), '.*.*.*.*.*b')]]
  -- wait (so script can reach the loop)
  assert(f:read() == "15")
  assert(os.execute("sleep 1"))
  -- must send at least two INT signals to stop this Lua script
  local n = 100
  for i = 0, 100 do   -- keep sending signals
    if not kill(pid) then   -- until it fails
      n = i   -- number of non-failed kills
      break
    end
  end
  assert(f:close())
  assert(n >= 2)
  print(string.format("done (with %d kills)", n))

end

print("OK")
