--[[

Understanding is easier in a common language

SSA == CPS == ANF

--]]

--[[

// factorial in C:    
unsigned factorial(unsigned x) { 
	unsigned factor = x;
	unsigned result = 1;
	while (factor != 0) {
		result = result * factor;
		factor = factor - 1;
	}
	return result;
}
--]]

-- factorial in Lua:
local
function factorial(x)
	local factor = x
	local result = 1
	while factor ~= 0 do
		result = result * factor
		factor = factor - 1
	end
	return result
end
print(factorial(10))

--[[
In Lua bytecode:

	1	[22]	MOVE     	1 0
	2	[23]	LOADK    	2 -1	; 1
	3	[24]	EQ       	1 1 -2	; - 0
	4	[24]	JMP      	3	; to 8
	5	[25]	MUL      	2 2 1
	6	[26]	SUB      	1 1 -1	; - 1
	7	[26]	JMP      	-5	; to 3
	8	[28]	RETURN   	2 2

Which means:

entry:
	v1=x 		// factor (phi)
	v2=1		// result (phi)
test:
	v3 = neq v1, 0
	br v3 tru fal 		
fal:
	goto after;
tru:
	v2 = v2 * v1
	v1 = v1 - 1
	goto test;
after:
	goto exit;
exit:
	ret v2
--]]


--[[

; factorial in LLVM (SSA form)
define i32 @factorial(i32 %x) { 
entry:
  br label %test
test:
  %result = phi i32 [1, %entry], [%result_mul_factor, %neq0]
  %factor = phi i32 [%x, %entry], [%factor_sub_one, %neq0]
  %factor_eq_zero = icmp eq i32 %factor, 0
  br i1 %factor_eq_zero, label %eq0, label %neq0
eq0:                                  ; preds = %entry
  ret i32 %result
neq0:                                 ; preds = %entry
  %result_mul_factor = mul i32 %result, %factor
  %factor_sub_one = sub i32 %factor, 1
  br label %test 
}

--]]


-- SSA is uglier in Lua because there is no goto:
local
function factorial(x)
	local _from = "entry"	
	local _jump = "entry"
	-- phis:
	local result, factor
	-- all vars:
	local result_mul_factor, factor_sub_one, factor_eq_zero
	while true do
		-- phis:
		if _from == "entry" then
			result = 1
			factor = x
		elseif _from == "neq0" then
			result = result_mul_factor
			factor = factor_sub_one
		end
		-- frame pointer:
		_from = _jump
		
		if _jump == "entry" then
			_jump = "test"
		elseif _jump == "test" then
			-- assignments:
			factor_eq_zero = (factor == 0)
			-- cond branch:
			if factor_eq_zero then
				_jump = "eq0"
			else
				_jump = "neq0"
			end
		elseif _jump == "eq0" then
			-- ret branch:
			return result
		elseif _jump == "neq0" then
			result_mul_factor = result * factor
			factor_sub_one = factor - 1
			_jump = "test"
		end
	end
end
print(factorial(10))


--[[
; direct style
(define (factorial factor) 
	(if (= factor 0)
		1
		(* factor (factorial (- factor 1)))))

; continuation-passing style
(define (cps-factorial factor cont)
	(cps-equal factor 0 (lambda (factor_eq_zero)
			(if factor_eq_zero
				(cont 1)
				(cps-subtract factor 1 (lambda (factor_sub_one)
					(cps-factorial factor_sub_one (lambda (factorial_next)     
									(cps-multiply factor factorial_next cont)))))))))
--]]

-- CPS style requires new versions of every basic primitive
-- which take and use continuations
-- But notice how none of these functions return values... 
local function cps_equal(a, b, cont) cont(a == b) end
local function cps_subtract(a, b, cont) cont(a - b) end
local function cps_multiply(a, b, cont) cont(a * b) end

local 
function factorial(factor, cont)
	cps_equal(factor, 0, function(factor_eq_zero)
		if factor_eq_zero then
			cont(1)
		else
			cps_subtract(factor, 1, function(factor_sub_one)
				-- recursion:
				factorial(factor_sub_one, function(factorial_next)
					cps_multiply(factor, factorial_next, cont)
				end)
			end)
		end
	end)
end
-- even the way you call it is weird!
factorial(10, print)

-- variation lifting out all the lambdas, shows how computation got turned inside out
-- (i.e. back to front)
local 
function factorial(factor, cont)
	local function neq1(factorial_next)
		cps_multiply(factor, factorial_next, cont)
	end
	local function neq0(factor_sub_one)
		-- recursion:
		factorial(factor_sub_one, neq1)
	end
	local function test(factor_eq_zero)
		if factor_eq_zero then
			cont(1)
		else
			cps_subtract(factor, 1, neq0)
		end
	end
	cps_equal(factor, 0, test)
end
factorial(10, print)

--[[

Administrative Normal Form:

ANF grammar:

<def>   :=   define <f>			  	  ; global definition
<exp>   :=   let <x> = <v> in <exp>	    ; copy
		|	let <x> = <v>(<v*>) in <exp>  ; call
		|	<v>						   ; return value-of v
		|	<v>(<v*>)				     ; jump
		|	if <v> then <exp> else <exp>  ; branch
		|	letrec <f*> in <exp>		  ; labels
<f>	 	:=   <x>(<x'>) = <exp>
<v>	 	:=   <x> | <c>
<x>	 	:=   variables
<c> 	:=   constants

define factorial(x) =
	letrec 
	entry(factor,result) = 
		letrec 
		eq0() = result
		neq0() =
			let result_mul_factor = _mul(result, factor)
			in
				let factor_sub_one = _sub(factor, 1)
				in 
					entry(factor_sub_one,result_mul_factor)
		in
			let factor_eq_zero = _eq(factor, 0)
			in 
				if factor_eq_zero eq0() else neq0()
	in 
		entry(x, 1)
--]]

-- ANF -> Lua:
-- ANF is very easy to write in Lua, which already has local functions
-- Sub-blocks need to be explicitly turned into functions because Lua doesn't have chunks-with-return-values
function factorial(x)
	-- letrec:
	local function entry(factor, result)
		-- letrec:
		local function neq0() 
			-- let:
			local result_mul_factor = result * factor
			-- in, let:
			local factor_sub_one = factor - 1
			-- in:
			return entry(factor_sub_one, result_mul_factor)
		end
		-- in, let:
		local factor_eq_zero = (factor == 0)
		-- in:
		return factor_eq_zero and result or neq0()
	end
	-- in:
	return entry(x, 1)
end

print(factorial(10))

--[[

Try Lua -> ANF
	SSA -> ANF
	C -> ANF

Try ANF -> SSA
	ANF -> C
--]]

--[[
Lua -> ANF

while / for need to be turned into letrec / if

pre
while (cond) do
	block
end
post

->

pre
letrec after(phis1)
	post
end
letrec f(phis)		--> destinations: f, after
	if cond then
		block
		-- if there was a break, it would go to after(phis1)
		f(phis)
	else
		after(phis1)
	end
in 
f(phis)

--]]

--[[ 
ANF->C:

The main issue is the lack of local functions, which need to either be
	a) inlined
	b) turned into while() loops
	c) turned into global functions

Inlining is OK for relatively short functions and primitive operations, but not for recursive jumps. Inlining is also ok for a function that is only called once.

While loops are the only option for recursive jumps. To avoid using GOTO, this is only permitted in the tail-call position. It must go via a conditional branch to avoid infinite loops. There may be other restrictions.
It may be necessary only allow jumps in one side of a conditional branch to avoid multiple control-joins. 

Global functions should be a last resort. All scoped variables need to be converted to arguments, which shoudld be limited. 

Another issue to absorb is that ANF doesn't explicity capture side-effects. We will need to also allow void lets / returns. 


Advantages of ANF over SSA:
	Variable scope is more explicit. 
		This can help constant folding, type annotation etc.
	Phis are represented as arguments to calls. 

--]]



factorial = {
	{ "letrec", 
		{ "entry", { "factor", "result" }, 
			{ "letrec", 
				{ "eq0", {}, { "ret", "result" } }, 
				{ "neq0", {},  
					{ "let", "result_mul_factor", { "mul", "result", "factor" } },
					{ "let", "factor_sub_one", { "sub", "factor", 1 } },
					{ "br", { "entry", "factor_sub_one", "result_mul_factor" } },
				},
			},
			{ "let", "factor_eq_zero", { "eq", "factor", 0 } },
			{ "brcond", "factor_eq_zero", {"eq0"}, {"neq0"} }
		}
	},
	{ "br", { "entry", "x", 1 }},
}

local format = string.format

local
function ind(i)
	return string.rep("  ", i)
end

local
function p(t, indent)
	if type(t) == "table" then
		local op = t[1]
		if type(op) ~= "string" then
			local stats = {}
			for i, v in ipairs(t) do stats[i] = p(v, indent) end
			if #t > 0 then
				stats[#stats] = "return "..stats[#stats]
			end
			return table.concat(stats, "\n"..ind(indent))
		else
			local args = {}
			for i = 2, #t do
				args[i-1] = p(t[i], indent)
			end
			if op == "letrec" or op == "define" then
				local stats = {}
				for i = 2, #t do
					local f = t[i]
					local name = f[1]
					local args = f[2]
					local exp = f[3]
					local a = {}
					for j, v in ipairs(args) do a[j] = p(v, indent) end
					if op=="letrec" then
						stats[#stats+1] = format("local function %s(%s)", name, table.concat(a, ", "))
					else
						stats[#stats+1] = format("return function(%s)", table.concat(a, ", "))
					end
					stats[#stats+1] = p(exp, indent+1)
					stats[#stats+1] = "end"
				end
				return table.concat(stats, "\n"..ind(indent))
			elseif op == "let" then
				return format("local %s = %s", args[1], args[2])
			elseif op == "ret" then
				return format("return %s", args[1])
			elseif op == "br" then
				return format("%s", args[1])
			elseif op == "brcond" then
				return format("%s and %s or %s", args[1], args[2], args[3])
			elseif op == "mul" then
				return format("%s * %s", args[1], args[2])
			elseif op == "sub" then
				return format("%s - %s", args[1], args[2])
			elseif op == "eq" then
				return format("(%s == %s)", args[1], args[2])
			else
				return format("%s(%s)", op, table.concat(args, ", "))
			end
		end
	else
		return t
	end
end


--[[

; factorial in LLVM (SSA form)
define i32 @factorial(i32 %x) { 
entry:
  br label %test
test:
  %result = phi i32 [1, %entry], [%result_mul_factor, %neq0]
  %factor = phi i32 [%x, %entry], [%factor_sub_one, %neq0]
  %factor_eq_zero = icmp eq i32 %factor, 0
  br i1 %factor_eq_zero, label %eq0, label %neq0
eq0:                                  ; preds = %entry
  ret i32 %result
neq0:                                 ; preds = %entry
  %result_mul_factor = mul i32 %result, %factor
  %factor_sub_one = sub i32 %factor, 1
  br label %test 
}

--]]
function s(t, ctx)
	if type(t) == "table" then
		local op = t[1]
		if type(op) ~= "string" then
			local stats = {}
			for i, v in ipairs(t) do stats[i] = s(v, ctx) end
			return table.concat(stats, "\n")
		else
			if op == "define" then
				local def = t[2]
				local name, args, exp = unpack(def)
				local a = {}
				for i, v in ipairs(args) do
					a[i] = format("i32 %%%s", v)
				end
				local stats = {
					format("define i32 @%s(%s) {", name, table.concat(a, ", "))
				}
				ctx.name = "entry"
				stats[#stats+1] = s(exp, ctx)
				stats[#stats+1] = "}"
				return table.concat(stats, "\n")
			elseif op == "letrec" then
				--[[ 
				this is the most complex
				need to gather contents first, then write later
				need to gather phi applications as it goes
				where things are written depends on their type
				final order is: phis, lets, br, letrecs
				
				--]]
				local blocks = {}
				for i = 2, #t do
					local f = t[i]
					local name, args = f[1], f[2]
					
					local phis = {}
					local stats = {}
					local letrecs = {}
					
					local old = ctx[name]
					local oldname = ctx.name
					
					-- add this to the context:
					local label = { }
					ctx[name] = label
					ctx.name = name
					
					for j, v in ipairs(args) do
						label[j] = { name=v }
					end
					for i = 3, #f do
						local v = f[i]
						local op = v[1]
						if op == "letrec" then
							letrecs[#letrecs+1] = s(v, ctx)
						else
							stats[#stats+1] = s(v, ctx)
						end
					end
					
					-- capture phi nodes:
					blocks[#blocks+1] = format("%s:", name)
					for i, phi in ipairs(label) do
						local inphis = {}
						for j, v in ipairs(phi) do
							inphis[#inphis+1] = format("[%s, %%%s]", unpack(v))
						end	
						blocks[#blocks+1] = format("  %%%s = phi i32 %s", phi.name, table.concat(inphis, ", "))
					end
					
					blocks[#blocks+1] = table.concat(stats, "\n")
					blocks[#blocks+1] = table.concat(letrecs, "\n")
					
					ctx[name] = old or label
					ctx.name = oldname or "entry"
				end
				return table.concat(blocks, "\n")
			elseif op == "let" then
				return format("  %%%s = %s", t[2], s(t[3], ctx))
			elseif op == "br" then
				local call = t[2]
				local name = call[1]
				
				local label = ctx[name]
				-- TODO: phi sources
				-- need to look up function name
				-- to find out argument names
				-- then add binding
				local args = {}
				for i = 1, #call-1 do
					local v = s(call[i+1], ctx)
					local arg = label[i]
					arg[#arg+1] = { v, ctx.name }
					--args[i-1] = s(t[i], phis)
				end
				return format("  br label %s", name)
			elseif op == "brcond" then
				-- TODO: phi sources
				return format("  br i1 %%%s, label %%%s, label %%%s", t[2], s(t[3], ctx), s(t[4], ctx))
			elseif op == "ret" then
				return format("  ret i32 %%%s", t[2])
			else
				local args = {}
				for i = 2, #t do
					args[i-1] = s(t[i], ctx)
				end
				return format("%s %s ", op, table.concat(args, ", "))
			end
		end
	elseif type(t) == "number" then
		return tostring(t)
	else
		return format("%%%s", t)
	end
end

function parse(t) 
	local ps = p(t, 0) 
	local ss = s(t, {  })
	print(ss)
	return ps, assert(loadstring(ps))()
end

local code, f = parse{"define", {"factorial", {"x"}, factorial} }
print(code)
print( f(10) )

--[[
factorial = {
	{ "letrec", 
		-- entry has one internal and one external call
		{ "entry", { "factor", "result" }, 			
			{ "letrec", 
				{ "eq0", {}, { "ret", "result" } }, 
				{ "neq0", {},  
					{ "let", "result_mul_factor", { "mul", "result", "factor" } },
					{ "let", "factor_sub_one", { "sub", "factor", 1 } },
					{ "br", { "entry", "factor_sub_one", "result_mul_factor" } },
				},
			},
			{ "let", "factor_eq_zero", { "eq", "factor", 0 } },
			{ "brcond", "factor_eq_zero", {"eq0"}, {"neq0"} }
		}
	},
	{ "br", { "entry", "x", 1 }},
}

--]]

--[[

Essense of ANF: 

a chain of (zero or more) primitive bindings, whose RHS are either
	copy from constant
	copy from scoped local
	call (all arguments are scoped locals)
	function definitions with arguments (the only locations for possible phis)
		functions have a new chain as their body
chains may split with conditional branches
each chain is terminated by returning a local or tail-calling with locals

Another way of saying it, which may be more helfpul to understand:

BLOCK := LET* { BRANCH | RETURN }
LET	:=	local <VAR> = { <CONSTANT> | <VAR> | CALL | FUNC }
CALL := <VAR>(<VAR>*)
FUNC := function(<VAR>*) BLOCK end
BRANCH := if <VAR> then BLOCK else BLOCK end
RETURN := return { <VAR> | CALL }

Clearer here that FUNC and BRANCH are the two ways in which BLOCKS are nested.

Seems like it could be further normalized by removing the tail-call return, but probably more useful to use a tail-call return wherever possible, as it helps to detect loops.
Let bindings (including funcs) should be pushed as deeply as possible; no higher than the scope at which they are used.

A full valid ANF example in Lua:

-- top-level chain:
local a = 1		-- local copy
local c = function(i) 
	-- another chain here
	return i	-- terminator
end
local b = c(a)	-- local call
if d then
	-- another chain here
	return a	-- terminator
else
	-- alternate chain here
	return c(b)	-- terminator
end


TURNING THIS INTO C:

The let-constant is replaced by an alias to the constant value.
The let-var is similarly an alias to the argument.
The let-call rule must either be inlined (short functions or functions used only once) or require definition of a global function.

The let-func is like a GOTO label.  
It could be used as a body for inlining (short functions or single-use functions) or creat a new global function. Latter may be hard to do while preserving scoped variables (which need to become additional arguments...)
We can leverage the fact that the function is only used within later statements of the chain.


The branch may cause an if/else() or while(). We know it only captures a single return value. 
Raises the question of how side-effects are handled in ANF?
Is it enough to allow that some let bindings are effect-ful, and possibly the LHS is void?
(Also requires return void)

A tail-call return indicates a loop IFF the function is called from within its own body (or sub-blocks). To avoid infinite loops, there must be a branch between the tail-call and the func it calls.

func x(y)
	z = y-1
	if y > 0 then
		return x(z)
	else
		return 0
	end	
end

->

do {
	y = y-1;
} while (y > 0);

Otherwise, the tail-call is just another let-call, to be inlined or globalized:

func x(y)
	z = y-1
	return z
end
if y > 0 then
	return x(y)	
else
	return 0
end

->

if (y > 0) {
	return y-1;
} else {
	return 0;
}

Nested loops:

while (x > 0) {
	local y = x;
	while (y > 0) {
		x = x - y;
		y = y - 1;
		// cond/break or cond/continue here? 
	}
	x = -x / 2
}
-> x = 0


func f1(x)
	y = x
	func f2(x, y)
		if y > 0 then
			x1 = x - y
			y1 = y - 1
			return f2(x1, y1)
		else
			return x
		end
	end
	x1 = f2(x, y)
	x2 = -x1
	x3 = x2/2
	if x > 0 then
		return f1(x3)
	else
		return x3
	end
end	


Generally, a while() loop becomes a let-func terminated by a branch, one side of which tail-calls the function.

Reversed: a function which contains a sub-branch that calls itself is converted to a while(). If there is no branching in between, this is an infinite loop.

It may be preferable to use for() rather than while(), in order to properly identify constant-numbers of iterations (e.g. for i=1,4 do), which can be more efficiently implemented than a while()?

for i=1, 4 do
	x = x * (x-1)
end

->

func f0(x)
	func f1(x, i)
		if i < 4 then
			local x1 = x - 1
			local x2 = x * x1
			local i1 = i + 1
			return f1(x2, i1)
		else
			return x
		end
	end
	return f1(x, 1)
end

Function contains a call to itself (hence loop), via a branch (hence not infinite)
How to deduce that this always occurs four times only? Very hard. 
Notice that i is constant in all external calls to f1. 

i = 1;
while (i < 4) {
	x1 = x - 1;
	x2 = x * x1;
	i = i + 1;
	// tail-call to loop:
	x = x2;
	i = i1;
}
return x;


Notice how the loop branch alternates are always simple values. 
What happens if not:

func f1(x)
	if x > 0 then
		x1 = x - 1
		return f1(x1)
	else
		x1 = x * 2
		return x1	
	end
end

->

while (x > 0) {
	x1 = x - 1;
	x = x1;
}
return x * 2;

The non-looping branch is appended to the looping branch. 


How about a recursive call that is not a tail-call?

func f1(x)
	if x > 0 then
		x1 = x - 1
		x2 = f1(x1)
		x3 = x2 / 2
		return x3
	else
		x1 = x * 2
		return x1	
	end
end
	
->

The loop is created at the point of the call, and subsequent instructions follow to the return, followed by the non-looping branch.

while (x > 0) {
	x1 = x - 1;
	x = x1;
}
x3 = x / 2
x4 = x3 * 2
return x4

In all these cases the required annotation is to know which side of a branch is the looping one.

Recursive jumps create loops, non-recursive jumps create inlining or global functions.
Conditional within a recursive loop become the loop condition, outside a recursive loop create if() blocks.

What happens with multiple branches in a loop?:

func f1(x)
	if x > 0 then
		y = x * 2
		if y < 10 then
			return f1(y)
		else
			return 10
		end
	else
		return x
	end
end

-> explicit use of continue / break can handle more complex forms:

while (x > 0) {
	y = x * 2;
	if (y < 10) {
		x = y;
		continue;
	} else {
		x = 10;
		break;
	}
}
return x;



Multiple recursive jumps are harder. It's not really clear what this means.

func f1(x)
	if x > 0 then
		x1 = x - 1
		x2 = f1(x1)
		x3 = f1(x2)
		return x3
	else
		x1 = x * 2
		return x1	
	end
end



PROCEDURAL -> ANF:

pre
while (cond) {
	block (phis are any assignments to vars declared above the block)
}
post

->

pre
func loop1(phis)
	if cond then
		block
		return loop1(phis')
	else
		post
	end
end
loop1(phis)

For loop is a variation of while. init is appended to pre, incr is appended to block, and the incr phis are added to the block phis.

pre
for (init, cond, incr) {
	block (phis)
}
post 

->

pre
init
func loop1(phis, incrphis)
	if cond then
		block
		incr
		return loop1(phis', incrphis)
	else
		post
	end
end
loop1(phis, incrphis)

Detecting a numeric for is hard this way; perhaps a 'special form' variant of if?

for i = 1, 4 LETS in BLOCK


How does it handle multiple side-effects?

a, b = 0, 0
if x > 0 then
	a = 1
	b = 2
else
	if x < 0 then
		a = 2
	end
	b = 1
end
print(a,b)

->

func start(x) 
	func postif0(a,b)
		return print(a,b)
	end
	a, b = 0, 0
	if x > 0 then
		a1, b1 = 1, 2
		postif0(a1, b1)
	else
		funct postif1(a1)
			b1 = 1
			return postif0(a1, b1)
		end
		if x < 0 then
			a1 = 2
			return postif1(a1)
		else
			return postif1(a)
		end
	end
end

->

start(x) {
	a, b = 0, 0;
	// continues at postif0
	if (x > 0) {
		a1, b1 = 1, 2;
		// continue:
		a, b = a1, b1;
	} else {
		a1;
		// continues at postif1
		if (x < 0) {
			a1 = 2;
			// continue:
			a1 = a1;
		} else {
			// continue:
			a1 = a;
		}
		// postif1:
		b1 = 1;
		// continue:
		a = a1;
		b = b1;
	}
	// postif0(a, b)
	print(a, b);
}


func start(x) 
	func A(a,b)
		return print(a,b)
	end
	func B(a,b)
		return print(b,a)
	end
	a, b = 0, 0
	if x > 0 then
		a1, b1 = 1, 2
		A(a1, b1)
	else
		funct C(a1)
			b1 = 1
			return A(a1, b1)
		end
		if x < 0 then
			a1 = 2
			if al < x then
				return C(a1)
			else
				return B(a, b)
			end	
		else
			a1 = 3
			if a1 < x then
				return B(a, b)
			else
				return C(x)
			end
		end
	end
end



->

start(x) {
	
	if (x > 0) {
		a, b = 1, 2;
	} else {
		if (x < 0) {
			a1 = 2;
			if (a1 < x) {
				a = a1;
			} else {
				goto B...
			}
		} else {
			a1 = 3;
			if (a1 < x) {
				goto B...
			} else {
				a = x;
			}
			// C: 
			b1 = 1;
			a, b = a, b1;
		}
	}
	// A:
	print(a, b);
}


One option is to further restrict the ANF format that recursive calls can only be in the tail-call position (i.e. normal calls cannot be recursive). This seems consistent with the semantics of C/C++. Probably related to the fact that C/C++ can't represent coroutines either...

Notice that a function must be called from two or more locations, otherwise it could be inlined. 



--]]



--[[
local
function factorial(x)
	local factor = x
	local result = 1
	while factor ~= 0 do
		result = result * factor
		factor = factor - 1
	end
	return result
end

-- CONVERT ANF TO LUA (OR C):
	-- first necesary to annotate letrec labels with their sources
	-- found by searching for br and brcond
	-- e.g. 'entry' has one internal and one external source
	-- eq0 and neq0 have only one external source each
	-- one or more internal sources indicate a loop construct
	-- multiple internal sources indicates ???
	-- multiple external sources indicates a function?
	
	
	-- we know that entry() is called from inside itself, so there is a loop
	
	
	-- we're going to call entry() next:
	local factor
	local result
	factor = x	-- one external source, safe to assign here
	result = 1
	-- entry is a loop. what is the exit condition?
	while true do
		local factor_eq_zero = (factor == 0)
		if factor_eq_zero then	
			-- inline eq0 (single use)
			return result
		else
			-- inline neq0 (single use)
			local result_mul_factor = result * factor
			local factor_sub_one = factor - 1
			-- jump to entry... this means it is a loop!
			-- set phi args:
			factor = factor_sub_one
			result = result_mul_factor
			-- continue... 
		end
	end


The main issue is that several of the control-flow structures it can easily represent require the use of GOTO, which is obviously a no-no. 

E.g. psuedo-C or SSA code:

A:
	< stuff computing x >
	if x goto B else goto C
B: 
	< stuff computing y >
	if y goto D else goto E
C: 
	< stuff computing z >
	if z goto E else goto D
D: 
	< more stuff computing i >
	return i
E:
	< different stuff computing j >
	return j

This is a relatively simple example, with no backward jumps, but nevertheless tricky because there are two distinct routes from A to D as well as from A to E; i.e. it cannot be expressed in C as a tree of if() blocks. If some of the <stuff> sections are very simple it could be achieved by inlining them (duplicating code), otherwise it requires that either D or E be turned into global functions.  

There are some similar problems for backward jumps (loops); but these I think can be resolved by adding the constraint that backward jumps must go via a branch (to prevent infinite loops), must only occur in the tail-call position, and the alternate branch must not be backward jump. E.g. this is valid:

myfunc(x, lim) {
	return (x < lim) ? myfunc(x*2, lim) : x;
}
	
is relatively simply translated into:

int f(x) {
	while (x < lim) {
		x = x * 2;
	}
	return x;
}

But if myfunc wasn't in the tail-call position, it can easily get to the point where GOTO is required.
--]]

--[[

--]]
