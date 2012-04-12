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

function factorial(x)
	-- letrec:
	local function entry(factor, result)
		-- letrec:
		local function eq0() return result end
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
		return factor_eq_zero and eq0() or neq0()
	end
	return entry(x, 1)
end

print(factorial(10))

--[[

Try ANF -> SSA
	ANF -> Lua
	ANF -> C

But
? -> ANF?

--]]

--[[
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
--]]

--[[

Basic structure of a block:

	letrec functions*
		name, args, block
	let bindings*
		name, expression
	br tailcall | br conditional tailcall | return value

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
--print(code)
print( f(10) )