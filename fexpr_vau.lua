--[[
http://www.dalnefre.com/wp/2011/11/fexpr-the-ultimate-lambda/
http://www.dalnefre.com/wp/2011/12/semantic-extensibility-with-vau/
http://fexpr.blogspot.com/2011/04/fexpr.html
http://www.wpi.edu/Pubs/ETD/Available/etd-090110-124904/unrestricted/jshutt.pdf
ftp://ftp.cs.wpi.edu/pub/techreports/pdf/05-07.pdf
http://lambda-the-ultimate.org/node/4093#comment-62873

Essence: 
Scheme's implicit apply() is applicative: evaluates arguments before combining them
The exceptions are the special forms and the quote.

Fexpr is an operative: combines without evaluating. Any operative can be wrapped into an applicative. 

--]]

-- util:
local format = string.format
local concat = table.concat

-- emulating LISP:
function lookup(expr, env) return env[expr] end
function car(expr) return expr[1] end
function cdr(expr) return {select(2,unpack(expr))} end
function map(f)
	return function(t, ...)
		local res = {}
		for i, v in ipairs(t) do res[i] = f(v, ...) end
		return res
	end
end

function make_environment(t, parent)
	local e = setmetatable(t or {}, parent)
	e.__index = e
	return e
end	

-- emulating Kernel:
-- the non-primitive types:
function is_symbol(expr) return type(expr) == "string" end
function is_environment(expr) return type(expr) == "table" and expr.__index == expr end
function is_applicative(expr) return (type(expr) == "table") and expr.op ~= nil end
function is_pair(expr) return (type(expr) == "table") and (expr.op == nil) and expr.__index == nil end
function is_operative(expr) return type(expr) == "function" end
-- move toward applicative (wrap) or operative (unwrap):
function wrap(combiner) return { op=combiner } end
function unwrap(combiner) return combiner.op end

-- utility for printing:
-- <applicative>
-- [list]
-- {environment}
-- primitive
function ser(t, more, ...)
	local res
	local ty = type(t)
	if ty == "table" then
		if is_applicative(t) then
			res = format("<%s>", tostring(t.op))
		elseif is_environment(t) then
			-- an environment (dictionary)
			local r = {}
			for k, v in pairs(t) do 
				if k ~= "__index" then
					r[#r+1] = format("%s=%s", k, ser(v)) 
				end
			end
			res = format("{%s}", concat(r, ", "))
		elseif is_pair(t) then
			res = format("(%s)", ser(unpack(t)))
		else
			print(tostring(t))
		end
	elseif t == nil then
		res = ""
	--elseif is_symbol(t) then
		--res = format("%q", tostring(t))
	else
		res = tostring(t)
	end
	if more then res = res .. " " .. ser(more, ...) end
	return res
end

local oldprint = print
local indent = 0
function print(...)	
	oldprint(format("%s%s", string.rep("    ", indent), ser(...)))
end
function push() indent = indent + 1 end
function pop() indent = indent - 1 end


--[[
($define! eval
   ($lambda (expr env)
      ($cond ((symbol? expr)  (lookup expr env))
             ((pair? expr)
                (combine (eval (car expr) env)
                         (cdr expr)
                         env)
             )
             (#t  expr))
	)
)

($define! combine
   ($lambda (combiner operands env)
      ($if (operative? combiner)
           (operate combiner operands env)
           (combine (unwrap combiner)
                    (map-eval operands env)
                    env)
       )
    )
)
--]]

-- the first element of pairs (lists, trees, structures) is understood as a combiner
--		combiner is reduced to an operative (eval'ing the operands as it goes)
-- 		then evaluated operands are passed to the operative
-- symbols are looked up in the environment
-- everything else (primitives) return themselves
function eval(expr, env)
	print("eval:", expr)
	push()
	if is_pair(expr) then
		local combiner = eval(car(expr), env)
		local operands = cdr(expr)
		-- if it is an applicative, then unwrap and eval the args
		-- until an operative is reached:
		while is_applicative(combiner) do
			print("combine:", combiner, operands)
			-- applicative; apply:
			-- unwrap combiner by one level
			combiner = unwrap(combiner)
			-- eval args:
			-- warning: this is a non-tail-call recursion.
			-- but could be eased noticing that it is an in-place replacement?
			-- todo: handle cyclic operands
			for i = 1, #operands do operands[i] = eval(operands[i], env) end
		end
		print("combine:", combiner, operands)
		assert(is_operative(combiner), "failed to reduce expression to operative")
		-- operative (does not eval the operands):
		--print("operative, operands:", combiner, operands)
		pop()
		-- dynamic env is passed to the operative
		-- but actually most operatives won't use it
		return combiner(operands, env)
	elseif is_symbol(expr) then
		pop()
		local v = assert(lookup(expr, env), format("%s is not bound", tostring(expr)))
		return v
	else
		pop()
		return expr
	end
end

-- here's an example of an essential primitive operative:
function opif(a, env)
	local test, consequent, alternative = unpack(a)
	if eval(test, env) then
		return eval(consequent, env)
	else
		return eval(alternative, env)
	end
end

-- some trivial primitives:
-- ($define! $quote
--	($vau (x) #ignore x))
quote = function(a, env) return a[1] end
-- ($define! get-current-environment
--	(wrap ($vau () e e)))
getenv = wrap(function(a, env) return env end)
-- ($define! list
--	(wrap ($vau x #ignore x))
list = wrap(function(a, env) return a end)


-- easier to wrap in Lua because we already have most of the primitive types, and function environments
-- the three primitive operatives are $if, $define! and $vau
-- all other applicatives and operatives can be constructed from these (in theory)
-- probably also want eq?, equal?, cons, make-environment, set-car!, set-cdr! as primitives
-- primitive types: list (pair), nil, string, environment (dictionary + __index), primitive operative (lua function/closure), applicative (table with op)

-- The primitive constructor of operatives is an operative called $vau, which is almost the same as Scheme's lambda in both syntax and semantics, except that the combiners it constructs are operative (do not evaluate their arguments)
-- not sure if this is right.
-- "$vau is the only way to create new environments in Kernel"

-- for us, $vau must define a Lua function with arguments (args, env)
function vau(va, ve)
	local argnames, static_env, body = unpack(va)
	print("vau:", argnames, body)
	return function(a, e)
		-- create temporary environment for call:
		local fenv = make_environment({}, ve) -- TODO: e or ve?
		-- bind operands:
		for i, k in ipairs(argnames) do fenv[k] = a[i] end
		--print("fenv", fenv)
		return eval(body, fenv)
	end
end

-- equiv:
-- ($define! list
--	($lambda x x))
--list = {"lambda", "x", "x" }

--[[
($define! $lambda
($vau (ptree . body) static-env
(wrap (eval (list* $vau ptree #ignore body)
static-env))))
--]]
lambda = function(a, env)
	print("lambda:", a)
	local params, body = unpack(a)		-- TODO: handle multiple statement body?
	-- greatly simplified by using Lua facilities:
	local f = vau({ params, "#ignore", body }, env)
	return wrap(f)
end

define = function(a, env)
	--print("define", name, val)
	local name, val = unpack(a)
	env[name] = eval(val, env)
end

--[[
An example of defining a fexpr-based 'and' special form:
($define! $and?
   ($vau x e
      ($cond ((null? x)         #t)
             ((null? (cdr x))   (eval (car x) e))
             ((eval (car x) e)  (apply (wrap $and?) (cdr x) e))
             (#t                #f))))

-- define fand...?
body = function(x, env)
	if x == nil then 
		return true
	elseif cdr(x) == nil then 
		return eval(car(x), env)
	elseif eval(car(x), env) then
		return apply(wrap(fand), cdr(x), env)
	else
		return false
	end
end--]]

local env = make_environment{ a=1, b=2, c=3, }

env.eval = wrap(eval)
env["if"] = wrap(opif)
env["+"] = wrap(function(a, env) return a[1] + a[2] end)
env["*"] = wrap(function(a, env) return a[1] * a[2] end)
env.neg = wrap(function(a, env) return -a[1] end)
env[">"] = wrap(function(a, env) return a[1] > a[2] end)
env.list = wrap(function(a, env) return a end)
env.lambda = lambda
env.vau = vau
env.define = define

function test(expr)
	print(eval(expr, env))
end

-- amazing that this works:

test{ "if", {">", 2, 1 }, 99, 0 }
test{ "eval", "neg", {"+", "a", "b" }}
test{ "lambda", {"x"}, "x" }
test{ { "lambda", {"x"}, { "*", "x", "x" } }, {"*", "b", "c"} }
test{ "define", "square", { "lambda", {"x"}, { "*", "x", "x" } } }
test{ "square", 4 }



