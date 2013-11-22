--[[

Concatenative language
@see http://www.concatenative.org/wiki/view/Concatenative%20language

Juxtaposition of values denotes function composition (like Forth: 2 3 +)
i.e. foo(bar(baz(x))) is just "baz bar foo"
Values are not named (tacit), only subroutines are named.
Uses multiple stacks, e.g. mapping [past-value-stack future-call-queue] to [past-value-stack future-call-queue].

As a byte-code format, it is amenable to streaming (infinite streams!), metaprogramming, stack-machine implementation; probably also good for generative programming, evo etc. 
Can also apply map-reduce: divide a program into fragments to compile in parallel, then join up the results (relies on compiler supporting partial eval)
Naturally push-oriented.

Concat functions are called "words".
I guess that FAUST is a concatenative language.

Anonymous functions are quotations.
Functions that take functions (quotations) as args are combinators.

Factor uses [] to create a quotation. E.g. "[2 >]" defines a function that returns the function "2 >". 

It could be cute to extend the parser to recognize infix operations, perhaps by wrapping in some syntax. Could be a pre-processor step (macro):

Multiple queue/stack pairs would support parallelism (coroutines). Easy to also imagine adding a "wait" word to yield control from run() back to a scheduler.



--]]
local format = string.format
local concat = table.concat

--- utilities

-- generate a unique name
local uid = (function()
	local id = 0
	return function(prefix)
		id = id + 1
		return (prefix or "v") .. id
	end
end)()

-- friendly print:
function stackdump(s)
	-- create reversed copy of s for easy printing
	local s1 = {} for i = #s, 1, -1 do s1[#s1+1] = tostring(s[i]) end
	return table.concat(s1, ", ") 
end

--- literal
-- an object to represent a string
local literal = { isliteral = true }
literal.__index = literal
function literal.new(str) return setmetatable({str=str, raw=str:sub(2, #str-1)}, literal) end
function literal:__tostring() return self.raw end

--- stack
-- stack is needed for the results list. semantics of FIFO.
local stack = { isstack = true }
stack.__index = stack
function stack:__tostring() return format("{%s}", stackdump(self)) end
function stack.new() return setmetatable({}, stack) end
function stack:push(w) self[#self+1] = w return self end
function stack:pop() return table.remove(self) end
function stack:top() return self[#self] end

--- queue
-- for the continuations list. semantics of LIFO.
local q = { isq = true }
q.__index = q
function q.new() return setmetatable({}, q) end
function q:__tostring() return format("[%s]", stackdump(self)) end
function q:insert(w) table.insert(self, w) return self end
function q:enqueue(w) table.insert(self, 1, w) return self end
function q:dequeue() return table.remove(self) end

--- list
-- for creating data within code. semantics of LIFO.
local list = {}
list.__index = list
function list.new() return setmetatable({}, list) end
function list:enqueue(w) self[#self+1] = w return self end
function list:dequeue() return table.remove(self, 1) end

function list:parse_tokens(tokens, i)
	i = i or 1
	local w = tokens[i]
	while w ~= nil do
		if type(w) == "string" and w:sub(1, 1) == "}" then
			return self, i
		end
		self[#self+1] = w
		i = i + 1
		w = tokens[i]
	end
end

function q:parse_tokens(tokens, i)
	-- interate tokens:
	i = i or 1
	local w = tokens[i]
	while w ~= nil do
		-- check for special syntax:
		if type(w) == "string" then
			local c = w:sub(1, 1)
			-- a list
			if c == "{" then
				-- start a list:
				w, i = list.new():parse_tokens(tokens, i+1)
			-- q queue
			elseif c == "[" then
				local quote = q.new()
				--quote.isquote = true
				w, i = quote:parse_tokens(tokens, i+1)
			elseif w:sub(#w, #w) == "]" then
				-- end of queue:
				return self, i
			end
		end
		-- add to queue:
		self:enqueue(w)
		-- iterate:
		i = i + 1
		w = tokens[i]
	end
end

-- parse a string of code to fill the queue:
function q:parse(code)
	-- tokenize
	local tokens = {}
	for w in code:gmatch("[^%s]+") do 	
		-- filter special values (numbers, booleans)
		local n = tonumber(w)
		if n then
			w = n
		elseif w == "false" then
			w = false
		elseif w == "true" then
			w = true
		else
			-- filter string literals:
			local c = w:sub(1, 1)
			if c == '"' or c == "'" then
				-- it is a literal string:
				w = literal.new(w)
			end
		end
		tokens[#tokens+1] = w 
	end	
	--print( #tokens, "tokens:", unpack(tokens))	
	self:parse_tokens(tokens, 1)
	return self
end

--- execution context
-- has a stack of results and queue of continuations
local ctx = {}
ctx.__index = ctx
function ctx.new()
	return setmetatable({ 
		s = stack.new(), 	-- stack of computed values (past)
		q = q.new(), 		-- queue of remaining operations to compute	(future)
	}, ctx)
end

-- takes a quotation and adds to the current queue
function ctx:eval(q)
	assert(q ~= self.q, "recursive eval???")
	if type(q) == "table" then
		for i = #q, 1, -1 do self.q:insert(q[i]) end
	else
		self:push(q)
	end
	return self
end

-- push/pop/top refer to the results stack:
function ctx:push(w) self.s:push(w) return self end
function ctx:pop() 
	assert(#self.s > 0, "stack underflow (partial evaluation NYI)")
	return self.s:pop() 
end
function ctx:top() return self.s:top() end

-- debugging:
function ctx:dump() 
	return tostring(self.s) 
			.. "\t => \t" 
			.. tostring(self.q)
end
ctx.__tostring = ctx.dump

-- parse a string of code to fill the queue:
function ctx:parse(code)
	self.q:parse(code)
	return self
end

-- built-in operators ("words"):
local words = {}

-- stack operations:
-- push the current stack size:
words.stack_size = function(self)
	self:push(#self.s)
end
-- pushes a copy of top item (duplicates it)
words.dup = function(self)
	assert(#self.s > 0, "insufficient values on stack (partial evaluation NYI)")
	self:push(self:top())
end

words["unpack"] = function(self)
	assert(#self.s >= 1, "insufficient values on stack (partial evaluation NYI)")
	local t = self:pop()
	if type(t) == "table" then
		for i = 1, #t do
			self:push(t[i])
		end	
	else
		-- fail gracefully!
		self:push(t)
	end
end

words["if"] = function(self)
	assert(#self.s >= 3, "insufficient values on stack (partial evaluation NYI)")
	local f, t, cond = self:pop(), self:pop(), self:pop()
	if cond then
		self:eval(t)
	else
		self:eval(f)
	end
end

-- math:
words["+"] = function(self)
	assert(#self.s > 1, "insufficient values on stack (partial evaluation NYI)")
	local b, a = self:pop(), self:pop()
	self:push(a + b)
end
words["-"] = function(self)
	assert(#self.s > 1, "insufficient values on stack (partial evaluation NYI)")
	local b, a = self:pop(), self:pop()
	self:push(a - b)
end
words["*"] = function(self)
	assert(#self.s > 1, "insufficient values on stack (partial evaluation NYI)")
	local b, a = self:pop(), self:pop()
	self:push(a * b)
end
words["/"] = function(self)
	assert(#self.s > 1, "insufficient values on stack (partial evaluation NYI)")
	local b, a = self:pop(), self:pop()
	self:push(a / b)
end

words["print"] = function(self)
	print(self:pop())
end

-- run whatever is currently available:
function ctx:run()
	print("run:\n\t", self)
	-- get next token:
	local w = self.q:dequeue()
	while w ~= nil do
		-- pass constants (literals, numbers, etc.)
		if type(w) == "table" and w.isliteral then
			self:push(tostring(w))
		elseif type(w) ~= "string" then
			self:push(w)
		else
			-- assume it is a "word" (operator)
			local op = words[w]
			if op then
				print("\t", w, self)	
				-- dispatch to the implementation of this word:
				op(self)
			else
				print("skipping operation", w)
			end
		end
		-- iterate to next word:
		w = self.q:dequeue()
	end
	print("remainder:", unpack(self.s))
	return unpack(self.s)
end

-- inherit the standard words:
local genwords = setmetatable({}, {__index = words})

-- specialize:
genwords["+"] = function(self, res)
	assert(#self.s > 1, "insufficient values on stack (partial evaluation NYI)")
	local b, a = self:pop(), self:pop()
	local v = uid("v")
	res[#res+1] = format("local %s = %s + %s", v, a, b)
	self:push(v)
end
genwords["-"] = function(self, res)
	assert(#self.s > 1, "insufficient values on stack (partial evaluation NYI)")
	local b, a = self:pop(), self:pop()
	local v = uid("v")
	res[#res+1] = format("local %s = %s - %s", v, a, b)
	self:push(v)
end
genwords["*"] = function(self, res)
	assert(#self.s > 1, "insufficient values on stack (partial evaluation NYI)")
	local b, a = self:pop(), self:pop()
	local v = uid("v")
	res[#res+1] = format("local %s = %s * %s", v, a, b)
	self:push(v)
end
genwords["/"] = function(self, res)
	assert(#self.s > 1, "insufficient values on stack (partial evaluation NYI)")
	local b, a = self:pop(), self:pop()
	local v = uid("v")
	res[#res+1] = format("local %s = %s / %s", v, a, b)
	self:push(v)
end

genwords["print"] = function(self, res)
	assert(#self.s > 0, "insufficient values on stack (partial evaluation NYI)")
	res[#res+1] = format("print(%s)", self:pop())
end


genwords["unpack"] = function(self, res)
	assert(#self.s >= 1, "insufficient values on stack (partial evaluation NYI)")
	local t = self:pop()
	if type(t) == "table" then
		for i = 1, #t do
			local v = uid("v")
			print(v, t[i])
			res[#res+1] = format("local %s = %s", v, tostring(t[i]))
			self:push(v)
		end	
	else
		-- fail gracefully!
		local v = uid("v")
		res[#res+1] = format("local %s = %s", v, tostring(t))
		self:push(v)
	end
end

genwords["if"] = function(self, res)
	assert(#self.s >= 3, "insufficient values on stack (partial evaluation NYI)")
	local f, t, cond = self:pop(), self:pop(), self:pop()
	-- here we could distinguish between static and dynamic if
	-- assume dynamic by default
	
	-- it's really complicated: we need a separate stack for the sub-scope
	-- we also need to pre-declare "out" variables for any left on the stack after each branch of the if
	-- complex, since different branches could leave different numbers of returns
	-- I guess leftover outputs will have value nil
	
	error("IF is not working for codegen yet")
	
	res[#res+1] = format("if %s then", tostring(cond))
		self:geneval(t)
	res[#res+1] = "else"
		self:geneval(f)
	res[#res+1] = "end"
	
	for i, v in ipairs(res) do print(v) end
end

-- takes a quotation and adds to the current queue
function ctx:geneval(q)
	assert(q ~= self.q, "recursive eval???")
	if type(q) == "table" then
		for i = #q, 1, -1 do 
			print("eval", i, q[i])
			self:gen_word(q[i], res)
		end
	else
		self:gen_word(q, res)
	end
	return self
end

function ctx:gen_word(w, res)
	-- debug: operation plus stack
	print("\t", w, self)	
	
	if type(w) ~= "string" then
		self:push(w)
	else
		local op = genwords[w]
		if op then
			-- dispatch to the implementation:
			op(self, res)
			print("generated", res[#res])
		else
			print("skipping operation", w)
		end
	end
end

-- generate code for whatever is available:
function ctx:gen()
	local res = {}
	local w = self.q:dequeue()
	while w ~= nil do
		self:gen_word(w, res)
		-- next word:
		w = self.q:dequeue()
	end
	return res
end

function ctx:run_test(code)
	return self:parse(code):run()
end

function ctx:gen_test(code)
	print("gen:")
	self:parse(code)
	local res = self:gen()
	res = table.concat(res, "\n")
	print("done", self)
	local lua = format("%s\nreturn %s\n", res, table.concat(self.s, ", "))
	print("--- generated Lua code: ---")
	print(lua)
	print("--- run: ---")
	loadstring(lua)()
end

--------------------------------------------------------------------------------
-- Test
--------------------------------------------------------------------------------

local code = [[18 7 4 - / dup * print]]

--local code = [[ true [ "ok" ] [ { "notok" 1 2 3 } ] if unpack print ]]


-- load some code & run it:
ctx.new():run_test(code)
-- use the same principle to generate code:
ctx.new():gen_test(code)
