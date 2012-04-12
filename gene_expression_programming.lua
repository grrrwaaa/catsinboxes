
--[[
	Gene expression programming
	@see http://www.gene-expression-programming.com/GepBook/Chapter2/Section1/SS1.htm
--]]

function binop(op) return function(_, a, b) return string.format("(%s %s %s)", a, op, b) end end
function cmp(op) return function(_, a, b) return string.format("((%s %s %s) and 1 or 0)", a, op, b) end end
function call1(op) return function(_, a) return string.format("%s(%s)", op, a) end end
function call2(op) return function(_, a, b) return string.format("%s(%s, %s)", op, a, b) end end


-- library of operators:
lib = {
	{ name = "add", char = "+", arity = 2, impl = binop("+"), },
	{ name = "sub", char = "-", arity = 2, impl = binop("-"), },
	{ name = "mul", char = "*", arity = 2, impl = binop("*"), },
	{ name = "div", char = "/", arity = 2, impl = binop("/"), },
	{ name = "pow", char = "^", arity = 2, impl = binop("^"), },
	{ name = "mod", char = "%", arity = 2, impl = binop("%"), },
	{ name = "gt", char = ">", arity = 2, impl = cmp(">"), },
	{ name = "lt", char = "<", arity = 2, impl = cmp("<"), },
	{ name = "sqrt", char = "Q", arity = 1, impl = call1("math.sqrt"), },
	{ name = "abs", char = "A", arity = 1, impl = call1("math.abs"), },
	{ name = "sin", char = "S", arity = 1, impl = call1("math.sin"), },
	{ name = "cos", char = "C", arity = 1, impl = call1("math.cos"), },
	{ name = "tan", char = "T", arity = 1, impl = call1("math.tan"), },
	{ name = "log", char = "L", arity = 1, impl = call1("math.log"), },
	{ name = "floor", char = "F", arity = 2, impl = call2("math.floor"), },
	{ name = "exp", char = "E", arity = 2, impl = call2("math.exp"), },
	{ name = "max", char = "M", arity = 2, impl = call2("math.max"), },
	{ name = "pi", char = "P", arity = 0, impl = function() return "math.pi" end, },
	
}
-- reverse lookup:
for i, v in ipairs(lib) do lib[v.char] = v end

function random_codon(percent)
	local percent = percent or math.random()
	if math.random() >= percent then
		return lib[math.random(#lib)].char
	elseif math.random() < 0.25 then
		return math.random(9)
	else
		return string.char(96+math.random(2))
	end
end

-- generate random gene:
function random_gene(len)
	local codons = {}
	for i = 1, len do
		local percent = (i-1)/len
		codons[i] = random_codon(percent)
	end
	return table.concat(codons, "")
end

local test_gene = "Q*-+abcd"

--[[
	-- should get:
	{ "Q", { "*", {"-", "b", "a"}, {"+", "c", "d"}, } }
	-- in lines:
	1: { "Q", t21 }
	2: t21={ "*", t31, t32, }
	3: t31={"-", "b", "a"}, t32={"+", "c", "d"}
--]]


function consume(str)
	return str:sub(1, 1), str:sub(2)
end

function express(str)
	local root = {}
	local linescount = 1
	local lines = { { root } }
	local c
	local line = 1
	while line <= #lines do
		local nextline
		for i, v in ipairs(lines[line]) do
			c, str = consume(str)
			c = c or 0
			local op = lib[c]
			if op and op.arity > 0 then
				local arity = op.arity
				v[1] = op.char
				nextline = nextline or {}
				for a = 2, op.arity+1 do
					v[a] = {}	-- placeholder for argument
					table.insert(nextline, v[a])
				end
				lines[line+1] = nextline
			elseif c == "" then
				-- we ran out of codons!
				v[1] = "1"	
			else
				v[1] = c
			end
		end
		line = line + 1
	end
	return root, str
end

function nodetostring(node)
	if type(node) == "table" then
		local args = {}
		for i, v in ipairs(node) do args[i] = nodetostring(v) or "?" end
		return string.format("{%s}", table.concat(args, ","))
	else
		return tostring(node)
	end
end

function nodetolua_aux(node)
	if type(node) == "table" then
		local args = {}
		for i, v in ipairs(node) do args[i] = nodetolua_aux(v) end
		local op = lib[node[1]]
		if op then
			return op.impl(unpack(args))
		else
			return node[1]
		end
	else
		return tostring(node)
	end
end

function nodetolua(node)
	return string.format("return function(a, b) return %s end", nodetolua_aux(node))
end

-- mutates a random codon
function mutate(str)
	local index = math.random(#str)
	return str:sub(1, index-1) .. random_codon(index/#str) .. str:sub(index+1)
end

-- inserts a random codon
function mutate_extend(str)
	local index = math.random(#str)
	return str:sub(1, index) .. random_codon(index/#str) .. str:sub(index+1)
end

-- mutates a percentage (based on length) of the gene, (minimum 1 codon)
function mutate_percent(str, percent)
	local percent = percent or 0
	local mutations = math.max(1, percent * #str)
	for i = 1, mutations do str = mutate(str) end
	return str
end

-- crossover with another gene
-- equal length genes are kept equal
-- variable length genes will produce something in-between
function crossover(str1, str2)
	if #str1 == #str2 then
		local index = math.random(#str1)
		return str1:sub(1, index) .. str2:sub(index+1)
	else
		if math.random(2) == 1 then
			str2, str1 = str1, str2
		end
		local percent = math.random()
		local index1 = percent * #str1
		local index2 = percent * #str2
		return str1:sub(1, index1) .. str2:sub(index2+1)
	end
end


-- population --

local function mag(vec)
	local sum = 0
	for i, v in ipairs(vec) do sum = sum + v*v end
	return math.sqrt(sum)
end

local function dot(a, b)
	local sum = 0
	for i, v in ipairs(a) do sum = sum + a[i]*b[i] end
	return sum
end

function cosine_similarity(a, b)
	local ma, mb = mag(a), mag(b)
	-- we want ma * mb to be close. 
	return dot(a, b) / (ma*mb), mb/ma
end

function test_fitness(pop, target, cutoff)
	for i = cutoff, #pop do
		local p = pop[i]
		p.results = {}
		for k, params in ipairs(target.params) do
			p.results[k] = p.func(unpack(params))
		end
		p.fitness, p.ratio = cosine_similarity(p.results, target.results)
		if p.fitness == math.huge or p.fitness ~= p.fitness then 
			p.fitness = -1 
		end
	end
end

-- best result at pop[1], worst result at pop[#pop]
function sort_fitness(pop)
	table.sort(pop, function(a, b) return a.fitness > b.fitness end)
end

function make_population(size, target, newgene)
	local pop = {}
	for i = 1, size do
		local p = { gene = newgene() }
		p.node = express(p.gene)
		p.code = nodetolua(p.node)
		p.func = loadstring(p.code)()
		pop[i] = p
	end
	test_fitness(pop, target, 1)
	sort_fitness(pop)
	return pop
end

function run_population(pop, target, steps, ratio, mutatio)
	local lastbest = -1
	for i = 1, steps do
		-- breed half of population:
		local cutoff = math.floor(#pop * ratio)
		for i = cutoff, #pop do
			local mum = pop[math.random(cutoff)].gene
			local dad = pop[math.random(cutoff)].gene
			local p = { gene = mutate_percent(crossover(mum, dad), mutatio) }
			p.node = express(p.gene)
			p.code = nodetolua(express(p.gene))
			p.func = loadstring(p.code)()
			pop[i] = p
		end
		test_fitness(pop, target, cutoff)
		sort_fitness(pop)
		local best = pop[1].fitness
		if best > lastbest then
			lastbest = best
			print(string.format("gen %03d fit %0.6f %s", i, best, pop[1].gene))
			wait(0.01)
		end
	end
end

function newgene()
	return random_gene(math.random(60))
end

math.randomseed(os.time())
local target = { params = {}, results = {} }
for i = 1, 6 do
	target.params[i] = { math.random(), math.random() }
	target.results[i] = math.random()
end

local pop = make_population(500, target, newgene)
run_population(pop, target, 500, .1, .5)

print(pop[1].code)
for i, v in ipairs(target.results) do
	print(i, v - (pop[1].results[i] * pop[1].ratio))
end


print('ok')