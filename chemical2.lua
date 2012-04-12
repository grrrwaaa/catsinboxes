-- see "Efficient exact stochastic simulation of chemical systems with many species and many channels"

--[[ TODO: what to do about concentrations that reach 0?
	Once they reach zero, the 'next time' becomes inf.
	In reality, we'll always have some external interactions: chemicals that re-enter the system. When they do, we need to recalculate the next reaction times.
	
	Also want to add factors for e.g. volume, temperature... 
	
--]]

--math.randomseed(os.time())
elements = { A = 100, B = 100, C = 100, D = 100, E = 100, F = 100, G = 100 }

--[[

A + B 	-> C 		@k1
B + C 	-> D 		@k2
D + E 	-> E + F 	@k3
F 		-> D + G 	@k4
E + G 	-> A 		@k5

--]]

reagents = {
	{ A = 1, B = 1 },
	{ B = 1, C = 1 },
	{ D = 1, E = 1 },
	{ F = 1 },
	{ E = 1, G = 1 },
}

products = {
	{ C = 1 },
	{ D = 1 },
	{ E = 1, F = 1 },
	{ D = 1, G = 1 },
	{ A = 1 },
}

local rate = 1
k = { rate, rate, rate, rate, rate }	-- rate constants

-- dependency graphs (sparse matrices, boolean value)
-- which reaction propensity each element concentration affects:
affects = {}
-- which reaction propensities each reaction affects:
dependencies = {} 

for e in pairs(elements) do
	affects[e] = {}
	for i, r in ipairs(reagents) do
		if r[e] then
			affects[e][i] = true
		end
	end
end

-- calculate reaction dependency graph:
for i, r in ipairs(reagents) do
	-- find out reaction sum:
	dependencies[i] = {}
	for e in pairs(elements) do
		local sum = ((products[i][e] or 0) - (reagents[i][e] or 0))
		if sum ~= 0 then
			-- each dependent:
			for j in pairs(affects[e]) do
				dependencies[i][j] = true
			end
		end
	end
end

-- propensity of reaction
a = {}
-- putative time of next reaction
time = {}
t = 0

-- priority queue of 'next reactions'
q = {}
qsort = function(a, b)
	return time[a] < time[b]
end

function calculate_propensity(i)
	a[i] = k[i]
	-- this would be partially evaluated by JIT:
	for e, n in pairs(reagents[i]) do
		a[i] = a[i] * math.max(0, elements[e]^n)
	end
end

function calculate_time(i)
	if a[i] > 0 then
		local r = math.random(10)
		time[i] = t + (r*r)/a[i]	-- is this right?
	else
		time[i] = math.huge			
	end
end

for i, _ in ipairs(k) do
	calculate_propensity(i)
	calculate_time(i)
	
	-- insert to queue:
	table.insert(q, i)
end


-- sort queue:
table.sort(q, qsort)

-- list queue:
print(unpack(q))

local reactioncount = 0
counts = {}
for i = 1, #reagents do
	counts[i] = 0
end

local i = q[1]
--t = math.huge
while i and t < math.huge do
	-- get next reaction:
	t = time[i]
	--[[
	print("+++++++++++++++++++++++++++++++++++")
	print("reaction", i, "at", t)
	print("time", unpack(time))
	print("q", unpack(q))
	print("a", unpack(a))
	--]]
	print(t)
	
	if a[i] > 0 then
		counts[i] = counts[i] + 1
	
		-- THIS IS A STRONG CANDIDATE FOR JIT:
		
		-- execute reaction:
		for e, n in pairs(reagents[i]) do	-- static eval loop for JIT
			elements[e] = elements[e] - n	-- n static
		end	
		for e, n in pairs(products[i]) do	-- static eval loop for JIT
			elements[e] = elements[e] + n	-- n static
		end
		-- update propensities:
		for d in pairs(dependencies[i]) do	-- static eval loop for JIT
			-- recalculate a[j]
			local olda = a[d]
			calculate_propensity(d)	--< inline for JIT
			if i == d then
				calculate_time(d)	--< inline for JIT
			else
				-- modify the next reaction time:
				if a[d] > 0 then
					-- just stretch (or shrink):
					time[d] = t + (time[d] - t)*(olda/a[d])
				else
					time[d] = math.huge
				end
			end
		end
		
		-- THIS COULD BE FASTER (using a proper priority queue)
		-- and re-sort only the changed items?
		-- re-sort q:
		table.sort(q, qsort)
	end
	
	
	wait(0.001)
	i = q[1]
	
	reactioncount = reactioncount + 1
end

for k, e in pairs(elements) do
	print(k, e)
end 

print("reaction counts", unpack(counts))

print("done after", reactioncount)