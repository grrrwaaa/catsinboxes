module(..., package.seeall)

require "lpeg"
local P = lpeg.P
local R = lpeg.R
local C = lpeg.C
local Cc = lpeg.Cc
local Cg = lpeg.Cg
local Ct = lpeg.Ct
local S = lpeg.S
local V = lpeg.V

local ws = lpeg.S(" \t")^0
local nl = ws * lpeg.S("\n\r")
local eol = ws * P";"^0 * nl
local alpha = R"az" + R"AZ"
local num = R"09"
local word = alpha * (alpha + num + P"_")^0

local 
function rule(name, pattern)
	return Ct(Cg(Cc(name), "type") * pattern)
end

local G = lpeg.P{
	--V"syntax",
	V"opt_eol" * V"syntax",
	
	syntax = ((V"rule" * V"syntax") + V"rule"),
	rule = rule("production", V"opt_ws" * Cg(P"<" * V"rule_name" * P">", "name") * V"opt_ws" * P"::=" * V"opt_ws" * V"expression" * V"eol") + V"err",
	rule_name = (R"az" + R"AZ" + P"-" + P"_")^1,
	expression = (Ct(V"list") * V"opt_eol" * V"opt_ws" * P"|" * V"opt_ws" * V"expression") + Ct(V"list"),
	list = (V"term" * V"opt_ws" * V"list") + V"term",
	term = rule("rule", C(P"<" * V"rule_name" * P">")) + rule("literal", C(V"literal")),
	literal = P'"' * V"text" * P'"',
	text = ((P(1) - P'"')^0),
	
	ws = S(" \t")^1,
	opt_ws = S(" \t")^0,
	eol = (V"opt_ws" * S("\n\r"))^1,
	opt_eol = V"opt_ws" * S("\n\r")^0,
	
	err = ((P(1) - nl)^1) / error,	
}

local 
function dump(t, i, memo)
	local i = i or 0
	local memo = memo or {}
	memo[t] = true
	for k, v in pairs(t) do 
		if type(v) == "table" and not memo[v] then
			print(string.format("%s[%s] = {", string.rep(" ", i), k))
			dump(v, i+1)
			print(string.format("%s}", string.rep(" ", i)))
		else
			print(string.format("%s[%s] = %s", string.rep(" ", i), k, tostring(v)))
		end
	end
end

function parse(bnf)
	local rules = { lpeg.match(G, bnf) }
	local S = rules[1].name
	local N = {}
	local T = {}
	local P = {}
	
	for i, rule in ipairs(rules) do
		P[rule.name] = rule
		for j, option in ipairs(rule) do
			for k, v in ipairs(option) do
				local tag = v[1]
				if v.type == "literal" then 
					T[v[1]] = tag:sub(2, #tag-1):gsub("\\n", '\n'):gsub("\\t", '\t')
				else
					N[v[1]] = tag:sub(2, #tag-1)
				end
			end
		end
	end
	return { S=S, T=T, N=N, P=P }
end

function patt_from_literal(def) return P(def[1]) end
function patt_from_rule(def) return V(def[1]) end
function concat(def, ...)
	local p
	for i, v in ipairs{...} do
		
	end
end
function patt_from_production(def) 
	
end

function patt_from_any(def)
	if def.type == "literal" then return P(def[1]) 
	elseif def.type == "rule" then return V(def[1]) 
	elseif def.type == "production" then
		local patt = patt_from_any(def[1])
		for i=2, #def do
			patt = patt + patt_from_any(def[i])
		end
		return patt
	else
		local patt = patt_from_any(def[1])
		for i=2, #def do
			patt = patt * patt_from_any(def[i])
		end
		return patt
	end
end

function generate_lpeg(bnf)
	local rules = { lpeg.match(G, bnf) }
	
	-- start symbol
	local g = { V(rules[1].name) }	
	
	-- productions:
	for i, rule in ipairs(rules) do
		g[rule.name] = patt_from_any(rule)
	end
	for k, v in pairs(g) do print(k, v) end
	return g
end