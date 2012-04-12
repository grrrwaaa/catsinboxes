--[[
	Grammatical evolution
	@see http://en.wikipedia.org/wiki/Grammatical_evolutionf
	
	Uses BNF grammar specification to evolve programs from arbitrary bit strings
	Essesnce: start from the start symbol S, and use production rules P. If several rules are possible, use bit string as pseudo-random seed to modulo options.
--]]

local example = [[
<postal-address> 	::= <name-part> <street-address> <zip-part>
<name-part> 		::= <personal-part> <whitespace> <last-name> <whitespace> <opt-jr-part> <EOL> 
					  | <personal-part> <whitespace> <name-part> <EOL>
<personal-part> 	::= <first-name> 
					  | <initial> "." 
<street-address> 	::= <house-num> <whitespace> <street-name> <whitespace> <opt-apt-num> <EOL>
<zip-part> 			::= <town-name> ", " <state-code> <whitespace> <ZIP-code> <EOL>
<opt-jr-part> 		::= "Sr." 
					  | "Jr." 
					  | <roman-numeral> 
					  | "" 
<EOL>				::= "\n"
<whitespace>		::= " "
]]

local BNF = [[
<syntax> 			::= <rule> <syntax>
					  | <rule>
<rule>   			::= <opt-whitespace> "<" <rule-name> ">" <opt-whitespace> "::=" <opt-whitespace> <expression> <line-end>
<opt-whitespace> 	::= " " <opt-whitespace> 
					  | ""  
<expression>     	::= <list> "|" <expression>
					  | <list>
<line-end>       	::= <opt-whitespace> <EOL> 
<list>    			::= <term> <opt-whitespace> <list>
					  | <term>
<term>    			::= "<" <rule-name> ">"
					  | <literal> 
<literal> 			::= "'" <text> "'" 
<EOL>				::= "\n"
<opt-whitespace>	::= " "
]]

local ge = [[

<func>				::= <header>
<header>			::= "double symb(double X) {\n" <body> "}"
<body>				::= <declarations> <code> <return>
<declarations>		::= "\tfloat a;\n"
<code>				::= "\ta = " <expr> ";\n"
<return>			::= "\treturn a;\n"
<expr>				::= <expr> <op> <expr>
					  | "(" <expr> <op> <expr> ")"
					  | <pre-op> "(" <expr> ")"
					  | <var>
<op>				::= "+"
					  | "-"
					  | "/"
					  | "*"
<pre-op>			::= "sin"
					  | "cos"
					  | "tan"
					  | "log"
<var>				::= "X"				  

]]

require "bnf"

-- kind of works, except that some productions might not halt; need to add limiters (and default values for productions in such cases)
-- second problem: BNF doesn't handle semantics, such as type attributes. need to synthesize environments (symbol tables) during production to validate, and need to extend BNF grammar with a semantic grammar.

function produce(g, rulename, indent)
	indent = indent or 1
	local rule = g.P[rulename]
	if rule then
		local opts = #rule
		local optnum = math.random(opts)
		--print(string.format("%schose", string.rep(" ", indent)), optnum, "of", opts)
		local option = rule[optnum]
		local results = {}
		for i, v in ipairs(option) do
			--print(string.format("%s %d -> %s: %s", string.rep(" ", indent), i, v.type, v[1]))
			table.insert(results, produce(g, v[1], indent+1))
		end
		return table.concat(results, "")
	else
		return g.T[rulename] or g.N[rulename] or rulename
	end
end

math.randomseed(os.time())

local g = bnf.parse(ge)
print(produce(g, g.S))