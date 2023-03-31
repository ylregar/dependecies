-- perfect user interface • gamesense
----- enQ#1349




----<  Header  >----------------------------------------------------------------

--------------------------------------------------------------------------------
-- #region: < Header >


--
-- #region : Definitions

--#region: localization

local assert, collectgarbage, defer, error, getfenv, setfenv, getmetatable, setmetatable, ipairs,
pairs, load, next, pcall, printf, rawequal, rawset, rawlen, readfile, require, select,
tonumber, tostring, toticks, totime, type, unpack, xpcall =
assert, collectgarbage, defer, error, getfenv, setfenv, getmetatable, setmetatable, ipairs,
pairs, load, next, pcall, printf, rawequal, rawset, rawlen, readfile, require, select,
tonumber, tostring, toticks, totime, type, unpack, xpcall


local function mcopy (o)
	if type(o) ~= "table" then return o end
	local res = {} for k, v in pairs(o) do res[mcopy(k)] = mcopy(v) end return res
end

local table, math, string = mcopy(table), mcopy(math), mcopy(string)
local ui, client = mcopy(ui), mcopy(client)

--#endregion

--#region: globals

table.find = function (t, j)  for k, v in pairs(t) do if v == j then return k end end return false  end
table.ifind = function (t, j)  for i = 1, table.maxn(t) do if t[i] == j then return i end end  end
table.ihas = function (t, ...) local arg = {...} for i = 1, table.maxn(t) do for j = 1, #arg do if t[i] == arg[j] then return true end end end return false end

table.filter = function (t)  local res = {} for i = 1, table.maxn(t) do if t[i] ~= nil then res[#res+1] = t[i] end end return res  end
table.append = function (t, ...)  for i, v in ipairs{...} do table.insert(t, v) end  end
table.copy = mcopy

math.round = function (value)  return math.floor (value + 0.5)  end
math.clamp = function (a, mn, mx)  return math.min (mx, math.max(a, mn))  end
math.lerp = function (a, b, w)  return a + (b - a) * w  end

local ternary = function (c, a, b)  if c then return a else return b end  end
local contend = function (func, callback, ...)
	local t = { pcall(func, ...) }
	if not t[1] then return type(callback) == "function" and callback(t[2]) or error(t[2], callback or 2) end
	return unpack(t, 2)
end

--#endregion

--#region: directory tools

local dirs = {
	execute = function (t, path, func)
		local p, k for _, s in ipairs(path) do
			k, p, t = s, t, t[s]
			if t == nil then return end
		end
		if p[k] then func(p[k]) end
	end,
	replace = function (t, path, value)
		local p, k for _, s in ipairs(path) do
			k, p, t = s, t, t[s]
			if t == nil then return end
		end
		p[k] = value
	end,
	find = function (t, path)
		local p, k for _, s in ipairs(path) do
			k, p, t = s, t, t[s]
			if t == nil then return end
		end
		return p[k]
	end,
}

dirs.pave = function (t, place, path)
	local p = t
	for i, v in ipairs(path) do
		if type(p[v]) == "table" then
			p = p[v]
		else
			p[v] = (i < #path) and {} or place
			p = p[v]
		end
	end
	return t
end

dirs.extract = function (t, path)
	if not path or #path == 0 then return t end
    local j = dirs.find(t, path)
    return dirs.pave({}, j, path)
end

--#endregion

-- #endregion
--

--
-- #region : pui declarations

local pui, pui_mt, methods_mt, config = {}, {}, {}, {}

-- #endregion
--


-- #endregion ------------------------------------------------------------------
--

--------------------------------------------------------------------------------
-- #region: < Elements-related stuff >


--
-- #region : Elements

local elements = {
	button		= { type = "function",	arg = 2, unsavable = true },
	checkbox	= { type = "boolean",	arg = 1	},
	color_picker= { type = "number",	arg = 5 },
	combobox	= { type = "string",	arg = 2, variable = true },
	hotkey		= { type = "boolean",	arg = 3 },
	label		= { type = "string",	arg = 1, unsavable = true },
	listbox		= { type = "number",	arg = 2 },
	multiselect	= { type = "table",		arg = 2, variable = true },
	slider		= { type = "number",	arg = 8 },
	string		= { type = "string",	arg = 2 },
	unknown		= { type = "string",	arg = 2 }, -- new_string type
	textbox		= { type = "string",	arg = 1 }
}

-- #endregion
--

--
-- #region : Element tools

local etools = {}

etools.multiple = function (this)
	if this.type == "button" then
		ui.set_callback(this.ref, function ()
			for i = 1, #this[0].callbacks, 1 do this[0].callbacks[i](this) end
		end)
	elseif this.type == "color_picker" or this.type == "hotkey" then
		ui.set_callback(this.ref, function (self)
			this.value = { ui.get(self) }
			for i = 1, #this[0].callbacks, 1 do this[0].callbacks[i](this) end
		end)
		return { ui.get(this.ref) }
	else
		ui.set_callback(this.ref, function (self)
			this.value = ui.get(self)
			for i = 1, #this[0].callbacks, 1 do this[0].callbacks[i](this) end
		end)
		return ui.get(this.ref)
	end
end

etools.new = function (ref)
	local this = {
		__name = "pui::element",
		ref = ref
	}
	
	this.name, this.type = ui.name(this.ref), ui.type(this.ref)

	this[0] = {
		is_visible = true, is_enabled = true,
		overridden = false, original = this.value,
		savable = not elements[this.type].unsavable, donotsave = false,
		callbacks = {}
	}
	this.__depend = {
		list = {}, count = 0
	}

	setmetatable(this, methods_mt)
	this.value = etools.multiple(this)

	return this
end

etools.new_string = function (name, default)
	local this = {
		__name = "pui::element"
	}
		
	this.ref = ui.new_string(name, default or "")
	this.type = "string"

	this.set, this.get = methods_mt.set, methods_mt.get
	this[0] = {savable = true}

	return this
end

etools.features = function (self, args)
	do
		local v, kind = args[1], type(args[1])

		if kind == "table" then
			self.color = etools.new( ui.new_color_picker(self.tab, self.container, self.name, v[1] or 255, v[2] or 255, v[3] or 255, v[4] or 255) )
		elseif kind == "number" then
			self.hotkey = etools.new( ui.new_hotkey(self.tab, self.container, self.name, true, v) )
		end
	end
	do
		self[0].donotsave = args[2] == false
	end
end

etools.depend_check = nil do
	local cases = {
		combobox = function (v)
			for i = 2, #v do
				if v[1].value == v[i] then return true end
			end
			return false
		end,
		listbox = function (v)
			for i = 2, #v do
				if v[1].value == v[i] then return true end
			end
			return false
		end,
		multiselect = function (v)
			return table.ihas(v[1].value, unpack(v, 2))
		end,
		slider = function (v)
			return v[2] <= v[1].value and v[1].value <= (v[3] or v[2])
		end,
	}

	etools.depend_check = function (__depend)
		for i, v in ipairs(__depend.list) do
			local condition = false

			if type(v[2]) == "function" then
				condition = v[2]( v[1] )
			else
				if cases[v[1].type] then
					condition = cases[v[1].type](v)
				else
					condition = v[1].value == v[2]
				end
			end

			__depend.count = __depend.count + (condition and 1 or 0)
		end

		local activate = __depend.count == #__depend.list
		__depend.count = 0

		return activate
	end
end

etools.memorize = function (self, path)
	if self[0].donotsave then return end

	if self[0].savable then
		dirs.pave(config, self.ref, path)
	end

	if self.color then
		path[#path] = path[#path] .. "_c"
		dirs.pave(config, self.color.ref, path)
	end
end

-- #endregion
--

--
-- #region : Utils

local utils = {}

utils.rgb_to_hex = function (color)
	return string.format("%02X%02X%02X%02X", color[1], color[2], color[3], color[4] or 255)
end

utils.hex_to_rgb = function (hex)
	hex = hex:gsub("#", "")
	return tonumber(hex:sub(1, 2), 16), tonumber(hex:sub(3, 4), 16), tonumber(hex:sub(5, 6), 16), tonumber(hex:sub(7, 8), 16) or 255
end

utils.gradient_text = function (text, colors, precision)
	precision = precision or 1
	local symbols, length = {}, #text
	local s = 1 / (#colors - 1)

	for i = 1, length, precision do
		local weight = i / length
		local cw = weight / s
		local j = math.ceil(cw)
		local w = (cw / j)
		local left, right = colors[j], colors[j+1]

		local r = left[1] + (right[1] - left[1]) * w
		local g = left[2] + (right[2] - left[2]) * w
		local b = left[3] + (right[3] - left[3]) * w
		local a = left[4] + (right[4] - left[4]) * w

		
		symbols[#symbols+1] = ("\a%02x%02x%02x%02x%s"):format(r, g, b, a, text:sub(i, i+precision-1))
	end

	return table.concat(symbols)
end

utils.format = nil do
	local process = function (col, text)
		local colors = {}; for w in string.gmatch(col, "\b%x+") do
			colors[#colors+1] = { utils.hex_to_rgb(w:sub(2)) }
		end

		return utils.gradient_text(text, colors, #text > 8 and 2 or 1) .. "\aCDCDCDFF"
	end
	
	utils.format = function (s)
		if type(s) ~= "string" then return s end
	
		s = string.gsub(s, "[\v\r]", {["\v"] = "\a".. pui.accent, ["\r"] = "\aCDCDCDFF"})
		s = string.gsub(s, "([\b%x]+)%[(.*)%]", process)
	
		return s
	end
end

utils.dispense = function (key, raw, ...)
	local args, group, ctx = {...}, {}, elements[key]

	if type(raw) == "table" then
		group[1], group[2] = raw[1], raw[2]
	else
		group[1], group[2] = raw, args[1]
		table.remove(args, 1)
	end

	args.n = table.maxn(args)

	local variable, counter = (ctx and ctx.variable) and type(args[2]) == "string", 1
	args.req, args.misc = (ctx and not variable) and ctx.arg or args.n, {}

	for i = 1, args.n do
		if type(args[i]) == "string" then
			args[i] = string.sub(utils.format(args[i]), 1, 123)
		end

		if i > args.req then
			args.misc[counter], counter = args[i], counter + 1
		end
	end

	return args, group
end

-- #endregion
--


-- #endregion ------------------------------------------------------------------
--





----<  Main  >------------------------------------------------------------------

--------------------------------------------------------------------------------
-- #region: < pui >


--
-- #region : pui

--#region: variables

pui.__name = "pui::basement"

pui.accent = nil do
	local reference = ui.reference("MISC", "Settings", "Menu color")
	pui.accent = utils.rgb_to_hex{ ui.get(reference) }
end

--#endregion

--#region: features

pui.group = function (tab, container)
	return setmetatable({
		__name = "pui::group",
		[1] = tab, [2] = container
	}, pui_mt)
end

pui.reference = function (tab, container, name)
	local found = { contend(ui.reference, nil, tab, container, name) }

	for i, v in ipairs(found) do
		found[i] = etools.new(v)
	end

	if found[2] and (found[2].type == "hotkey" or found[2].type == "color_picker") then
		local addition = found[2].type == "color_picker" and "color" or "hotkey"
		found[1][addition] = found[2]
		table.remove(found, 2)
	end

	return unpack(found)
end

pui.format = utils.format

pui.traverse = function (t, f, p)
	p = p or {}

	if type(t) == "table" and t.__name ~= "pui::element" and t[#t] ~= "~" then
		for k, v in next, t do
			local np = table.copy(p); np[#np+1] = k
			pui.traverse(v, f, np)
		end
	else
		f(t, p)
	end
end

--#endregion

--#region: config system

pui.setup = function (t)
	pui.traverse(t, etools.memorize)
	return t
end

pui.save = function (...)
	local packed = {}

	pui.traverse(dirs.extract(config, {...}), function (ref, path)
		local value
		local etype = ui.type(ref)

		if etype == "color_picker" then
			value = "#".. utils.rgb_to_hex{ ui.get(ref) }
		elseif etype == "hotkey" then
			value = { ui.get(ref) };

			table.remove(value, 1);

			value[#value+1] = "~"
		elseif etype == "multiselect" then
			value = { ui.get(ref) }
			value[#value+1] = "~"
		else
			value = ui.get(ref)
		end

		dirs.pave(packed, value, path)
	end)

	return packed
end

pui.load = function (cfg, ...)
	pui.traverse(dirs.extract(cfg, {...}), function (value, path)
		local kind = type(value)

		if kind == "string" and value:sub(1, 1) == "#" then
			value, kind = { utils.hex_to_rgb(value) }, "table"
		elseif kind == "table" and value[#value] == "~" then
			value[#value] = nil
		end

		dirs.execute(config, path, function(element)
			if kind == "table" then
				ui.set(element, unpack(value))
			else
				ui.set(element, value)
			end
		end)
	end)
end

--#endregion

-- #endregion
--

--
-- #region : pui_mt, elements

pui_mt.__index = function (self, key)
	key = key:gsub("new_", "")
	if not elements[key] then return ui[key] end
	if key == "string" then return etools.new_string end

	return function (origin, ...)
		local args, group = utils.dispense(key, origin, ...)

		local this = etools.new( contend(ui["new_".. key], 3, group[1], group[2], unpack(args, 1, args.n < args.req and args.n or args.req)) )
		this.tab, this.container = group[1], group[2]

		if this.type == "button" then
			this[0].callbacks[#this[0].callbacks+1] = args[2]
		end

		etools.features(this, args.misc)

		return this
	end
end

-- #endregion
--

--
-- #region : methods

methods_mt = {
	__type = "pui::element", __name = "pui::element",

	__eq = function (this, that) return this.ref == that.ref end,
	__tostring = function (self) return "pui::element.".. self.type .." - ".. self.name end,

	get = function (self) return ui.get(self.ref) end,
	set = function (self, ...) ui.set(self.ref, ...) end,
	
	update = function (self, ...) ui.update(self.ref, ...) end,

	get_color = function (self) return self.color and ui.get(self.color.ref) or nil end,
	get_hotkey = function (self)
		if not self.hotkey then return nil end
		return ui.get(self.hotkey.ref)
	end,

	get_type = function (self) return self.type end,
	get_name = function (self) return self.name end,

	set_visible = function (self, visible)
		ui.set_visible(self.ref, visible)
		self[0].is_visible = visible
	end,
	is_visible = function (self) return self[0].is_visible end,

	set_enabled = function (self, enabled)
		ui.set_enabled(self.ref, enabled)
		self[0].is_enabled = enabled
	end,
	is_enabled = function (self) return self[0].is_enabled end,

	set_callback = function (self, func, once)
		if once then func(self) end
		self[0].callbacks[#self[0].callbacks+1] = func
	end,
	detach_callback = function (self, func)
		table.remove(self[0].callbacks, table.ifind(self[0].callbacks, func) or 0)
	end,

	get_original = function (self) return self[0].original end,
	override = function (self, value)
		if value ~= nil then
			if not self[0].overridden then self[0].original = self.value end
			self[0].overridden = true

			ui.set(self.ref, value)
		else
			if self[0].overridden then
				ui.set(self.ref, self[0].original)
				self[0].overridden = false
			end
		end
	end,

	depend = function (self, ...)
		local ctx = self.__depend
		table.append(ctx.list, ...)

		local check = function ()
			local allow = etools.depend_check(ctx)
			self:set_visible(allow)
			if self.hotkey then self.hotkey:set_visible(allow) end
			if self.color then self.color:set_visible(allow) end
		end

		for i, v in ipairs{...} do
			v[1]:set_callback(check)
		end

		check()
	end
}	methods_mt.__index = methods_mt

-- #endregion
--


-- #endregion ------------------------------------------------------------------
--



--
return setmetatable(pui, pui_mt)