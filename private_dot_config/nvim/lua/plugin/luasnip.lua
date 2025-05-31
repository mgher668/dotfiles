local os = require('os')

local function prequire(...)
local status, lib = pcall(require, ...)
if (status) then return lib end
  return nil
end

local ls = prequire('luasnip')
local types = require('luasnip.util.types')
local events = require "luasnip.util.events"
local cmp = prequire('cmp')

-- create snippet
-- s(context, nodes, condition, ...)
local snippet = ls.snippet
local snippet_node = ls.snippet_node

local fmt = require('luasnip.extras.fmt').fmt

local c = ls.choice_node

-- Function Node
--  Takes a function that returns text
local f = ls.function_node

--  Multiple lines are by passing a table of strings.
--  t { "line 1", "line 2" }
local t = ls.text_node

--  To create placeholder text, pass it as the second argument
--      i(2, "this is placeholder text")
local i = ls.insert_node

--  Useful for dynamic nodes and choice nodes
local snippet_from_nodes = ls.sn

local l = require("luasnip.extras").lambda

local d = ls.dynamic_node


ls.config.set_config {
  -- This tells LuaSnip to remember to keep around the last snippet.
  -- You can jump back into it even if you move outside of the selection
  history = true,

  -- This one is cool cause if you have dynamic snippets, it updates as you type!
  updateevents = "TextChanged,TextChangedI",

  -- treesitter-hl has 100, use something higher (default is 200).
	ext_base_prio = 300,
	-- minimal increase in priority.
	ext_prio_increase = 1,

  -- Autosnippets:
  enable_autosnippets = true,

  -- Crazy highlights!!
  -- #vid3
  -- ext_opts = nil,
  ext_opts = {
    [types.choiceNode] = {
      active = {
        virt_text = { { " <- Current Choice", "NonTest" } },
      },
    },
  },
}

local shortcut = function(val)
  if type(val) == "string" then
    return { t { val }, i(0) }
  end

  if type(val) == "table" then
    for k, v in ipairs(val) do
      if type(v) == "string" then
        val[k] = t { v }
      end
    end
  end

  return val
end

local make = function(tbl)
  local result = {}
  for k, v in pairs(tbl) do
    table.insert(result, (snippet({ trig = k, desc = v.desc }, shortcut(v))))
  end

  return result
end

local same = function(index)
  return f(function(args)
    return args[1]
  end, { index })
end

local snippets = {}

local toexpand_count = 0

snippets.all = {
  ls.parser.parse_snippet("expand", "-- this is test!"),
  ls.parser.parse_snippet(
    "lspsyn",
    "Wow! This ${1:Stuff} really ${2:works. ${3:Well, a bit.}}"
  ),

  -- When wordTrig is set to false, snippets may also expand inside other words.
  ls.parser.parse_snippet(
    { trig = "te", wordTrig = false },
    "${1:cond} ? ${2:true} : ${3:false}"
  ),
  -- basic, don't need to know anything else
  --    arg 1: string
  --    arg 2: a node
  snippet("simple", t "wow, you were right!"),

  snippet("trig", { t("Wow! This is text snip!") }),

  -- callbacks table
  snippet("toexpand", c(1, { t"hello", t "world", t "last" }), {
    callbacks = {
      [1] = {
        [events.enter] = function(--[[ node ]])
          toexpand_count = toexpand_count + 1
          print("Number of times entered:", toexpand_count)
        end,
      }
    },
  }),

  -- regTrig
  --    snippet.captures
  -- snippet({ trig = "AbstractGenerator.*Factory", regTrig = true }, { t "yo" }),

  -- third arg,
  snippet("never_expands", t "this will never expand, condition is false", {
    condition = function()
      return false
    end,
  }),

  -- docTrig ??

  -- functions

  -- date -> Tue 16 Nov 2021 09:43:49 AM EST
  snippet({ trig = "date" }, {
    f(function()
      return string.format(string.gsub(vim.bo.commentstring, "%%s", " %%s"), os.date())
    end, {}),
  }),

  -- Simple snippet, basics
  snippet("for", {
    t "for ",
    i(1, "k, v"),
    t " in ",
    i(2, "ipairs()"),
    t { "do", "  " },
    i(0),
    t { "", "" },
    t "end",
  }),

  --[[
        -- Alternative printf-like notation for defining snippets. It uses format
        -- string with placeholders similar to the ones used with Python's .format().
        s(
            "fmt1",
            fmt("To {title} {} {}.", {
                i(2, "Name"),
                i(3, "Surname"),
                title = c(1, { t("Mr."), t("Ms.") }),
            })
        ),
  --]]
}

-- snippets.lua = {
--   ls.parser.parse_snippet("expand", "-- this is test!"),
-- }

table.insert(snippets.all, ls.parser.parse_snippet("example", "-- $TM_FILENAME\nfunc $1($2) $3 {\n\t$0\n}"))

table.insert(
  snippets.all,
  snippet("cond", {
    t "will only expand in c-style comments",
  }, {
    condition = function(
      line_to_cursor --[[ , matched_trigger, captures ]]
    )
      local commentstring = "%s*" .. vim.bo.commentstring:gsub("%%s", "")
      -- optional whitespace followed by //
      return line_to_cursor:match(commentstring)
    end,
  })
)

-- Make sure to not pass an invalid command, as io.popen() may write over nvim-text.
table.insert(
  snippets.all,
  snippet(
    { trig = "$$ (.*)", regTrig = true },
    f(function(_, snip, command)
      if snip.captures[1] then
        command = snip.captures[1]
      end

      local file = io.popen(command, "r")
      local res = { "$ " .. snip.captures[1] }
      for line in file:lines() do
        table.insert(res, line)
      end
      return res
    end, {}, "ls"),
    {
      -- Don't show this one, because it's not useful as a general purpose snippet.
      show_condition = function()
        return false
      end,
    }
  )
)

-- Lambda example
table.insert(
  snippets.all,
  snippet("transform2", {
    i(1, "initial text here"),
    t " :: ",
    i(2, "replacement for text"),
    t " :: ",
    -- t { "", "" },
    -- Lambdas can also apply transforms USING the text of other nodes:
    l(l._1:gsub("text", l._2), { 1, 2 }),
  })
)

-- initial text :: this is going to be replaced :: initial tthis is going to be replacedxt
-- this is where we have text :: TEXT :: this is where we have TEXT

-- for _, ft_path in ipairs(vim.api.nvim_get_runtime_file("lua/tj/snips/ft/*.lua", true)) do
--   local ft = vim.fn.fnamemodify(ft_path, ":t:r")
--   snippets[ft] = make(loadfile(ft_path)())
-- end

local js_attr_split = function(args)
  local val = args[1][1]
  local split = vim.split(val, ".", { plain = true })

  local choices = {}
  local thus_far = {}
  for index = 0, #split - 1 do
    table.insert(thus_far, 1, split[#split - index])
    table.insert(choices, t { table.concat(thus_far, ".") })
  end

  return snippet_from_nodes(nil, c(1, choices))
end

local fill_line = function(char)
  return function()
    local row = vim.api.nvim_win_get_cursor(0)[1]
    local lines = vim.api.nvim_buf_get_lines(0, row - 2, row, false)
    return string.rep(char, #lines[1])
  end
end

snippets.rst = make {
  jsa = {
    ":js:attr:`",
    d(2, js_attr_split, { 1 }),
    " <",
    i(1),
    ">",
    "`",
  },

  link = { ".. _", i(1), ":" },

  head = f(fill_line "=", {}),
  sub = f(fill_line "-", {}),
  subsub = f(fill_line "^", {}),

  ref = { ":ref:`", same(1), " <", i(1), ">`" },
}

-- ls.snippets = snippets
ls.snippets = {
  all = {}
}

-- test working ----------------------------------------------------------------

-- Returns a snippet_node wrapped around an insert_node whose initial
-- text value is set to the current date in the desired format.
local date_input = function(args, snip, old_state, fmt)
  local fmt = fmt or "%Y-%m-%d"
  return snippet_from_nodes(nil, i(1, os.date(fmt)))
end

-- args is a table, where 1 is the text in Placeholder 1, 2 the text in
-- placeholder 2,...
local function copy(args)
  return args[1]
end

ls.add_snippets("all", {
  snippet("ternary", {
    -- equivalent to "${1:cond} ? ${2:then} : ${3:else}"
    i(1, "cond"), t(" ? "), i(2, "then"), t(" : "), i(3, "else")
  }),
  -- Use a dynamic_node to interpolate the output of a
  -- function (see date_input above) into the initial
  -- value of an insert_node.
  -- snippet("novel", {
  --   t("It was a dark and stormy night on "),
  --   d(1, date_input, {}, { user_args = { "%A, %B %d of %Y" } }),
  --   t(" and the clocks were striking thirteen."),
  -- }),
})

ls.add_snippets("all", {
  snippet("dt-1", { t(os.date("%Y-%m-%d")) }),
  snippet("dt-2", { t(os.date("%Y/%m/%d")) })
  -- snippet("trig", { "This is text by snip!!!" }),
  -- snippet("trig", { t("Wow! This is text snip!") }),
})

ls.add_snippets("rust", {
  snippet('modtest', fmt(
    [[
      #[cfg(test)]
      mod test {{
      {}

          {}
      }}
    ]],
    {
      c(1, { t "      use super::*;", t "" }),
      i(0),
    }
  )),
});

-- ls.add_snippets("all", {
--   snippet("trig", c(1, {
--     t("Ugn boring, a text node"),
--     i(nil, "At least I can edit something now..."),
--     f(function (args)
--       return "Still only counts as text!!"
--     end, {})
--   })),
-- });

ls.add_snippets("all", {
  snippet("trig", {
    t"text: ", i(1), t{"", "copy: "},
    d(2, function(args)
        -- the returned snippetNode doesn't need a position; it's inserted
        -- "inside" the dynamicNode.
        return snippet_node(nil, {
          -- jump-indices are local to each snippetNode, so restart at 1.
          i(1, args[1])
        })
      end,
    {1})
  })
});

ls.add_snippets("all", {
  snippet("example2", fmt([[
  if {} then
    {}
  end
  ]], {
    -- i(1) is at nodes[1], i(2) at nodes[2].
    i(1, "not now"), i(2, "when")
  })),
  -- snippet("example4", fmt([[
  -- if {} then
  --   {}
  -- end
  -- ]], {
  --   -- i(1) is at nodes[1], i(2) at nodes[2].
  --   i(1, "now"), i(2, "fk")
  -- }, {
  --   delimiters = "<>"
  -- })),
});

ls.add_snippets("all", {
  -- snippet("dt-1", { t(os.date("%Y-%m-%d")) }),
  -- snippet("dt-2", { t(os.date("%Y/%m/%d")) }),
  -- snippet('fc', fmt(
  --   [[
  --     import React from 'react';

  --     interface ComponentProps {
  --       test?: string;
  --     }
  --   ]],
  --   {
  --     c(1, { t "      use super::*;", t "" }),
  --     i(0),
  --   }
  -- )),
  -- snippet('fc', fmt()),
  -- snippet('fc', fmt([[
  --   import React from 'react';

  --   interface ComponentProps {
  --     test?: string;
  --   }

  --   const Component: React.FC<ComponentProps> = (props) => {
  --     const { test } = props;
  --     return (
  --       <div>
  --         <h1>Component</h1>
  --         {/* other content... */}
  --       </div>
  --     );
  --   };

  --   export default Component;
  -- ]], {})),
  -- snippet('fc', fmt([[
  --   import React from 'react';

  --   interface {name}Props {
  --     test?: string;
  --   }

  --   const {name}: React.FC<{name}Props> = (props) => {
  --     const { test } = props;
  --     return (
  --       <div>
  --         <h1>{name}</h1>
  --         {/* other content... */}
  --       </div>
  --     );
  --   };

  --   export default {name};
  -- ]], {
  --   name = i(1, "Component"),
  -- })),
  -- snippet("fc", {
  --   -- equivalent to "${1:cond} ? ${2:then} : ${3:else}"
  --   t("import React from 'react';\n\n"),
  --   t("interface "),
  --   -- i(1, "Component"),
  --   t("Component"),
  --   t("Props {\n"),
  --   -- i(2, "  test?: string;\n"),
  --   t("  test?: string;\n"),
  --   t("}\n\n"),
  --   t("const "),
  --   -- i(1, "Component"),
  --   t("Component"),
  --   t(": React.FC<"),
  --   -- i(1, "Component"),
  --   t("Component"),
  --   t("Props> = (props) => {"),
  --   t(": React.FC<ComponentProps> = (props) => {\n"),
  --   t("  const { test } = props;"),
  --   t("  return (\n"),
  --   -- i(3, "    <div>\n      <h1>Title</h1>\n      {/* content... */}\n</div>\n"),
  --   t("    <div>\n      <h1>Title</h1>\n      {/* content... */}\n</div>\n"),
  --   t("  );\n"),
  --   t("};\n"),
  --   t("export default ;"),
  --   -- i(1, "Component"),
  --   t("Component"),
  --   t(";"),
  --   -- i(1, "cond"), t(" ? "), i(2, "then"), t(" : "), i(3, "else")
  -- }),
  -- snippet("trig", { "This is text by snip!!!" }),
  -- snippet("trig", { t("Wow! This is text snip!") }),
})
-- test working ----------------------------------------------------------------

ls.autosnippets = {
  all = {
    ls.parser.parse_snippet("$file$", "$TM_FILENAME"),
  },
}

-- <c-j> is my expansion key
-- this will expand the current item or jump to the next item within the snippet.
vim.keymap.set({ "i", "s" }, "<c-j>", function()
  if ls.expand_or_jumpable() then
    ls.expand_or_jump()
  end
end, { silent = true })

-- <c-k> is my jump backwards key.
-- this always moves to the previous item within the snippet
vim.keymap.set({ "i", "s" }, "<c-k>", function()
  if ls.jumpable(-1) then
    ls.jump(-1)
  end
end, { silent = true })

-- vim.keymap.set({ "i", "s" }, "<c-n", function()
--   ls.change_choice
-- end)

vim.keymap.set({ "i", "s" }, "<c-n>", function()
  if ls.choice_active() then
    ls.change_choice(1)
  end
end)

vim.keymap.set({ "i", "s" }, "<c-p>", function()
  if ls.choice_active() then
    ls.change_choice(-1)
  end
end)

-- -- <c-l> is selecting within a list of options.
-- -- This is useful for choice nodes (introduced in the forthcoming episode 2)
-- vim.keymap.set("i", "<c-l>", function()
--   if ls.choice_active() then
--     ls.change_choice(1)
--   end
-- end)

-- vim.keymap.set("i", "<c-u>", require "luasnip.extras.select_choice")

-- shorcut to source my luasnips file again, which will reload my snippets
vim.keymap.set("n", "<leader><leader>s", "<cmd>source ~/.config/nvim/lua/plugin/luasnip.lua<CR>")

require("luasnip.loaders.from_vscode").lazy_load()



-- simple config
-- local t = function(str)
--   return vim.api.nvim_replace_termcodes(str, true, true, true)
-- end

-- local check_back_space = function()
--   local col = vim.fn.col('.') - 1
--   if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
--     return true
--   else
--     return false
--   end
-- end

-- _G.tab_complete = function()
--   if cmp and cmp.visible() then
--     cmp.select_next_item()
--   elseif ls and ls.expand_or_jumpable() then
--     return t("<Plug>luasnip-expand-or-jump")
--   elseif check_back_space() then
--     return t "<Tab>"
--   else
--     cmp.complete()
--   end
--   return ""
-- end
-- _G.s_tab_complete = function()
--   if cmp and cmp.visible() then
--     cmp.select_prev_item()
--   elseif ls and ls.jumpable(-1) then
--     return t("<Plug>luasnip-jump-prev")
--   else
--     return t "<S-Tab>"
--   end
--   return ""
-- end

-- vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
-- vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
-- vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
-- vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
-- vim.api.nvim_set_keymap("i", "<C-E>", "<Plug>luasnip-next-choice", {})
-- vim.api.nvim_set_keymap("s", "<C-E>", "<Plug>luasnip-next-choice", {})
