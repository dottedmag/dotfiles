local log = hs.logger.new("init")

hs.window.animationDuration = 0
hs.grid.setMargins(hs.geometry(nil, nil, 0, 0))

local function recalcGrids()
   for i, screen in pairs(hs.screen.allScreens()) do
      local m = screen:currentMode()

      local columns, rows
      if m.w*m.scale < 3000 then
	 columns = 2
      elseif m.w*m.scale < 4000 then
	 columns = 3
      else
         columns = 4
      end

      if m.h*m.scale < 2000 then
	 rows = 2
      else
	 rows = 3
      end
      hs.grid.setGrid(hs.geometry(nil, nil, columns, rows), screen)

      local m = "Grid for "..m.desc..": "..columns.."x"..rows
      hs.alert.show(m)
      log.i(m)
   end
end

-- Calculate grids on currently connected and future displays
hs.screen.watcher.new(recalcGrids):start()
recalcGrids()

local function expand_left()
   local w = hs.window.frontmostWindow()

   hs.grid.adjustWindow(function(cell)
         if cell.x > 0 then
            cell.x = cell.x - 1
            cell.w = cell.w + 1
         end
   end, w)
end

local function shrink_left()
   local w = hs.window.frontmostWindow()

   hs.grid.adjustWindow(function(cell)
         if cell.w > 1 then
            cell.w = cell.w - 1
         end
   end, w)
end

local function expand_right()
   local w = hs.window.frontmostWindow()
   local s = hs.screen(w:frame())
   local g = hs.grid.getGrid(s)
   local columns = g.w

   hs.grid.adjustWindow(function(cell)
         if cell.x+cell.w < columns then
            cell.w = cell.w + 1
         end
   end, w)
end

local function shrink_right()
   local w = hs.window.frontmostWindow()

   hs.grid.adjustWindow(function(cell)
         if cell.w > 1 then
            cell.x = cell.x + 1
            cell.w = cell.w - 1
         end
   end, w)
end

local function expand_up()
   local w = hs.window.frontmostWindow()

   hs.grid.adjustWindow(function(cell)
         if cell.y > 0 then
            cell.y = cell.y - 1
            cell.h = cell.h + 1
         end
   end, w)
end

local function shrink_up()
   local w = hs.window.frontmostWindow()

   hs.grid.adjustWindow(function(cell)
         if cell.h > 1 then
            cell.h = cell.h - 1
         end
   end, w)
end

local function expand_down()
   local w = hs.window.frontmostWindow()
   local s = hs.screen(w:frame())
   local g = hs.grid.getGrid(s)
   local rows = g.h

   hs.grid.adjustWindow(function(cell)
         if cell.y+cell.h < rows then
            cell.h = cell.h + 1
         end
   end, w)
end

local function shrink_down()
   local w = hs.window.frontmostWindow()

   hs.grid.adjustWindow(function(cell)
         if cell.h > 1 then
            cell.y = cell.y + 1
            cell.h = cell.h - 1
         end
   end, w)
end

local function fullscreen()
   local w = hs.window.frontmostWindow()
   local s = hs.screen(w:frame())
   local g = hs.grid.getGrid(s)
   local columns = g.w
   local rows = g.h

   hs.grid.adjustWindow(function(cell)
         cell.x = 0
         cell.y = 0
         cell.w = columns
         cell.h = rows
   end, w)
end

hs.hotkey.bind({"cmd", "alt"}, "Left", expand_left)
hs.hotkey.bind({"cmd", "alt"}, "Right", expand_right)
hs.hotkey.bind({"cmd", "alt"}, "Down", expand_down)
hs.hotkey.bind({"cmd", "alt"}, "Up", expand_up)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Left", shrink_left)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Right", shrink_right)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Down", shrink_down)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Up", shrink_up)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Return", fullscreen)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "R", function()
      hs.reload()
end)
hs.alert.show("Config reloaded")
