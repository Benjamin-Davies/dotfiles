local hotkey = require "mjolnir.hotkey"
local tiling = require "mjolnir.tiling"
local application = require "mjolnir.application"

-- Command prefix
local mash = {"ctrl", "cmd"}

hotkey.bind(mash, "r", function() mjolnir.reload() end)

hotkey.bind(mash, "t", function() tiling.retile() end)
hotkey.bind(mash, "w", function() tiling.cyclelayout() end)
hotkey.bind(mash, "j", function() tiling.cycle(1) end)
hotkey.bind(mash, "k", function() tiling.cycle(-1) end)
hotkey.bind(mash, "space", function() tiling.promote() end)

tiling.set('layouts', {
  'fullscreen', 'main-vertical'
})

hotkey.bind(mash, "return", function() application.launchorfocus("Terminal") end)
hotkey.bind(mash, "c", function() application.launchorfocus("Google Chrome") end)
