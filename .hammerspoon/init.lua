local hotkey = require "hs.hotkey"
local application = require "hs.application"
local tiling = require "hs.tiling"
local layouts = require "hs.tiling.layouts"

-- Command prefix
local mash = {"ctrl", "cmd"}

hotkey.bind(mash, "r", function() hs.reload() end)

hotkey.bind(mash, "t", function() tiling.retile() end)
hotkey.bind(mash, "w", function() tiling.cycleLayout() end)
hotkey.bind(mash, "j", function() tiling.cycle(1) end)
hotkey.bind(mash, "k", function() tiling.cycle(-1) end)
hotkey.bind(mash, "space", function() tiling.promote() end)

tiling.addLayout('main-vertical-custom', function (windows)
  getSpace().mainVertical = 0.55

  layouts['main-vertical-variable'](windows)

  for win in windows do
    frame = win:frame()
    --frame.x = 20
    win:setFrame(frame)
  end
end)

tiling.set('layouts', {
  'fullscreen', 'main-vertical-custom'
})

hotkey.bind(mash, "return", function() application.launchOrFocus("Terminal") end)
hotkey.bind(mash, "c", function() os.execute("open -n -a Google\\ Chrome --args --profile-directory=Default") end)
hotkey.bind(mash, "s", function() os.execute("open -n -a Google\\ Chrome --args --profile-directory=Profile\\ 1 https://moodle.mmc.school.nz/login/index.php") end)
hotkey.bind(mash, "y", function() os.execute("open -n -a Google\\ Chrome https://youtube.com/ --args --profile-directory=Profile\\ 1") end)
