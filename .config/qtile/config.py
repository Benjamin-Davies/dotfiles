from subprocess import call, Popen

from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.lazy import lazy
from libqtile import layout, bar, widget, hook

mod = "mod4"

background = "/usr/share/backgrounds/cinnamon_default.jpg"
term = "st"

@hook.subscribe.startup_once
def autostart():
    call("xrdb ~/.Xresources", shell=True)
    call("xmodmap ~/.speedswapper", shell=True)
    call("feh --bg-fill " + background, shell=True)
    Popen("compton", shell=True)
    Popen("TMUX=TMUX albert", shell=True)
    Popen("google-chrome-stable --no-startup-window", shell=True)
    Popen("redshift -l -38:176 -t 6500:3500", shell=True)

keys = [
    # Control and power
    Key([mod], "q", lazy.window.kill()),
    Key([mod, "shift"], "q", lazy.shutdown()),
    Key([mod], "r", lazy.restart()),
    Key([mod], "space", lazy.spawn("albert show")),
    Key([mod, "shift"], "space", lazy.spawncmd()),

    # Navigation
    Key([mod], "k", lazy.layout.down()),
    Key([mod], "j", lazy.layout.up()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_up()),
    Key([mod], "Tab", lazy.next_layout()),

    # Applications
    Key([mod], "Return", lazy.spawn(term)),
]

groups = [Group(str(i)) for i in range(1, 10)]

for i in groups:
    keys.extend([
        Key([mod], i.name, lazy.group[i.name].toscreen()),
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name)),
    ])

layouts = [
    layout.Max(),
    layout.Stack(num_stacks=2)
]

widget_defaults = dict(
    font='sans',
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.GroupBox(),
                widget.Prompt(),
                widget.WindowName(),
                widget.Systray(),
                widget.Clock(format='%I:%M'),
            ],
            32,
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname': 'branchdialog'},  # gitk
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
])
auto_fullscreen = True
focus_on_window_activation = "smart"

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
