from subprocess import call, Popen

from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.lazy import lazy
from libqtile import layout, bar, widget, hook

import colors

mod = 'mod4'

background = '/usr/share/backgrounds/gnome/Wood.jpg'
term = 'st'

@hook.subscribe.startup_once
def autostart_once():
    call('xmodmap ~/.speedswapper', shell=True)
    Popen('redshift -l -38:176 -t 6500:3500', shell=True)
    Popen('google-chrome-stable --no-startup-window', shell=True)

@hook.subscribe.startup
def autostart():
    call('xrdb ~/.Xresources', shell=True)
    call('feh --bg-fill ' + background, shell=True)
    Popen('compton', shell=True)

def lazyDmenu(cmd='dmenu_run'):
    return lazy.spawn(cmd + ' -f -b -i -fn "InputSans-Regular:pixelsize=16"'
                          + f' -nb {colors.black} -nf {colors.white}'
                          + f' -sb {colors.blue} -sf {colors.black}')

def lazyBrowser(url, profile=0):
    profile_dir = 'Default'
    if profile > 0:
        profile_dir = f'Profile {profile}'

    return lazy.spawn(f'google-chrome-stable --profile-directory={profile_dir} --new-window {url}')

keys = [
    # Control and power
    Key([mod], 'q', lazy.window.kill()),
    Key([mod, 'shift'], 'q', lazy.shutdown()),
    Key([mod], 'r', lazy.restart()),
    Key([mod], 'F4', lazy.spawn('poweroff')),

    # Navigation
    Key([mod], 'j', lazy.layout.down()),
    Key([mod], 'k', lazy.layout.up()),
    Key([mod, 'shift'], 'j', lazy.layout.shuffle_down()),
    Key([mod, 'shift'], 'k', lazy.layout.shuffle_up()),
    Key([mod], 'Tab', lazy.next_layout()),
    Key([mod], 't', lazy.window.toggle_floating()),
    Key([mod], 'F11', lazy.window.toggle_fullscreen()),

    # DMenus
    Key([mod, 'shift'], 'space', lazyDmenu()),
    Key([mod], 'space', lazyDmenu('dmenu_desktop')),
    Key([mod], 'comma', lazyDmenu('dmenu_config')),

    # Media
    Key([], 'XF86AudioPlay', lazy.spawn('mpc toggle')),
    Key([], 'XF86AudioMute', lazy.spawn('pactl set-sink-mute 0 toggle')),
    Key([], 'XF86AudioLowerVolume', lazy.spawn('pactl set-sink-volume 0 -2%')),
    Key([], 'XF86AudioRaiseVolume', lazy.spawn('pactl set-sink-volume 0 +2%')),

    # Applications
    Key([mod], 'Return', lazy.spawn(term)),
    Key([mod], 'e', lazy.spawn('nemo')),
    Key([mod], 'c', lazyBrowser('chrome://newtab')),
    Key([mod], 'y', lazyBrowser('https://youtube.com/')),
    Key([mod], 's', lazyBrowser('https://moodle.mmc.school.nz', profile=1)),
]

groups = [Group(str(i)) for i in range(1, 10)]

for i in groups:
    keys.extend([
        Key([mod], i.name, lazy.group[i.name].toscreen()),
        Key([mod, 'shift'], i.name, lazy.window.togroup(i.name)),
    ])

layouts = [
    layout.MonadTall(ratio=0.55, margin=24),
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
                widget.GroupBox(this_current_screen_border=colors.blue,
                                borderwidth=2,
                                highlight_method='line',
                                padding=0),
                widget.WindowName(),
                widget.Systray(),
                widget.Volume(),
                widget.Clock(format='%H:%M'),
            ],
            24,
            background=colors.black,
            opacity=0.8,
        ),
    ),
]

mouse = [
    Drag([mod], 'Button1', lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], 'Button3', lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], 'Button2', lazy.window.bring_to_front())
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
focus_on_window_activation = 'smart'
widget_defaults={'font': 'InputSans', 'fontsize': 14, 'padding': 5}

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = 'LG3D'
