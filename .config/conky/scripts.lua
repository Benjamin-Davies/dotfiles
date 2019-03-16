-- Adapted from https://github.com/JakeStanger/.conky/tree/3.0

require 'cairo'

COLOR_PRIMARY_R = 0.773
COLOR_PRIMARY_G = 0.784
COLOR_PRIMARY_B = 0.776

COLOR_SECONDARY_R = 0.177
COLOR_SECONDARY_G = 0.169
COLOR_SECONDARY_B = 0.200

function init_cairo()
    if conky_window == nil then
        return false
    end

    local cs = cairo_xlib_surface_create(conky_window.display,
        conky_window.drawable,
        conky_window.visual,
        conky_window.width,
        conky_window.height)

    cr = cairo_create(cs)

    local font = "Fira Code Sans"

    cairo_select_font_face(cr, font, CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
    cairo_set_source_rgba(cr, COLOR_PRIMARY_R, COLOR_PRIMARY_G, COLOR_PRIMARY_B, 1)

    return true
end

function conky_clock()
    if (not init_cairo()) then
        return
    end

    cairo_set_source_rgba(cr, COLOR_PRIMARY_R, COLOR_PRIMARY_G, COLOR_PRIMARY_B, 1)

    -- CLOCK
    local center_x = 200
    local center_y = 200
    local radius = 170

    local font_size = radius / 5.5

    local date_table = os.date('*t')

    local hours = date_table['hour']
    local minutes = date_table['min']
    local seconds = date_table['sec']

    local hours_str = tostring(hours)
    if string.len(hours_str) == 1 then hours_str = '0' .. hours_str end

    local minutes_str = tostring(minutes)
    if string.len(minutes_str) == 1 then minutes_str = '0' .. minutes_str end

    local seconds_str = tostring(seconds)
    if string.len(seconds_str) == 1 then seconds_str = '0' .. seconds_str end

    cairo_move_to(cr, center_x - font_size * 2.5, center_y + font_size / 2.5)
    cairo_set_font_size(cr, font_size)
    cairo_show_text(cr, hours_str .. ':' .. minutes_str .. ':' .. seconds_str)
    cairo_stroke(cr)

    if hours > 12 then hours = hours - 12 end


    local line_width = radius/8
    local start_angle = -math.pi / 2

    local end_angle = start_angle + ((hours + minutes / 60 + seconds / 3600) / 12) * 2 * math.pi
    cairo_set_line_width(cr, line_width)
    cairo_arc(cr, center_x, center_y, radius, start_angle, end_angle)
    cairo_stroke(cr)

    local end_angle = start_angle + ((minutes + seconds / 60) / 60) * 2 * math.pi
    cairo_set_line_width(cr, line_width)
    cairo_arc(cr, center_x, center_y, radius * 0.8, start_angle, end_angle)
    cairo_stroke(cr)

    if seconds == 0 then seconds = 60 end
    local end_angle = start_angle + (seconds / 60) * 2 * math.pi
    cairo_set_line_width(cr, line_width)
    cairo_arc(cr, center_x, center_y, radius * 0.6, start_angle, end_angle)
    cairo_stroke(cr)
end
