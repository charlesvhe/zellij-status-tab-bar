mod first_line;
mod second_line;
mod tip;
mod line;
mod tab;

use ansi_term::{
    ANSIString,
    Colour::{Fixed, RGB},
    Style,
};

use std::convert::TryInto;
use std::fmt::{Display, Error, Formatter};

use zellij_tile::prelude::actions::Action;
use zellij_tile::prelude::*;
use zellij_tile_utils::{palette_match, style};

use first_line::first_line;
use second_line::{
    floating_panes_are_visible, fullscreen_panes_to_hide, keybinds,
    locked_floating_panes_are_visible, locked_fullscreen_panes_to_hide, system_clipboard_error,
    text_copied_hint,
};
use tip::utils::get_cached_tip_name;

use line::tab_line;
use tab::tab_style;

// for more of these, copy paste from: https://en.wikipedia.org/wiki/Box-drawing_character
static ARROW_SEPARATOR: &str = "";
static MORE_MSG: &str = " ... ";
/// Shorthand for `Action::SwitchToMode(InputMode::Normal)`.
const TO_NORMAL: Action = Action::SwitchToMode(InputMode::Normal);

#[derive(Default)]
struct State {
    tabs: Vec<TabInfo>,
    active_tab_idx: usize,
    mode_info: ModeInfo,
    should_change_tab: bool,
    tip_name: String,
    text_copy_destination: Option<CopyDestination>,
    display_system_clipboard_failure: bool,
}

register_plugin!(State);

#[derive(Debug, Default)]
pub struct TabLinePart {
    part: String,
    len: usize,
    tab_index: Option<usize>,
}

#[derive(Default)]
pub struct LinePart {
    part: String,
    len: usize,
}

impl LinePart {
    pub fn append(&mut self, to_append: &LinePart) {
        self.part.push_str(&to_append.part);
        self.len += to_append.len;
    }
}

impl Display for LinePart {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.part)
    }
}

#[derive(Clone, Copy)]
pub struct ColoredElements {
    pub selected: SegmentStyle,
    pub unselected: SegmentStyle,
    pub unselected_alternate: SegmentStyle,
    pub disabled: SegmentStyle,
    // superkey
    pub superkey_prefix: Style,
    pub superkey_suffix_separator: Style,
}

#[derive(Clone, Copy)]
pub struct SegmentStyle {
    pub prefix_separator: Style,
    pub char_left_separator: Style,
    pub char_shortcut: Style,
    pub char_right_separator: Style,
    pub styled_text: Style,
    pub suffix_separator: Style,
}

// I really hate this, but I can't come up with a good solution for this,
// we need different colors from palette for the default theme
// plus here we can add new sources in the future, like Theme
// that can be defined in the config perhaps
fn color_elements(palette: Palette, different_color_alternates: bool) -> ColoredElements {
    let background = match palette.theme_hue {
        ThemeHue::Dark => palette.black,
        ThemeHue::Light => palette.white,
    };
    let foreground = match palette.theme_hue {
        ThemeHue::Dark => palette.white,
        ThemeHue::Light => palette.black,
    };
    let alternate_background_color = if different_color_alternates {
        match palette.theme_hue {
            ThemeHue::Dark => palette.white,
            ThemeHue::Light => palette.black,
        }
    } else {
        palette.fg
    };
    match palette.source {
        PaletteSource::Default => ColoredElements {
            selected: SegmentStyle {
                prefix_separator: style!(background, palette.green),
                char_left_separator: style!(background, palette.green).bold(),
                char_shortcut: style!(palette.red, palette.green).bold(),
                char_right_separator: style!(background, palette.green).bold(),
                styled_text: style!(background, palette.green).bold(),
                suffix_separator: style!(palette.green, background).bold(),
            },
            unselected: SegmentStyle {
                prefix_separator: style!(background, palette.fg),
                char_left_separator: style!(background, palette.fg).bold(),
                char_shortcut: style!(palette.red, palette.fg).bold(),
                char_right_separator: style!(background, palette.fg).bold(),
                styled_text: style!(background, palette.fg).bold(),
                suffix_separator: style!(palette.fg, background),
            },
            unselected_alternate: SegmentStyle {
                prefix_separator: style!(background, alternate_background_color),
                char_left_separator: style!(background, alternate_background_color).bold(),
                char_shortcut: style!(palette.red, alternate_background_color).bold(),
                char_right_separator: style!(background, alternate_background_color).bold(),
                styled_text: style!(background, alternate_background_color).bold(),
                suffix_separator: style!(alternate_background_color, background),
            },
            disabled: SegmentStyle {
                prefix_separator: style!(background, palette.fg),
                char_left_separator: style!(background, palette.fg).dimmed().italic(),
                char_shortcut: style!(background, palette.fg).dimmed().italic(),
                char_right_separator: style!(background, palette.fg).dimmed().italic(),
                styled_text: style!(background, palette.fg).dimmed().italic(),
                suffix_separator: style!(palette.fg, background),
            },
            superkey_prefix: style!(foreground, background).bold(),
            superkey_suffix_separator: style!(background, background),
        },
        PaletteSource::Xresources => ColoredElements {
            selected: SegmentStyle {
                prefix_separator: style!(background, palette.green),
                char_left_separator: style!(palette.fg, palette.green).bold(),
                char_shortcut: style!(palette.red, palette.green).bold(),
                char_right_separator: style!(palette.fg, palette.green).bold(),
                styled_text: style!(background, palette.green).bold(),
                suffix_separator: style!(palette.green, background).bold(),
            },
            unselected: SegmentStyle {
                prefix_separator: style!(background, palette.fg),
                char_left_separator: style!(background, palette.fg).bold(),
                char_shortcut: style!(palette.red, palette.fg).bold(),
                char_right_separator: style!(background, palette.fg).bold(),
                styled_text: style!(background, palette.fg).bold(),
                suffix_separator: style!(palette.fg, background),
            },
            unselected_alternate: SegmentStyle {
                prefix_separator: style!(background, alternate_background_color),
                char_left_separator: style!(background, alternate_background_color).bold(),
                char_shortcut: style!(palette.red, alternate_background_color).bold(),
                char_right_separator: style!(background, alternate_background_color).bold(),
                styled_text: style!(background, alternate_background_color).bold(),
                suffix_separator: style!(alternate_background_color, background),
            },
            disabled: SegmentStyle {
                prefix_separator: style!(background, palette.fg),
                char_left_separator: style!(background, palette.fg).dimmed(),
                char_shortcut: style!(background, palette.fg).dimmed(),
                char_right_separator: style!(background, palette.fg).dimmed(),
                styled_text: style!(background, palette.fg).dimmed(),
                suffix_separator: style!(palette.fg, background),
            },
            superkey_prefix: style!(background, palette.fg).bold(),
            superkey_suffix_separator: style!(palette.fg, background),
        },
    }
}

impl ZellijPlugin for State {
    fn load(&mut self) {
        // TODO: Should be able to choose whether to use the cache through config.
        self.tip_name = get_cached_tip_name();
        set_selectable(false);
        subscribe(&[
            EventType::ModeUpdate,
            EventType::TabUpdate,
            EventType::CopyToClipboard,
            EventType::InputReceived,
            EventType::SystemClipboardFailure,
        ]);
    }

    fn update(&mut self, event: Event) -> bool {
        let mut should_render = false;
        match event {
            Event::ModeUpdate(mode_info) => {
                if self.mode_info != mode_info {
                    should_render = true;
                }
                self.mode_info = mode_info;
            },
            Event::TabUpdate(tabs) => {
                if let Some(active_tab_index) = tabs.iter().position(|t| t.active) {
                    // tabs are indexed starting from 1 so we need to add 1
                    let active_tab_idx = active_tab_index + 1;
                    if self.active_tab_idx != active_tab_idx || self.tabs != tabs {
                        should_render = true;
                    }
                    self.active_tab_idx = active_tab_idx;
                    self.tabs = tabs;
                } else {
                    eprintln!("Could not find active tab.");
                }
            },
            Event::CopyToClipboard(copy_destination) => {
                match self.text_copy_destination {
                    Some(text_copy_destination) => {
                        if text_copy_destination != copy_destination {
                            should_render = true;
                        }
                    },
                    None => {
                        should_render = true;
                    },
                }
                self.text_copy_destination = Some(copy_destination);
            },
            Event::SystemClipboardFailure => {
                should_render = true;
                self.display_system_clipboard_failure = true;
            },
            Event::InputReceived => {
                if self.text_copy_destination.is_some()
                    || self.display_system_clipboard_failure == true
                {
                    should_render = true;
                }
                self.text_copy_destination = None;
                self.display_system_clipboard_failure = false;
            },
            _ => {},
        };
        should_render
    }

    fn render(&mut self, rows: usize, cols: usize) {
        let supports_arrow_fonts = !self.mode_info.capabilities.arrow_fonts;
        let separator = if supports_arrow_fonts {
            ARROW_SEPARATOR
        } else {
            ""
        };

        // Tab bar
        if self.tabs.is_empty() {
            return;
        }
        let mut all_tabs: Vec<TabLinePart> = vec![];
        let mut active_tab_index = 0;
        let mut is_alternate_tab = false;
        for t in &mut self.tabs {
            let mut tabname = t.name.clone();
            if t.active && self.mode_info.mode == InputMode::RenameTab {
                if tabname.is_empty() {
                    tabname = String::from("Enter name...");
                }
                active_tab_index = t.position;
            } else if t.active {
                active_tab_index = t.position;
            }
            let tab = tab_style(
                tabname,
                t,
                is_alternate_tab,
                self.mode_info.style.colors,
                self.mode_info.capabilities,
            );
            is_alternate_tab = !is_alternate_tab;
            all_tabs.push(tab);
        }
        let tab_line = tab_line(
            self.mode_info.session_name.as_deref(),
            all_tabs,
            active_tab_index,
            cols.saturating_sub(1),
            self.mode_info.style.colors,
            self.mode_info.capabilities,
            self.mode_info.style.hide_session_name,
        );
        let mut s = String::new();
        let mut len_cnt = 0;
        for bar_part in tab_line {
            s = format!("{}{}", s, &bar_part.part);

            if self.should_change_tab
                && bar_part.tab_index.is_some()
            {
                // Tabs are indexed starting from 1, therefore we need add 1 to tab_index.
                let tab_index: u32 = bar_part.tab_index.unwrap().try_into().unwrap();
                switch_tab_to(tab_index + 1);
            }
            len_cnt += bar_part.len;
        }
        self.should_change_tab = false;

        // Status bar
        let active_tab = self.tabs.iter().find(|t| t.active);
        let first_line = s + first_line(&self.mode_info, active_tab, cols - len_cnt, separator).part.as_str();
        let second_line = self.second_line(cols);

        let background = match self.mode_info.style.colors.theme_hue {
            ThemeHue::Dark => self.mode_info.style.colors.black,
            ThemeHue::Light => self.mode_info.style.colors.white,
        };

        // [48;5;238m is white background, [0K is so that it fills the rest of the line
        // [m is background reset, [0K is so that it clears the rest of the line
        match background {
            PaletteColor::Rgb((r, g, b)) => {
                if rows > 1 {
                    println!("{}\u{1b}[48;2;{};{};{}m\u{1b}[0K", first_line, r, g, b);
                } else {
                    if self.mode_info.mode == InputMode::Normal {
                        print!("{}\u{1b}[48;2;{};{};{}m\u{1b}[0K", first_line, r, g, b);
                    } else {
                        print!("\u{1b}[m{}\u{1b}[0K", second_line);
                    }
                }
            },
            PaletteColor::EightBit(color) => {
                if rows > 1 {
                    println!("{}\u{1b}[48;5;{}m\u{1b}[0K", first_line, color);
                } else {
                    if self.mode_info.mode == InputMode::Normal {
                        print!("{}\u{1b}[48;5;{}m\u{1b}[0K", first_line, color);
                    } else {
                        print!("\u{1b}[m{}\u{1b}[0K", second_line);
                    }
                }
            },
        }

        if rows > 1 {
            print!("\u{1b}[m{}\u{1b}[0K", second_line);
        }
    }
}

impl State {
    fn second_line(&self, cols: usize) -> LinePart {
        let active_tab = self.tabs.iter().find(|t| t.active);

        if let Some(copy_destination) = self.text_copy_destination {
            text_copied_hint(&self.mode_info.style.colors, copy_destination)
        } else if self.display_system_clipboard_failure {
            system_clipboard_error(&self.mode_info.style.colors)
        } else if let Some(active_tab) = active_tab {
            if active_tab.is_fullscreen_active {
                match self.mode_info.mode {
                    InputMode::Normal => fullscreen_panes_to_hide(
                        &self.mode_info.style.colors,
                        active_tab.panes_to_hide,
                    ),
                    InputMode::Locked => locked_fullscreen_panes_to_hide(
                        &self.mode_info.style.colors,
                        active_tab.panes_to_hide,
                    ),
                    _ => keybinds(&self.mode_info, &self.tip_name, cols),
                }
            } else if active_tab.are_floating_panes_visible {
                match self.mode_info.mode {
                    InputMode::Normal => floating_panes_are_visible(&self.mode_info),
                    InputMode::Locked => {
                        locked_floating_panes_are_visible(&self.mode_info.style.colors)
                    },
                    _ => keybinds(&self.mode_info, &self.tip_name, cols),
                }
            } else {
                keybinds(&self.mode_info, &self.tip_name, cols)
            }
        } else {
            LinePart::default()
        }
    }
}

/// Get a common modifier key from a key vector.
///
/// Iterates over all keys and returns any found common modifier key. Possible modifiers that will
/// be detected are "Ctrl" and "Alt".
pub fn get_common_modifier(keyvec: Vec<&Key>) -> Option<String> {
    let mut modifier = "";
    let mut new_modifier;
    for key in keyvec.iter() {
        match key {
            Key::Ctrl(_) => new_modifier = "Ctrl",
            Key::Alt(_) => new_modifier = "Alt",
            _ => return None,
        }
        if modifier.is_empty() {
            modifier = new_modifier;
        } else if modifier != new_modifier {
            // Prefix changed!
            return None;
        }
    }
    match modifier.is_empty() {
        true => None,
        false => Some(modifier.to_string()),
    }
}

/// Get key from action pattern(s).
///
/// This function takes as arguments a `keymap` that is a `Vec<(Key, Vec<Action>)>` and contains
/// all keybindings for the current mode and one or more `p` patterns which match a sequence of
/// actions to search for. If within the keymap a sequence of actions matching `p` is found, all
/// keys that trigger the action pattern are returned as vector of `Vec<Key>`.
pub fn action_key(keymap: &[(Key, Vec<Action>)], action: &[Action]) -> Vec<Key> {
    keymap
        .iter()
        .filter_map(|(key, acvec)| {
            let matching = acvec
                .iter()
                .zip(action)
                .filter(|(a, b)| a.shallow_eq(b))
                .count();

            if matching == acvec.len() && matching == action.len() {
                Some(*key)
            } else {
                None
            }
        })
        .collect::<Vec<Key>>()
}

/// Get multiple keys for multiple actions.
///
/// An extension of [`action_key`] that iterates over all action tuples and collects the results.
pub fn action_key_group(keymap: &[(Key, Vec<Action>)], actions: &[&[Action]]) -> Vec<Key> {
    let mut ret = vec![];
    for action in actions {
        ret.extend(action_key(keymap, action));
    }
    ret
}

/// Style a vector of [`Key`]s with the given [`Palette`].
///
/// Creates a line segment of style `<KEYS>`, with correct theming applied: The brackets have the
/// regular text color, the enclosed keys are painted green and bold. If the keys share a common
/// modifier (See [`get_common_modifier`]), it is printed in front of the keys, painted green and
/// bold, separated with a `+`: `MOD + <KEYS>`.
///
/// If multiple [`Key`]s are given, the individual keys are separated with a `|` char. This does
/// not apply to the following groups of keys which are treated specially and don't have a
/// separator between them:
///
/// - "hjkl"
/// - "HJKL"
/// - "←↓↑→"
/// - "←→"
/// - "↓↑"
///
/// The returned Vector of [`ANSIString`] is suitable for transformation into an [`ANSIStrings`]
/// type.
pub fn style_key_with_modifier(
    keyvec: &[Key],
    palette: &Palette,
    background: Option<PaletteColor>,
) -> Vec<ANSIString<'static>> {
    // Nothing to do, quit...
    if keyvec.is_empty() {
        return vec![];
    }

    let text_color = palette_match!(match palette.theme_hue {
        ThemeHue::Dark => palette.white,
        ThemeHue::Light => palette.black,
    });
    let green_color = palette_match!(palette.green);
    let orange_color = palette_match!(palette.orange);
    let mut ret = vec![];

    // Prints modifier key
    let modifier_str = match get_common_modifier(keyvec.iter().collect()) {
        Some(modifier) => modifier,
        None => "".to_string(),
    };
    let no_modifier = modifier_str.is_empty();
    let painted_modifier = if modifier_str.is_empty() {
        Style::new().paint("")
    } else {
        if let Some(background) = background {
            let background = palette_match!(background);
            Style::new()
                .fg(orange_color)
                .on(background)
                .bold()
                .paint(modifier_str)
        } else {
            Style::new().fg(orange_color).bold().paint(modifier_str)
        }
    };
    ret.push(painted_modifier);

    // Prints key group start
    let group_start_str = if no_modifier { "<" } else { " + <" };
    if let Some(background) = background {
        let background = palette_match!(background);
        ret.push(
            Style::new()
                .fg(text_color)
                .on(background)
                .paint(group_start_str),
        );
    } else {
        ret.push(Style::new().fg(text_color).paint(group_start_str));
    }

    // Prints the keys
    let key = keyvec
        .iter()
        .map(|key| {
            if no_modifier {
                format!("{}", key)
            } else {
                match key {
                    Key::Ctrl(c) => format!("{}", Key::Char(*c)),
                    Key::Alt(c) => format!("{}", c),
                    _ => format!("{}", key),
                }
            }
        })
        .collect::<Vec<String>>();

    // Special handling of some pre-defined keygroups
    let key_string = key.join("");
    let key_separator = match &key_string[..] {
        "HJKL" => "",
        "hjkl" => "",
        "←↓↑→" => "",
        "←→" => "",
        "↓↑" => "",
        "[]" => "",
        _ => "|",
    };

    for (idx, key) in key.iter().enumerate() {
        if idx > 0 && !key_separator.is_empty() {
            if let Some(background) = background {
                let background = palette_match!(background);
                ret.push(
                    Style::new()
                        .fg(text_color)
                        .on(background)
                        .paint(key_separator),
                );
            } else {
                ret.push(Style::new().fg(text_color).paint(key_separator));
            }
        }
        if let Some(background) = background {
            let background = palette_match!(background);
            ret.push(
                Style::new()
                    .fg(green_color)
                    .on(background)
                    .bold()
                    .paint(key.clone()),
            );
        } else {
            ret.push(Style::new().fg(green_color).bold().paint(key.clone()));
        }
    }

    let group_end_str = ">";
    if let Some(background) = background {
        let background = palette_match!(background);
        ret.push(
            Style::new()
                .fg(text_color)
                .on(background)
                .paint(group_end_str),
        );
    } else {
        ret.push(Style::new().fg(text_color).paint(group_end_str));
    }

    ret
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use ansi_term::unstyle;
    use ansi_term::ANSIStrings;
    use zellij_tile::prelude::CharOrArrow;
    use zellij_tile::prelude::Direction;

    fn big_keymap() -> Vec<(Key, Vec<Action>)> {
        vec![
            (Key::Char('a'), vec![Action::Quit]),
            (Key::Ctrl('b'), vec![Action::ScrollUp]),
            (Key::Ctrl('d'), vec![Action::ScrollDown]),
            (
                Key::Alt(CharOrArrow::Char('c')),
                vec![Action::ScrollDown, Action::SwitchToMode(InputMode::Normal)],
            ),
            (
                Key::Char('1'),
                vec![TO_NORMAL, Action::SwitchToMode(InputMode::Locked)],
            ),
        ]
    }

    #[test]
    fn common_modifier_with_ctrl_keys() {
        let keyvec = vec![Key::Ctrl('a'), Key::Ctrl('b'), Key::Ctrl('c')];
        let ret = get_common_modifier(keyvec.iter().collect());
        assert_eq!(ret, Some("Ctrl".to_string()));
    }

    #[test]
    fn common_modifier_with_alt_keys_chars() {
        let keyvec = vec![
            Key::Alt(CharOrArrow::Char('1')),
            Key::Alt(CharOrArrow::Char('t')),
            Key::Alt(CharOrArrow::Char('z')),
        ];
        let ret = get_common_modifier(keyvec.iter().collect());
        assert_eq!(ret, Some("Alt".to_string()));
    }

    #[test]
    fn common_modifier_with_alt_keys_arrows() {
        let keyvec = vec![
            Key::Alt(CharOrArrow::Direction(Direction::Left)),
            Key::Alt(CharOrArrow::Direction(Direction::Right)),
        ];
        let ret = get_common_modifier(keyvec.iter().collect());
        assert_eq!(ret, Some("Alt".to_string()));
    }

    #[test]
    fn common_modifier_with_alt_keys_arrows_and_chars() {
        let keyvec = vec![
            Key::Alt(CharOrArrow::Direction(Direction::Left)),
            Key::Alt(CharOrArrow::Direction(Direction::Right)),
            Key::Alt(CharOrArrow::Char('t')),
            Key::Alt(CharOrArrow::Char('z')),
        ];
        let ret = get_common_modifier(keyvec.iter().collect());
        assert_eq!(ret, Some("Alt".to_string()));
    }

    #[test]
    fn common_modifier_with_mixed_alt_ctrl_keys() {
        let keyvec = vec![
            Key::Alt(CharOrArrow::Direction(Direction::Left)),
            Key::Alt(CharOrArrow::Char('z')),
            Key::Ctrl('a'),
            Key::Ctrl('1'),
        ];
        let ret = get_common_modifier(keyvec.iter().collect());
        assert_eq!(ret, None);
    }

    #[test]
    fn common_modifier_with_any_keys() {
        let keyvec = vec![Key::Backspace, Key::Char('f'), Key::Down];
        let ret = get_common_modifier(keyvec.iter().collect());
        assert_eq!(ret, None);
    }

    #[test]
    fn common_modifier_with_ctrl_and_normal_keys() {
        let keyvec = vec![Key::Ctrl('a'), Key::Char('f'), Key::Down];
        let ret = get_common_modifier(keyvec.iter().collect());
        assert_eq!(ret, None);
    }

    #[test]
    fn common_modifier_with_alt_and_normal_keys() {
        let keyvec = vec![Key::Alt(CharOrArrow::Char('a')), Key::Char('f'), Key::Down];
        let ret = get_common_modifier(keyvec.iter().collect());
        assert_eq!(ret, None);
    }

    #[test]
    fn action_key_simple_pattern_match_exact() {
        let keymap = &[(Key::Char('f'), vec![Action::Quit])];
        let ret = action_key(keymap, &[Action::Quit]);
        assert_eq!(ret, vec![Key::Char('f')]);
    }

    #[test]
    fn action_key_simple_pattern_match_pattern_too_long() {
        let keymap = &[(Key::Char('f'), vec![Action::Quit])];
        let ret = action_key(keymap, &[Action::Quit, Action::ScrollUp]);
        assert_eq!(ret, Vec::new());
    }

    #[test]
    fn action_key_simple_pattern_match_pattern_empty() {
        let keymap = &[(Key::Char('f'), vec![Action::Quit])];
        let ret = action_key(keymap, &[]);
        assert_eq!(ret, Vec::new());
    }

    #[test]
    fn action_key_long_pattern_match_exact() {
        let keymap = big_keymap();
        let ret = action_key(&keymap, &[Action::ScrollDown, TO_NORMAL]);
        assert_eq!(ret, vec![Key::Alt(CharOrArrow::Char('c'))]);
    }

    #[test]
    fn action_key_long_pattern_match_too_short() {
        let keymap = big_keymap();
        let ret = action_key(&keymap, &[TO_NORMAL]);
        assert_eq!(ret, Vec::new());
    }

    #[test]
    fn action_key_group_single_pattern() {
        let keymap = big_keymap();
        let ret = action_key_group(&keymap, &[&[Action::Quit]]);
        assert_eq!(ret, vec![Key::Char('a')]);
    }

    #[test]
    fn action_key_group_two_patterns() {
        let keymap = big_keymap();
        let ret = action_key_group(&keymap, &[&[Action::ScrollDown], &[Action::ScrollUp]]);
        // Mind the order!
        assert_eq!(ret, vec![Key::Ctrl('d'), Key::Ctrl('b')]);
    }

    fn get_palette() -> Palette {
        Palette::default()
    }

    #[test]
    fn style_key_with_modifier_only_chars() {
        let keyvec = vec![Key::Char('a'), Key::Char('b'), Key::Char('c')];
        let palette = get_palette();

        let ret = style_key_with_modifier(&keyvec, &palette, None);
        let ret = unstyle(&ANSIStrings(&ret));

        assert_eq!(ret, "<a|b|c>".to_string())
    }

    #[test]
    fn style_key_with_modifier_special_group_hjkl() {
        let keyvec = vec![
            Key::Char('h'),
            Key::Char('j'),
            Key::Char('k'),
            Key::Char('l'),
        ];
        let palette = get_palette();

        let ret = style_key_with_modifier(&keyvec, &palette, None);
        let ret = unstyle(&ANSIStrings(&ret));

        assert_eq!(ret, "<hjkl>".to_string())
    }

    #[test]
    fn style_key_with_modifier_special_group_hjkl_broken() {
        // Sorted the wrong way
        let keyvec = vec![
            Key::Char('h'),
            Key::Char('k'),
            Key::Char('j'),
            Key::Char('l'),
        ];
        let palette = get_palette();

        let ret = style_key_with_modifier(&keyvec, &palette, None);
        let ret = unstyle(&ANSIStrings(&ret));

        assert_eq!(ret, "<h|k|j|l>".to_string())
    }

    #[test]
    fn style_key_with_modifier_special_group_all_arrows() {
        let keyvec = vec![
            Key::Char('←'),
            Key::Char('↓'),
            Key::Char('↑'),
            Key::Char('→'),
        ];
        let palette = get_palette();

        let ret = style_key_with_modifier(&keyvec, &palette, None);
        let ret = unstyle(&ANSIStrings(&ret));

        assert_eq!(ret, "<←↓↑→>".to_string())
    }

    #[test]
    fn style_key_with_modifier_special_group_left_right_arrows() {
        let keyvec = vec![Key::Char('←'), Key::Char('→')];
        let palette = get_palette();

        let ret = style_key_with_modifier(&keyvec, &palette, None);
        let ret = unstyle(&ANSIStrings(&ret));

        assert_eq!(ret, "<←→>".to_string())
    }

    #[test]
    fn style_key_with_modifier_special_group_down_up_arrows() {
        let keyvec = vec![Key::Char('↓'), Key::Char('↑')];
        let palette = get_palette();

        let ret = style_key_with_modifier(&keyvec, &palette, None);
        let ret = unstyle(&ANSIStrings(&ret));

        assert_eq!(ret, "<↓↑>".to_string())
    }

    #[test]
    fn style_key_with_modifier_common_ctrl_modifier_chars() {
        let keyvec = vec![
            Key::Ctrl('a'),
            Key::Ctrl('b'),
            Key::Ctrl('c'),
            Key::Ctrl('d'),
        ];
        let palette = get_palette();

        let ret = style_key_with_modifier(&keyvec, &palette, None);
        let ret = unstyle(&ANSIStrings(&ret));

        assert_eq!(ret, "Ctrl + <a|b|c|d>".to_string())
    }

    #[test]
    fn style_key_with_modifier_common_alt_modifier_chars() {
        let keyvec = vec![
            Key::Alt(CharOrArrow::Char('a')),
            Key::Alt(CharOrArrow::Char('b')),
            Key::Alt(CharOrArrow::Char('c')),
            Key::Alt(CharOrArrow::Char('d')),
        ];
        let palette = get_palette();

        let ret = style_key_with_modifier(&keyvec, &palette, None);
        let ret = unstyle(&ANSIStrings(&ret));

        assert_eq!(ret, "Alt + <a|b|c|d>".to_string())
    }

    #[test]
    fn style_key_with_modifier_common_alt_modifier_with_special_group_all_arrows() {
        let keyvec = vec![
            Key::Alt(CharOrArrow::Direction(Direction::Left)),
            Key::Alt(CharOrArrow::Direction(Direction::Down)),
            Key::Alt(CharOrArrow::Direction(Direction::Up)),
            Key::Alt(CharOrArrow::Direction(Direction::Right)),
        ];
        let palette = get_palette();

        let ret = style_key_with_modifier(&keyvec, &palette, None);
        let ret = unstyle(&ANSIStrings(&ret));

        assert_eq!(ret, "Alt + <←↓↑→>".to_string())
    }

    #[test]
    fn style_key_with_modifier_ctrl_alt_char_mixed() {
        let keyvec = vec![
            Key::Alt(CharOrArrow::Char('a')),
            Key::Ctrl('b'),
            Key::Char('c'),
        ];
        let palette = get_palette();

        let ret = style_key_with_modifier(&keyvec, &palette, None);
        let ret = unstyle(&ANSIStrings(&ret));

        assert_eq!(ret, "<Alt+a|Ctrl+b|c>".to_string())
    }

    #[test]
    fn style_key_with_modifier_unprintables() {
        let keyvec = vec![
            Key::Backspace,
            Key::Char('\n'),
            Key::Char(' '),
            Key::Char('\t'),
            Key::PageDown,
            Key::Delete,
            Key::Home,
            Key::End,
            Key::Insert,
            Key::BackTab,
            Key::Esc,
        ];
        let palette = get_palette();

        let ret = style_key_with_modifier(&keyvec, &palette, None);
        let ret = unstyle(&ANSIStrings(&ret));

        assert_eq!(
            ret,
            "<BACKSPACE|ENTER|SPACE|TAB|PgDn|DEL|HOME|END|INS|TAB|ESC>".to_string()
        )
    }

    #[test]
    fn style_key_with_modifier_unprintables_with_common_ctrl_modifier() {
        let keyvec = vec![Key::Ctrl('\n'), Key::Ctrl(' '), Key::Ctrl('\t')];
        let palette = get_palette();

        let ret = style_key_with_modifier(&keyvec, &palette, None);
        let ret = unstyle(&ANSIStrings(&ret));

        assert_eq!(ret, "Ctrl + <ENTER|SPACE|TAB>".to_string())
    }

    #[test]
    fn style_key_with_modifier_unprintables_with_common_alt_modifier() {
        let keyvec = vec![
            Key::Alt(CharOrArrow::Char('\n')),
            Key::Alt(CharOrArrow::Char(' ')),
            Key::Alt(CharOrArrow::Char('\t')),
        ];
        let palette = get_palette();

        let ret = style_key_with_modifier(&keyvec, &palette, None);
        let ret = unstyle(&ANSIStrings(&ret));

        assert_eq!(ret, "Alt + <ENTER|SPACE|TAB>".to_string())
    }
}
