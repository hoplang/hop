use std::collections::{HashMap, HashSet};

mod class_groups_lex;

#[cfg(test)]
mod tests;

use class_groups_lex::{ClassGroup, get_class_group};

/// A parsed Tailwind class name
#[derive(Debug)]
struct ParsedClass {
    /// Sorted and joined modifiers for conflict detection
    modifiers: String,
    /// Whether the class has an important modifier (!)
    has_important: bool,
    /// The class group this class belongs to
    group: ClassGroup,
}

/// Parse a single class name into its components.
/// Returns None if the class doesn't belong to a known Tailwind class group.
fn parse_class(class: &str) -> Option<ParsedClass> {
    let mut modifiers: Vec<&str> = Vec::new();
    let mut bracket_depth: u32 = 0;
    let mut paren_depth: u32 = 0;
    let mut modifier_start = 0;
    let mut postfix_modifier_pos: Option<usize> = None;

    let bytes = class.as_bytes();
    let len = bytes.len();

    for i in 0..len {
        let c = bytes[i];

        if bracket_depth == 0 && paren_depth == 0 {
            if c == b':' {
                modifiers.push(&class[modifier_start..i]);
                modifier_start = i + 1;
                continue;
            }

            if c == b'/' {
                postfix_modifier_pos = Some(i);
                continue;
            }
        }

        if c == b'[' {
            bracket_depth += 1;
        } else if c == b']' {
            bracket_depth = bracket_depth.saturating_sub(1);
        } else if c == b'(' {
            paren_depth += 1;
        } else if c == b')' {
            paren_depth = paren_depth.saturating_sub(1);
        }
    }

    // Get base class (everything after the last modifier separator)
    let base_with_important = if modifiers.is_empty() {
        class
    } else {
        &class[modifier_start..]
    };

    // Check for important modifier (! at start or end)
    let (base_class, has_important) = if let Some(stripped) = base_with_important.strip_suffix('!')
    {
        (stripped, true)
    } else if let Some(stripped) = base_with_important.strip_prefix('!') {
        (stripped, true)
    } else {
        (base_with_important, false)
    };

    // Adjust postfix modifier position relative to base_class
    let postfix_modifier_pos = postfix_modifier_pos.and_then(|pos| {
        if pos > modifier_start {
            Some(pos - modifier_start)
        } else {
            None
        }
    });

    // Get the class group, using base class without postfix modifier
    let base_for_lookup = if let Some(pos) = postfix_modifier_pos {
        &base_class[..pos]
    } else {
        base_class
    };
    let group = get_class_group(base_for_lookup)?;

    // Sort and join modifiers for conflict detection
    let modifiers = if modifiers.is_empty() {
        String::new()
    } else {
        sort_modifiers(&modifiers).join(":")
    };

    Some(ParsedClass {
        modifiers,
        has_important,
        group,
    })
}

/// Get the class groups that conflict with a given group
fn get_conflicts(group: ClassGroup) -> &'static [ClassGroup] {
    use ClassGroup::*;
    match group {
        // Margin conflicts
        Margin => &[
            MarginX, MarginY, MarginS, MarginE, MarginT, MarginR, MarginB, MarginL,
        ],
        MarginX => &[MarginR, MarginL],
        MarginY => &[MarginT, MarginB],

        // Padding conflicts
        Padding => &[
            PaddingX, PaddingY, PaddingS, PaddingE, PaddingT, PaddingR, PaddingB, PaddingL,
        ],
        PaddingX => &[PaddingR, PaddingL],
        PaddingY => &[PaddingT, PaddingB],

        // Inset conflicts
        Inset => &[InsetX, InsetY, Start, End, Top, Right, Bottom, Left],
        InsetX => &[Right, Left],
        InsetY => &[Top, Bottom],

        // Overflow conflicts
        Overflow => &[OverflowX, OverflowY],

        // Overscroll conflicts
        Overscroll => &[OverscrollX, OverscrollY],

        // Border spacing conflicts
        BorderSpacing => &[BorderSpacingX, BorderSpacingY],

        // Size conflicts
        Size => &[Width, Height],

        // Gap conflicts
        Gap => &[GapX, GapY],

        // Flex conflicts
        Flex => &[Basis, Grow, Shrink],

        // Border width conflicts
        BorderW => &[
            BorderWX, BorderWY, BorderWS, BorderWE, BorderWT, BorderWR, BorderWB, BorderWL,
        ],
        BorderWX => &[BorderWR, BorderWL],
        BorderWY => &[BorderWT, BorderWB],

        // Border color conflicts
        BorderColor => &[
            BorderColorX,
            BorderColorY,
            BorderColorS,
            BorderColorE,
            BorderColorT,
            BorderColorR,
            BorderColorB,
            BorderColorL,
        ],
        BorderColorX => &[BorderColorR, BorderColorL],
        BorderColorY => &[BorderColorT, BorderColorB],

        // Font-size conflicts with leading (when using postfix modifier like text-lg/7)
        FontSize => &[Leading],

        // Font variant numeric conflicts
        FvnNormal => &[
            FvnOrdinal,
            FvnSlashedZero,
            FvnFigure,
            FvnSpacing,
            FvnFraction,
        ],
        FvnOrdinal => &[FvnNormal],
        FvnSlashedZero => &[FvnNormal],
        FvnFigure => &[FvnNormal],
        FvnSpacing => &[FvnNormal],
        FvnFraction => &[FvnNormal],

        // Touch conflicts
        Touch => &[TouchX, TouchY, TouchPz],
        TouchX => &[Touch],
        TouchY => &[Touch],
        TouchPz => &[Touch],

        // Line clamp conflicts
        LineClamp => &[Display, Overflow],

        // Grid column/row conflicts
        ColSpan => &[Col],
        Col => &[ColSpan],
        RowSpan => &[Row],
        Row => &[RowSpan],

        // Rounded conflicts
        Rounded => &[
            RoundedS, RoundedE, RoundedT, RoundedR, RoundedB, RoundedL, RoundedSs, RoundedSe,
            RoundedEe, RoundedEs, RoundedTl, RoundedTr, RoundedBr, RoundedBl,
        ],
        RoundedS => &[RoundedSs, RoundedEs],
        RoundedE => &[RoundedSe, RoundedEe],
        RoundedT => &[RoundedTl, RoundedTr],
        RoundedR => &[RoundedTr, RoundedBr],
        RoundedB => &[RoundedBr, RoundedBl],
        RoundedL => &[RoundedTl, RoundedBl],

        // Translate conflicts
        Translate => &[TranslateX, TranslateY, TranslateNone],
        TranslateNone => &[Translate, TranslateX, TranslateY, TranslateZ],

        // Scroll margin conflicts
        ScrollM => &[
            ScrollMx, ScrollMy, ScrollMs, ScrollMe, ScrollMt, ScrollMr, ScrollMb, ScrollMl,
        ],
        ScrollMx => &[ScrollMr, ScrollMl],
        ScrollMy => &[ScrollMt, ScrollMb],

        // Scroll padding conflicts
        ScrollP => &[
            ScrollPx, ScrollPy, ScrollPs, ScrollPe, ScrollPt, ScrollPr, ScrollPb, ScrollPl,
        ],
        ScrollPx => &[ScrollPr, ScrollPl],
        ScrollPy => &[ScrollPt, ScrollPb],

        _ => &[],
    }
}

/// Sort modifiers to normalize for conflict detection
/// Order-sensitive modifiers (*, before, after, etc.) are NOT sorted - they create breaks
fn sort_modifiers<'a>(modifiers: &[&'a str]) -> Vec<&'a str> {
    // Order-sensitive modifiers that should NOT be sorted
    // These modifiers affect elements where the order matters (pseudo-elements, etc.)
    static ORDER_SENSITIVE: &[&str] = &[
        "*",
        "**",
        "after",
        "backdrop",
        "before",
        "details-content",
        "file",
        "first-letter",
        "first-line",
        "marker",
        "placeholder",
        "selection",
    ];

    let mut result: Vec<&'a str> = Vec::new();
    let mut current_segment: Vec<&'a str> = Vec::new();

    for modifier in modifiers {
        // Check if modifier is order-sensitive (starts with '[' for arbitrary variants, or in list)
        let is_arbitrary = modifier.starts_with('[');
        let is_order_sensitive = ORDER_SENSITIVE.contains(modifier);

        if is_arbitrary || is_order_sensitive {
            // Sort and flush current segment
            if !current_segment.is_empty() {
                current_segment.sort();
                result.append(&mut current_segment);
            }
            result.push(modifier);
        } else {
            // Regular modifier - add to segment for batch sorting
            current_segment.push(modifier);
        }
    }

    // Sort and add any remaining segment
    if !current_segment.is_empty() {
        current_segment.sort();
        result.append(&mut current_segment);
    }

    result
}

/// Stateful class merger that tracks seen groups and determines conflicts
#[derive(Default)]
struct ClassMerger {
    /// Maps (has_important, modifiers) to set of seen class groups
    seen_groups: HashMap<(bool, String), HashSet<ClassGroup>>,
}

impl ClassMerger {
    /// Parse a class and return whether it should be kept (process in reverse order)
    fn should_keep(&mut self, class: &str) -> bool {
        let Some(parsed) = parse_class(class) else {
            // Non-Tailwind class - always keep
            return true;
        };

        let key = (parsed.has_important, parsed.modifiers);
        let groups = self.seen_groups.entry(key).or_default();

        // Skip - already have a class from this group with same modifiers
        if groups.contains(&parsed.group) {
            return false;
        }

        // Record this group and conflicts
        groups.insert(parsed.group);
        groups.extend(get_conflicts(parsed.group));

        true
    }
}

/// Merge Tailwind CSS classes, removing conflicting classes
pub fn tw_merge(input: &str) -> String {
    let mut merger = ClassMerger::default();
    let mut result: Vec<&str> = Vec::new();

    for class in input.split_whitespace().rev() {
        if merger.should_keep(class) {
            result.push(class);
        }
    }

    result.reverse();
    result.join(" ")
}
