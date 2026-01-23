// re2c lexer for Tailwind CSS class group detection
// Generate with: re2c -W -Werror --lang rust -o class_groups_lex.rs class_groups_lex.re

#![allow(unused_unsafe, unused_assignments, clippy::all)]

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ClassGroup {
    Accent,
    AlignContent,
    AlignItems,
    AlignSelf,
    Animate,
    Appearance,
    Arbitrary,
    Aspect,
    AutoCols,
    AutoRows,
    BackdropBlur,
    BackdropBrightness,
    BackdropContrast,
    BackdropFilter,
    BackdropGrayscale,
    BackdropHueRotate,
    BackdropInvert,
    BackdropOpacity,
    BackdropSaturate,
    BackdropSepia,
    Backface,
    Basis,
    BgAttachment,
    BgBlend,
    BgClip,
    BgColor,
    BgImage,
    BgOrigin,
    BgPosition,
    BgRepeat,
    BgSize,
    Blur,
    BorderCollapse,
    BorderColor,
    BorderColorB,
    BorderColorE,
    BorderColorL,
    BorderColorR,
    BorderColorS,
    BorderColorT,
    BorderColorX,
    BorderColorY,
    BorderSpacing,
    BorderSpacingX,
    BorderSpacingY,
    BorderStyle,
    BorderW,
    BorderWB,
    BorderWE,
    BorderWL,
    BorderWR,
    BorderWS,
    BorderWT,
    BorderWX,
    BorderWY,
    Bottom,
    Box,
    BoxDecoration,
    Break,
    BreakAfter,
    BreakBefore,
    BreakInside,
    Brightness,
    Caption,
    CaretColor,
    Clear,
    Col,
    ColEnd,
    ColSpan,
    ColStart,
    ColorScheme,
    Columns,
    Container,
    Content,
    Contrast,
    Cursor,
    Delay,
    Display,
    DivideColor,
    DivideStyle,
    DivideX,
    DivideXReverse,
    DivideY,
    DivideYReverse,
    DropShadow,
    DropShadowColor,
    Duration,
    Ease,
    End,
    FieldSizing,
    Fill,
    Filter,
    Flex,
    FlexDirection,
    FlexWrap,
    Float,
    FontFamily,
    FontSize,
    FontSmoothing,
    FontStretch,
    FontStyle,
    FontWeight,
    ForcedColorAdjust,
    FvnFigure,
    FvnFraction,
    FvnNormal,
    FvnOrdinal,
    FvnSlashedZero,
    FvnSpacing,
    Gap,
    GapX,
    GapY,
    GradientFrom,
    GradientFromPos,
    GradientTo,
    GradientToPos,
    GradientVia,
    GradientViaPos,
    Grayscale,
    GridCols,
    GridFlow,
    GridRows,
    Grow,
    Height,
    HueRotate,
    Hyphens,
    Indent,
    Inset,
    InsetRingColor,
    InsetRingW,
    InsetShadow,
    InsetShadowColor,
    InsetX,
    InsetY,
    Invert,
    Isolation,
    JustifyContent,
    JustifyItems,
    JustifySelf,
    Leading,
    Left,
    LineClamp,
    ListImage,
    ListStylePosition,
    ListStyleType,
    Margin,
    MarginB,
    MarginE,
    MarginL,
    MarginR,
    MarginS,
    MarginT,
    MarginX,
    MarginY,
    MaskClip,
    MaskComposite,
    MaskImage,
    MaskMode,
    MaskOrigin,
    MaskPosition,
    MaskRepeat,
    MaskSize,
    MaskType,
    MaxH,
    MaxW,
    MinH,
    MinW,
    MixBlend,
    ObjectFit,
    ObjectPosition,
    Opacity,
    Order,
    OutlineColor,
    OutlineOffset,
    OutlineStyle,
    OutlineW,
    Overflow,
    OverflowX,
    OverflowY,
    Overscroll,
    OverscrollX,
    OverscrollY,
    Padding,
    PaddingB,
    PaddingE,
    PaddingL,
    PaddingR,
    PaddingS,
    PaddingT,
    PaddingX,
    PaddingY,
    Perspective,
    PerspectiveOrigin,
    PlaceContent,
    PlaceItems,
    PlaceSelf,
    PlaceholderColor,
    PointerEvents,
    Position,
    Resize,
    Right,
    RingColor,
    RingOffsetColor,
    RingOffsetW,
    RingW,
    RingWInset,
    Rotate,
    RotateX,
    RotateY,
    RotateZ,
    Rounded,
    RoundedB,
    RoundedBl,
    RoundedBr,
    RoundedE,
    RoundedEe,
    RoundedEs,
    RoundedL,
    RoundedR,
    RoundedS,
    RoundedSe,
    RoundedSs,
    RoundedT,
    RoundedTl,
    RoundedTr,
    Row,
    RowEnd,
    RowSpan,
    RowStart,
    Saturate,
    Scale,
    Scale3d,
    ScaleX,
    ScaleY,
    ScaleZ,
    ScrollBehavior,
    ScrollM,
    ScrollMb,
    ScrollMe,
    ScrollMl,
    ScrollMr,
    ScrollMs,
    ScrollMt,
    ScrollMx,
    ScrollMy,
    ScrollP,
    ScrollPb,
    ScrollPe,
    ScrollPl,
    ScrollPr,
    ScrollPs,
    ScrollPt,
    ScrollPx,
    ScrollPy,
    Select,
    Sepia,
    Shadow,
    ShadowColor,
    Shrink,
    Size,
    Skew,
    SkewX,
    SkewY,
    SnapAlign,
    SnapStop,
    SnapStrictness,
    SnapType,
    SpaceX,
    SpaceXReverse,
    SpaceY,
    SpaceYReverse,
    Sr,
    Start,
    Stroke,
    StrokeW,
    TableLayout,
    TextAlignment,
    TextColor,
    TextDecoration,
    TextDecorationColor,
    TextDecorationStyle,
    TextDecorationThickness,
    TextOverflow,
    TextShadow,
    TextShadowColor,
    TextTransform,
    TextWrap,
    Top,
    Touch,
    TouchPz,
    TouchX,
    TouchY,
    Tracking,
    Transform,
    TransformOrigin,
    TransformStyle,
    Transition,
    TransitionBehavior,
    Translate,
    TranslateNone,
    TranslateX,
    TranslateY,
    TranslateZ,
    UnderlineOffset,
    VerticalAlign,
    Visibility,
    Whitespace,
    Width,
    WillChange,
    Wrap,
    ZIndex,
}

/// Match a Tailwind class name to its conflict group.
///
/// The inner lexer (generated by re2c) requires a null-terminated byte slice.
/// To avoid heap allocation for every call, we use a stack buffer for class
/// names under 64 bytes, which covers the vast majority of Tailwind classes.
/// Only unusually long arbitrary values (e.g. `bg-[very-long-css-value]`)
/// fall back to heap allocation.
pub fn get_class_group(input: &str) -> Option<ClassGroup> {
    let bytes = input.as_bytes();
    let len = bytes.len();

    if len < 64 {
        // Fast path: copy to stack buffer with null terminator
        let mut buf = [0u8; 64];
        buf[..len].copy_from_slice(bytes);
        return get_class_group_inner(&buf);
    }

    // Slow path: heap allocate for long arbitrary values
    let mut bytes = bytes.to_vec();
    bytes.push(0);
    get_class_group_inner(&bytes)
}

#[rust_analyzer::skip]
fn get_class_group_inner(input: &[u8]) -> Option<ClassGroup> {
    let mut cur: usize = 0;
    let mut mar: usize = 0;

    /*!re2c
        re2c:define:YYCTYPE = u8;
        re2c:define:YYPEEK = "input[cur]";
        re2c:define:YYSKIP = "cur += 1;";
        re2c:define:YYBACKUP = "mar = cur;";
        re2c:define:YYRESTORE = "cur = mar;";
        re2c:yyfill:enable = 0;
        re2c:sentinel = 0;

        // Helper patterns
        d = [0-9];
        any = [^\x00];

        // Arbitrary length value pattern: matches 3px, 0.5rem, 10%, 50vw, etc.
        // CSS length units: px, em, rem, %, vw, vh, vmin, vmax, ch, ex, cap, lh, rlh, cqw, cqh, cqi, cqb, cqmin, cqmax, dvw, dvh, svw, svh, lvw, lvh, cm, mm, in, pc, pt
        length_unit = "px" | "em" | "rem" | "%" | "vw" | "vh" | "vmin" | "vmax" | "ch" | "ex" | "cap" | "lh" | "rlh" | "cqw" | "cqh" | "cqi" | "cqb" | "cqmin" | "cqmax" | "dvw" | "dvh" | "svw" | "svh" | "lvw" | "lvh" | "cm" | "mm" | "in" | "pc" | "pt" | "fr";
        arbitrary_length = "-"? d+ ("." d+)? length_unit;

        // Size helpers
        shadow_size = "3xs" | "2xs" | "xs" | "sm" | "md" | "lg" | "xl" | "2xl" | "3xl" | "4xl" | "5xl" | "6xl" | "7xl" | "8xl" | "9xl" | "none" | "inner";
        font_size = "xs" | "sm" | "md" | "base" | "lg" | "xl" | "2xl" | "3xl" | "4xl" | "5xl" | "6xl" | "7xl" | "8xl" | "9xl" | d+ "." d+ "xl";

        // === LAYOUT ===

        // Display
        ("block" | "inline-block" | "inline" | "flex" | "inline-flex" | "table" | "inline-table" | "table-caption" | "table-cell" | "table-column" | "table-column-group" | "table-footer-group" | "table-header-group" | "table-row-group" | "table-row" | "flow-root" | "grid" | "inline-grid" | "contents" | "list-item" | "hidden") [\x00] { return Some(ClassGroup::Display); }

        // Position
        ("static" | "fixed" | "absolute" | "relative" | "sticky") [\x00] { return Some(ClassGroup::Position); }

        // Visibility
        ("visible" | "invisible" | "collapse") [\x00] { return Some(ClassGroup::Visibility); }

        // Isolation
        ("isolate" | "isolation-auto") [\x00] { return Some(ClassGroup::Isolation); }

        // Box
        ("box-border" | "box-content") [\x00] { return Some(ClassGroup::Box); }
        ("box-decoration-slice" | "box-decoration-clone") [\x00] { return Some(ClassGroup::BoxDecoration); }

        // Container
        "container" [\x00] { return Some(ClassGroup::Container); }

        // Columns
        "columns-" any+ [\x00] { return Some(ClassGroup::Columns); }

        // Break utilities
        "break-after-" any+ [\x00] { return Some(ClassGroup::BreakAfter); }
        "break-before-" any+ [\x00] { return Some(ClassGroup::BreakBefore); }
        "break-inside-" any+ [\x00] { return Some(ClassGroup::BreakInside); }
        ("break-normal" | "break-words" | "break-all" | "break-keep") [\x00] { return Some(ClassGroup::Break); }

        // Float/Clear
        "float-" any+ [\x00] { return Some(ClassGroup::Float); }
        "clear-" any+ [\x00] { return Some(ClassGroup::Clear); }

        // Object
        ("object-contain" | "object-cover" | "object-fill" | "object-none" | "object-scale-down") [\x00] { return Some(ClassGroup::ObjectFit); }
        "object-" any+ [\x00] { return Some(ClassGroup::ObjectPosition); }

        // Overflow
        "overflow-x-" any+ [\x00] { return Some(ClassGroup::OverflowX); }
        "overflow-y-" any+ [\x00] { return Some(ClassGroup::OverflowY); }
        "overflow-" any+ [\x00] { return Some(ClassGroup::Overflow); }

        // Overscroll
        "overscroll-x-" any+ [\x00] { return Some(ClassGroup::OverscrollX); }
        "overscroll-y-" any+ [\x00] { return Some(ClassGroup::OverscrollY); }
        "overscroll-" any+ [\x00] { return Some(ClassGroup::Overscroll); }

        // Inset ring (must be before general inset)
        "inset-ring-" d+ [\x00] { return Some(ClassGroup::InsetRingW); }
        "inset-ring-[" arbitrary_length "]" [\x00] { return Some(ClassGroup::InsetRingW); }
        "inset-ring-" any+ [\x00] { return Some(ClassGroup::InsetRingColor); }

        // Inset shadow (must be before general inset)
        "inset-shadow-" shadow_size [\x00] { return Some(ClassGroup::InsetShadow); }
        "inset-shadow-" any+ [\x00] { return Some(ClassGroup::InsetShadowColor); }

        // Inset (with negative support)
        "-"? "inset-x" ("-" any+)? [\x00] { return Some(ClassGroup::InsetX); }
        "-"? "inset-y" ("-" any+)? [\x00] { return Some(ClassGroup::InsetY); }
        "-"? "inset" ("-" any+)? [\x00] { return Some(ClassGroup::Inset); }
        "-"? "top-" any+ [\x00] { return Some(ClassGroup::Top); }
        "-"? "right-" any+ [\x00] { return Some(ClassGroup::Right); }
        "-"? "bottom-" any+ [\x00] { return Some(ClassGroup::Bottom); }
        "-"? "left-" any+ [\x00] { return Some(ClassGroup::Left); }
        "-"? "start-" any+ [\x00] { return Some(ClassGroup::Start); }
        "-"? "end-" any+ [\x00] { return Some(ClassGroup::End); }

        // Z-index (with negative support)
        "-"? "z-" any+ [\x00] { return Some(ClassGroup::ZIndex); }

        // SR
        ("sr-only" | "not-sr-only") [\x00] { return Some(ClassGroup::Sr); }

        // Forced color
        ("forced-color-adjust-auto" | "forced-color-adjust-none") [\x00] { return Some(ClassGroup::ForcedColorAdjust); }

        // === FLEXBOX & GRID ===

        // Flex direction
        ("flex-row" | "flex-row-reverse" | "flex-col" | "flex-col-reverse") [\x00] { return Some(ClassGroup::FlexDirection); }

        // Flex wrap
        ("flex-wrap" | "flex-wrap-reverse" | "flex-nowrap") [\x00] { return Some(ClassGroup::FlexWrap); }

        // Flex shorthand
        ("flex-1" | "flex-auto" | "flex-initial" | "flex-none") [\x00] { return Some(ClassGroup::Flex); }
        "flex-[" [^\]\x00]+ "]" [\x00] { return Some(ClassGroup::Flex); }

        // Grow/Shrink/Basis
        "grow" ("-" any+)? [\x00] { return Some(ClassGroup::Grow); }
        "shrink" ("-" any+)? [\x00] { return Some(ClassGroup::Shrink); }
        "basis-" any+ [\x00] { return Some(ClassGroup::Basis); }

        // Order
        "-"? "order-" any+ [\x00] { return Some(ClassGroup::Order); }

        // Grid
        "grid-cols-" any+ [\x00] { return Some(ClassGroup::GridCols); }
        "grid-rows-" any+ [\x00] { return Some(ClassGroup::GridRows); }
        "grid-flow-" any+ [\x00] { return Some(ClassGroup::GridFlow); }
        "auto-cols-" any+ [\x00] { return Some(ClassGroup::AutoCols); }
        "auto-rows-" any+ [\x00] { return Some(ClassGroup::AutoRows); }
        "col-span-" any+ [\x00] { return Some(ClassGroup::ColSpan); }
        "col-start-" any+ [\x00] { return Some(ClassGroup::ColStart); }
        "col-end-" any+ [\x00] { return Some(ClassGroup::ColEnd); }
        "col-" any+ [\x00] { return Some(ClassGroup::Col); }
        "row-span-" any+ [\x00] { return Some(ClassGroup::RowSpan); }
        "row-start-" any+ [\x00] { return Some(ClassGroup::RowStart); }
        "row-end-" any+ [\x00] { return Some(ClassGroup::RowEnd); }
        "row-" any+ [\x00] { return Some(ClassGroup::Row); }

        // Gap
        "gap-x-" any+ [\x00] { return Some(ClassGroup::GapX); }
        "gap-y-" any+ [\x00] { return Some(ClassGroup::GapY); }
        "gap-" any+ [\x00] { return Some(ClassGroup::Gap); }

        // Justify
        "justify-items-" any+ [\x00] { return Some(ClassGroup::JustifyItems); }
        "justify-self-" any+ [\x00] { return Some(ClassGroup::JustifySelf); }
        "justify-" any+ [\x00] { return Some(ClassGroup::JustifyContent); }

        // Content (distinguish CSS content vs align-content)
        "content-none" [\x00] { return Some(ClassGroup::Content); }
        "content-[" [^\]\x00]+ "]" [\x00] { return Some(ClassGroup::Content); }
        "content-" any+ [\x00] { return Some(ClassGroup::AlignContent); }

        // Items/Self/Place
        "items-" any+ [\x00] { return Some(ClassGroup::AlignItems); }
        "self-" any+ [\x00] { return Some(ClassGroup::AlignSelf); }
        "place-content-" any+ [\x00] { return Some(ClassGroup::PlaceContent); }
        "place-items-" any+ [\x00] { return Some(ClassGroup::PlaceItems); }
        "place-self-" any+ [\x00] { return Some(ClassGroup::PlaceSelf); }

        // === SPACING ===

        // Margin (with negative support)
        "-"? "mx-" any+ [\x00] { return Some(ClassGroup::MarginX); }
        "-"? "my-" any+ [\x00] { return Some(ClassGroup::MarginY); }
        "-"? "ms-" any+ [\x00] { return Some(ClassGroup::MarginS); }
        "-"? "me-" any+ [\x00] { return Some(ClassGroup::MarginE); }
        "-"? "mt-" any+ [\x00] { return Some(ClassGroup::MarginT); }
        "-"? "mr-" any+ [\x00] { return Some(ClassGroup::MarginR); }
        "-"? "mb-" any+ [\x00] { return Some(ClassGroup::MarginB); }
        "-"? "ml-" any+ [\x00] { return Some(ClassGroup::MarginL); }
        "-"? "m-" any+ [\x00] { return Some(ClassGroup::Margin); }

        // Padding
        "px-" any+ [\x00] { return Some(ClassGroup::PaddingX); }
        "py-" any+ [\x00] { return Some(ClassGroup::PaddingY); }
        "ps-" any+ [\x00] { return Some(ClassGroup::PaddingS); }
        "pe-" any+ [\x00] { return Some(ClassGroup::PaddingE); }
        "pt-" any+ [\x00] { return Some(ClassGroup::PaddingT); }
        "pr-" any+ [\x00] { return Some(ClassGroup::PaddingR); }
        "pb-" any+ [\x00] { return Some(ClassGroup::PaddingB); }
        "pl-" any+ [\x00] { return Some(ClassGroup::PaddingL); }
        "p-" any+ [\x00] { return Some(ClassGroup::Padding); }

        // Space (with negative support)
        "space-x-reverse" [\x00] { return Some(ClassGroup::SpaceXReverse); }
        "space-y-reverse" [\x00] { return Some(ClassGroup::SpaceYReverse); }
        "-"? "space-x-" any+ [\x00] { return Some(ClassGroup::SpaceX); }
        "-"? "space-y-" any+ [\x00] { return Some(ClassGroup::SpaceY); }

        // === SIZING ===

        "min-w-" any+ [\x00] { return Some(ClassGroup::MinW); }
        "max-w-" any+ [\x00] { return Some(ClassGroup::MaxW); }
        "min-h-" any+ [\x00] { return Some(ClassGroup::MinH); }
        "max-h-" any+ [\x00] { return Some(ClassGroup::MaxH); }
        "size-" any+ [\x00] { return Some(ClassGroup::Size); }
        "w-" any+ [\x00] { return Some(ClassGroup::Width); }
        "h-" any+ [\x00] { return Some(ClassGroup::Height); }

        // Aspect
        "aspect-" any+ [\x00] { return Some(ClassGroup::Aspect); }

        // === TYPOGRAPHY ===

        // Font weight (must be before font-family)
        ("font-thin" | "font-extralight" | "font-light" | "font-normal" | "font-medium" | "font-semibold" | "font-bold" | "font-extrabold" | "font-black") [\x00] { return Some(ClassGroup::FontWeight); }

        // Font stretch
        "font-stretch-" any+ [\x00] { return Some(ClassGroup::FontStretch); }

        // Font family (catch-all for font-)
        "font-" any+ [\x00] { return Some(ClassGroup::FontFamily); }

        // Font style
        ("italic" | "not-italic") [\x00] { return Some(ClassGroup::FontStyle); }

        // Font smoothing
        ("antialiased" | "subpixel-antialiased") [\x00] { return Some(ClassGroup::FontSmoothing); }

        // Text alignment
        ("text-left" | "text-center" | "text-right" | "text-justify" | "text-start" | "text-end") [\x00] { return Some(ClassGroup::TextAlignment); }

        // Text wrap
        ("text-wrap" | "text-nowrap" | "text-balance" | "text-pretty") [\x00] { return Some(ClassGroup::TextWrap); }

        // Text overflow
        ("truncate" | "text-ellipsis" | "text-clip") [\x00] { return Some(ClassGroup::TextOverflow); }

        // Text shadow (before text-color) - distinguish size from color
        "text-shadow-" shadow_size [\x00] { return Some(ClassGroup::TextShadow); }
        "text-shadow-" any+ [\x00] { return Some(ClassGroup::TextShadowColor); }

        // Font size (before text-color)
        "text-" font_size [\x00] { return Some(ClassGroup::FontSize); }
        "text-[length:" [^\]\x00]+ "]" [\x00] { return Some(ClassGroup::FontSize); }
        "text-[" d* "."? d+ ("px"|"em"|"rem"|"ch"|"ex"|"vw"|"vh"|"vmin"|"vmax"|"cm"|"mm"|"in"|"pt"|"pc"|"%") "]" [\x00] { return Some(ClassGroup::FontSize); }
        "text-[color:" [^\]\x00]+ "]" [\x00] { return Some(ClassGroup::TextColor); }
        "text-(" [^)\x00]+ ")" [\x00] { return Some(ClassGroup::TextColor); }

        // Text color (catch-all)
        "text-" any+ [\x00] { return Some(ClassGroup::TextColor); }

        // Text decoration
        ("underline" | "overline" | "line-through" | "no-underline") [\x00] { return Some(ClassGroup::TextDecoration); }

        // Text decoration style
        ("decoration-solid" | "decoration-double" | "decoration-dotted" | "decoration-dashed" | "decoration-wavy") [\x00] { return Some(ClassGroup::TextDecorationStyle); }

        // Text decoration thickness (numbers)
        ("decoration-auto" | "decoration-from-font") [\x00] { return Some(ClassGroup::TextDecorationThickness); }
        "decoration-" d+ [\x00] { return Some(ClassGroup::TextDecorationThickness); }

        // Text decoration color (catch-all)
        "decoration-" any+ [\x00] { return Some(ClassGroup::TextDecorationColor); }

        // Underline offset
        "underline-offset-" any+ [\x00] { return Some(ClassGroup::UnderlineOffset); }

        // Text transform
        ("uppercase" | "lowercase" | "capitalize" | "normal-case") [\x00] { return Some(ClassGroup::TextTransform); }

        // Tracking
        "tracking-" any+ [\x00] { return Some(ClassGroup::Tracking); }

        // Leading
        "leading-" any+ [\x00] { return Some(ClassGroup::Leading); }

        // Indent
        "indent-" any+ [\x00] { return Some(ClassGroup::Indent); }

        // Vertical align
        "align-" any+ [\x00] { return Some(ClassGroup::VerticalAlign); }

        // Whitespace
        "whitespace-" any+ [\x00] { return Some(ClassGroup::Whitespace); }

        // Hyphens
        "hyphens-" any+ [\x00] { return Some(ClassGroup::Hyphens); }

        // Font variant numeric
        "normal-nums" [\x00] { return Some(ClassGroup::FvnNormal); }
        ("tabular-nums" | "proportional-nums") [\x00] { return Some(ClassGroup::FvnSpacing); }
        ("diagonal-fractions" | "stacked-fractions") [\x00] { return Some(ClassGroup::FvnFraction); }
        "ordinal" [\x00] { return Some(ClassGroup::FvnOrdinal); }
        "slashed-zero" [\x00] { return Some(ClassGroup::FvnSlashedZero); }
        ("lining-nums" | "oldstyle-nums") [\x00] { return Some(ClassGroup::FvnFigure); }

        // List
        ("list-inside" | "list-outside") [\x00] { return Some(ClassGroup::ListStylePosition); }
        ("list-disc" | "list-decimal" | "list-none") [\x00] { return Some(ClassGroup::ListStyleType); }
        "list-image-" any+ [\x00] { return Some(ClassGroup::ListImage); }
        "list-[" [^\]\x00]+ "]" [\x00] { return Some(ClassGroup::ListStyleType); }

        // Line clamp
        "line-clamp-" any+ [\x00] { return Some(ClassGroup::LineClamp); }

        // Caption
        "caption-" any+ [\x00] { return Some(ClassGroup::Caption); }

        // === BACKGROUNDS ===

        // Background position
        ("bg-center" | "bg-top" | "bg-bottom" | "bg-left" | "bg-right" | "bg-left-top" | "bg-left-bottom" | "bg-right-top" | "bg-right-bottom" | "bg-top-left" | "bg-top-right" | "bg-bottom-left" | "bg-bottom-right") [\x00] { return Some(ClassGroup::BgPosition); }

        // Background size
        ("bg-auto" | "bg-cover" | "bg-contain") [\x00] { return Some(ClassGroup::BgSize); }

        // Background repeat
        ("bg-repeat" | "bg-no-repeat" | "bg-repeat-x" | "bg-repeat-y" | "bg-repeat-round" | "bg-repeat-space") [\x00] { return Some(ClassGroup::BgRepeat); }

        // Background attachment
        ("bg-fixed" | "bg-local" | "bg-scroll") [\x00] { return Some(ClassGroup::BgAttachment); }

        // Background clip
        "bg-clip-" any+ [\x00] { return Some(ClassGroup::BgClip); }

        // Background origin
        "bg-origin-" any+ [\x00] { return Some(ClassGroup::BgOrigin); }

        // Background blend
        "bg-blend-" any+ [\x00] { return Some(ClassGroup::BgBlend); }

        // Background image (gradients)
        "bg-none" [\x00] { return Some(ClassGroup::BgImage); }
        "bg-linear-" any+ [\x00] { return Some(ClassGroup::BgImage); }
        "bg-radial" any* [\x00] { return Some(ClassGroup::BgImage); }
        "bg-conic" any* [\x00] { return Some(ClassGroup::BgImage); }
        "bg-gradient-" any+ [\x00] { return Some(ClassGroup::BgImage); }

        // Background color (catch-all)
        "bg-" any+ [\x00] { return Some(ClassGroup::BgColor); }

        // Gradients
        "from-" d+ "%" [\x00] { return Some(ClassGroup::GradientFromPos); }
        "from-" any+ [\x00] { return Some(ClassGroup::GradientFrom); }
        "via-" d+ "%" [\x00] { return Some(ClassGroup::GradientViaPos); }
        "via-" any+ [\x00] { return Some(ClassGroup::GradientVia); }
        "to-" d+ "%" [\x00] { return Some(ClassGroup::GradientToPos); }
        "to-" any+ [\x00] { return Some(ClassGroup::GradientTo); }

        // === BORDERS ===

        // Border width (standalone directions)
        "border-t" [\x00] { return Some(ClassGroup::BorderWT); }
        "border-r" [\x00] { return Some(ClassGroup::BorderWR); }
        "border-b" [\x00] { return Some(ClassGroup::BorderWB); }
        "border-l" [\x00] { return Some(ClassGroup::BorderWL); }
        "border-s" [\x00] { return Some(ClassGroup::BorderWS); }
        "border-e" [\x00] { return Some(ClassGroup::BorderWE); }
        "border-x" [\x00] { return Some(ClassGroup::BorderWX); }
        "border-y" [\x00] { return Some(ClassGroup::BorderWY); }

        // Border width (with size)
        "border-t-" d+ [\x00] { return Some(ClassGroup::BorderWT); }
        "border-r-" d+ [\x00] { return Some(ClassGroup::BorderWR); }
        "border-b-" d+ [\x00] { return Some(ClassGroup::BorderWB); }
        "border-l-" d+ [\x00] { return Some(ClassGroup::BorderWL); }
        "border-s-" d+ [\x00] { return Some(ClassGroup::BorderWS); }
        "border-e-" d+ [\x00] { return Some(ClassGroup::BorderWE); }
        "border-x-" d+ [\x00] { return Some(ClassGroup::BorderWX); }
        "border-y-" d+ [\x00] { return Some(ClassGroup::BorderWY); }
        "border-" d+ [\x00] { return Some(ClassGroup::BorderW); }
        "border" [\x00] { return Some(ClassGroup::BorderW); }

        // Border style
        ("border-solid" | "border-dashed" | "border-dotted" | "border-double" | "border-hidden" | "border-none") [\x00] { return Some(ClassGroup::BorderStyle); }

        // Border collapse
        ("border-collapse" | "border-separate") [\x00] { return Some(ClassGroup::BorderCollapse); }

        // Border spacing
        "border-spacing-x-" any+ [\x00] { return Some(ClassGroup::BorderSpacingX); }
        "border-spacing-y-" any+ [\x00] { return Some(ClassGroup::BorderSpacingY); }
        "border-spacing-" any+ [\x00] { return Some(ClassGroup::BorderSpacing); }

        // Border color (catch-all, after width checks)
        "border-t-" any+ [\x00] { return Some(ClassGroup::BorderColorT); }
        "border-r-" any+ [\x00] { return Some(ClassGroup::BorderColorR); }
        "border-b-" any+ [\x00] { return Some(ClassGroup::BorderColorB); }
        "border-l-" any+ [\x00] { return Some(ClassGroup::BorderColorL); }
        "border-s-" any+ [\x00] { return Some(ClassGroup::BorderColorS); }
        "border-e-" any+ [\x00] { return Some(ClassGroup::BorderColorE); }
        "border-x-" any+ [\x00] { return Some(ClassGroup::BorderColorX); }
        "border-y-" any+ [\x00] { return Some(ClassGroup::BorderColorY); }
        "border-" any+ [\x00] { return Some(ClassGroup::BorderColor); }

        // Divide
        "divide-x-reverse" [\x00] { return Some(ClassGroup::DivideXReverse); }
        "divide-y-reverse" [\x00] { return Some(ClassGroup::DivideYReverse); }
        "divide-x" ("-" d+)? [\x00] { return Some(ClassGroup::DivideX); }
        "divide-y" ("-" d+)? [\x00] { return Some(ClassGroup::DivideY); }
        ("divide-solid" | "divide-dashed" | "divide-dotted" | "divide-double" | "divide-none" | "divide-hidden") [\x00] { return Some(ClassGroup::DivideStyle); }
        "divide-" any+ [\x00] { return Some(ClassGroup::DivideColor); }

        // Rounded
        "rounded-ss" ("-" any+)? [\x00] { return Some(ClassGroup::RoundedSs); }
        "rounded-se" ("-" any+)? [\x00] { return Some(ClassGroup::RoundedSe); }
        "rounded-ee" ("-" any+)? [\x00] { return Some(ClassGroup::RoundedEe); }
        "rounded-es" ("-" any+)? [\x00] { return Some(ClassGroup::RoundedEs); }
        "rounded-tl" ("-" any+)? [\x00] { return Some(ClassGroup::RoundedTl); }
        "rounded-tr" ("-" any+)? [\x00] { return Some(ClassGroup::RoundedTr); }
        "rounded-br" ("-" any+)? [\x00] { return Some(ClassGroup::RoundedBr); }
        "rounded-bl" ("-" any+)? [\x00] { return Some(ClassGroup::RoundedBl); }
        "rounded-s" ("-" any+)? [\x00] { return Some(ClassGroup::RoundedS); }
        "rounded-e" ("-" any+)? [\x00] { return Some(ClassGroup::RoundedE); }
        "rounded-t" ("-" any+)? [\x00] { return Some(ClassGroup::RoundedT); }
        "rounded-r" ("-" any+)? [\x00] { return Some(ClassGroup::RoundedR); }
        "rounded-b" ("-" any+)? [\x00] { return Some(ClassGroup::RoundedB); }
        "rounded-l" ("-" any+)? [\x00] { return Some(ClassGroup::RoundedL); }
        "rounded" ("-" any+)? [\x00] { return Some(ClassGroup::Rounded); }

        // === EFFECTS ===

        // Ring
        "ring-offset-" d+ [\x00] { return Some(ClassGroup::RingOffsetW); }
        "ring-offset-[" arbitrary_length "]" [\x00] { return Some(ClassGroup::RingOffsetW); }
        "ring-offset-" any+ [\x00] { return Some(ClassGroup::RingOffsetColor); }
        "ring-inset" [\x00] { return Some(ClassGroup::RingWInset); }
        "ring-" d+ [\x00] { return Some(ClassGroup::RingW); }
        "ring-[" arbitrary_length "]" [\x00] { return Some(ClassGroup::RingW); }
        "ring" [\x00] { return Some(ClassGroup::RingW); }
        "ring-" any+ [\x00] { return Some(ClassGroup::RingColor); }

        // Shadow
        "shadow-" shadow_size [\x00] { return Some(ClassGroup::Shadow); }
        "shadow" [\x00] { return Some(ClassGroup::Shadow); }
        "shadow-" any+ [\x00] { return Some(ClassGroup::ShadowColor); }

        // Outline
        "outline-offset-" any+ [\x00] { return Some(ClassGroup::OutlineOffset); }
        ("outline-solid" | "outline-dashed" | "outline-dotted" | "outline-double" | "outline-none" | "outline-hidden") [\x00] { return Some(ClassGroup::OutlineStyle); }
        "outline-" d+ [\x00] { return Some(ClassGroup::OutlineW); }
        "outline-[" arbitrary_length "]" [\x00] { return Some(ClassGroup::OutlineW); }
        "outline" [\x00] { return Some(ClassGroup::OutlineW); }
        "outline-" any+ [\x00] { return Some(ClassGroup::OutlineColor); }

        // Opacity
        "opacity-" any+ [\x00] { return Some(ClassGroup::Opacity); }

        // Mix blend
        "mix-blend-" any+ [\x00] { return Some(ClassGroup::MixBlend); }

        // === FILTERS ===

        ("filter" | "filter-none") [\x00] { return Some(ClassGroup::Filter); }
        "blur" ("-" any+)? [\x00] { return Some(ClassGroup::Blur); }
        "brightness-" any+ [\x00] { return Some(ClassGroup::Brightness); }
        "contrast-" any+ [\x00] { return Some(ClassGroup::Contrast); }
        "grayscale" ("-" any+)? [\x00] { return Some(ClassGroup::Grayscale); }
        "hue-rotate-" any+ [\x00] { return Some(ClassGroup::HueRotate); }
        "invert" ("-" any+)? [\x00] { return Some(ClassGroup::Invert); }
        "saturate-" any+ [\x00] { return Some(ClassGroup::Saturate); }
        "sepia" ("-" any+)? [\x00] { return Some(ClassGroup::Sepia); }
        "drop-shadow-" shadow_size [\x00] { return Some(ClassGroup::DropShadow); }
        "drop-shadow-[shadow:" [^\]\x00]+ "]" [\x00] { return Some(ClassGroup::DropShadow); }
        "drop-shadow-" any+ [\x00] { return Some(ClassGroup::DropShadowColor); }

        // Backdrop filters
        ("backdrop-filter" | "backdrop-filter-none") [\x00] { return Some(ClassGroup::BackdropFilter); }
        "backdrop-blur" ("-" any+)? [\x00] { return Some(ClassGroup::BackdropBlur); }
        "backdrop-brightness-" any+ [\x00] { return Some(ClassGroup::BackdropBrightness); }
        "backdrop-contrast-" any+ [\x00] { return Some(ClassGroup::BackdropContrast); }
        "backdrop-grayscale" ("-" any+)? [\x00] { return Some(ClassGroup::BackdropGrayscale); }
        "backdrop-hue-rotate-" any+ [\x00] { return Some(ClassGroup::BackdropHueRotate); }
        "backdrop-invert" ("-" any+)? [\x00] { return Some(ClassGroup::BackdropInvert); }
        "backdrop-opacity-" any+ [\x00] { return Some(ClassGroup::BackdropOpacity); }
        "backdrop-saturate-" any+ [\x00] { return Some(ClassGroup::BackdropSaturate); }
        "backdrop-sepia" ("-" any+)? [\x00] { return Some(ClassGroup::BackdropSepia); }

        // === TRANSFORMS (with negative support) ===

        "-"? "scale-x-" any+ [\x00] { return Some(ClassGroup::ScaleX); }
        "-"? "scale-y-" any+ [\x00] { return Some(ClassGroup::ScaleY); }
        "-"? "scale-z-" any+ [\x00] { return Some(ClassGroup::ScaleZ); }
        "scale-3d" [\x00] { return Some(ClassGroup::Scale3d); }
        "-"? "scale-" any+ [\x00] { return Some(ClassGroup::Scale); }

        "-"? "rotate-x-" any+ [\x00] { return Some(ClassGroup::RotateX); }
        "-"? "rotate-y-" any+ [\x00] { return Some(ClassGroup::RotateY); }
        "-"? "rotate-z-" any+ [\x00] { return Some(ClassGroup::RotateZ); }
        "-"? "rotate" ("-" any+)? [\x00] { return Some(ClassGroup::Rotate); }

        "translate-none" [\x00] { return Some(ClassGroup::TranslateNone); }
        "-"? "translate-x-" any+ [\x00] { return Some(ClassGroup::TranslateX); }
        "-"? "translate-y-" any+ [\x00] { return Some(ClassGroup::TranslateY); }
        "-"? "translate-z-" any+ [\x00] { return Some(ClassGroup::TranslateZ); }
        "-"? "translate-" any+ [\x00] { return Some(ClassGroup::Translate); }

        "-"? "skew-x-" any+ [\x00] { return Some(ClassGroup::SkewX); }
        "-"? "skew-y-" any+ [\x00] { return Some(ClassGroup::SkewY); }
        "-"? "skew-" any+ [\x00] { return Some(ClassGroup::Skew); }

        "origin-" any+ [\x00] { return Some(ClassGroup::TransformOrigin); }
        "perspective-origin-" any+ [\x00] { return Some(ClassGroup::PerspectiveOrigin); }
        "perspective-" any+ [\x00] { return Some(ClassGroup::Perspective); }

        ("transform-3d" | "transform-flat") [\x00] { return Some(ClassGroup::TransformStyle); }
        "transform-" any+ [\x00] { return Some(ClassGroup::Transform); }

        "backface-" any+ [\x00] { return Some(ClassGroup::Backface); }

        // === TRANSITIONS & ANIMATION ===

        ("transition-normal" | "transition-discrete") [\x00] { return Some(ClassGroup::TransitionBehavior); }
        ("transition-all" | "transition-colors" | "transition-opacity" | "transition-shadow" | "transition-transform" | "transition-none") [\x00] { return Some(ClassGroup::Transition); }
        "transition" [\x00] { return Some(ClassGroup::Transition); }
        "duration-" any+ [\x00] { return Some(ClassGroup::Duration); }
        "delay-" any+ [\x00] { return Some(ClassGroup::Delay); }
        "ease-" any+ [\x00] { return Some(ClassGroup::Ease); }
        "animate-" any+ [\x00] { return Some(ClassGroup::Animate); }

        // === INTERACTIVITY ===

        "cursor-" any+ [\x00] { return Some(ClassGroup::Cursor); }
        "caret-" any+ [\x00] { return Some(ClassGroup::CaretColor); }
        "accent-" any+ [\x00] { return Some(ClassGroup::Accent); }
        ("pointer-events-auto" | "pointer-events-none") [\x00] { return Some(ClassGroup::PointerEvents); }
        ("resize" | "resize-none" | "resize-x" | "resize-y") [\x00] { return Some(ClassGroup::Resize); }
        ("scroll-auto" | "scroll-smooth") [\x00] { return Some(ClassGroup::ScrollBehavior); }

        // Scroll margin (with negative support)
        "-"? "scroll-mx-" any+ [\x00] { return Some(ClassGroup::ScrollMx); }
        "-"? "scroll-my-" any+ [\x00] { return Some(ClassGroup::ScrollMy); }
        "-"? "scroll-ms-" any+ [\x00] { return Some(ClassGroup::ScrollMs); }
        "-"? "scroll-me-" any+ [\x00] { return Some(ClassGroup::ScrollMe); }
        "-"? "scroll-mt-" any+ [\x00] { return Some(ClassGroup::ScrollMt); }
        "-"? "scroll-mr-" any+ [\x00] { return Some(ClassGroup::ScrollMr); }
        "-"? "scroll-mb-" any+ [\x00] { return Some(ClassGroup::ScrollMb); }
        "-"? "scroll-ml-" any+ [\x00] { return Some(ClassGroup::ScrollMl); }
        "-"? "scroll-m-" any+ [\x00] { return Some(ClassGroup::ScrollM); }

        // Scroll padding
        "scroll-px-" any+ [\x00] { return Some(ClassGroup::ScrollPx); }
        "scroll-py-" any+ [\x00] { return Some(ClassGroup::ScrollPy); }
        "scroll-ps-" any+ [\x00] { return Some(ClassGroup::ScrollPs); }
        "scroll-pe-" any+ [\x00] { return Some(ClassGroup::ScrollPe); }
        "scroll-pt-" any+ [\x00] { return Some(ClassGroup::ScrollPt); }
        "scroll-pr-" any+ [\x00] { return Some(ClassGroup::ScrollPr); }
        "scroll-pb-" any+ [\x00] { return Some(ClassGroup::ScrollPb); }
        "scroll-pl-" any+ [\x00] { return Some(ClassGroup::ScrollPl); }
        "scroll-p-" any+ [\x00] { return Some(ClassGroup::ScrollP); }

        // Snap
        ("snap-start" | "snap-end" | "snap-center" | "snap-align-none") [\x00] { return Some(ClassGroup::SnapAlign); }
        ("snap-normal" | "snap-always") [\x00] { return Some(ClassGroup::SnapStop); }
        ("snap-none" | "snap-x" | "snap-y" | "snap-both") [\x00] { return Some(ClassGroup::SnapType); }
        ("snap-mandatory" | "snap-proximity") [\x00] { return Some(ClassGroup::SnapStrictness); }

        // Touch
        ("touch-pan-x" | "touch-pan-left" | "touch-pan-right") [\x00] { return Some(ClassGroup::TouchX); }
        ("touch-pan-y" | "touch-pan-up" | "touch-pan-down") [\x00] { return Some(ClassGroup::TouchY); }
        "touch-pinch-zoom" [\x00] { return Some(ClassGroup::TouchPz); }
        ("touch-auto" | "touch-none" | "touch-manipulation") [\x00] { return Some(ClassGroup::Touch); }

        "select-" any+ [\x00] { return Some(ClassGroup::Select); }
        "will-change-" any+ [\x00] { return Some(ClassGroup::WillChange); }

        // === SVG ===

        "fill-" any+ [\x00] { return Some(ClassGroup::Fill); }
        "stroke-" d+ [\x00] { return Some(ClassGroup::StrokeW); }
        "stroke-[" d+ "]" [\x00] { return Some(ClassGroup::StrokeW); }
        "stroke-" any+ [\x00] { return Some(ClassGroup::Stroke); }

        // === TABLES ===

        ("table-auto" | "table-fixed") [\x00] { return Some(ClassGroup::TableLayout); }

        // === MASK UTILITIES ===

        "mask-type-" any+ [\x00] { return Some(ClassGroup::MaskType); }
        ("mask-clip-" any+ | "mask-no-clip") [\x00] { return Some(ClassGroup::MaskClip); }
        "mask-origin-" any+ [\x00] { return Some(ClassGroup::MaskOrigin); }
        ("mask-alpha" | "mask-luminance" | "mask-match") [\x00] { return Some(ClassGroup::MaskMode); }
        ("mask-add" | "mask-subtract" | "mask-intersect" | "mask-exclude") [\x00] { return Some(ClassGroup::MaskComposite); }
        ("mask-repeat" | "mask-no-repeat" | "mask-repeat-x" | "mask-repeat-y" | "mask-repeat-round" | "mask-repeat-space") [\x00] { return Some(ClassGroup::MaskRepeat); }
        ("mask-auto" | "mask-cover" | "mask-contain") [\x00] { return Some(ClassGroup::MaskSize); }
        "mask-size-" any+ [\x00] { return Some(ClassGroup::MaskSize); }
        ("mask-center" | "mask-top" | "mask-bottom" | "mask-left" | "mask-right") [\x00] { return Some(ClassGroup::MaskPosition); }
        "mask-position-" any+ [\x00] { return Some(ClassGroup::MaskPosition); }
        "mask-linear-from-" any+ [\x00] { return Some(ClassGroup::MaskImage); }
        "mask-linear-to-" any+ [\x00] { return Some(ClassGroup::MaskImage); }
        "mask-linear-" any+ [\x00] { return Some(ClassGroup::MaskImage); }
        "mask-radial-from-" any+ [\x00] { return Some(ClassGroup::MaskImage); }
        "mask-radial-to-" any+ [\x00] { return Some(ClassGroup::MaskImage); }
        "mask-radial-" any+ [\x00] { return Some(ClassGroup::MaskImage); }
        "mask-conic-from-" any+ [\x00] { return Some(ClassGroup::MaskImage); }
        "mask-conic-to-" any+ [\x00] { return Some(ClassGroup::MaskImage); }
        "mask-conic-" any+ [\x00] { return Some(ClassGroup::MaskImage); }
        ("mask-t-" | "mask-b-" | "mask-l-" | "mask-r-" | "mask-x-" | "mask-y-") any+ [\x00] { return Some(ClassGroup::MaskImage); }
        "mask-none" [\x00] { return Some(ClassGroup::MaskImage); }
        "mask-" any+ [\x00] { return Some(ClassGroup::MaskImage); }

        // === OTHER ===

        "appearance-" any+ [\x00] { return Some(ClassGroup::Appearance); }
        "scheme-" any+ [\x00] { return Some(ClassGroup::ColorScheme); }
        "placeholder-" any+ [\x00] { return Some(ClassGroup::PlaceholderColor); }
        "field-sizing-" any+ [\x00] { return Some(ClassGroup::FieldSizing); }
        "wrap-" any+ [\x00] { return Some(ClassGroup::Wrap); }

        // Arbitrary properties [property:value]
        "[" [^\]\x00]+ ":" [^\]\x00]+ "]" [\x00] { return Some(ClassGroup::Arbitrary); }

        // No match
        * { return None; }
    */
}
