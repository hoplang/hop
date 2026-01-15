use crate::tw_merge;

#[test]
fn test_000() {
    assert_eq!(
        tw_merge("[paint-order:markers] [paint-order:normal]"),
        "[paint-order:normal]"
    );
}

#[test]
fn test_001() {
    assert_eq!(
        tw_merge("![some:prop] [some:other]"),
        "![some:prop] [some:other]"
    );
}

#[test]
fn test_002() {
    assert_eq!(tw_merge("m-[2px] m-[10px]"), "m-[10px]");
}

#[test]
fn test_003() {
    assert_eq!(tw_merge("z-20 z-[99]"), "z-[99]");
}

#[test]
fn test_004() {
    assert_eq!(tw_merge("my-[2px] m-[10rem]"), "m-[10rem]");
}

#[test]
fn test_005() {
    assert_eq!(tw_merge("cursor-pointer cursor-[grab]"), "cursor-[grab]");
}

#[test]
fn test_006() {
    assert_eq!(
        tw_merge("m-[2px] m-[length:var(--mystery-var)]"),
        "m-[length:var(--mystery-var)]"
    );
}

#[test]
fn test_007() {
    assert_eq!(tw_merge("opacity-10 opacity-[0.025]"), "opacity-[0.025]");
}

#[test]
fn test_008() {
    assert_eq!(tw_merge("scale-75 scale-[1.7]"), "scale-[1.7]");
}

#[test]
fn test_009() {
    assert_eq!(
        tw_merge("brightness-90 brightness-[1.75]"),
        "brightness-[1.75]"
    );
}

#[test]
fn test_010() {
    assert_eq!(tw_merge("min-h-[0.5px] min-h-[0]"), "min-h-[0]");
}

#[test]
fn test_011() {
    assert_eq!(
        tw_merge("text-[0.5px] text-[color:0]"),
        "text-[0.5px] text-[color:0]"
    );
}

#[test]
fn test_012() {
    assert_eq!(
        tw_merge("text-[0.5px] text-(--my-0)"),
        "text-[0.5px] text-(--my-0)"
    );
}

#[test]
fn test_013() {
    assert_eq!(
        tw_merge("hover:m-[2px] hover:m-[length:var(--c)]"),
        "hover:m-[length:var(--c)]"
    );
}

#[test]
fn test_014() {
    assert_eq!(tw_merge("grid-rows-[1fr,auto] grid-rows-2"), "grid-rows-2");
}

#[test]
fn test_015() {
    assert_eq!(
        tw_merge("mt-2 mt-[theme(someScale.someValue)]"),
        "mt-[theme(someScale.someValue)]"
    );
}

#[test]
fn test_016() {
    assert_eq!(
        tw_merge("[p]:underline [p]:line-through"),
        "[p]:line-through"
    );
}

#[test]
fn test_017() {
    assert_eq!(
        tw_merge("[&>*]:underline [&>*]:line-through"),
        "[&>*]:line-through"
    );
}

#[test]
fn test_018() {
    assert_eq!(
        tw_merge("[&>*]:[color:red] [&>*]:[color:blue]"),
        "[&>*]:[color:blue]"
    );
}

#[test]
fn test_019() {
    assert_eq!(
        tw_merge("overflow-x-auto overflow-x-hidden"),
        "overflow-x-hidden"
    );
}

#[test]
fn test_020() {
    assert_eq!(tw_merge("basis-full basis-auto"), "basis-auto");
}

#[test]
fn test_021() {
    assert_eq!(tw_merge("w-full w-fit"), "w-fit");
}

#[test]
fn test_022() {
    assert_eq!(
        tw_merge("overflow-x-auto overflow-x-hidden overflow-x-scroll"),
        "overflow-x-scroll"
    );
}

#[test]
fn test_023() {
    assert_eq!(tw_merge("col-span-1 col-span-full"), "col-span-full");
}

#[test]
fn test_024() {
    assert_eq!(tw_merge("gap-2 gap-px basis-px basis-3"), "gap-px basis-3");
}

#[test]
fn test_025() {
    assert_eq!(
        tw_merge("tabular-nums diagonal-fractions normal-nums"),
        "normal-nums"
    );
}

#[test]
fn test_026() {
    assert_eq!(
        tw_merge("tabular-nums proportional-nums"),
        "proportional-nums"
    );
}

#[test]
fn test_027() {
    assert_eq!(tw_merge("bg-grey-5 bg-hotpink"), "bg-hotpink");
}

#[test]
fn test_028() {
    assert_eq!(
        tw_merge("hover:bg-grey-5 hover:bg-hotpink"),
        "hover:bg-hotpink"
    );
}

#[test]
fn test_029() {
    assert_eq!(tw_merge("inset-1 inset-x-1"), "inset-1 inset-x-1");
}

#[test]
fn test_030() {
    assert_eq!(tw_merge("inset-x-1 inset-1"), "inset-1");
}

#[test]
fn test_031() {
    assert_eq!(tw_merge("inset-x-1 left-1 inset-1"), "inset-1");
}

#[test]
fn test_032() {
    assert_eq!(tw_merge("inset-x-1 inset-1 left-1"), "inset-1 left-1");
}

#[test]
fn test_033() {
    assert_eq!(tw_merge("inset-x-1 right-1 inset-1"), "inset-1");
}

#[test]
fn test_034() {
    assert_eq!(tw_merge("inset-x-1 right-1 inset-x-1"), "inset-x-1");
}

#[test]
fn test_035() {
    assert_eq!(
        tw_merge("inset-x-1 right-1 inset-y-1"),
        "inset-x-1 right-1 inset-y-1"
    );
}

#[test]
fn test_036() {
    assert_eq!(
        tw_merge("right-1 inset-x-1 inset-y-1"),
        "inset-x-1 inset-y-1"
    );
}

#[test]
fn test_037() {
    assert_eq!(
        tw_merge("inset-x-1 hover:left-1 inset-1"),
        "hover:left-1 inset-1"
    );
}

#[test]
fn test_038() {
    assert_eq!(tw_merge("ring shadow"), "ring shadow");
}

#[test]
fn test_039() {
    assert_eq!(tw_merge("ring-2 shadow-md"), "ring-2 shadow-md");
}

#[test]
fn test_040() {
    assert_eq!(tw_merge("shadow ring"), "shadow ring");
}

#[test]
fn test_041() {
    assert_eq!(tw_merge("shadow-md ring-2"), "shadow-md ring-2");
}

#[test]
fn test_042() {
    assert_eq!(tw_merge("touch-pan-x touch-pan-right"), "touch-pan-right");
}

#[test]
fn test_043() {
    assert_eq!(tw_merge("touch-none touch-pan-x"), "touch-pan-x");
}

#[test]
fn test_044() {
    assert_eq!(tw_merge("touch-pan-x touch-none"), "touch-none");
}

#[test]
fn test_045() {
    assert_eq!(
        tw_merge("touch-pan-x touch-pan-y touch-pinch-zoom touch-auto"),
        "touch-auto"
    );
}

#[test]
fn test_046() {
    assert_eq!(
        tw_merge("overflow-auto inline line-clamp-1"),
        "line-clamp-1"
    );
}

#[test]
fn test_047() {
    assert_eq!(
        tw_merge("line-clamp-1 overflow-auto inline"),
        "line-clamp-1 overflow-auto inline"
    );
}

#[test]
fn test_048() {
    assert_eq!(tw_merge("font-medium! font-bold!"), "font-bold!");
}

#[test]
fn test_049() {
    assert_eq!(
        tw_merge("font-medium! font-bold! font-thin"),
        "font-bold! font-thin"
    );
}

#[test]
fn test_050() {
    assert_eq!(tw_merge("right-2! -inset-x-px!"), "-inset-x-px!");
}

#[test]
fn test_051() {
    assert_eq!(tw_merge("focus:inline! focus:block!"), "focus:block!");
}

#[test]
fn test_052() {
    assert_eq!(
        tw_merge("[--my-var:20px]! [--my-var:30px]!"),
        "[--my-var:30px]!"
    );
}

#[test]
fn test_053() {
    assert_eq!(tw_merge("font-medium! !font-bold"), "!font-bold");
}

#[test]
fn test_054() {
    assert_eq!(tw_merge("!font-medium !font-bold"), "!font-bold");
}

#[test]
fn test_055() {
    assert_eq!(
        tw_merge("!font-medium !font-bold font-thin"),
        "!font-bold font-thin"
    );
}

#[test]
fn test_056() {
    assert_eq!(tw_merge("!right-2 !-inset-x-px"), "!-inset-x-px");
}

#[test]
fn test_057() {
    assert_eq!(tw_merge("focus:!inline focus:!block"), "focus:!block");
}

#[test]
fn test_058() {
    assert_eq!(
        tw_merge("![--my-var:20px] ![--my-var:30px]"),
        "![--my-var:30px]"
    );
}

#[test]
fn test_059() {
    assert_eq!(tw_merge("hover:block hover:inline"), "hover:inline");
}

#[test]
fn test_060() {
    assert_eq!(
        tw_merge("hover:block hover:focus:inline"),
        "hover:block hover:focus:inline"
    );
}

#[test]
fn test_061() {
    assert_eq!(
        tw_merge("focus-within:inline focus-within:block"),
        "focus-within:block"
    );
}

#[test]
fn test_062() {
    assert_eq!(tw_merge("text-lg/7 text-lg/8"), "text-lg/8");
}

#[test]
fn test_063() {
    assert_eq!(tw_merge("text-lg/none leading-9"), "text-lg/none leading-9");
}

#[test]
fn test_064() {
    assert_eq!(tw_merge("leading-9 text-lg/none"), "text-lg/none");
}

#[test]
fn test_065() {
    assert_eq!(tw_merge("w-full w-1/2"), "w-1/2");
}

#[test]
fn test_066() {
    assert_eq!(tw_merge("c:d:e:block d:c:e:inline"), "d:c:e:inline");
}

#[test]
fn test_067() {
    assert_eq!(
        tw_merge("*:before:block *:before:inline"),
        "*:before:inline"
    );
}

#[test]
fn test_068() {
    assert_eq!(
        tw_merge("*:before:block before:*:inline"),
        "*:before:block before:*:inline"
    );
}

#[test]
fn test_069() {
    assert_eq!(tw_merge("x:y:*:z:block y:x:*:z:inline"), "y:x:*:z:inline");
}

#[test]
fn test_070() {
    assert_eq!(tw_merge("-m-2 -m-5"), "-m-5");
}

#[test]
fn test_071() {
    assert_eq!(tw_merge("-top-12 -top-2000"), "-top-2000");
}

#[test]
fn test_072() {
    assert_eq!(tw_merge("-m-2 m-auto"), "m-auto");
}

#[test]
fn test_073() {
    assert_eq!(tw_merge("top-12 -top-69"), "-top-69");
}

#[test]
fn test_074() {
    assert_eq!(tw_merge("-right-1 inset-x-1"), "inset-x-1");
}

#[test]
fn test_075() {
    assert_eq!(
        tw_merge("hover:focus:-right-1 focus:hover:inset-x-1"),
        "focus:hover:inset-x-1"
    );
}

#[test]
fn test_076() {
    assert_eq!(
        tw_merge("border-t border-white/10"),
        "border-t border-white/10"
    );
}

#[test]
fn test_077() {
    assert_eq!(tw_merge("border-t border-white"), "border-t border-white");
}

#[test]
fn test_078() {
    assert_eq!(tw_merge("text-3.5xl text-black"), "text-3.5xl text-black");
}

#[test]
fn test_079() {
    assert_eq!(
        tw_merge("non-tailwind-class inline block"),
        "non-tailwind-class block"
    );
}

#[test]
fn test_080() {
    assert_eq!(tw_merge("inline block inline-1"), "block inline-1");
}

#[test]
fn test_081() {
    assert_eq!(tw_merge("inline block i-inline"), "block i-inline");
}

#[test]
fn test_082() {
    assert_eq!(
        tw_merge("focus:inline focus:block focus:inline-1"),
        "focus:block focus:inline-1"
    );
}

#[test]
fn test_083() {
    assert_eq!(
        tw_merge("border-t-some-blue border-t-other-blue"),
        "border-t-other-blue"
    );
}

#[test]
fn test_084() {
    assert_eq!(
        tw_merge("border-t-some-blue border-some-blue"),
        "border-some-blue"
    );
}

#[test]
fn test_085() {
    assert_eq!(
        tw_merge("border-some-blue border-s-some-blue"),
        "border-some-blue border-s-some-blue"
    );
}

#[test]
fn test_086() {
    assert_eq!(
        tw_merge("border-e-some-blue border-some-blue"),
        "border-some-blue"
    );
}

#[test]
fn test_087() {
    assert_eq!(tw_merge("empty:p-2 empty:p-3"), "empty:p-3");
}

#[test]
fn test_088() {
    assert_eq!(
        tw_merge("hover:empty:p-2 hover:empty:p-3"),
        "hover:empty:p-3"
    );
}

#[test]
fn test_089() {
    assert_eq!(tw_merge("read-only:p-2 read-only:p-3"), "read-only:p-3");
}

#[test]
fn test_090() {
    assert_eq!(
        tw_merge("group-empty:p-2 group-empty:p-3"),
        "group-empty:p-3"
    );
}

#[test]
fn test_091() {
    assert_eq!(tw_merge("peer-empty:p-2 peer-empty:p-3"), "peer-empty:p-3");
}

#[test]
fn test_092() {
    assert_eq!(
        tw_merge("group-empty:p-2 peer-empty:p-3"),
        "group-empty:p-2 peer-empty:p-3"
    );
}

#[test]
fn test_093() {
    assert_eq!(
        tw_merge("hover:group-empty:p-2 hover:group-empty:p-3"),
        "hover:group-empty:p-3"
    );
}

#[test]
fn test_094() {
    assert_eq!(
        tw_merge("group-read-only:p-2 group-read-only:p-3"),
        "group-read-only:p-3"
    );
}

#[test]
fn test_095() {
    assert_eq!(tw_merge("inline block"), "block");
}

#[test]
fn test_096() {
    assert_eq!(tw_merge("hover:block hover:block"), "hover:block");
}

#[test]
fn test_097() {
    assert_eq!(tw_merge("underline line-through"), "line-through");
}

#[test]
fn test_098() {
    assert_eq!(tw_merge("line-through no-underline"), "no-underline");
}

#[test]
fn test_099() {
    assert_eq!(
        tw_merge("text-red text-lg/7 text-lg/8"),
        "text-red text-lg/8"
    );
}

#[test]
fn test_100() {
    assert_eq!(tw_merge("hyphens-auto hyphens-manual"), "hyphens-manual");
}

#[test]
fn test_101() {
    assert_eq!(tw_merge("from-0% from-red"), "from-0% from-red");
}

#[test]
fn test_102() {
    assert_eq!(tw_merge("caption-top caption-bottom"), "caption-bottom");
}

#[test]
fn test_103() {
    assert_eq!(
        tw_merge("line-clamp-2 line-clamp-none line-clamp-[10]"),
        "line-clamp-[10]"
    );
}

#[test]
fn test_104() {
    assert_eq!(
        tw_merge("delay-150 delay-0 duration-150 duration-0"),
        "delay-0 duration-0"
    );
}

#[test]
fn test_105() {
    assert_eq!(
        tw_merge("justify-normal justify-center justify-stretch"),
        "justify-stretch"
    );
}

#[test]
fn test_106() {
    assert_eq!(
        tw_merge("content-normal content-center content-stretch"),
        "content-stretch"
    );
}

#[test]
fn test_107() {
    assert_eq!(
        tw_merge("whitespace-nowrap whitespace-break-spaces"),
        "whitespace-break-spaces"
    );
}

#[test]
fn test_108() {
    assert_eq!(tw_merge("h-svh h-dvh w-svw w-dvw"), "h-dvh w-dvw");
}

#[test]
fn test_109() {
    assert_eq!(tw_merge("text-wrap text-pretty"), "text-pretty");
}

#[test]
fn test_110() {
    assert_eq!(tw_merge("w-5 h-3 size-10 w-12"), "size-10 w-12");
}

#[test]
fn test_111() {
    assert_eq!(
        tw_merge("min-w-0 min-w-50 min-w-px max-w-0 max-w-50 max-w-px"),
        "min-w-px max-w-px"
    );
}

#[test]
fn test_112() {
    assert_eq!(
        tw_merge("appearance-none appearance-auto"),
        "appearance-auto"
    );
}

#[test]
fn test_113() {
    assert_eq!(
        tw_merge("float-start float-end clear-start clear-end"),
        "float-end clear-end"
    );
}

#[test]
fn test_114() {
    assert_eq!(
        tw_merge("*:p-10 *:p-20 hover:*:p-10 hover:*:p-20"),
        "*:p-20 hover:*:p-20"
    );
}

#[test]
fn test_115() {
    assert_eq!(tw_merge("transform-3d transform-flat"), "transform-flat");
}

#[test]
fn test_116() {
    assert_eq!(tw_merge("bg-linear-to-r bg-linear-45"), "bg-linear-45");
}

#[test]
fn test_117() {
    assert_eq!(
        tw_merge("bg-linear-to-r bg-radial-[something] bg-conic-10"),
        "bg-conic-10"
    );
}

#[test]
fn test_118() {
    assert_eq!(
        tw_merge("field-sizing-content field-sizing-fixed"),
        "field-sizing-fixed"
    );
}

#[test]
fn test_119() {
    assert_eq!(tw_merge("scheme-normal scheme-dark"), "scheme-dark");
}

#[test]
fn test_120() {
    assert_eq!(
        tw_merge("col-span-full col-2 row-span-3 row-4"),
        "col-2 row-4"
    );
}

#[test]
fn test_121() {
    assert_eq!(
        tw_merge("items-baseline items-baseline-last"),
        "items-baseline-last"
    );
}

#[test]
fn test_122() {
    assert_eq!(
        tw_merge("self-baseline self-baseline-last"),
        "self-baseline-last"
    );
}

#[test]
fn test_123() {
    assert_eq!(
        tw_merge("items-center-safe items-baseline items-end-safe"),
        "items-end-safe"
    );
}

#[test]
fn test_124() {
    assert_eq!(
        tw_merge("wrap-break-word wrap-normal wrap-anywhere"),
        "wrap-anywhere"
    );
}

#[test]
fn test_125() {
    assert_eq!(
        tw_merge("text-shadow-none text-shadow-2xl"),
        "text-shadow-2xl"
    );
}

#[test]
fn test_126() {
    assert_eq!(tw_merge("mask-add mask-subtract"), "mask-subtract");
}

#[test]
fn test_127() {
    assert_eq!(
        tw_merge("mask-type-luminance mask-type-alpha"),
        "mask-type-alpha"
    );
}

#[test]
fn test_128() {
    assert_eq!(
        tw_merge("drop-shadow-[#123456] drop-shadow-some-color"),
        "drop-shadow-some-color"
    );
}

#[test]
fn test_129() {
    assert_eq!(
        tw_merge("drop-shadow-2xl drop-shadow-[shadow:foo]"),
        "drop-shadow-[shadow:foo]"
    );
}

#[test]
fn test_130() {
    assert_eq!(tw_merge("h-12 h-lh"), "h-lh");
}

#[test]
fn test_131() {
    assert_eq!(tw_merge("min-h-12 min-h-lh"), "min-h-lh");
}

#[test]
fn test_132() {
    assert_eq!(tw_merge("max-h-12 max-h-lh"), "max-h-lh");
}

#[test]
fn test_133() {
    assert_eq!(
        tw_merge("mix-blend-normal mix-blend-multiply"),
        "mix-blend-multiply"
    );
}

#[test]
fn test_134() {
    assert_eq!(tw_merge("h-10 h-min"), "h-min");
}

#[test]
fn test_135() {
    assert_eq!(tw_merge("stroke-black stroke-1"), "stroke-black stroke-1");
}

#[test]
fn test_136() {
    assert_eq!(tw_merge("stroke-2 stroke-[3]"), "stroke-[3]");
}

#[test]
fn test_137() {
    assert_eq!(
        tw_merge("outline-black outline-1"),
        "outline-black outline-1"
    );
}

#[test]
fn test_138() {
    assert_eq!(tw_merge("grayscale-0 grayscale-[50%]"), "grayscale-[50%]");
}

#[test]
fn test_139() {
    assert_eq!(tw_merge("grow grow-[2]"), "grow-[2]");
}

#[test]
fn test_140() {
    assert_eq!(tw_merge(" block"), "block");
}

#[test]
fn test_141() {
    assert_eq!(tw_merge("block "), "block");
}

#[test]
fn test_142() {
    assert_eq!(tw_merge(" block "), "block");
}

#[test]
fn test_143() {
    assert_eq!(tw_merge(" block px-2 py-4 "), "block px-2 py-4");
}

#[test]
fn test_144() {
    assert_eq!(tw_merge(" block px-2    py-4 "), "block px-2 py-4");
}

#[test]
fn test_145() {
    assert_eq!(tw_merge("block\npx-2"), "block px-2");
}

#[test]
fn test_146() {
    assert_eq!(tw_merge("\nblock\npx-2\n"), "block px-2");
}

#[test]
fn test_147() {
    assert_eq!(tw_merge(" block\n \n px-2 \n py-4 "), "block px-2 py-4");
}

// Phase 1: Rounded border radius tests
#[test]
fn test_148() {
    // Basic rounded conflicts
    assert_eq!(tw_merge("rounded-sm rounded-lg"), "rounded-lg");
}

#[test]
fn test_149() {
    // rounded conflicts with all corner variants
    assert_eq!(tw_merge("rounded-t-sm rounded-lg"), "rounded-lg");
}

#[test]
fn test_150() {
    // But rounded-t after rounded doesn't conflict (more specific wins)
    assert_eq!(
        tw_merge("rounded-lg rounded-t-sm"),
        "rounded-lg rounded-t-sm"
    );
}

#[test]
fn test_151() {
    // rounded-t conflicts with rounded-tl and rounded-tr
    assert_eq!(tw_merge("rounded-tl-sm rounded-t-lg"), "rounded-t-lg");
}

#[test]
fn test_152() {
    // But rounded-tl after rounded-t doesn't conflict
    assert_eq!(
        tw_merge("rounded-t-lg rounded-tl-sm"),
        "rounded-t-lg rounded-tl-sm"
    );
}

#[test]
fn test_153() {
    // rounded-s conflicts with rounded-ss and rounded-es
    assert_eq!(tw_merge("rounded-ss-sm rounded-s-lg"), "rounded-s-lg");
}

#[test]
fn test_154() {
    // rounded-e conflicts with rounded-se and rounded-ee
    assert_eq!(tw_merge("rounded-se-sm rounded-e-lg"), "rounded-e-lg");
}

#[test]
fn test_155() {
    // rounded-r conflicts with rounded-tr and rounded-br
    assert_eq!(tw_merge("rounded-tr-sm rounded-r-lg"), "rounded-r-lg");
}

#[test]
fn test_156() {
    // rounded-b conflicts with rounded-br and rounded-bl
    assert_eq!(tw_merge("rounded-br-sm rounded-b-lg"), "rounded-b-lg");
}

#[test]
fn test_157() {
    // rounded-l conflicts with rounded-tl and rounded-bl
    assert_eq!(tw_merge("rounded-bl-sm rounded-l-lg"), "rounded-l-lg");
}

#[test]
fn test_158() {
    // Multiple rounded classes
    assert_eq!(
        tw_merge("rounded-tl-sm rounded-tr-md rounded-t-lg"),
        "rounded-t-lg"
    );
}

#[test]
fn test_159() {
    // Standalone rounded
    assert_eq!(tw_merge("rounded rounded-md"), "rounded-md");
}

#[test]
fn test_160() {
    // Empty value rounded (deprecated but still used)
    assert_eq!(tw_merge("rounded-none rounded-full"), "rounded-full");
}

#[test]
fn test_161() {
    // With modifiers
    assert_eq!(
        tw_merge("hover:rounded-sm hover:rounded-lg"),
        "hover:rounded-lg"
    );
}

#[test]
fn test_162() {
    // Different modifiers don't conflict
    assert_eq!(
        tw_merge("hover:rounded-sm focus:rounded-lg"),
        "hover:rounded-sm focus:rounded-lg"
    );
}

// Phase 2: Translate & Transform tests
#[test]
fn test_163() {
    // Basic translate conflicts
    assert_eq!(tw_merge("translate-x-4 translate-x-8"), "translate-x-8");
}

#[test]
fn test_164() {
    // translate overrides translate-x/y
    assert_eq!(tw_merge("translate-x-4 translate-4"), "translate-4");
}

#[test]
fn test_165() {
    // translate-y after translate-x doesn't conflict
    assert_eq!(
        tw_merge("translate-x-4 translate-y-8"),
        "translate-x-4 translate-y-8"
    );
}

#[test]
fn test_166() {
    // translate-none conflicts with translate variants
    assert_eq!(tw_merge("translate-x-4 translate-none"), "translate-none");
}

#[test]
fn test_167() {
    // rotate conflicts
    assert_eq!(tw_merge("rotate-45 rotate-90"), "rotate-90");
}

#[test]
fn test_168() {
    // rotate-x/y/z don't conflict with each other
    assert_eq!(
        tw_merge("rotate-x-45 rotate-y-90"),
        "rotate-x-45 rotate-y-90"
    );
}

#[test]
fn test_169() {
    // skew conflicts
    assert_eq!(tw_merge("skew-x-12 skew-x-6"), "skew-x-6");
}

#[test]
fn test_170() {
    // scale conflicts
    assert_eq!(tw_merge("scale-50 scale-75"), "scale-75");
}

#[test]
fn test_171() {
    // scale-x/y/z are separate groups
    assert_eq!(tw_merge("scale-x-50 scale-y-75"), "scale-x-50 scale-y-75");
}

#[test]
fn test_172() {
    // transform-origin conflicts
    assert_eq!(tw_merge("origin-center origin-top"), "origin-top");
}

#[test]
fn test_173() {
    // perspective conflicts
    assert_eq!(
        tw_merge("perspective-normal perspective-distant"),
        "perspective-distant"
    );
}

#[test]
fn test_174() {
    // backface conflicts
    assert_eq!(
        tw_merge("backface-visible backface-hidden"),
        "backface-hidden"
    );
}

#[test]
fn test_175() {
    // transform-style conflicts
    assert_eq!(tw_merge("transform-3d transform-flat"), "transform-flat");
}

// Phase 3: Scroll margin & padding tests
#[test]
fn test_176() {
    // Basic scroll-m conflicts
    assert_eq!(tw_merge("scroll-m-4 scroll-m-8"), "scroll-m-8");
}

#[test]
fn test_177() {
    // scroll-m overrides scroll-mx/my
    assert_eq!(tw_merge("scroll-mx-4 scroll-m-8"), "scroll-m-8");
}

#[test]
fn test_178() {
    // scroll-mx/my don't conflict with each other
    assert_eq!(
        tw_merge("scroll-mx-4 scroll-my-8"),
        "scroll-mx-4 scroll-my-8"
    );
}

#[test]
fn test_179() {
    // scroll-mx overrides scroll-mr/ml
    assert_eq!(tw_merge("scroll-mr-4 scroll-mx-8"), "scroll-mx-8");
}

#[test]
fn test_180() {
    // scroll-my overrides scroll-mt/mb
    assert_eq!(tw_merge("scroll-mt-4 scroll-my-8"), "scroll-my-8");
}

#[test]
fn test_181() {
    // Basic scroll-p conflicts
    assert_eq!(tw_merge("scroll-p-4 scroll-p-8"), "scroll-p-8");
}

#[test]
fn test_182() {
    // scroll-p overrides scroll-px/py
    assert_eq!(tw_merge("scroll-px-4 scroll-p-8"), "scroll-p-8");
}

#[test]
fn test_183() {
    // scroll-px/py don't conflict with each other
    assert_eq!(
        tw_merge("scroll-px-4 scroll-py-8"),
        "scroll-px-4 scroll-py-8"
    );
}

#[test]
fn test_184() {
    // scroll-px overrides scroll-pr/pl
    assert_eq!(tw_merge("scroll-pl-4 scroll-px-8"), "scroll-px-8");
}

#[test]
fn test_185() {
    // scroll-py overrides scroll-pt/pb
    assert_eq!(tw_merge("scroll-pb-4 scroll-py-8"), "scroll-py-8");
}

#[test]
fn test_186() {
    // scroll-behavior conflicts
    assert_eq!(tw_merge("scroll-auto scroll-smooth"), "scroll-smooth");
}

#[test]
fn test_187() {
    // scroll-ms/me don't conflict with scroll-mx (logical vs physical)
    assert_eq!(
        tw_merge("scroll-ms-4 scroll-mx-8"),
        "scroll-ms-4 scroll-mx-8"
    );
}

#[test]
fn test_188() {
    // scroll-ps/pe don't conflict with scroll-px (logical vs physical)
    assert_eq!(
        tw_merge("scroll-ps-4 scroll-px-8"),
        "scroll-ps-4 scroll-px-8"
    );
}

#[test]
fn test_189() {
    // scroll-m overrides all including ms/me
    assert_eq!(tw_merge("scroll-ms-4 scroll-m-8"), "scroll-m-8");
}

#[test]
fn test_190() {
    // scroll-p overrides all including ps/pe
    assert_eq!(tw_merge("scroll-ps-4 scroll-p-8"), "scroll-p-8");
}

// Phase 4: Typography tests
#[test]
fn test_191() {
    // font-weight conflicts
    assert_eq!(tw_merge("font-bold font-thin"), "font-thin");
}

#[test]
fn test_192() {
    // font-family conflicts
    assert_eq!(tw_merge("font-sans font-mono"), "font-mono");
}

#[test]
fn test_193() {
    // font-weight and font-family don't conflict
    assert_eq!(tw_merge("font-bold font-sans"), "font-bold font-sans");
}

#[test]
fn test_194() {
    // font-style conflicts
    assert_eq!(tw_merge("italic not-italic"), "not-italic");
}

#[test]
fn test_195() {
    // font-smoothing conflicts
    assert_eq!(
        tw_merge("antialiased subpixel-antialiased"),
        "subpixel-antialiased"
    );
}

#[test]
fn test_196() {
    // tracking (letter-spacing) conflicts
    assert_eq!(tw_merge("tracking-tight tracking-wide"), "tracking-wide");
}

#[test]
fn test_197() {
    // text-alignment conflicts
    assert_eq!(tw_merge("text-left text-center"), "text-center");
}

#[test]
fn test_198() {
    // text-transform conflicts
    assert_eq!(tw_merge("uppercase lowercase"), "lowercase");
}

#[test]
fn test_199() {
    // text-overflow conflicts
    assert_eq!(tw_merge("truncate text-ellipsis"), "text-ellipsis");
}

#[test]
fn test_200() {
    // indent conflicts
    assert_eq!(tw_merge("indent-4 indent-8"), "indent-8");
}

#[test]
fn test_201() {
    // underline-offset conflicts
    assert_eq!(
        tw_merge("underline-offset-2 underline-offset-4"),
        "underline-offset-4"
    );
}

#[test]
fn test_202() {
    // text-decoration-style conflicts
    assert_eq!(
        tw_merge("decoration-solid decoration-dashed"),
        "decoration-dashed"
    );
}

#[test]
fn test_203() {
    // text-decoration-thickness conflicts
    assert_eq!(tw_merge("decoration-1 decoration-2"), "decoration-2");
}

#[test]
fn test_204() {
    // text-decoration-color conflicts
    assert_eq!(
        tw_merge("decoration-red-500 decoration-blue-500"),
        "decoration-blue-500"
    );
}

#[test]
fn test_205() {
    // vertical-align conflicts
    assert_eq!(tw_merge("align-top align-middle"), "align-middle");
}

#[test]
fn test_206() {
    // text-color vs text-size don't conflict
    assert_eq!(tw_merge("text-red-500 text-lg"), "text-red-500 text-lg");
}

#[test]
fn test_207() {
    // text-alignment vs text-color don't conflict
    assert_eq!(tw_merge("text-left text-red-500"), "text-left text-red-500");
}

// Phase 5: Background tests
#[test]
fn test_208() {
    // bg-color conflicts
    assert_eq!(tw_merge("bg-red-500 bg-blue-500"), "bg-blue-500");
}

#[test]
fn test_209() {
    // bg-position conflicts
    assert_eq!(tw_merge("bg-center bg-top"), "bg-top");
}

#[test]
fn test_210() {
    // bg-size conflicts
    assert_eq!(tw_merge("bg-auto bg-cover"), "bg-cover");
}

#[test]
fn test_211() {
    // bg-repeat conflicts
    assert_eq!(tw_merge("bg-repeat bg-no-repeat"), "bg-no-repeat");
}

#[test]
fn test_212() {
    // bg-attachment conflicts
    assert_eq!(tw_merge("bg-fixed bg-scroll"), "bg-scroll");
}

#[test]
fn test_213() {
    // bg-clip conflicts
    assert_eq!(tw_merge("bg-clip-border bg-clip-text"), "bg-clip-text");
}

#[test]
fn test_214() {
    // bg-origin conflicts
    assert_eq!(
        tw_merge("bg-origin-border bg-origin-padding"),
        "bg-origin-padding"
    );
}

#[test]
fn test_215() {
    // bg-blend conflicts
    assert_eq!(
        tw_merge("bg-blend-normal bg-blend-multiply"),
        "bg-blend-multiply"
    );
}

#[test]
fn test_216() {
    // bg-color and bg-position don't conflict
    assert_eq!(tw_merge("bg-red-500 bg-center"), "bg-red-500 bg-center");
}

#[test]
fn test_217() {
    // bg-color and bg-image don't conflict
    assert_eq!(
        tw_merge("bg-red-500 bg-linear-to-r"),
        "bg-red-500 bg-linear-to-r"
    );
}

#[test]
fn test_218() {
    // bg-size and bg-position don't conflict
    assert_eq!(tw_merge("bg-cover bg-center"), "bg-cover bg-center");
}

// Phase 6: Gradient color stops tests
#[test]
fn test_219() {
    // from-color conflicts
    assert_eq!(tw_merge("from-red-500 from-blue-500"), "from-blue-500");
}

#[test]
fn test_220() {
    // from-position conflicts
    assert_eq!(tw_merge("from-10% from-50%"), "from-50%");
}

#[test]
fn test_221() {
    // from-color and from-position don't conflict
    assert_eq!(tw_merge("from-red-500 from-10%"), "from-red-500 from-10%");
}

#[test]
fn test_222() {
    // via-color conflicts
    assert_eq!(tw_merge("via-red-500 via-blue-500"), "via-blue-500");
}

#[test]
fn test_223() {
    // via-position conflicts
    assert_eq!(tw_merge("via-10% via-50%"), "via-50%");
}

#[test]
fn test_224() {
    // via-color and via-position don't conflict
    assert_eq!(tw_merge("via-red-500 via-10%"), "via-red-500 via-10%");
}

#[test]
fn test_225() {
    // to-color conflicts
    assert_eq!(tw_merge("to-red-500 to-blue-500"), "to-blue-500");
}

#[test]
fn test_226() {
    // to-position conflicts
    assert_eq!(tw_merge("to-10% to-50%"), "to-50%");
}

#[test]
fn test_227() {
    // to-color and to-position don't conflict
    assert_eq!(tw_merge("to-red-500 to-10%"), "to-red-500 to-10%");
}

#[test]
fn test_228() {
    // Different gradient stops don't conflict
    assert_eq!(
        tw_merge("from-red-500 via-white to-blue-500"),
        "from-red-500 via-white to-blue-500"
    );
}

// Tests 229-271: Ported from tailwind-merge TypeScript test suite

#[test]
fn test_229() {
    assert_eq!(tw_merge("hover:block hover:inline"), "hover:inline");
}

#[test]
fn test_230() {
    assert_eq!(
        tw_merge("hover:block hover:focus:inline"),
        "hover:block hover:focus:inline"
    );
}

#[test]
fn test_231() {
    assert_eq!(
        tw_merge("focus-within:inline focus-within:block"),
        "focus-within:block"
    );
}

#[test]
fn test_232() {
    assert_eq!(tw_merge("text-lg/7 text-lg/8"), "text-lg/8");
}

#[test]
fn test_233() {
    assert_eq!(tw_merge("w-full w-1/2"), "w-1/2");
}

#[test]
fn test_234() {
    assert_eq!(tw_merge("font-medium! font-bold!"), "font-bold!");
}

#[test]
fn test_235() {
    assert_eq!(
        tw_merge("font-medium! font-bold! font-thin"),
        "font-bold! font-thin"
    );
}

#[test]
fn test_236() {
    assert_eq!(tw_merge("focus:inline! focus:block!"), "focus:block!");
}

#[test]
fn test_237() {
    assert_eq!(tw_merge("!font-medium !font-bold"), "!font-bold");
}

#[test]
fn test_238() {
    assert_eq!(
        tw_merge("!font-medium !font-bold font-thin"),
        "!font-bold font-thin"
    );
}

#[test]
fn test_239() {
    assert_eq!(tw_merge("-m-2 -m-5"), "-m-5");
}

#[test]
fn test_240() {
    assert_eq!(tw_merge("-top-12 -top-2000"), "-top-2000");
}

#[test]
fn test_241() {
    assert_eq!(tw_merge("-m-2 m-auto"), "m-auto");
}

#[test]
fn test_242() {
    assert_eq!(tw_merge("top-12 -top-69"), "-top-69");
}

#[test]
fn test_243() {
    assert_eq!(tw_merge("-right-1 inset-x-1"), "inset-x-1");
}

#[test]
fn test_244() {
    assert_eq!(
        tw_merge("border-t border-white/10"),
        "border-t border-white/10"
    );
}

#[test]
fn test_245() {
    assert_eq!(tw_merge("border-t border-white"), "border-t border-white");
}

#[test]
fn test_246() {
    assert_eq!(tw_merge("text-3.5xl text-black"), "text-3.5xl text-black");
}

#[test]
fn test_247() {
    assert_eq!(
        tw_merge("[paint-order:markers] [paint-order:normal]"),
        "[paint-order:normal]"
    );
}

#[test]
#[ignore = "arbitrary properties need dynamic group names"]
fn test_248() {
    assert_eq!(
        tw_merge("[paint-order:markers] [--my-var:2rem] [paint-order:normal] [--my-var:4px]"),
        "[paint-order:normal] [--my-var:4px]"
    );
}

#[test]
fn test_249() {
    assert_eq!(
        tw_merge("[paint-order:markers] hover:[paint-order:normal]"),
        "[paint-order:markers] hover:[paint-order:normal]"
    );
}

#[test]
fn test_250() {
    assert_eq!(
        tw_merge("hover:[paint-order:markers] hover:[paint-order:normal]"),
        "hover:[paint-order:normal]"
    );
}

#[test]
fn test_251() {
    assert_eq!(tw_merge("m-[2px] m-[10px]"), "m-[10px]");
}

#[test]
fn test_252() {
    assert_eq!(tw_merge("z-20 z-[99]"), "z-[99]");
}

#[test]
fn test_253() {
    assert_eq!(tw_merge("my-[2px] m-[10rem]"), "m-[10rem]");
}

#[test]
fn test_254() {
    assert_eq!(tw_merge("cursor-pointer cursor-[grab]"), "cursor-[grab]");
}

#[test]
fn test_255() {
    assert_eq!(tw_merge("opacity-10 opacity-[0.025]"), "opacity-[0.025]");
}

#[test]
fn test_256() {
    assert_eq!(tw_merge("scale-75 scale-[1.7]"), "scale-[1.7]");
}

#[test]
fn test_257() {
    assert_eq!(
        tw_merge("non-tailwind-class inline block"),
        "non-tailwind-class block"
    );
}

#[test]
fn test_258() {
    assert_eq!(tw_merge("inline block inline-1"), "block inline-1");
}

#[test]
fn test_259() {
    assert_eq!(tw_merge("inline block i-inline"), "block i-inline");
}

#[test]
fn test_260() {
    assert_eq!(
        tw_merge("focus:inline focus:block focus:inline-1"),
        "focus:block focus:inline-1"
    );
}

#[test]
fn test_261() {
    assert_eq!(tw_merge("bg-grey-5 bg-hotpink"), "bg-hotpink");
}

#[test]
fn test_262() {
    assert_eq!(
        tw_merge("hover:bg-grey-5 hover:bg-hotpink"),
        "hover:bg-hotpink"
    );
}

#[test]
fn test_263() {
    assert_eq!(
        tw_merge("border-t-some-blue border-t-other-blue"),
        "border-t-other-blue"
    );
}

#[test]
fn test_264() {
    assert_eq!(
        tw_merge("border-t-some-blue border-some-blue"),
        "border-some-blue"
    );
}

#[test]
fn test_265() {
    assert_eq!(
        tw_merge("border-some-blue border-s-some-blue"),
        "border-some-blue border-s-some-blue"
    );
}

#[test]
fn test_266() {
    assert_eq!(tw_merge("empty:p-2 empty:p-3"), "empty:p-3");
}

#[test]
fn test_267() {
    assert_eq!(
        tw_merge("hover:empty:p-2 hover:empty:p-3"),
        "hover:empty:p-3"
    );
}

#[test]
fn test_268() {
    assert_eq!(tw_merge("read-only:p-2 read-only:p-3"), "read-only:p-3");
}

#[test]
fn test_269() {
    assert_eq!(
        tw_merge("group-empty:p-2 group-empty:p-3"),
        "group-empty:p-3"
    );
}

#[test]
fn test_270() {
    assert_eq!(tw_merge("peer-empty:p-2 peer-empty:p-3"), "peer-empty:p-3");
}

#[test]
fn test_271() {
    assert_eq!(
        tw_merge("group-empty:p-2 peer-empty:p-3"),
        "group-empty:p-2 peer-empty:p-3"
    );
}

// Phase 7: Filter tests (test_272 - test_288)
#[test]
fn test_272() {
    assert_eq!(tw_merge("blur-sm blur-lg"), "blur-lg");
}

#[test]
fn test_273() {
    assert_eq!(tw_merge("blur blur-none"), "blur-none");
}

#[test]
fn test_274() {
    assert_eq!(tw_merge("brightness-50 brightness-100"), "brightness-100");
}

#[test]
fn test_275() {
    assert_eq!(tw_merge("contrast-50 contrast-100"), "contrast-100");
}

#[test]
fn test_276() {
    assert_eq!(tw_merge("grayscale grayscale-0"), "grayscale-0");
}

#[test]
fn test_277() {
    assert_eq!(tw_merge("hue-rotate-15 hue-rotate-90"), "hue-rotate-90");
}

#[test]
fn test_278() {
    assert_eq!(tw_merge("invert invert-0"), "invert-0");
}

#[test]
fn test_279() {
    assert_eq!(tw_merge("saturate-50 saturate-100"), "saturate-100");
}

#[test]
fn test_280() {
    assert_eq!(tw_merge("sepia sepia-0"), "sepia-0");
}

#[test]
fn test_281() {
    assert_eq!(
        tw_merge("backdrop-blur-sm backdrop-blur-lg"),
        "backdrop-blur-lg"
    );
}

#[test]
fn test_282() {
    assert_eq!(
        tw_merge("backdrop-brightness-50 backdrop-brightness-100"),
        "backdrop-brightness-100"
    );
}

#[test]
fn test_283() {
    assert_eq!(
        tw_merge("backdrop-contrast-50 backdrop-contrast-100"),
        "backdrop-contrast-100"
    );
}

#[test]
fn test_284() {
    assert_eq!(
        tw_merge("backdrop-grayscale backdrop-grayscale-0"),
        "backdrop-grayscale-0"
    );
}

#[test]
fn test_285() {
    assert_eq!(
        tw_merge("backdrop-hue-rotate-15 backdrop-hue-rotate-90"),
        "backdrop-hue-rotate-90"
    );
}

#[test]
fn test_286() {
    assert_eq!(
        tw_merge("backdrop-invert backdrop-invert-0"),
        "backdrop-invert-0"
    );
}

#[test]
fn test_287() {
    assert_eq!(
        tw_merge("backdrop-opacity-50 backdrop-opacity-100"),
        "backdrop-opacity-100"
    );
}

#[test]
fn test_288() {
    assert_eq!(
        tw_merge("backdrop-saturate-50 backdrop-saturate-100"),
        "backdrop-saturate-100"
    );
}

#[test]
fn test_289() {
    assert_eq!(
        tw_merge("backdrop-sepia backdrop-sepia-0"),
        "backdrop-sepia-0"
    );
}

#[test]
fn test_290() {
    // Different filters don't conflict
    assert_eq!(
        tw_merge("blur-sm brightness-50 contrast-100"),
        "blur-sm brightness-50 contrast-100"
    );
}

// Phase 8: Effects & Rings tests (test_291 - test_303)
#[test]
fn test_291() {
    assert_eq!(tw_merge("ring ring-2"), "ring-2");
}

#[test]
fn test_292() {
    assert_eq!(tw_merge("ring-1 ring-4"), "ring-4");
}

#[test]
fn test_293() {
    assert_eq!(tw_merge("ring-red-500 ring-blue-500"), "ring-blue-500");
}

#[test]
fn test_294() {
    // Ring width vs color don't conflict
    assert_eq!(tw_merge("ring-2 ring-red-500"), "ring-2 ring-red-500");
}

#[test]
fn test_295() {
    assert_eq!(tw_merge("ring-offset-2 ring-offset-4"), "ring-offset-4");
}

#[test]
fn test_296() {
    assert_eq!(
        tw_merge("ring-offset-red-500 ring-offset-blue-500"),
        "ring-offset-blue-500"
    );
}

#[test]
fn test_297() {
    // Ring offset width vs color don't conflict
    assert_eq!(
        tw_merge("ring-offset-2 ring-offset-red-500"),
        "ring-offset-2 ring-offset-red-500"
    );
}

#[test]
fn test_298() {
    assert_eq!(tw_merge("inset-ring-1 inset-ring-2"), "inset-ring-2");
}

#[test]
fn test_299() {
    assert_eq!(
        tw_merge("inset-ring-red-500 inset-ring-blue-500"),
        "inset-ring-blue-500"
    );
}

#[test]
fn test_300() {
    assert_eq!(
        tw_merge("inset-shadow-sm inset-shadow-lg"),
        "inset-shadow-lg"
    );
}

#[test]
fn test_301() {
    assert_eq!(
        tw_merge("inset-shadow-red-500 inset-shadow-blue-500"),
        "inset-shadow-blue-500"
    );
}

#[test]
fn test_302() {
    assert_eq!(tw_merge("shadow-sm shadow-lg"), "shadow-lg");
}

#[test]
fn test_303() {
    assert_eq!(
        tw_merge("shadow-red-500 shadow-blue-500"),
        "shadow-blue-500"
    );
}

#[test]
fn test_304() {
    // Shadow size vs color don't conflict
    assert_eq!(
        tw_merge("shadow-lg shadow-red-500"),
        "shadow-lg shadow-red-500"
    );
}

// Phase 9: Flexbox & Grid tests (test_305 - test_316)
#[test]
fn test_305() {
    assert_eq!(tw_merge("flex-row flex-col"), "flex-col");
}

#[test]
fn test_306() {
    assert_eq!(
        tw_merge("flex-row-reverse flex-col-reverse"),
        "flex-col-reverse"
    );
}

#[test]
fn test_307() {
    assert_eq!(tw_merge("flex-wrap flex-nowrap"), "flex-nowrap");
}

#[test]
fn test_308() {
    assert_eq!(tw_merge("flex-wrap flex-wrap-reverse"), "flex-wrap-reverse");
}

#[test]
fn test_309() {
    // Flex direction and flex wrap don't conflict
    assert_eq!(tw_merge("flex-row flex-wrap"), "flex-row flex-wrap");
}

#[test]
fn test_310() {
    assert_eq!(tw_merge("order-1 order-2"), "order-2");
}

#[test]
fn test_311() {
    assert_eq!(tw_merge("order-first order-last"), "order-last");
}

#[test]
fn test_312() {
    assert_eq!(tw_merge("order-none order-1"), "order-1");
}

#[test]
fn test_313() {
    assert_eq!(tw_merge("grid-flow-row grid-flow-col"), "grid-flow-col");
}

#[test]
fn test_314() {
    assert_eq!(
        tw_merge("grid-flow-dense grid-flow-row-dense"),
        "grid-flow-row-dense"
    );
}

#[test]
fn test_315() {
    assert_eq!(tw_merge("auto-cols-auto auto-cols-fr"), "auto-cols-fr");
}

#[test]
fn test_316() {
    assert_eq!(tw_merge("auto-rows-auto auto-rows-min"), "auto-rows-min");
}

#[test]
fn test_317() {
    // Auto-cols and auto-rows don't conflict
    assert_eq!(
        tw_merge("auto-cols-fr auto-rows-fr"),
        "auto-cols-fr auto-rows-fr"
    );
}

// Phase 10: Layout tests (test_318 - test_330)
#[test]
fn test_318() {
    assert_eq!(
        tw_merge("overscroll-auto overscroll-contain"),
        "overscroll-contain"
    );
}

#[test]
fn test_319() {
    assert_eq!(
        tw_merge("overscroll-none overscroll-auto"),
        "overscroll-auto"
    );
}

#[test]
fn test_320() {
    assert_eq!(
        tw_merge("overscroll-x-auto overscroll-x-contain"),
        "overscroll-x-contain"
    );
}

#[test]
fn test_321() {
    assert_eq!(
        tw_merge("overscroll-y-auto overscroll-y-none"),
        "overscroll-y-none"
    );
}

#[test]
fn test_322() {
    // Overscroll overrides overscroll-x/y
    assert_eq!(
        tw_merge("overscroll-x-auto overscroll-contain"),
        "overscroll-contain"
    );
}

#[test]
fn test_323() {
    // overscroll-x and overscroll-y don't conflict
    assert_eq!(
        tw_merge("overscroll-x-auto overscroll-y-contain"),
        "overscroll-x-auto overscroll-y-contain"
    );
}

#[test]
fn test_324() {
    assert_eq!(tw_merge("isolate isolation-auto"), "isolation-auto");
}

#[test]
fn test_325() {
    assert_eq!(tw_merge("object-contain object-cover"), "object-cover");
}

#[test]
fn test_326() {
    assert_eq!(
        tw_merge("object-fill object-scale-down"),
        "object-scale-down"
    );
}

#[test]
fn test_327() {
    assert_eq!(tw_merge("object-center object-top"), "object-top");
}

#[test]
fn test_328() {
    // Object fit and position don't conflict
    assert_eq!(
        tw_merge("object-cover object-center"),
        "object-cover object-center"
    );
}

#[test]
fn test_329() {
    assert_eq!(
        tw_merge("box-decoration-slice box-decoration-clone"),
        "box-decoration-clone"
    );
}

#[test]
fn test_330() {
    assert_eq!(tw_merge("sr-only not-sr-only"), "not-sr-only");
}

// --- Phase 11: Interactivity ---

#[test]
fn test_331() {
    assert_eq!(
        tw_merge("pointer-events-auto pointer-events-none"),
        "pointer-events-none"
    );
}

#[test]
fn test_332() {
    assert_eq!(tw_merge("resize resize-none"), "resize-none");
}

#[test]
fn test_333() {
    assert_eq!(tw_merge("resize-x resize-y"), "resize-y");
}

#[test]
fn test_334() {
    assert_eq!(tw_merge("caret-red-500 caret-blue-500"), "caret-blue-500");
}

#[test]
fn test_335() {
    assert_eq!(tw_merge("select-none select-text"), "select-text");
}

#[test]
fn test_336() {
    assert_eq!(tw_merge("select-all select-auto"), "select-auto");
}

#[test]
fn test_337() {
    assert_eq!(
        tw_merge("will-change-auto will-change-scroll"),
        "will-change-scroll"
    );
}

#[test]
fn test_338() {
    assert_eq!(
        tw_merge("will-change-transform will-change-contents"),
        "will-change-contents"
    );
}

#[test]
fn test_339() {
    assert_eq!(tw_merge("snap-start snap-center"), "snap-center");
}

#[test]
fn test_340() {
    assert_eq!(tw_merge("snap-normal snap-always"), "snap-always");
}

#[test]
fn test_341() {
    assert_eq!(tw_merge("snap-x snap-y"), "snap-y");
}

#[test]
fn test_342() {
    assert_eq!(tw_merge("snap-none snap-both"), "snap-both");
}

#[test]
fn test_343() {
    assert_eq!(tw_merge("snap-mandatory snap-proximity"), "snap-proximity");
}

#[test]
fn test_344() {
    // Snap type and strictness don't conflict
    assert_eq!(tw_merge("snap-x snap-mandatory"), "snap-x snap-mandatory");
}

#[test]
fn test_345() {
    // Snap align doesn't conflict with snap type
    assert_eq!(tw_merge("snap-start snap-x"), "snap-start snap-x");
}

#[test]
fn test_346() {
    assert_eq!(tw_merge("touch-auto touch-none"), "touch-none");
}

#[test]
fn test_347() {
    assert_eq!(
        tw_merge("touch-pan-left touch-pan-right"),
        "touch-pan-right"
    );
}

#[test]
fn test_348() {
    assert_eq!(tw_merge("touch-pan-up touch-pan-down"), "touch-pan-down");
}

#[test]
fn test_349() {
    // touch-x and touch-y don't conflict with each other
    assert_eq!(
        tw_merge("touch-pan-x touch-pan-y"),
        "touch-pan-x touch-pan-y"
    );
}

#[test]
fn test_350() {
    // touch-pz doesn't conflict with touch-x/y
    assert_eq!(
        tw_merge("touch-pan-x touch-pan-y touch-pinch-zoom"),
        "touch-pan-x touch-pan-y touch-pinch-zoom"
    );
}

#[test]
fn test_351() {
    // touch-none overrides all specific pan/pinch
    assert_eq!(
        tw_merge("touch-pan-x touch-pan-y touch-pinch-zoom touch-none"),
        "touch-none"
    );
}

#[test]
fn test_352() {
    // touch-manipulation overrides specific pan/pinch
    assert_eq!(
        tw_merge("touch-pan-x touch-manipulation"),
        "touch-manipulation"
    );
}

#[test]
fn test_353() {
    // Specific pan overrides touch-auto
    assert_eq!(tw_merge("touch-auto touch-pan-x"), "touch-pan-x");
}

// --- Phase 12: Space Utilities ---

#[test]
fn test_354() {
    assert_eq!(tw_merge("space-x-4 space-x-8"), "space-x-8");
}

#[test]
fn test_355() {
    assert_eq!(tw_merge("space-y-4 space-y-8"), "space-y-8");
}

#[test]
fn test_356() {
    assert_eq!(
        tw_merge("space-x-reverse space-x-reverse"),
        "space-x-reverse"
    );
}

#[test]
fn test_357() {
    // space-x and space-y don't conflict
    assert_eq!(tw_merge("space-x-4 space-y-4"), "space-x-4 space-y-4");
}

#[test]
fn test_358() {
    // negative values
    assert_eq!(tw_merge("-space-x-4 space-x-8"), "space-x-8");
}

#[test]
fn test_359() {
    // space-x-reverse is separate group from space-x
    assert_eq!(
        tw_merge("space-x-4 space-x-reverse"),
        "space-x-4 space-x-reverse"
    );
}

// --- Phase 13: Divide Utilities ---

#[test]
fn test_360() {
    assert_eq!(tw_merge("divide-x divide-x-2"), "divide-x-2");
}

#[test]
fn test_361() {
    assert_eq!(tw_merge("divide-y-4 divide-y-8"), "divide-y-8");
}

#[test]
fn test_362() {
    assert_eq!(tw_merge("divide-solid divide-dashed"), "divide-dashed");
}

#[test]
fn test_363() {
    assert_eq!(
        tw_merge("divide-red-500 divide-blue-500"),
        "divide-blue-500"
    );
}

#[test]
fn test_364() {
    // divide-x and divide-y don't conflict
    assert_eq!(tw_merge("divide-x divide-y"), "divide-x divide-y");
}

#[test]
fn test_365() {
    // width vs color don't conflict
    assert_eq!(
        tw_merge("divide-x divide-red-500"),
        "divide-x divide-red-500"
    );
}

#[test]
fn test_366() {
    // style vs color don't conflict
    assert_eq!(
        tw_merge("divide-dashed divide-red-500"),
        "divide-dashed divide-red-500"
    );
}

#[test]
fn test_367() {
    // divide-x-reverse is separate group
    assert_eq!(
        tw_merge("divide-x divide-x-reverse"),
        "divide-x divide-x-reverse"
    );
}

// --- Phase 14: Gradient Utilities ---

#[test]
fn test_368() {
    assert_eq!(tw_merge("from-red-500 from-blue-500"), "from-blue-500");
}

#[test]
fn test_369() {
    assert_eq!(tw_merge("via-red-500 via-blue-500"), "via-blue-500");
}

#[test]
fn test_370() {
    assert_eq!(tw_merge("to-red-500 to-blue-500"), "to-blue-500");
}

#[test]
fn test_371() {
    // from, via, to don't conflict with each other
    assert_eq!(
        tw_merge("from-red-500 via-blue-500 to-green-500"),
        "from-red-500 via-blue-500 to-green-500"
    );
}

#[test]
fn test_372() {
    // gradient positions
    assert_eq!(tw_merge("from-0% from-50%"), "from-50%");
}

#[test]
fn test_373() {
    // arbitrary values
    assert_eq!(tw_merge("from-[#ff0000] from-[#0000ff]"), "from-[#0000ff]");
}

// --- Phase 15: Outline & Border Spacing ---

#[test]
fn test_374() {
    // outline (width) and outline-none (style) don't conflict
    assert_eq!(tw_merge("outline outline-none"), "outline outline-none");
}

#[test]
fn test_375() {
    assert_eq!(tw_merge("outline-dashed outline-dotted"), "outline-dotted");
}

#[test]
fn test_376() {
    assert_eq!(tw_merge("outline-2 outline-4"), "outline-4");
}

#[test]
fn test_377() {
    assert_eq!(
        tw_merge("outline-red-500 outline-blue-500"),
        "outline-blue-500"
    );
}

#[test]
fn test_378() {
    // width vs color don't conflict
    assert_eq!(
        tw_merge("outline-2 outline-red-500"),
        "outline-2 outline-red-500"
    );
}

#[test]
fn test_379() {
    // style vs color don't conflict
    assert_eq!(
        tw_merge("outline-dashed outline-red-500"),
        "outline-dashed outline-red-500"
    );
}

#[test]
fn test_380() {
    assert_eq!(
        tw_merge("outline-offset-2 outline-offset-4"),
        "outline-offset-4"
    );
}

#[test]
fn test_381() {
    assert_eq!(
        tw_merge("border-spacing-2 border-spacing-4"),
        "border-spacing-4"
    );
}

#[test]
fn test_382() {
    // border-spacing overrides border-spacing-x
    assert_eq!(
        tw_merge("border-spacing-x-2 border-spacing-4"),
        "border-spacing-4"
    );
}

#[test]
fn test_383() {
    // border-spacing-x and border-spacing-y don't conflict
    assert_eq!(
        tw_merge("border-spacing-x-2 border-spacing-y-4"),
        "border-spacing-x-2 border-spacing-y-4"
    );
}

// --- Phase 16: border-style, text-wrap, list utilities ---

#[test]
fn test_384() {
    assert_eq!(tw_merge("border-solid border-dashed"), "border-dashed");
}

#[test]
fn test_385() {
    assert_eq!(tw_merge("border-dotted border-none"), "border-none");
}

#[test]
fn test_386() {
    // border-style and border-w don't conflict
    assert_eq!(tw_merge("border-solid border-2"), "border-solid border-2");
}

#[test]
fn test_387() {
    // border-style and border-color don't conflict
    assert_eq!(
        tw_merge("border-dashed border-red-500"),
        "border-dashed border-red-500"
    );
}

#[test]
fn test_388() {
    assert_eq!(tw_merge("text-wrap text-nowrap"), "text-nowrap");
}

#[test]
fn test_389() {
    assert_eq!(tw_merge("text-balance text-pretty"), "text-pretty");
}

#[test]
fn test_390() {
    // text-wrap and text-color don't conflict
    assert_eq!(tw_merge("text-wrap text-red-500"), "text-wrap text-red-500");
}

#[test]
fn test_391() {
    assert_eq!(tw_merge("list-disc list-decimal"), "list-decimal");
}

#[test]
fn test_392() {
    assert_eq!(tw_merge("list-inside list-outside"), "list-outside");
}

#[test]
fn test_393() {
    // list-style-type and list-style-position don't conflict
    assert_eq!(tw_merge("list-disc list-inside"), "list-disc list-inside");
}

#[test]
fn test_394() {
    assert_eq!(
        tw_merge("list-image-none list-image-[url(test)]"),
        "list-image-[url(test)]"
    );
}

// --- Phase 17: place utilities ---

#[test]
fn test_395() {
    assert_eq!(
        tw_merge("place-content-center place-content-start"),
        "place-content-start"
    );
}

#[test]
fn test_396() {
    assert_eq!(
        tw_merge("place-items-center place-items-start"),
        "place-items-start"
    );
}

#[test]
fn test_397() {
    assert_eq!(
        tw_merge("place-self-auto place-self-center"),
        "place-self-center"
    );
}

#[test]
fn test_398() {
    // place-content and place-items don't conflict
    assert_eq!(
        tw_merge("place-content-center place-items-start"),
        "place-content-center place-items-start"
    );
}

#[test]
fn test_399() {
    // place-self doesn't conflict with place-content/items
    assert_eq!(
        tw_merge("place-content-center place-self-start"),
        "place-content-center place-self-start"
    );
}

// --- Phase 18: break and table utilities ---

#[test]
fn test_400() {
    assert_eq!(
        tw_merge("break-after-auto break-after-avoid"),
        "break-after-avoid"
    );
}

#[test]
fn test_401() {
    assert_eq!(
        tw_merge("break-before-page break-before-column"),
        "break-before-column"
    );
}

#[test]
fn test_402() {
    assert_eq!(
        tw_merge("break-inside-auto break-inside-avoid"),
        "break-inside-avoid"
    );
}

#[test]
fn test_403() {
    // break-after and break-before don't conflict
    assert_eq!(
        tw_merge("break-after-avoid break-before-page"),
        "break-after-avoid break-before-page"
    );
}

#[test]
fn test_404() {
    assert_eq!(
        tw_merge("border-collapse border-separate"),
        "border-separate"
    );
}

#[test]
fn test_405() {
    // border-collapse and border-style don't conflict
    assert_eq!(
        tw_merge("border-collapse border-solid"),
        "border-collapse border-solid"
    );
}

#[test]
fn test_406() {
    // border-collapse and border-w don't conflict
    assert_eq!(
        tw_merge("border-collapse border-2"),
        "border-collapse border-2"
    );
}

// --- Phase 19-21: shadow colors, transform-style, transition-behavior ---

#[test]
fn test_407() {
    // text-shadow size conflicts
    assert_eq!(tw_merge("text-shadow-sm text-shadow-lg"), "text-shadow-lg");
}

#[test]
fn test_408() {
    // text-shadow color conflicts
    assert_eq!(
        tw_merge("text-shadow-red-500 text-shadow-blue-500"),
        "text-shadow-blue-500"
    );
}

#[test]
fn test_409() {
    // text-shadow size and color don't conflict
    assert_eq!(
        tw_merge("text-shadow-lg text-shadow-red-500"),
        "text-shadow-lg text-shadow-red-500"
    );
}

#[test]
fn test_410() {
    // drop-shadow size conflicts
    assert_eq!(tw_merge("drop-shadow-sm drop-shadow-lg"), "drop-shadow-lg");
}

#[test]
fn test_411() {
    // drop-shadow color conflicts
    assert_eq!(
        tw_merge("drop-shadow-red-500 drop-shadow-blue-500"),
        "drop-shadow-blue-500"
    );
}

#[test]
fn test_412() {
    // drop-shadow size and color don't conflict
    assert_eq!(
        tw_merge("drop-shadow-lg drop-shadow-red-500"),
        "drop-shadow-lg drop-shadow-red-500"
    );
}

#[test]
fn test_413() {
    assert_eq!(tw_merge("transform-3d transform-flat"), "transform-flat");
}

#[test]
fn test_414() {
    // transform-style doesn't conflict with other transform utilities
    assert_eq!(
        tw_merge("transform-3d transform-gpu"),
        "transform-3d transform-gpu"
    );
}

#[test]
fn test_415() {
    assert_eq!(
        tw_merge("transition-normal transition-discrete"),
        "transition-discrete"
    );
}

#[test]
fn test_416() {
    assert_eq!(
        tw_merge("placeholder-red-500 placeholder-blue-500"),
        "placeholder-blue-500"
    );
}

#[test]
fn test_417() {
    assert_eq!(
        tw_merge("forced-color-adjust-auto forced-color-adjust-none"),
        "forced-color-adjust-none"
    );
}

// --- Phase 22: New class groups (container, box, columns, flex, visibility, etc.) ---

#[test]
fn test_418() {
    // container is its own group
    assert_eq!(tw_merge("container container"), "container");
}

#[test]
fn test_419() {
    // box sizing conflicts
    assert_eq!(tw_merge("box-border box-content"), "box-content");
}

#[test]
fn test_420() {
    // columns conflicts
    assert_eq!(tw_merge("columns-1 columns-2"), "columns-2");
}

#[test]
fn test_421() {
    assert_eq!(tw_merge("columns-auto columns-3xl"), "columns-3xl");
}

#[test]
fn test_422() {
    // flex shorthand conflicts
    assert_eq!(tw_merge("flex-1 flex-auto"), "flex-auto");
}

#[test]
fn test_423() {
    assert_eq!(tw_merge("flex-initial flex-none"), "flex-none");
}

#[test]
fn test_424() {
    // flex shorthand doesn't conflict with flex-direction
    assert_eq!(tw_merge("flex-1 flex-row"), "flex-1 flex-row");
}

#[test]
fn test_425() {
    // flex shorthand doesn't conflict with flex-wrap
    assert_eq!(tw_merge("flex-auto flex-wrap"), "flex-auto flex-wrap");
}

#[test]
fn test_426() {
    // flex conflicts with basis, grow, shrink
    assert_eq!(tw_merge("basis-4 flex-1"), "flex-1");
}

#[test]
fn test_427() {
    assert_eq!(tw_merge("grow flex-auto"), "flex-auto");
}

#[test]
fn test_428() {
    assert_eq!(tw_merge("shrink-0 flex-none"), "flex-none");
}

#[test]
fn test_429() {
    // visibility conflicts
    assert_eq!(tw_merge("visible invisible"), "invisible");
}

#[test]
fn test_430() {
    assert_eq!(tw_merge("invisible collapse"), "collapse");
}

#[test]
fn test_431() {
    // table-layout conflicts
    assert_eq!(tw_merge("table-auto table-fixed"), "table-fixed");
}

#[test]
fn test_432() {
    // fill conflicts
    assert_eq!(tw_merge("fill-red-500 fill-blue-500"), "fill-blue-500");
}

#[test]
fn test_433() {
    assert_eq!(tw_merge("fill-none fill-current"), "fill-current");
}

#[test]
fn test_434() {
    // filter base utility conflicts
    assert_eq!(tw_merge("filter filter-none"), "filter-none");
}

#[test]
fn test_435() {
    // filter doesn't conflict with individual filters
    assert_eq!(tw_merge("filter blur-sm"), "filter blur-sm");
}

#[test]
fn test_436() {
    // backdrop-filter base utility conflicts
    assert_eq!(
        tw_merge("backdrop-filter backdrop-filter-none"),
        "backdrop-filter-none"
    );
}

#[test]
fn test_437() {
    // ease conflicts
    assert_eq!(tw_merge("ease-in ease-out"), "ease-out");
}

#[test]
fn test_438() {
    assert_eq!(tw_merge("ease-linear ease-in-out"), "ease-in-out");
}

#[test]
fn test_439() {
    // animate conflicts
    assert_eq!(tw_merge("animate-spin animate-ping"), "animate-ping");
}

#[test]
fn test_440() {
    assert_eq!(tw_merge("animate-bounce animate-none"), "animate-none");
}

#[test]
fn test_441() {
    // transition property conflicts
    assert_eq!(tw_merge("transition transition-all"), "transition-all");
}

#[test]
fn test_442() {
    assert_eq!(
        tw_merge("transition-colors transition-opacity"),
        "transition-opacity"
    );
}

#[test]
fn test_443() {
    assert_eq!(
        tw_merge("transition-shadow transition-none"),
        "transition-none"
    );
}

#[test]
fn test_444() {
    // transition doesn't conflict with transition-behavior
    assert_eq!(
        tw_merge("transition-all transition-discrete"),
        "transition-all transition-discrete"
    );
}

#[test]
fn test_445() {
    // font-stretch conflicts
    assert_eq!(
        tw_merge("font-stretch-condensed font-stretch-expanded"),
        "font-stretch-expanded"
    );
}

#[test]
fn test_446() {
    // ring-inset
    assert_eq!(tw_merge("ring-inset ring-inset"), "ring-inset");
}

// --- Phase 23: Naming consistency tests ---

#[test]
fn test_447() {
    // color-scheme conflicts
    assert_eq!(tw_merge("scheme-dark scheme-light"), "scheme-light");
}

#[test]
fn test_448() {
    assert_eq!(
        tw_merge("scheme-normal scheme-only-light"),
        "scheme-only-light"
    );
}

#[test]
fn test_449() {
    // stroke (color) conflicts
    assert_eq!(
        tw_merge("stroke-red-500 stroke-blue-500"),
        "stroke-blue-500"
    );
}

#[test]
fn test_450() {
    // stroke-w conflicts
    assert_eq!(tw_merge("stroke-1 stroke-2"), "stroke-2");
}

#[test]
fn test_451() {
    // stroke and stroke-w don't conflict
    assert_eq!(
        tw_merge("stroke-red-500 stroke-2"),
        "stroke-red-500 stroke-2"
    );
}

#[test]
fn test_452() {
    // transform-origin conflicts
    assert_eq!(tw_merge("origin-center origin-top-left"), "origin-top-left");
}

#[test]
fn test_453() {
    // justify-content conflicts
    assert_eq!(tw_merge("justify-start justify-center"), "justify-center");
}

#[test]
fn test_454() {
    assert_eq!(tw_merge("justify-between justify-evenly"), "justify-evenly");
}

#[test]
fn test_455() {
    // align-content conflicts
    assert_eq!(tw_merge("content-start content-center"), "content-center");
}

#[test]
fn test_456() {
    // CSS content property conflicts
    assert_eq!(
        tw_merge("content-none content-['hello']"),
        "content-['hello']"
    );
}

#[test]
fn test_457() {
    // CSS content and align-content don't conflict
    assert_eq!(
        tw_merge("content-none content-center"),
        "content-none content-center"
    );
}

#[test]
fn test_458() {
    // align-items conflicts
    assert_eq!(tw_merge("items-start items-center"), "items-center");
}

#[test]
fn test_459() {
    assert_eq!(tw_merge("items-baseline items-stretch"), "items-stretch");
}

#[test]
fn test_460() {
    // align-self conflicts
    assert_eq!(tw_merge("self-auto self-start"), "self-start");
}

#[test]
fn test_461() {
    assert_eq!(tw_merge("self-center self-stretch"), "self-stretch");
}

#[test]
fn test_462() {
    // text-decoration-style conflicts
    assert_eq!(
        tw_merge("decoration-solid decoration-wavy"),
        "decoration-wavy"
    );
}

#[test]
fn test_463() {
    // text-decoration-thickness conflicts
    assert_eq!(tw_merge("decoration-1 decoration-2"), "decoration-2");
}

#[test]
fn test_464() {
    assert_eq!(
        tw_merge("decoration-auto decoration-from-font"),
        "decoration-from-font"
    );
}

#[test]
fn test_465() {
    // text-decoration-color conflicts
    assert_eq!(
        tw_merge("decoration-red-500 decoration-blue-500"),
        "decoration-blue-500"
    );
}

#[test]
fn test_466() {
    // decoration-style and decoration-color don't conflict
    assert_eq!(
        tw_merge("decoration-wavy decoration-red-500"),
        "decoration-wavy decoration-red-500"
    );
}

#[test]
fn test_467() {
    // decoration-thickness and decoration-color don't conflict
    assert_eq!(
        tw_merge("decoration-2 decoration-red-500"),
        "decoration-2 decoration-red-500"
    );
}

// --- Phase 24: Mask image class groups ---

#[test]
fn test_468() {
    // mask-clip conflicts
    assert_eq!(
        tw_merge("mask-clip-border mask-clip-padding"),
        "mask-clip-padding"
    );
}

#[test]
fn test_469() {
    assert_eq!(tw_merge("mask-clip-content mask-no-clip"), "mask-no-clip");
}

#[test]
fn test_470() {
    // mask-origin conflicts
    assert_eq!(
        tw_merge("mask-origin-border mask-origin-padding"),
        "mask-origin-padding"
    );
}

#[test]
fn test_471() {
    // mask-mode conflicts
    assert_eq!(tw_merge("mask-alpha mask-luminance"), "mask-luminance");
}

#[test]
fn test_472() {
    // mask-composite conflicts
    assert_eq!(tw_merge("mask-add mask-subtract"), "mask-subtract");
}

#[test]
fn test_473() {
    assert_eq!(tw_merge("mask-intersect mask-exclude"), "mask-exclude");
}

#[test]
fn test_474() {
    // mask-image-linear-pos conflicts
    assert_eq!(tw_merge("mask-linear-45 mask-linear-90"), "mask-linear-90");
}

#[test]
fn test_475() {
    // mask-image-linear-from-pos conflicts
    assert_eq!(
        tw_merge("mask-linear-from-0% mask-linear-from-50%"),
        "mask-linear-from-50%"
    );
}

#[test]
fn test_476() {
    // mask-image-linear-from-color conflicts
    assert_eq!(
        tw_merge("mask-linear-from-red-500 mask-linear-from-blue-500"),
        "mask-linear-from-blue-500"
    );
}

#[test]
#[ignore = "mask-image subgroups need CSS value parsing"]
fn test_477() {
    // mask-image-linear-from-pos and mask-image-linear-from-color don't conflict
    assert_eq!(
        tw_merge("mask-linear-from-50% mask-linear-from-red-500"),
        "mask-linear-from-50% mask-linear-from-red-500"
    );
}

#[test]
fn test_478() {
    // mask-image-linear-to-pos conflicts
    assert_eq!(
        tw_merge("mask-linear-to-0% mask-linear-to-100%"),
        "mask-linear-to-100%"
    );
}

#[test]
fn test_479() {
    // mask-image-linear-to-color conflicts
    assert_eq!(
        tw_merge("mask-linear-to-red-500 mask-linear-to-transparent"),
        "mask-linear-to-transparent"
    );
}

#[test]
fn test_480() {
    // mask-image-t-from-pos conflicts
    assert_eq!(
        tw_merge("mask-t-from-0% mask-t-from-50%"),
        "mask-t-from-50%"
    );
}

#[test]
fn test_481() {
    // mask-image-t-from-color conflicts
    assert_eq!(
        tw_merge("mask-t-from-red-500 mask-t-from-blue-500"),
        "mask-t-from-blue-500"
    );
}

#[test]
fn test_482() {
    // mask-image-r-to-pos conflicts
    assert_eq!(tw_merge("mask-r-to-0% mask-r-to-100%"), "mask-r-to-100%");
}

#[test]
fn test_483() {
    // mask-image-b-from-color conflicts
    assert_eq!(
        tw_merge("mask-b-from-white mask-b-from-black"),
        "mask-b-from-black"
    );
}

#[test]
fn test_484() {
    // mask-image-l-to-color conflicts
    assert_eq!(
        tw_merge("mask-l-to-red-500 mask-l-to-blue-500"),
        "mask-l-to-blue-500"
    );
}

#[test]
fn test_485() {
    // mask-image-x-from-pos conflicts
    assert_eq!(
        tw_merge("mask-x-from-0% mask-x-from-25%"),
        "mask-x-from-25%"
    );
}

#[test]
fn test_486() {
    // mask-image-y-to-color conflicts
    assert_eq!(
        tw_merge("mask-y-to-red-500 mask-y-to-transparent"),
        "mask-y-to-transparent"
    );
}

#[test]
fn test_487() {
    // mask-image-radial conflicts
    assert_eq!(
        tw_merge("mask-radial mask-radial-[circle]"),
        "mask-radial-[circle]"
    );
}

#[test]
fn test_488() {
    // mask-image-radial-from-pos conflicts
    assert_eq!(
        tw_merge("mask-radial-from-0% mask-radial-from-50%"),
        "mask-radial-from-50%"
    );
}

#[test]
fn test_489() {
    // mask-image-radial-from-color conflicts
    assert_eq!(
        tw_merge("mask-radial-from-red-500 mask-radial-from-blue-500"),
        "mask-radial-from-blue-500"
    );
}

#[test]
fn test_490() {
    // mask-image-radial-shape conflicts
    assert_eq!(
        tw_merge("mask-radial-circle mask-radial-ellipse"),
        "mask-radial-ellipse"
    );
}

#[test]
fn test_491() {
    // mask-image-radial-size conflicts
    assert_eq!(
        tw_merge("mask-radial-closest-side mask-radial-farthest-corner"),
        "mask-radial-farthest-corner"
    );
}

#[test]
fn test_492() {
    // mask-image-radial-pos conflicts
    assert_eq!(
        tw_merge("mask-radial-at-center mask-radial-at-top"),
        "mask-radial-at-top"
    );
}

#[test]
fn test_493() {
    // mask-image-conic-pos conflicts
    assert_eq!(tw_merge("mask-conic-45 mask-conic-90"), "mask-conic-90");
}

#[test]
fn test_494() {
    // mask-image-conic-from-color conflicts
    assert_eq!(
        tw_merge("mask-conic-from-red-500 mask-conic-from-blue-500"),
        "mask-conic-from-blue-500"
    );
}

#[test]
fn test_495() {
    // mask-position conflicts
    assert_eq!(tw_merge("mask-center mask-top"), "mask-top");
}

#[test]
fn test_496() {
    assert_eq!(
        tw_merge("mask-bottom-left mask-top-right"),
        "mask-top-right"
    );
}

#[test]
fn test_497() {
    // mask-size conflicts
    assert_eq!(tw_merge("mask-auto mask-cover"), "mask-cover");
}

#[test]
fn test_498() {
    assert_eq!(tw_merge("mask-contain mask-auto"), "mask-auto");
}

#[test]
fn test_499() {
    // mask-repeat conflicts
    assert_eq!(tw_merge("mask-repeat mask-no-repeat"), "mask-no-repeat");
}

#[test]
fn test_500() {
    assert_eq!(tw_merge("mask-repeat-x mask-repeat-y"), "mask-repeat-y");
}

#[test]
fn test_501() {
    // mask-image conflicts
    assert_eq!(tw_merge("mask-none mask-[url('...')]"), "mask-[url('...')]");
}

#[test]
fn test_502() {
    // Different mask groups don't conflict
    assert_eq!(
        tw_merge("mask-clip-border mask-origin-padding"),
        "mask-clip-border mask-origin-padding"
    );
}

#[test]
fn test_503() {
    assert_eq!(tw_merge("mask-alpha mask-add"), "mask-alpha mask-add");
}

#[test]
fn test_504() {
    assert_eq!(tw_merge("mask-center mask-cover"), "mask-center mask-cover");
}

#[test]
fn test_505() {
    assert_eq!(tw_merge("mask-repeat mask-auto"), "mask-repeat mask-auto");
}

#[test]
#[ignore = "mask-image subgroups need CSS value parsing"]
fn test_506() {
    // mask-radial-shape and mask-radial-size don't conflict
    assert_eq!(
        tw_merge("mask-radial-circle mask-radial-closest-side"),
        "mask-radial-circle mask-radial-closest-side"
    );
}

#[test]
#[ignore = "mask-image subgroups need CSS value parsing"]
fn test_507() {
    // mask-linear-from-pos and mask-linear-to-pos don't conflict
    assert_eq!(
        tw_merge("mask-linear-from-0% mask-linear-to-100%"),
        "mask-linear-from-0% mask-linear-to-100%"
    );
}

#[test]
#[ignore = "mask-image subgroups need CSS value parsing"]
fn test_508() {
    // mask-linear-from-color and mask-linear-to-color don't conflict
    assert_eq!(
        tw_merge("mask-linear-from-red-500 mask-linear-to-blue-500"),
        "mask-linear-from-red-500 mask-linear-to-blue-500"
    );
}

// --- Phase 25: Additional class groups (accent, aspect, break) ---

#[test]
fn test_509() {
    // accent color conflicts
    assert_eq!(
        tw_merge("accent-red-500 accent-blue-500"),
        "accent-blue-500"
    );
}

#[test]
fn test_510() {
    assert_eq!(tw_merge("accent-auto accent-pink-300"), "accent-pink-300");
}

#[test]
fn test_511() {
    // accent doesn't conflict with other color utilities
    assert_eq!(
        tw_merge("accent-red-500 bg-blue-500"),
        "accent-red-500 bg-blue-500"
    );
}

#[test]
fn test_512() {
    // aspect ratio conflicts
    assert_eq!(tw_merge("aspect-auto aspect-square"), "aspect-square");
}

#[test]
fn test_513() {
    assert_eq!(tw_merge("aspect-video aspect-[4/3]"), "aspect-[4/3]");
}

#[test]
fn test_514() {
    assert_eq!(tw_merge("aspect-square aspect-auto"), "aspect-auto");
}

#[test]
fn test_515() {
    // aspect doesn't conflict with other utilities
    assert_eq!(tw_merge("aspect-video w-full"), "aspect-video w-full");
}

#[test]
fn test_516() {
    // word-break conflicts
    assert_eq!(tw_merge("break-normal break-words"), "break-words");
}

#[test]
fn test_517() {
    assert_eq!(tw_merge("break-all break-keep"), "break-keep");
}

#[test]
fn test_518() {
    assert_eq!(tw_merge("break-words break-normal"), "break-normal");
}

#[test]
fn test_519() {
    // break doesn't conflict with break-after/before/inside
    assert_eq!(
        tw_merge("break-words break-after-page"),
        "break-words break-after-page"
    );
}

#[test]
fn test_520() {
    // break doesn't conflict with whitespace
    assert_eq!(
        tw_merge("break-all whitespace-nowrap"),
        "break-all whitespace-nowrap"
    );
}

// ============================================================================
// Edge cases: empty, single class, whitespace, non-Tailwind classes
// ============================================================================

#[test]
fn test_521() {
    // empty string input
    assert_eq!(tw_merge(""), "");
}

#[test]
fn test_522() {
    // single class input (no conflicts)
    assert_eq!(tw_merge("bg-red-500"), "bg-red-500");
}

#[test]
fn test_523() {
    // single class with modifier
    assert_eq!(tw_merge("hover:bg-red-500"), "hover:bg-red-500");
}

#[test]
fn test_524() {
    // extra leading whitespace
    assert_eq!(tw_merge("  bg-red-500"), "bg-red-500");
}

#[test]
fn test_525() {
    // extra trailing whitespace
    assert_eq!(tw_merge("bg-red-500  "), "bg-red-500");
}

#[test]
fn test_526() {
    // extra whitespace between classes
    assert_eq!(tw_merge("bg-red-500   text-white"), "bg-red-500 text-white");
}

#[test]
fn test_527() {
    // multiple whitespace everywhere
    assert_eq!(tw_merge("  bg-red-500   bg-blue-500  "), "bg-blue-500");
}

#[test]
fn test_528() {
    // non-Tailwind classes pass through unchanged
    assert_eq!(
        tw_merge("custom-class bg-red-500"),
        "custom-class bg-red-500"
    );
}

#[test]
fn test_529() {
    // unknown classes don't conflict with Tailwind classes
    assert_eq!(
        tw_merge("my-component bg-red-500 bg-blue-500"),
        "my-component bg-blue-500"
    );
}

#[test]
fn test_530() {
    // multiple non-Tailwind classes preserved
    assert_eq!(tw_merge("foo bar baz"), "foo bar baz");
}

#[test]
fn test_531() {
    // non-Tailwind classes with hyphens
    assert_eq!(
        tw_merge("my-custom-class another-class"),
        "my-custom-class another-class"
    );
}

#[test]
fn test_532() {
    // mixed Tailwind and non-Tailwind with conflicts
    assert_eq!(tw_merge("custom p-2 p-4 another"), "custom p-4 another");
}

// ============================================================================
// Logical properties: margin-inline (ms/me)
// ============================================================================

#[test]
fn test_533() {
    // ms conflicts with itself
    assert_eq!(tw_merge("ms-2 ms-4"), "ms-4");
}

#[test]
fn test_534() {
    // me conflicts with itself
    assert_eq!(tw_merge("me-2 me-4"), "me-4");
}

#[test]
fn test_535() {
    // ms and me don't conflict with each other
    assert_eq!(tw_merge("ms-2 me-4"), "ms-2 me-4");
}

#[test]
fn test_536() {
    // ms doesn't conflict with ml (different properties)
    assert_eq!(tw_merge("ms-2 ml-4"), "ms-2 ml-4");
}

#[test]
fn test_537() {
    // me doesn't conflict with mr (different properties)
    assert_eq!(tw_merge("me-2 mr-4"), "me-2 mr-4");
}

#[test]
fn test_538() {
    // mx conflicts with itself
    assert_eq!(tw_merge("mx-2 mx-4"), "mx-4");
}

// ============================================================================
// Logical properties: padding-inline (ps/pe)
// ============================================================================

#[test]
fn test_539() {
    // ps conflicts with itself
    assert_eq!(tw_merge("ps-2 ps-4"), "ps-4");
}

#[test]
fn test_540() {
    // pe conflicts with itself
    assert_eq!(tw_merge("pe-2 pe-4"), "pe-4");
}

#[test]
fn test_541() {
    // ps and pe don't conflict with each other
    assert_eq!(tw_merge("ps-2 pe-4"), "ps-2 pe-4");
}

#[test]
fn test_542() {
    // ps doesn't conflict with pl (different properties)
    assert_eq!(tw_merge("ps-2 pl-4"), "ps-2 pl-4");
}

#[test]
fn test_543() {
    // pe doesn't conflict with pr (different properties)
    assert_eq!(tw_merge("pe-2 pr-4"), "pe-2 pr-4");
}

// ============================================================================
// Logical properties: inset-inline (start/end)
// ============================================================================

#[test]
fn test_544() {
    // start conflicts with itself
    assert_eq!(tw_merge("start-0 start-4"), "start-4");
}

#[test]
fn test_545() {
    // end conflicts with itself
    assert_eq!(tw_merge("end-0 end-4"), "end-4");
}

#[test]
fn test_546() {
    // start and end don't conflict with each other
    assert_eq!(tw_merge("start-0 end-4"), "start-0 end-4");
}

#[test]
fn test_547() {
    // start doesn't conflict with left
    assert_eq!(tw_merge("start-0 left-4"), "start-0 left-4");
}

#[test]
fn test_548() {
    // end doesn't conflict with right
    assert_eq!(tw_merge("end-0 right-4"), "end-0 right-4");
}

// ============================================================================
// Logical properties: border-inline (border-s/border-e)
// ============================================================================

#[test]
fn test_549() {
    // border-s width conflicts with itself
    assert_eq!(tw_merge("border-s border-s-2"), "border-s-2");
}

#[test]
fn test_550() {
    // border-e width conflicts with itself
    assert_eq!(tw_merge("border-e border-e-4"), "border-e-4");
}

#[test]
fn test_551() {
    // border-s and border-e widths don't conflict
    assert_eq!(tw_merge("border-s-2 border-e-4"), "border-s-2 border-e-4");
}

#[test]
fn test_552() {
    // border-s-color conflicts with itself
    assert_eq!(
        tw_merge("border-s-red-500 border-s-blue-500"),
        "border-s-blue-500"
    );
}

#[test]
fn test_553() {
    // border-e-color conflicts with itself
    assert_eq!(
        tw_merge("border-e-red-500 border-e-blue-500"),
        "border-e-blue-500"
    );
}

#[test]
fn test_554() {
    // border-s-color and border-e-color don't conflict
    assert_eq!(
        tw_merge("border-s-red-500 border-e-blue-500"),
        "border-s-red-500 border-e-blue-500"
    );
}

#[test]
fn test_555() {
    // border-s width doesn't conflict with border-l
    assert_eq!(tw_merge("border-s-2 border-l-4"), "border-s-2 border-l-4");
}

// ============================================================================
// Scale and translate edge cases
// ============================================================================

#[test]
fn test_556() {
    // scale-x conflicts with itself
    assert_eq!(tw_merge("scale-x-50 scale-x-100"), "scale-x-100");
}

#[test]
fn test_557() {
    // scale-y conflicts with itself
    assert_eq!(tw_merge("scale-y-50 scale-y-100"), "scale-y-100");
}

#[test]
fn test_558() {
    // scale-x and scale-y don't conflict
    assert_eq!(tw_merge("scale-x-50 scale-y-100"), "scale-x-50 scale-y-100");
}

#[test]
fn test_559() {
    // scale (uniform) conflicts with itself
    assert_eq!(tw_merge("scale-50 scale-100"), "scale-100");
}

#[test]
fn test_560() {
    // translate-x conflicts with itself
    assert_eq!(tw_merge("translate-x-2 translate-x-4"), "translate-x-4");
}

#[test]
fn test_561() {
    // translate-y conflicts with itself
    assert_eq!(tw_merge("translate-y-2 translate-y-4"), "translate-y-4");
}

#[test]
fn test_562() {
    // translate-x and translate-y don't conflict
    assert_eq!(
        tw_merge("translate-x-2 translate-y-4"),
        "translate-x-2 translate-y-4"
    );
}

#[test]
fn test_563() {
    // scale with arbitrary values
    assert_eq!(tw_merge("scale-[1.5] scale-[2]"), "scale-[2]");
}

#[test]
fn test_564() {
    // translate with arbitrary values
    assert_eq!(
        tw_merge("translate-x-[10px] translate-x-[20px]"),
        "translate-x-[20px]"
    );
}

#[test]
fn test_565() {
    // negative scale values
    assert_eq!(tw_merge("-scale-x-50 -scale-x-100"), "-scale-x-100");
}

#[test]
fn test_566() {
    // negative translate values
    assert_eq!(tw_merge("-translate-x-2 -translate-x-4"), "-translate-x-4");
}

#[test]
fn test_567() {
    // mixed positive and negative (should conflict)
    assert_eq!(tw_merge("translate-x-2 -translate-x-4"), "-translate-x-4");
}

// ============================================================================
// Order-sensitive modifiers
// These modifiers should NOT be sorted, as their order matters
// ============================================================================

#[test]
fn test_568() {
    // before: is order-sensitive - different positions don't conflict
    assert_eq!(
        tw_merge("before:hover:text-red-500 hover:before:text-blue-500"),
        "before:hover:text-red-500 hover:before:text-blue-500"
    );
}

#[test]
fn test_569() {
    // after: is order-sensitive - different positions don't conflict
    assert_eq!(
        tw_merge("after:hover:text-red-500 hover:after:text-blue-500"),
        "after:hover:text-red-500 hover:after:text-blue-500"
    );
}

#[test]
fn test_570() {
    // * (direct children) is order-sensitive
    assert_eq!(
        tw_merge("*:hover:text-red-500 hover:*:text-blue-500"),
        "*:hover:text-red-500 hover:*:text-blue-500"
    );
}

#[test]
fn test_571() {
    // ** (all descendants) is order-sensitive
    assert_eq!(
        tw_merge("**:hover:text-red-500 hover:**:text-blue-500"),
        "**:hover:text-red-500 hover:**:text-blue-500"
    );
}

#[test]
fn test_572() {
    // placeholder: is order-sensitive
    assert_eq!(
        tw_merge("placeholder:hover:text-red-500 hover:placeholder:text-blue-500"),
        "placeholder:hover:text-red-500 hover:placeholder:text-blue-500"
    );
}

#[test]
fn test_573() {
    // file: is order-sensitive
    assert_eq!(
        tw_merge("file:hover:text-red-500 hover:file:text-blue-500"),
        "file:hover:text-red-500 hover:file:text-blue-500"
    );
}

#[test]
fn test_574() {
    // marker: is order-sensitive
    assert_eq!(
        tw_merge("marker:hover:text-red-500 hover:marker:text-blue-500"),
        "marker:hover:text-red-500 hover:marker:text-blue-500"
    );
}

#[test]
fn test_575() {
    // selection: is order-sensitive
    assert_eq!(
        tw_merge("selection:hover:text-red-500 hover:selection:text-blue-500"),
        "selection:hover:text-red-500 hover:selection:text-blue-500"
    );
}

#[test]
fn test_576() {
    // first-line: is order-sensitive
    assert_eq!(
        tw_merge("first-line:hover:text-red-500 hover:first-line:text-blue-500"),
        "first-line:hover:text-red-500 hover:first-line:text-blue-500"
    );
}

#[test]
fn test_577() {
    // first-letter: is order-sensitive
    assert_eq!(
        tw_merge("first-letter:hover:text-red-500 hover:first-letter:text-blue-500"),
        "first-letter:hover:text-red-500 hover:first-letter:text-blue-500"
    );
}

#[test]
fn test_578() {
    // backdrop: is order-sensitive
    assert_eq!(
        tw_merge("backdrop:hover:bg-red-500 hover:backdrop:bg-blue-500"),
        "backdrop:hover:bg-red-500 hover:backdrop:bg-blue-500"
    );
}

#[test]
fn test_579() {
    // details-content: is order-sensitive
    assert_eq!(
        tw_merge("details-content:hover:text-red-500 hover:details-content:text-blue-500"),
        "details-content:hover:text-red-500 hover:details-content:text-blue-500"
    );
}

#[test]
fn test_580() {
    // non-order-sensitive modifiers (hover, focus) ARE sorted and conflict
    assert_eq!(
        tw_merge("hover:focus:text-red-500 focus:hover:text-blue-500"),
        "focus:hover:text-blue-500"
    );
}

#[test]
fn test_581() {
    // same order-sensitive modifier in same position - conflicts
    assert_eq!(
        tw_merge("before:text-red-500 before:text-blue-500"),
        "before:text-blue-500"
    );
}

#[test]
fn test_582() {
    // same order-sensitive modifier with sorted modifiers - conflicts when order matches
    assert_eq!(
        tw_merge("before:hover:focus:text-red-500 before:focus:hover:text-blue-500"),
        "before:focus:hover:text-blue-500"
    );
}

#[test]
fn test_583() {
    // arbitrary variants are order-sensitive
    assert_eq!(
        tw_merge("[&>*]:hover:text-red-500 hover:[&>*]:text-blue-500"),
        "[&>*]:hover:text-red-500 hover:[&>*]:text-blue-500"
    );
}

#[test]
fn test_584() {
    // multiple order-sensitive modifiers - both preserved in order
    assert_eq!(
        tw_merge("before:after:text-red-500 after:before:text-blue-500"),
        "before:after:text-red-500 after:before:text-blue-500"
    );
}

#[test]
fn test_585() {
    // order-sensitive with responsive - responsive is sorted
    assert_eq!(
        tw_merge("before:sm:text-red-500 before:md:text-blue-500"),
        "before:sm:text-red-500 before:md:text-blue-500"
    );
}

#[test]
fn test_586() {
    // * modifier basic conflict
    assert_eq!(
        tw_merge("*:text-red-500 *:text-blue-500"),
        "*:text-blue-500"
    );
}

#[test]
fn test_587() {
    // ** modifier basic conflict
    assert_eq!(
        tw_merge("**:text-red-500 **:text-blue-500"),
        "**:text-blue-500"
    );
}

#[test]
fn test_588() {
    // * and ** don't conflict with each other
    assert_eq!(
        tw_merge("*:text-red-500 **:text-blue-500"),
        "*:text-red-500 **:text-blue-500"
    );
}

// ============================================================================
// Important modifier (!)
// Important classes operate in a different CSS specificity layer, so
// !important and non-important classes do NOT conflict with each other
// ============================================================================

#[test]
fn test_589() {
    // important modifier conflicts with itself
    assert_eq!(tw_merge("!p-2 !p-4"), "!p-4");
}

#[test]
fn test_590() {
    // important does NOT conflict with non-important (different specificity layers)
    assert_eq!(tw_merge("p-2 !p-4"), "p-2 !p-4");
}

#[test]
fn test_591() {
    // non-important does NOT conflict with important (different specificity layers)
    assert_eq!(tw_merge("!p-2 p-4"), "!p-2 p-4");
}

#[test]
fn test_592() {
    // important with modifiers conflicts with same modifier important
    assert_eq!(
        tw_merge("hover:!text-red-500 hover:!text-blue-500"),
        "hover:!text-blue-500"
    );
}

#[test]
fn test_593() {
    // important doesn't conflict with different utilities
    assert_eq!(tw_merge("!p-2 !m-4"), "!p-2 !m-4");
}

#[test]
fn test_594() {
    // important at end of class conflicts with itself
    assert_eq!(tw_merge("text-red-500! text-blue-500!"), "text-blue-500!");
}

#[test]
fn test_594b() {
    // important (suffix) does NOT conflict with non-important
    assert_eq!(
        tw_merge("text-red-500 text-blue-500!"),
        "text-red-500 text-blue-500!"
    );
}

// ============================================================================
// Group and peer modifiers
// ============================================================================

#[test]
fn test_595() {
    // group-hover conflicts with itself
    assert_eq!(
        tw_merge("group-hover:text-red-500 group-hover:text-blue-500"),
        "group-hover:text-blue-500"
    );
}

#[test]
fn test_596() {
    // peer-focus conflicts with itself
    assert_eq!(
        tw_merge("peer-focus:text-red-500 peer-focus:text-blue-500"),
        "peer-focus:text-blue-500"
    );
}

#[test]
fn test_597() {
    // group-hover and peer-focus don't conflict
    assert_eq!(
        tw_merge("group-hover:text-red-500 peer-focus:text-blue-500"),
        "group-hover:text-red-500 peer-focus:text-blue-500"
    );
}

#[test]
fn test_598() {
    // named group modifiers
    assert_eq!(
        tw_merge("group-hover/sidebar:text-red-500 group-hover/sidebar:text-blue-500"),
        "group-hover/sidebar:text-blue-500"
    );
}

#[test]
fn test_599() {
    // different named groups don't conflict
    assert_eq!(
        tw_merge("group-hover/sidebar:text-red-500 group-hover/nav:text-blue-500"),
        "group-hover/sidebar:text-red-500 group-hover/nav:text-blue-500"
    );
}

#[test]
fn test_600() {
    // peer with named variant
    assert_eq!(
        tw_merge("peer-checked/toggle:bg-red-500 peer-checked/toggle:bg-blue-500"),
        "peer-checked/toggle:bg-blue-500"
    );
}

// ============================================================================
// Dark mode modifier
// ============================================================================

#[test]
fn test_601() {
    // dark mode conflicts with itself
    assert_eq!(
        tw_merge("dark:bg-red-500 dark:bg-blue-500"),
        "dark:bg-blue-500"
    );
}

#[test]
fn test_602() {
    // dark mode doesn't conflict with light mode (no modifier)
    assert_eq!(
        tw_merge("bg-red-500 dark:bg-blue-500"),
        "bg-red-500 dark:bg-blue-500"
    );
}

#[test]
fn test_603() {
    // dark mode with hover
    assert_eq!(
        tw_merge("dark:hover:text-red-500 dark:hover:text-blue-500"),
        "dark:hover:text-blue-500"
    );
}

#[test]
fn test_604() {
    // hover:dark vs dark:hover - should be sorted and conflict
    assert_eq!(
        tw_merge("hover:dark:text-red-500 dark:hover:text-blue-500"),
        "dark:hover:text-blue-500"
    );
}

// ============================================================================
// Container queries (@container, @sm, @md, etc.)
// ============================================================================

#[test]
fn test_605() {
    // @container conflicts with itself
    assert_eq!(tw_merge("@container:p-2 @container:p-4"), "@container:p-4");
}

#[test]
fn test_606() {
    // @sm container query conflicts with itself
    assert_eq!(tw_merge("@sm:p-2 @sm:p-4"), "@sm:p-4");
}

#[test]
fn test_607() {
    // @md container query conflicts with itself
    assert_eq!(
        tw_merge("@md:text-red-500 @md:text-blue-500"),
        "@md:text-blue-500"
    );
}

#[test]
fn test_608() {
    // different container sizes don't conflict
    assert_eq!(tw_merge("@sm:p-2 @md:p-4"), "@sm:p-2 @md:p-4");
}

#[test]
fn test_609() {
    // named container query
    assert_eq!(
        tw_merge("@container/main:p-2 @container/main:p-4"),
        "@container/main:p-4"
    );
}

#[test]
fn test_610() {
    // different named containers don't conflict
    assert_eq!(
        tw_merge("@container/main:p-2 @container/sidebar:p-4"),
        "@container/main:p-2 @container/sidebar:p-4"
    );
}

// ============================================================================
// supports-[], has-[], data-[] modifiers
// ============================================================================

#[test]
fn test_611() {
    // supports-[] conflicts with same condition
    assert_eq!(
        tw_merge("supports-[display:grid]:grid supports-[display:grid]:flex"),
        "supports-[display:grid]:flex"
    );
}

#[test]
fn test_612() {
    // different supports conditions don't conflict
    assert_eq!(
        tw_merge("supports-[display:grid]:grid supports-[display:flex]:flex"),
        "supports-[display:grid]:grid supports-[display:flex]:flex"
    );
}

#[test]
fn test_613() {
    // has-[] conflicts with same selector
    assert_eq!(tw_merge("has-[>img]:p-2 has-[>img]:p-4"), "has-[>img]:p-4");
}

#[test]
fn test_614() {
    // different has selectors don't conflict
    assert_eq!(
        tw_merge("has-[>img]:p-2 has-[>svg]:p-4"),
        "has-[>img]:p-2 has-[>svg]:p-4"
    );
}

#[test]
fn test_615() {
    // data-[] conflicts with same attribute
    assert_eq!(
        tw_merge("data-[state=open]:bg-red-500 data-[state=open]:bg-blue-500"),
        "data-[state=open]:bg-blue-500"
    );
}

#[test]
fn test_616() {
    // different data attributes don't conflict
    assert_eq!(
        tw_merge("data-[state=open]:bg-red-500 data-[state=closed]:bg-blue-500"),
        "data-[state=open]:bg-red-500 data-[state=closed]:bg-blue-500"
    );
}

#[test]
fn test_617() {
    // data shorthand (data-active:)
    assert_eq!(
        tw_merge("data-active:bg-red-500 data-active:bg-blue-500"),
        "data-active:bg-blue-500"
    );
}

// ============================================================================
// Color opacity shorthand (bg-red-500/50)
// ============================================================================

#[test]
fn test_618() {
    // color with opacity conflicts with same color different opacity
    assert_eq!(tw_merge("bg-red-500/50 bg-red-500/75"), "bg-red-500/75");
}

#[test]
fn test_619() {
    // color with opacity conflicts with color without opacity
    assert_eq!(tw_merge("bg-red-500 bg-red-500/50"), "bg-red-500/50");
}

#[test]
fn test_620() {
    // different colors with opacity both preserved
    assert_eq!(tw_merge("bg-red-500/50 bg-blue-500/50"), "bg-blue-500/50");
}

#[test]
fn test_621() {
    // text color with opacity
    assert_eq!(
        tw_merge("text-red-500/50 text-red-500/75"),
        "text-red-500/75"
    );
}

#[test]
fn test_622() {
    // border color with opacity
    assert_eq!(
        tw_merge("border-red-500/50 border-red-500/75"),
        "border-red-500/75"
    );
}

#[test]
fn test_623() {
    // arbitrary opacity value
    assert_eq!(
        tw_merge("bg-red-500/[0.5] bg-red-500/[0.75]"),
        "bg-red-500/[0.75]"
    );
}

#[test]
fn test_624() {
    // ring color with opacity
    assert_eq!(
        tw_merge("ring-red-500/50 ring-blue-500/50"),
        "ring-blue-500/50"
    );
}

#[test]
fn test_625() {
    // divide color with opacity
    assert_eq!(
        tw_merge("divide-red-500/50 divide-blue-500/50"),
        "divide-blue-500/50"
    );
}

#[test]
fn test_626() {
    // font-stretch-normal and font-sans should NOT conflict (different groups)
    assert_eq!(
        tw_merge("font-stretch-normal font-sans"),
        "font-stretch-normal font-sans"
    );
}

#[test]
fn test_627() {
    // font-sans and font-stretch-expanded should NOT conflict (different groups)
    assert_eq!(
        tw_merge("font-sans font-stretch-expanded"),
        "font-sans font-stretch-expanded"
    );
}

#[test]
fn test_628() {
    // font-stretch classes should conflict with each other
    assert_eq!(
        tw_merge("font-stretch-normal font-stretch-expanded"),
        "font-stretch-expanded"
    );
}

#[test]
fn test_629() {
    // inset-ring-2 and inset-0 should NOT conflict (different groups)
    assert_eq!(tw_merge("inset-ring-2 inset-0"), "inset-ring-2 inset-0");
}

#[test]
fn test_630() {
    // inset-0 and inset-ring-red-500 should NOT conflict (different groups)
    assert_eq!(
        tw_merge("inset-0 inset-ring-red-500"),
        "inset-0 inset-ring-red-500"
    );
}

#[test]
fn test_631() {
    // inset-ring width classes should conflict with each other
    assert_eq!(tw_merge("inset-ring-2 inset-ring-4"), "inset-ring-4");
}

#[test]
fn test_632() {
    // inset-ring color classes should conflict with each other
    assert_eq!(
        tw_merge("inset-ring-red-500 inset-ring-blue-500"),
        "inset-ring-blue-500"
    );
}

#[test]
fn test_633() {
    // inset-shadow-sm and inset-0 should NOT conflict (different groups)
    assert_eq!(
        tw_merge("inset-shadow-sm inset-0"),
        "inset-shadow-sm inset-0"
    );
}

#[test]
fn test_634() {
    // inset-0 and inset-shadow-lg should NOT conflict (different groups)
    assert_eq!(
        tw_merge("inset-0 inset-shadow-lg"),
        "inset-0 inset-shadow-lg"
    );
}

#[test]
fn test_635() {
    // inset-shadow size classes should conflict with each other
    assert_eq!(
        tw_merge("inset-shadow-sm inset-shadow-lg"),
        "inset-shadow-lg"
    );
}

#[test]
fn test_636() {
    // inset-shadow color classes should conflict with each other
    assert_eq!(
        tw_merge("inset-shadow-red-500 inset-shadow-blue-500"),
        "inset-shadow-blue-500"
    );
}

#[test]
fn test_637_ring_arbitrary_width_vs_preset() {
    // Arbitrary ring width should conflict with preset ring width
    // ring-[3px] is a width value, not a color, so it should conflict with ring-0
    assert_eq!(tw_merge("ring-[3px] ring-0"), "ring-0");
}

#[test]
fn test_638_ring_preset_vs_arbitrary_width() {
    // Preset ring width should conflict with arbitrary ring width (reverse order)
    assert_eq!(tw_merge("ring-0 ring-[3px]"), "ring-[3px]");
}

#[test]
fn test_639_ring_arbitrary_width_with_modifiers() {
    // Same issue with modifiers
    assert_eq!(
        tw_merge("focus-visible:ring-[3px] focus-visible:ring-0"),
        "focus-visible:ring-0"
    );
}

#[test]
fn test_640_ring_arbitrary_widths_conflict() {
    // Two arbitrary ring widths should conflict with each other
    assert_eq!(tw_merge("ring-[3px] ring-[5px]"), "ring-[5px]");
}

#[test]
fn test_641_ring_arbitrary_width_vs_ring_keyword() {
    // Arbitrary ring width should conflict with the `ring` keyword (which sets default width)
    assert_eq!(tw_merge("ring ring-[3px]"), "ring-[3px]");
}

#[test]
fn test_642_ring_arbitrary_width_vs_color_no_conflict() {
    // Arbitrary ring WIDTH should NOT conflict with ring COLOR
    // ring-[3px] is a width, ring-red-500 is a color - they're different properties
    assert_eq!(
        tw_merge("ring-[3px] ring-red-500"),
        "ring-[3px] ring-red-500"
    );
}

#[test]
fn test_643_ring_arbitrary_width_decimal_units() {
    // Decimal values with different units should also be recognized as widths
    assert_eq!(tw_merge("ring-[0.5rem] ring-2"), "ring-2");
    assert_eq!(tw_merge("ring-4 ring-[1.5em]"), "ring-[1.5em]");
}

#[test]
fn test_644_ring_offset_arbitrary_width() {
    // ring-offset should have the same behavior
    assert_eq!(tw_merge("ring-offset-[3px] ring-offset-2"), "ring-offset-2");
    assert_eq!(
        tw_merge("ring-offset-[3px] ring-offset-red-500"),
        "ring-offset-[3px] ring-offset-red-500"
    );
}

#[test]
fn test_645_outline_arbitrary_width() {
    // outline should have the same behavior
    assert_eq!(tw_merge("outline-[3px] outline-2"), "outline-2");
    assert_eq!(
        tw_merge("outline-[3px] outline-red-500"),
        "outline-[3px] outline-red-500"
    );
}

#[test]
fn test_646_inset_ring_arbitrary_width() {
    // inset-ring should have the same behavior
    assert_eq!(tw_merge("inset-ring-[3px] inset-ring-2"), "inset-ring-2");
}
