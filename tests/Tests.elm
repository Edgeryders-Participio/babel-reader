module Tests exposing (suite)

-- These are only vendored tests from "html-parser"

import Dict
import Expect exposing (Expectation)
import Html.Parser exposing (Node(..))
import ParserUtil
import Test exposing (Test, describe, test)


testParseAll : String -> List Node -> (() -> Expectation)
testParseAll s astList =
    \_ ->
        Expect.equal (Ok astList) (Html.Parser.run s)


testParse : String -> Node -> (() -> Expectation)
testParse s ast =
    testParseAll s [ ast ]


testParseOk : String -> (() -> Expectation)
testParseOk s =
    \_ ->
        Html.Parser.run s
            |> Result.mapError (\e -> ParserUtil.deadEndsToString e s)
            |> Expect.ok


testError : String -> (() -> Expectation)
testError s =
    \_ ->
        let
            failed =
                case Html.Parser.run s of
                    Ok _ ->
                        False

                    Err _ ->
                        True
        in
        Expect.true s failed


textNodeTests : Test
textNodeTests =
    describe "TextNode"
        [ test "empty" (testParseAll "" [])
        , test "space" (testParse " " (Text " "))
        , test "basic1" (testParse "1" (Text "1"))
        , test "basic2" (testParse "a" (Text "a"))
        , test "basic3" (testParse "1a" (Text "1a"))
        , test "basic4" (testParse "^" (Text "^"))
        , test "decode1" (testParse "&" (Text "&"))
        , test "decode2" (testParse "&amp;" (Text "&"))
        , test "decode3" (testParse "&lt;" (Text "<"))
        , test "decode4" (testParse "&gt;" (Text ">"))
        , test "decode6" (testParse "&apos;" (Text "'"))
        , test "decode7" (testParse "&#38;" (Text "&"))
        , test "decode8" (testParse "&#x26;" (Text "&"))
        , test "decode9" (testParse "&#x3E;" (Text ">"))
        , test "decodeA" (testParse "&#383;" (Text "ſ"))
        , test "decodeB" (testParse "&nbsp;" (Text "\u{00A0}"))
        , test "decodeC" (testParse "&nbsp;&nbsp;" (Text "\u{00A0}\u{00A0}"))
        , test "decodeD" (testParse "a&nbsp;b" (Text "a\u{00A0}b"))
        , test "decodeE" (testParse "a&nbsp;&nbsp;b" (Text "a\u{00A0}\u{00A0}b"))
        , test "decodeF" (testParse """<img alt="&lt;">""" (Element "img" [ ( "alt", "<" ) ] []))
        , test "decodeG" (testParse "&#0038;" (Text "&"))
        ]


nodeTests : Test
nodeTests =
    describe "Node"
        [ test "basic1" (testParse "<a></a>" (Element "a" [] []))
        , test "basic2" (testParse "<a></a >" (Element "a" [] []))
        , test "basic3" (testParse "<A></A >" (Element "a" [] []))
        , test "basic4" (testParseAll " <a></a> " [ Text " ", Element "a" [] [], Text " " ])
        , test "basic5" (testParseAll "a<a></a>b" [ Text "a", Element "a" [] [], Text "b" ])
        , test "basic6" (testParse "<A></A>" (Element "a" [] []))
        , test "basic7" (testParse "<a>a</a>" (Element "a" [] [ Text "a" ]))
        , test "basic8" (testParse "<a> a </a>" (Element "a" [] [ Text " a " ]))
        , test "basic10" (testParse "<br>" (Element "br" [] []))
        , test "basic11" (testParse "<a><a></a></a>" (Element "a" [] [ Element "a" [] [] ]))
        , test "basic12" (testParse "<a> <a> </a> </a>" (Element "a" [] [ Text " ", Element "a" [] [ Text " " ], Text " " ]))
        , test "basic13" (testParse "<a> <br> </a>" (Element "a" [] [ Text " ", Element "br" [] [], Text " " ]))
        , test "basic14" (testParse "<a><a></a><a></a></a>" (Element "a" [] [ Element "a" [] [], Element "a" [] [] ]))
        , test "basic15" (testParse "<a><a><a></a></a></a>" (Element "a" [] [ Element "a" [] [ Element "a" [] [] ] ]))
        , test "basic16" (testParse "<a><a></a><b></b></a>" (Element "a" [] [ Element "a" [] [], Element "b" [] [] ]))
        , test "basic17" (testParse "<h1></h1>" (Element "h1" [] []))
        , test "start-only-tag1" (testParse "<br>" (Element "br" [] []))
        , test "start-only-tag2" (testParse "<BR>" (Element "br" [] []))
        , test "start-only-tag3" (testParse "<br >" (Element "br" [] []))
        , test "start-only-tag4" (testParse "<BR >" (Element "br" [] []))
        , test "start-only-tag5" (testParse "<a> <br> </a>" (Element "a" [] [ Text " ", Element "br" [] [], Text " " ]))
        , test "start-only-tag6" (testParse "<a><br><br></a>" (Element "a" [] [ Element "br" [] [], Element "br" [] [] ]))
        , test "start-only-tag7" (testParse "<a><br><img><hr><meta></a>" (Element "a" [] [ Element "br" [] [], Element "img" [] [], Element "hr" [] [], Element "meta" [] [] ]))
        , test "start-only-tag8" (testParse "<a>foo<br>bar</a>" (Element "a" [] [ Text "foo", Element "br" [] [], Text "bar" ]))
        , test "self-closing-tag1" (testParse "<br/>" (Element "br" [] []))
        , test "self-closing-tag2" (testParse "<br />" (Element "br" [] []))
        , test "self-closing-tag3" (testParse "<link href=\"something\" rel=\"something else\"/>" (Element "link" [ ( "href", "something" ), ( "rel", "something else" ) ] []))
        , test "web-component-tag" (testParse "<a-web-component></a-web-component>" (Element "a-web-component" [] []))
        ]


nodeToStringTests : Test
nodeToStringTests =
    describe "nodeToString"
        [ test "simple link" <|
            \_ ->
                Element "a" [ ( "href", "https://elm-lang.org" ) ] [ Text "Elm" ]
                    |> Html.Parser.nodeToString
                    |> Expect.equal "<a href=\"https://elm-lang.org\">Elm</a>"
        , test "container" <|
            \_ ->
                Element "div"
                    []
                    [ Element "p" [] [ Text "Hello," ]
                    , Element "p" [] [ Text "World!" ]
                    ]
                    |> Html.Parser.nodeToString
                    |> Expect.equal "<div><p>Hello,</p><p>World!</p></div>"
        , test "multiple attributes" <|
            \_ ->
                Element "a"
                    [ ( "href", "https://elm-lang.org" )
                    , ( "alt", "Elm website" )
                    ]
                    [ Text "Elm" ]
                    |> Html.Parser.nodeToString
                    |> Expect.equal "<a href=\"https://elm-lang.org\" alt=\"Elm website\">Elm</a>"
        , test "void element" <|
            \_ ->
                Element "br" [] [ Element "a" [] [ Text "should be ignored" ] ]
                    |> Html.Parser.nodeToString
                    |> Expect.equal "<br>"
        , test "comment" <|
            \_ ->
                Comment "This is a comment"
                    |> Html.Parser.nodeToString
                    |> Expect.equal "<!-- This is a comment -->"
        , test "text" <|
            \_ ->
                Text "Hello, world!"
                    |> Html.Parser.nodeToString
                    |> Expect.equal "Hello, world!"
        ]


scriptTests : Test
scriptTests =
    describe "Script"
        [ test "script1" (testParse """<script></script>""" (Element "script" [] []))
        , test "script2" (testParse """<SCRIPT></SCRIPT>""" (Element "script" [] []))
        , test "script3" (testParse """<script src="script.js">foo</script>""" (Element "script" [ ( "src", "script.js" ) ] [ Text "foo" ]))
        , test "script4" (testParse """<script>var a = 0 < 1; b = 1 > 0;</script>""" (Element "script" [] [ Text "var a = 0 < 1; b = 1 > 0;" ]))
        , test "script5" (testParse """<script><!----></script>""" (Element "script" [] [ Comment "" ]))
        , test "script6" (testParse """<script>a<!--</script><script>-->b</script>""" (Element "script" [] [ Text "a", Comment "</script><script>", Text "b" ]))
        , test "style" (testParse """<style>a<!--</style><style>-->b</style>""" (Element "style" [] [ Text "a", Comment "</style><style>", Text "b" ]))
        ]


commentTests : Test
commentTests =
    describe "Comment"
        [ test "basic1" (testParse """<!---->""" (Comment ""))
        , test "basic2" (testParse """<!--<div></div>-->""" (Comment "<div></div>"))
        , test "basic3" (testParse """<div><!--</div>--></div>""" (Element "div" [] [ Comment "</div>" ]))
        , test "basic4" (testParse """<!--<!---->""" (Comment "<!--"))
        , test "basic5" (testParse """<!--foo\t\u{000D}
        -->""" (Comment "foo\t\u{000D}\n        "))
        ]


attributeTests : Test
attributeTests =
    describe "Attribute"
        [ test "basic1" (testParse """<a href="example.com"></a>""" (Element "a" [ ( "href", "example.com" ) ] []))
        , test "basic2" (testParse """<a href='example.com'></a>""" (Element "a" [ ( "href", "example.com" ) ] []))
        , test "basic3" (testParse """<a href=example.com></a>""" (Element "a" [ ( "href", "example.com" ) ] []))
        , test "basic4" (testParse """<a HREF=example.com></a>""" (Element "a" [ ( "href", "example.com" ) ] []))
        , test "basic5" (testParse """<a href=bare></a>""" (Element "a" [ ( "href", "bare" ) ] []))
        , test "basic6" (testParse """<a href="example.com?a=b&amp;c=d"></a>""" (Element "a" [ ( "href", "example.com?a=b&c=d" ) ] []))
        , test "basic7" (testParse """<a href="example.com?a=b&c=d"></a>""" (Element "a" [ ( "href", "example.com?a=b&c=d" ) ] []))
        , test "basic8" (testParse """<input max=100 min = 10.5>""" (Element "input" [ ( "max", "100" ), ( "min", "10.5" ) ] []))
        , test "basic9" (testParse """<input disabled>""" (Element "input" [ ( "disabled", "" ) ] []))
        , test "basic10" (testParse """<input DISABLED>""" (Element "input" [ ( "disabled", "" ) ] []))
        , test "basic11" (testParse """<meta http-equiv=Content-Type>""" (Element "meta" [ ( "http-equiv", "Content-Type" ) ] []))
        , test "basic12" (testParse """<input data-foo2="a">""" (Element "input" [ ( "data-foo2", "a" ) ] []))
        , test "basic13" (testParse """<html xmlns:v="urn:schemas-microsoft-com:vml"></html>""" (Element "html" [ ( "xmlns:v", "urn:schemas-microsoft-com:vml" ) ] []))
        , test "basic14" (testParse """<link rel=stylesheet
        href="">""" (Element "link" [ ( "rel", "stylesheet" ), ( "href", "" ) ] []))

        -- Invalid attribute names shouldn't be parsed: https://github.com/elm/html/issues/46
        , test "invalid character" (testParse """<p\u{00A0} ></p>""" (Element "p" [] []))
        ]


errorTests : Test
errorTests =
    describe "Errors"
        [ test "invalid closing tag" (testError "<a><br></p>")
        , test "invalid tag name" (testError "<-></->")
        ]


previouslyFailedCases : Test
previouslyFailedCases =
    describe "Fixed bugs"
        [ test "asd" (testParseOk "<p>One of the men in balaclavas is handed a note by the cashier. It said: “you have beautiful eyes.”<div class=\"lightbox-wrapper\"><a class=\"lightbox\" href=\"https://bbu.world/uploads/babelbetweenus/original/1X/1a0079538fe3d953fe221052a169b5d45486c00c.jpeg\" data-download-href=\"https://bbu.world/uploads/babelbetweenus/1a0079538fe3d953fe221052a169b5d45486c00c\" title=\"BEAUTIFUL EYES.jpg\"><img src=\"https://bbu.world/uploads/babelbetweenus/optimized/1X/1a0079538fe3d953fe221052a169b5d45486c00c_2_666x500.jpeg\" alt=\"BEAUTIFUL%20EYES\" data-base62-sha1=\"3I1qDtCCY0r5bJbDX326GIztfPK\" width=\"666\" height=\"500\" srcset=\"https://bbu.world/uploads/babelbetweenus/optimized/1X/1a0079538fe3d953fe221052a169b5d45486c00c_2_666x500.jpeg, https://bbu.world/uploads/babelbetweenus/optimized/1X/1a0079538fe3d953fe221052a169b5d45486c00c_2_999x750.jpeg 1.5x, https://bbu.world/uploads/babelbetweenus/optimized/1X/1a0079538fe3d953fe221052a169b5d45486c00c_2_1332x1000.jpeg 2x\" data-small-upload=\"https://bbu.world/uploads/babelbetweenus/optimized/1X/1a0079538fe3d953fe221052a169b5d45486c00c_2_10x10.png\"><div class=\"meta\">\n<svg class=\"fa d-icon d-icon-far-image svg-icon\" aria-hidden=\"true\"><use xlink:href=\"#far-image\"></use></svg><span class=\"filename\">BEAUTIFUL EYES.jpg</span><span class=\"informations\">4032×3024 2.53 MB</span><svg class=\"fa d-icon d-icon-discourse-expand svg-icon\" aria-hidden=\"true\"><use xlink:href=\"#discourse-expand\"></use></svg>\n</div></a></div></p>\n<p>Her eyes tenderly meet his. He looks back down to the note for some time, then scans back to her face. She beams shyly and pinkly.</p>\n<p>He has never been told such a thing like this before and a prickly flush creeps under the hot black wool on his face. This mixed with the adrenaline of the terror-excitement (terrorcitement?) of his first heist could cause him to malfunction. He can’t have that.</p>\n<p>He screws the paper up in one hand and flicks it in her face. She barely blinks. He aims the gun between her eyes.</p>\n<p>“Bitch, I said put the cash in the bag or I’ll shoot your fucking teeth out!”</p>\n<p>She slowly and delicately picks at the bundles of money under the counter, placing it all in the duffle bag as if she’s lovingly preparing a child’s lunch bag. She’s entered a daydream in which her and the man have gone on a romantic evening. She’s wearing a smashing dress and he makes her twirl in it. She has no idea what his face looks like, so she’s just imagining him in the balaclava still, but wearing a crisply ironed shirt. His hazel eyes remind her of amber in sunlight with the longest, </p>")
        ]


suite : Test
suite =
    describe "HtmlParser"
        [ textNodeTests
        , nodeTests
        , nodeToStringTests
        , commentTests
        , attributeTests
        , errorTests
        , previouslyFailedCases

        --, scriptTests
        ]
