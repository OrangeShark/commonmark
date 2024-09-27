;; Copyright (C) 2024  David Thompson <dave@spritely.institute>
;;
;; This file is part of guile-commonmark
;;
;; guile-commonmark is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; guile-commonmark is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with guile-commonmark.  If not, see <http://www.gnu.org/licenses/>.

(define-module (test-html)
  #:use-module (srfi srfi-64)
  #:use-module (commonmark html))

(define-syntax-rule (test-html html markdown)
  (test-equal html (call-with-input-string markdown commonmark->html)))

(test-begin "node")

;; These tests are straight out of 4.6 of the CommonMark spec.

(test-html
 "<table><tr><td>
<pre>
**Hello**,
<p><em>world</em>.
</pre></p>
</td></tr></table>
"
 "<table><tr><td>
<pre>
**Hello**,

_world_.
</pre>
</td></tr></table>")

(test-html
 "<table>
  <tr>
    <td>
           hi
    </td>
  </tr>
</table>
<p>okay.</p>
"
 "<table>
  <tr>
    <td>
           hi
    </td>
  </tr>
</table>

okay.")

(test-html
 " <div>
  *hello*
         <foo><a>
"
 " <div>
  *hello*
         <foo><a>")

(test-html
 "</div>
*foo*
"
 "</div>
*foo*")

(test-html
 "<DIV CLASS=\"foo\">
<p><em>Markdown</em></p>
</DIV>
"
 "<DIV CLASS=\"foo\">

*Markdown*

</DIV>")

(test-html
 "<div id=\"foo\"
  class=\"bar\">
</div>
"
 "<div id=\"foo\"
  class=\"bar\">
</div>")

(test-html
 "<div id=\"foo\" class=\"bar
  baz\">
</div>
"
 "<div id=\"foo\" class=\"bar
  baz\">
</div>")

(test-html
 "<div>
*foo*
<p><em>bar</em></p>
"
 "<div>
*foo*

*bar*")

(test-html
 "<div id=\"foo\"
*hi*
"
 "<div id=\"foo\"
*hi*")

(test-html
 "<div class
foo
"
 "<div class
foo")

(test-html
 "<div *???-&&&-<---
*foo*
"
 "<div *???-&&&-<---
*foo*")

(test-html
 "<div><a href=\"bar\">*foo*</a></div>
"
 "<div><a href=\"bar\">*foo*</a></div>")

(test-html
 "<table><tr><td>
foo
</td></tr></table>
"
 "<table><tr><td>
foo
</td></tr></table>")

(test-html
 "<div></div>
``` c
int x = 33;
```
"
 "<div></div>
``` c
int x = 33;
```")

(test-html
 "<a href=\"foo\">
*bar*
</a>
"
 "<a href=\"foo\">
*bar*
</a>")

(test-html
 "<Warning>
*bar*
</Warning>
"
 "<Warning>
*bar*
</Warning>")

(test-html
 "<i class=\"foo\">
*bar*
</i>
"
 "<i class=\"foo\">
*bar*
</i>")

(test-html
 "</ins>
*bar*
"
 "</ins>
*bar*")

(test-html
 "<del>
*foo*
</del>
"
 "<del>
*foo*
</del>")

(test-html
 "<del>
<p><em>foo</em></p>
</del>
"
 "<del>

*foo*

</del>")

(test-html
 "<p><del><em>foo</em></del></p>
"
 "<del>*foo*</del>")

(test-html
 "<pre language=\"haskell\"><code>
import Text.HTML.TagSoup

main :: IO ()
main = print $ parseTags tags
</code></pre>
<p>okay</p>
"
 "<pre language=\"haskell\"><code>
import Text.HTML.TagSoup

main :: IO ()
main = print $ parseTags tags
</code></pre>
okay")

(test-html
 "<script type=\"text/javascript\">
// JavaScript example

document.getElementById(\"demo\").innerHTML = \"Hello JavaScript!\";
</script>
<p>okay</p>
"
"<script type=\"text/javascript\">
// JavaScript example

document.getElementById(\"demo\").innerHTML = \"Hello JavaScript!\";
</script>
okay")

(test-html
 "<textarea>

*foo*

_bar_

</textarea>
"
 "<textarea>

*foo*

_bar_

</textarea>")

(test-html
 "<style
  type=\"text/css\">
h1 {color:red;}

p {color:blue;}
</style>
<p>okay</p>
"
  "<style
  type=\"text/css\">
h1 {color:red;}

p {color:blue;}
</style>
okay")

(test-html
 "<style
  type=\"text/css\">

foo
"
 "<style
  type=\"text/css\">

foo")

(test-html
 "<blockquote>
<div>
foo
</blockquote>
<p>bar</p>
"
 "> <div>
> foo

bar")

(test-html
 "<ul>
<li>
<div>
</li>
<li>foo</li>
</ul>
"
 "- <div>
- foo")

(test-html
 "<style>p{color:red;}</style>
<p><em>foo</em></p>
"
 "<style>p{color:red;}</style>
*foo*")

(test-html
 "<!-- foo -->*bar*
<p><em>baz</em></p>
"
 "<!-- foo -->*bar*
*baz*")

(test-html
 "<script>
foo
</script>1. *bar*
"
 "<script>
foo
</script>1. *bar*")

(test-html
 "<!-- Foo

bar
   baz -->
<p>okay</p>
"
 "<!-- Foo

bar
   baz -->
okay")

(test-html
 "<?php

  echo '>';

?>
<p>okay</p>
"
 "<?php

  echo '>';

?>
okay")

(test-html "<!DOCTYPE html>\n" "<!DOCTYPE html>")

(test-html
 "<![CDATA[
function matchwo(a,b)
{
  if (a < b && a < 0) then {
    return 1;

  } else {

    return 0;
  }
}
]]>
<p>okay</p>
"
 "<![CDATA[
function matchwo(a,b)
{
  if (a < b && a < 0) then {
    return 1;

  } else {

    return 0;
  }
}
]]>
okay")

(test-html
 "  <!-- foo -->
<pre><code>&lt;!-- foo --&gt;
</code></pre>
"
 "  <!-- foo -->

    <!-- foo -->")

(test-html
 "  <div>
<pre><code>&lt;div&gt;
</code></pre>
"
 "  <div>

    <div>")

(test-html
 "<p>Foo</p>
<div>
bar
</div>
"
 "Foo
<div>
bar
</div>")

(test-html
 "<div>
bar
</div>
*foo*
"
 "<div>
bar
</div>
*foo*")

(test-html
 "<p>Foo
<a href=\"bar\">
baz</p>
"
"Foo
<a href=\"bar\">
baz")

(test-html
 "<div>
<p><em>Emphasized</em> text.</p>
</div>
"
 "<div>

*Emphasized* text.

</div>")

(test-html
 "<div>
*Emphasized* text.
</div>
"
 "<div>
*Emphasized* text.
</div>")

(test-html
 "<table>
<tr>
<td>
Hi
</td>
</tr>
</table>
"
 "<table>

<tr>

<td>
Hi
</td>

</tr>

</table>")

(test-html
 "<table>
  <tr>
<pre><code>&lt;td&gt;
  Hi
&lt;/td&gt;
</code></pre>
  </tr>
</table>
"
 "<table>

  <tr>

    <td>
      Hi
    </td>

  </tr>

</table>")

;; Test cases from section 6.6 of the spec.

(test-html
 "<p><a><bab><c2c></p>
"
 "<a><bab><c2c>")

(test-html
 "<p><a/><b2/></p>
"
 "<a/><b2/>")

(test-html
 "<p><a  /><b2
data=\"foo\" ></p>
"
 "<a  /><b2
data=\"foo\" >")

(test-html
 "<p><a foo=\"bar\" bam = 'baz <em>\"</em>'
_boolean zoop:33=zoop:33 /></p>
"
 "<a foo=\"bar\" bam = 'baz <em>\"</em>'
_boolean zoop:33=zoop:33 />")

(test-html
 "<p>Foo <responsive-image src=\"foo.jpg\" /></p>
"
 "Foo <responsive-image src=\"foo.jpg\" />")

(test-html
 "<p>&lt;33&gt; &lt;__&gt;</p>
"
 "<33> <__>")

(test-html
 "<p>&lt;a h*#ref=&quot;hi&quot;&gt;</p>
"
 "<a h*#ref=\"hi\">")

(test-html
 "<p>&lt;a href=&quot;hi'&gt; &lt;a href=hi'&gt;</p>
"
 "<a href=\"hi'> <a href=hi'>")

(test-html
 "<p>&lt; a&gt;&lt;
foo&gt;&lt;bar/ &gt;
&lt;foo bar=baz
bim!bop /&gt;</p>
"
 "< a><
foo><bar/ >
<foo bar=baz
bim!bop />")

(test-html
 "<p>&lt;a href='bar'title=title&gt;</p>
"
 "<a href='bar'title=title>")

(test-html
 "<p></a></foo ></p>
"
 "</a></foo >")

(test-html
 "<p>&lt;/a href=&quot;foo&quot;&gt;</p>
"
 "</a href=\"foo\">")

(test-html
 "<p>foo <!-- this is a --
comment - with hyphens --></p>
"
 "foo <!-- this is a --
comment - with hyphens -->")

(test-html
 "<p>foo <!--> foo --&gt;</p>
<p>foo <!---> foo --&gt;</p>
"
 "foo <!--> foo -->

foo <!---> foo -->")

(test-html
 "<p>foo <?php echo $a; ?></p>
"
 "foo <?php echo $a; ?>")

(test-html
 "<p>foo <!ELEMENT br EMPTY></p>
"
 "foo <!ELEMENT br EMPTY>")

(test-html
 "<p>foo <![CDATA[>&<]]></p>
"
 "foo <![CDATA[>&<]]>")

(test-html
 "<p>foo <a href=\"&ouml;\"></p>
"
 "foo <a href=\"&ouml;\">")

(test-html
 "<p>foo <a href=\"\\*\"></p>
"
 "foo <a href=\"\\*\">")

(test-html
 "<p>&lt;a href=&quot;&quot;&quot;&gt;</p>
"
 "<a href=\"\\\"\">")

(test-end)
