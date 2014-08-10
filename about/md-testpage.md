---
layout: page
title: Layout testpage
---

This page contains markdown constructs as a test to see how they get
rendered to html. It's the visual last test to see if I have gotten
the css stuff right. The page will be updated each time a css change
is made so it can be checked without having to hunt for content which
uses a special construct.

Header
======

And this is the paragraph that follows it, which should obviously have
a little bit of text to see the lines wrap to the next line. This
paragraph will be repeated everywhere we want to have a block of
text. We could write actual content but we haven't got anything to say
really.


# Header 1

And this is the paragraph that follows it, which should obviously have
a little bit of text to see the lines wrap to the next line. This
paragraph will be repeated everywhere we want to have a block of
text. We could write actual content but we haven't got anything to say
really.

## Header 2

And this is the paragraph that follows it, which should obviously have
a little bit of text to see the lines wrap to the next line. This
paragraph will be repeated everywhere we want to have a block of
text. We could write actual content but we haven't got anything to say
really.

### Header 3

And this is the paragraph that follows it, which should obviously have
a little bit of text to see the lines wrap to the next line. This
paragraph will be repeated everywhere we want to have a block of
text. We could write actual content but we haven't got anything to say
really.

#### Header 4

And this is the paragraph that follows it, which should obviously have
a little bit of text to see the lines wrap to the next line. This
paragraph will be repeated everywhere we want to have a block of
text. We could write actual content but we haven't got anything to say
really.

##### Header 5

And this is the paragraph that follows it, which should obviously have
a little bit of text to see the lines wrap to the next line. This
paragraph will be repeated everywhere we want to have a block of
text. We could write actual content but we haven't got anything to say
really.

###### Header 6

And this is the paragraph that follows it, which should obviously have
a little bit of text to see the lines wrap to the next line. This
paragraph will be repeated everywhere we want to have a block of
text. We could write actual content but we haven't got anything to say
really.


### Block quotes

> This is a blockquote with two paragraphs. Lorem ipsum dolor sit amet,
> consectetuer adipiscing elit. Aliquam hendrerit mi posuere lectus.
> Vestibulum enim wisi, viverra nec, fringilla in, laoreet vitae, risus.
>
> Donec sit amet nisl. Aliquam semper ipsum sit amet velit. Suspendisse
> id sem consectetuer libero luctus adipiscing.

### Pull quotes

{% pullquote %}
Surround your paragraph with the pull quote tags. {" Then when you come to
the text you want to pull,  surround it like this "} and that's all there is to it.
{% endpullquote %}

### Lists
The lists belows have a '*', '+', '-' and 'numbers' respectively:

* Red
* Green
* Blue

+ Red
+ Green
+ Blue

- Red
- Green
- Blue

1. Red
2. Green
3. Blue

### Code blocks
Code blocks are just normal paragraphs, but indented with at least 4
spaces

    And this is the paragraph that follows it, which should obviously have
    a little bit of text to see the lines wrap to the next line. This
    paragraph will be repeated everywhere we want to have a block of
    text. We could write actual content but we haven't got anything to
    say really.

### Higlighting code
Higlighting of code is supported, this requires python pygments and a
syntax highlighting stylesheet. (/css/syntax.css is ours)

{% highlight ruby %}
def foo
  puts 'foo'
end
{% endhighlight %}

Line numbers can also be used with this, although this is implemented
somewhat simplistically, because selection of the code will not leave
out the linenumbers.

{% highlight ruby linenos %}
def foo
  puts 'foo'
end
{% endhighlight %}

Other code related embedding is for githubs gist facility

{% gist 5555251 result.md %}

### Rules
No less than 5 different syntaxes for producing horizontal rules,
which all produce the same output.

* * *
***
*****
- - -
----------------------------------------------------

## Links

[This link](http://example.net/) has no title attribute.

This is [an example](http://example.com/ "Title") inline link.

See my [About](/about/) page for details.

This is [an example][id] reference-style link.

[id]: http://example.com/  "Optional Title Here"

[Google][] - quicklick used by defining it with key between square brackets.

[Google]: http://google.com

<http://example.com/>

<address@example.com>

### Emphasis

*single asterisks*

_single underscores_

**double asterisks**

__double underscores__

Use the `printf()` function. (backticks used)

``There is a literal backtick (`) here. Use two backticks for the
enclosing text.``

### Images

![Alt text](/css/images/calendar.gif)

![Alt text](/css/images/calendar.gif "Optional title")
