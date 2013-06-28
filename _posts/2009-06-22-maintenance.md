---
layout: post
title: Maintenance...
tags:
- integration
- web
status: publish
type: post
published: true
meta:
  _edit_last: '5'
  aktt_notify_twitter: 'yes'
  _wp_format_url: ''
  _wp_format_quote: ''
  _wp_format_quote_source: ''
  _wp_format_image: ''
  _wp_format_gallery: ''
  _wp_format_media: ''
  _stcr@_marcel@hsdev.com: '2013-02-25 10:40:52|Y'
  status_net: 'yes'
  _tc_post_format: markdown
  _tc_post_encoding: wptexturize
  bitcointips_address: 13eGffzRWxR8smbehRqHEaR8pt6VYFJk2A
---
Today, I have been <strike>fighting</strike> playing the html code in templates of Wordpress and the CSS of the site. I wanted to display the posts of the <a href="http://cobra.mrblog.nl">Cobra blog</a> a bit more visible than an RSS feed, but not (like it was) importing every entry into this blog, so I came up with a block on the homepage which is fairly visible so it will attract most, but not that disturbing for others.

How a little addition to CSS can make a big difference in how a page looks. CSS3 has a `box-shadow` property, which puts a nice drop-shadow behind an object. Support is still pretty much lacking from all browsers except the latest Firefox and Safari.

As most people wont have these, here's an image (which also has this dropshadow btw):
<div class="wrap" style="text-align:center"><img class="shadow" src="/files/2009/06/screen_008.png" alt="screen_008.png" style="border:thin solid black;width:400px" /></div>

Compare this with the rendering on the homepage in your browser. For guys like me who cant do graphics, CSS3 can still make me look like a pro ;-)

It also turns out that aligning <em>stuff</em> bottom-right is not that easy. (for me at least). Getting the wrapping right took the best part of the day for me. While I'm pretty proficient in XML and it's rules, I keep rowing against the HTML stream I think. Oh well, it's sort of decent now. (in FF and Safari at least. I've given up on IE for hobby projects)
