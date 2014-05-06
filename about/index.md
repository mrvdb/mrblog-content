---
layout: page
title: About this site
---

The site *MrBlog.nl* is my personal entry point to what I do on the
web. It is mostly for myself to experiment with things while
pretending to be a blog.

To build the site I have borrowed a number of tools and resources from
others. I try to keep a complete list of them complete with links back
to them:

* blog system is [jekyll][];
* theme is a built with [bootstrap][] components;
* all files are edited with the [Emacs][] editor;
* Comment system: [Disqus][]; ( TODO: move to something static or at least something open )
* I use [git][] to publish the blog to my server and to a repository on [github][].
* Content is mostly written in [orgmode][] or [markdown][].
* Some of the images are hosted on [flickr][]; ( TODO: move to http://media.mrblog.nl)
* the [Entypo][] icons are used;
* Orgmode files are converted using [org-ruby][] with the help of a
  little [org-converter][]
* [ruby-oembed][] is use for automatic embedding of media with the
  help of an [oembed liquid tag][]
* A locally generated json file is used to implement search
  functionality. A modified version of
  [jekyll-search][] is used.
* Bootstrap is dependent on the [jquery][] library, but I use it
  directly too for the live statusnet feed and the search function.
* the site is served with [nginx][] on machine running [Debian][]
GNU/Linux
* for the git log listing (see below), I adapted the [gitactivity][]
  tag plugin by Alexandre Girard

If you feel that credit for one of your programs should be in this list, let me know.

[orgmode]:				http://orgmode.org "Your life in plain text"
[markdown]:				http://daringfireball.net/projects/markdown/ "Markdown"
[jekyll]:				http://jekyllrb.com "Simple, blog-aware, static sites"
[class72]:				http://alanwho.com/wordpress/72class/ "A theme for wordpress"
[disqus]:				http://disqus.com "Elevating the discussion"
[inove]:				http://www.neoease.com/inove/ "iNove wordpress theme"
[octopress]:			http://octopress.org "A blogging framework for hackers"
[git]:					http://git-scm.com
[flickr]:				http://flickr.com
[entypo]:				http://entypo.com
[bootstrap]:			http://getbootstrap.com
[org-ruby]:				https://github.com/bdewey/org-ruby
[org-converter]:		https://gist.github.com/abhiyerra/7377603
[ruby-oembed]:			https://github.com/judofyr/ruby-oembed/
[oembed liquid tag]:    https://gist.github.com/vanto/1455726
[nginx]:                http://nginx.org
[Debian]:               http://debian.org
[tapir]:                http://tapirgo.com
[jquery]:               http://jquery.com/
[Emacs]:                http://www.gnu.org/software/emacs/
[jekyll-search]:        https://github.com/mathaywarduk/jekyll-search
[gitactivity]:          https://gist.github.com/alx/730347
[github]:               https://github.com/mrvdb/mrblog
### Some stats

- Total number of pages: {{ site.pages | size }}
- Total number of posts: {{ site.posts | size }}
- Total number of tags: {{ site.tags | size }}
- Last generated on: {{ site.time }}
- The canonical site url is {{ site.url }}

### Last {{ site.git.nrofcommits }} changes to the site

{% gitactivity %}
