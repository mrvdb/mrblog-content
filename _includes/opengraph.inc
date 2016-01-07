<!-- Opengraph meta data -->
<meta property="og:site_name" content="$site.name$"/>
<meta property="og.title" content="$title$"/>
<meta property="og:url" content="$site.url$/$url$"/>
<meta property="og:image" content="https://en.gravatar.com/userimage/5609963/02a82098dc7a91d3a540075c64a4ddca.jpeg"/>

<meta property="og:description" content="{% if page.excerpt %}{{ page.excerpt | strip_html }}{% else %}{{ site.description }}{% endif %}">


$if(page.date)$
  <!-- we have something with a date, either page or post -->
  <meta property="og:type" content="article"/>
  <meta property="article:published_time" content="{{page.date | date_to_xmlschema}}"/>
  <!-- {% for tag in page.tags %} -->
  <meta property="article:tag" content="{{tag}}"/>
  <!-- {% endfor %} -->
$else$
  <meta property="og:type" content="website"/>
$endif$
