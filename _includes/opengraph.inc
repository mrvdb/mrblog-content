<!-- Opengraph meta data -->
<meta property="og:site_name" content="{{ site.name }}"/>
<meta property="og.title" content="{{ page.title }}"/>
<meta property="og:url" content="{{site.url}}{{page.url}}"/>

<meta property="og:description" content="{% if page.excerpt %}{{ page.excerpt | strip_html }}{% else %}{{ site.description }}{% endif %}">

{% if page.date %}
  <!-- we have something with a date, either page or post -->
  <meta property="og:type" content="article"/>
  <meta property="article:published_time" content="{{page.date | date_to_xmlschema}}"/>
  {% for tag in page.tags %}
  <meta property="article:tag" content="{{tag}}"/>
  {% endfor %}
{% else %}
  <meta property="og:type" content="website"/>
{% endif %}
