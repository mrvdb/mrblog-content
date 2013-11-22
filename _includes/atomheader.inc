<title>{{ site.name }}</title>
<link href="{{site.url}}{{page.url}}" rel="self"/>
<link href="{{site.url}}/"/>
<updated>{{ site.time | date_to_xmlschema }}</updated>
<id>{{site.url}}/</id>
<author>
  <name><![CDATA[{{ site.author }}]]></name>
  {% if site.email %}<email><![CDATA[{{ site.email }}]]></email>{% endif %}
</author>
