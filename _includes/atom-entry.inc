<entry>
  <title type="html"><![CDATA[{{ post.title }}]]></title>
  <link href="{{site.url}}{{ post.url }}"/>
  <updated>{{ post.date | date_to_xmlschema }}</updated>
  <id>{{site.url}}{{ post.id }}</id>
  <content type="html">
    <![CDATA[{{ post.content }}]]>
  </content>
  <author>{{site.author}}</author>
  <summary type="html">
    <![CDATA[{{post.excerpt}}]]>
  </summary>
  {% for tag in post.tags %}
    <category term="{{ tag }}"/>
  {% endfor %}
</entry>
