<!-- Tag list -->
<span class="glyphicon glyphicon-tags the_tags">
  {% for tag in tagobject.tags %}
      <a  href="/tag/{{ tag }}" rel="tag">
	<span class="label label-default">{{ tag }}</span>
      </a>
   {% endfor %}
</span>
