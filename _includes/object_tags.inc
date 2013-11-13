<!-- Tag list -->
<span class="glyphicon glyphicon-tags the_tags">
  {% for tag in {{include.object.tags}} %}
      <a  href="/tag/{{ tag }}" rel="tag">
	<span class="label label-default">{{ tag }}</span>
      </a>
   {% endfor %}
</span>
