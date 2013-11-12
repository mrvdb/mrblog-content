<ul class="pager">
  {% if page.next %}
  <li class="previous">
    <a href="{{ page.next.url }}" >
      <span class="glyphicon glyphicon-arrow-left"></span>
      {{ page.next.title }}
    </a>
  </li>
  {% endif %}
  {% if page.previous %}
  <li class="next">
    <!-- When previous post is syndicated and setting is to have permalink go to the original blog, this points to somewhere outside us!! -->
    <a href="{{ page.previous.url }}" rel="prev">
      {{ page.previous.title }}
    <span class="glyphicon glyphicon-arrow-right"></span></a>
  </li>
  {% endif %}
</ul>
