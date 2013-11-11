<ul class="pager">
  <li class="next {% if paginator.previous_page == nil %}disabled{% endif %}">
    <a href="{{paginator.previous_page_path}}" >
    Newer entries <span class="glyphicon glyphicon-arrow-right"></span></a>
  </li>

  <li class="previous {% if paginator.next_page == nil %}disabled{% endif %}">
    <a href="{{ paginator.next_page_path }}" rel="prev">
      <span class="glyphicon glyphicon-arrow-left"></span> Older entries
    </a>
  </li>
</ul>
