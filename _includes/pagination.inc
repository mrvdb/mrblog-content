<ul class="pager">
  <!-- The class usage of previous/next is reversed to make it the
       same as the per post navigation -->
  <li class="previous {% if paginator.previous_page == nil %}disabled{% endif %}">
    <a href="{{paginator.previous_page_path}}" >
    <span class="glyphicon glyphicon-arrow-left"></span> Newer entries </a>
  </li>

  <li class="next {% if paginator.next_page == nil %}disabled{% endif %}">
    <a href="{{ paginator.next_page_path }}" rel="prev">
      Older entries&nbsp;<span class="glyphicon glyphicon-arrow-right"></span>
    </a>
  </li>
</ul>
