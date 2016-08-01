<ul class="pager">
  <!-- {% if page.next %} -->
  <li class="previous">
    <a href="$if(nextPost)$$nextPost$$endif$" >
      <span class="glyphicon glyphicon-arrow-left"></span>
      Newer
    </a>
  </li>
  <!-- {% endif %} -->
  <!-- {% if page.previous %} -->
  <li class="next">
    <!-- When previous post is syndicated and setting is to have permalink go to the original blog, this points to somewhere outside us!! -->
    <a href="$if(prevPost)$$prevPost$$endif$" rel="prev">
      Older
    <span class="glyphicon glyphicon-arrow-right"></span></a>
  </li>
  <!-- {% endif %} -->
</ul>
