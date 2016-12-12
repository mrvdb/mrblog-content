<!-- Tag list -->
<span class="glyphicon glyphicon-tags the_tags">
  $for(taglist)$
  <a  href="$site.url$/tag/$tag$" rel="tag">
	<span class="label label-default">$tag$</span>
  </a>
  $endfor$
</span>

