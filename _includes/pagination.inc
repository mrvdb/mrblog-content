<ul class="pager">

  $if(previousPageNum)$
  <li class="previous">
    <a href="$previousPageUrl$" >
    <span class="glyphicon glyphicon-arrow-left"></span> Newer entries </a>
  </li>
  $endif$

  $if(nextPageNum)$
  <li class="next">
    <a href="$nextPageUrl$" rel="prev">
      Older entries&nbsp;<span class="glyphicon glyphicon-arrow-right"></span>
    </a>
  </li>
  $endif$
</ul>
