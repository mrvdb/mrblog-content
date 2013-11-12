<!-- Navigation bar -->
<nav class="navbar navbar-inverse navbar-fixed-top"  role="navigation">
  <!-- Brand and toggle get grouped for better mobile display -->
  <div class="navbar-header">
    <button type="button" class="navbar-toggle" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1">
      <span class="sr-only">Toggle navigation</span>
      <span class="icon-bar"></span>
      <span class="icon-bar"></span>
      <span class="icon-bar"></span>
    </button>


    <a class="navbar-brand" href="/">
      <span class="glyphicon glyphicon-home"></span>
    </a>
    <a class="navbar-brand" href="/">{{ site.name }}</a>

  </div>

  <!-- Collect the nav links, forms, and other content for toggling -->
  <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
    <!-- Link to feed(s) -->
    <ul class="nav navbar-nav navbar-right">
      <li>
	<a href="/feed/atom.xml" title="Atom feed of postings" class="glyphicon icon rss-icon"></a>
      </li>
    </ul>

    <!-- Search form -->
    <form class="navbar-form navbar-right" role="search" action="/search">
      <div class="form-group">
        <input type="search" class="form-control input-sm" name="query" placeholder="Search"/>
      </div>
      <button type="submit" class="btn-sm btn-primary">Search</button>
    </form>

    <!-- Other menu items -->
    <!-- <ul class="nav navbar-nav navbar-right"> -->
    <!--     <li><a href="#">Link</a></li> -->
    <!--     <li class="dropdown"> -->
    <!-- 	<a href="#" class="dropdown-toggle" data-toggle="dropdown">Dropdown <b class="caret"></b></a> -->
    <!-- 	<ul class="dropdown-menu"> -->
    <!-- 	  <li><a href="#">Action</a></li> -->
    <!-- 	  <li><a href="#">Another action</a></li> -->
    <!-- 	  <li><a href="#">Something else here</a></li> -->
    <!-- 	  <li class="divider"></li> -->
    <!-- 	  <li><a href="#">Separated link</a></li> -->
    <!-- 	</ul> -->
    <!--     </li> -->
    <!--   </ul> -->
  </div><!-- /.navbar-collapse -->
</nav>