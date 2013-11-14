<div class="panel panel-primary">
  <div class="panel-heading">
    <h3 class="panel-title">Social timeline&hellip;</h3>
  </div>
  <div class="panel-body">
    <div id="timeline"></div>
    <script src="/js/jquery.min.js" type="text/javascript" ></script>
    <script src="/js/jquery.livetwitter.js"
	    type="text/javascript"></script>
    <script type="text/javascript">
      $('#timeline').liveTwitter('mrb', {limit: 6, mode: 'home_timeline', showAuthor: true, service: 'o.mrblog.nl'});
    </script>
  </div>
</div>
