<div class="panel panel-primary">
  <div class="panel-heading">
    <h3 class="panel-title"><a href="https://o.mrblog.nl/mrb/all">Social timeline&hellip;</a></h3>
  </div>
  <div class="panel-body">
    <div id="timeline"></div>
    <script src="/assets/js/jquery.min.js" type="text/javascript" ></script>
    <script src="/assets/js/jquery.livetwitter.js"
	    type="text/javascript"></script>
    <script type="text/javascript">
      $$('#timeline').liveTwitter(
        '{{site.social.user}}', {
	  limit: {{site.social.limit}},
	  mode:  '{{site.social.mode}}',
	  showAuthor: true,
	  service: '{{site.social.service}}'
	});
    </script>
  </div>
</div>
