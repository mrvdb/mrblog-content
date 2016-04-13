<title>
  $if(title)$
    $title$
  $else$
    $site.name$
  $endif$
</title>
<link href="$site.url$$url$" rel="self"/>
<link href="$site.url$/"/>
<updated>$pubdate$</updated> 
<id>$site.url$/</id>
<author>
  <name><![CDATA[ $site.author$ ]]></name>
  $if(site.email)$<email><![CDATA[ $site.email$ ]]></email>$endif$
</author>
