require 'git'

module Jekyll
  class GitActivityTag < Liquid::Tag

    def initialize(tag_name, text, tokens)
      super
    end

    def render(context)
      settings = context.registers[:site].config['git']

      dir = settings['dir'] or '.'
      url = settings['baseurl'] or '/'
      result = ""
      g = Git.open(File.join(Dir.getwd, dir))

      index = 0
      g.log.each do |log|
        if(index < (settings['nrofcommits'] or 10))
          result << "<li>"
          result << log.date.strftime((settings['dateformat'] or "%d %b"))
          result << " - <a href='"
          result << url
          result << log.sha
          result << "/'>"
          result << log.message.lines.first
          result << "</a></li>"
          index += 1
        end
      end
      "<ol class=\"git-log\">#{result}</ol>"
    end
  end
end

Liquid::Template.register_tag('gitactivity', Jekyll::GitActivityTag)
