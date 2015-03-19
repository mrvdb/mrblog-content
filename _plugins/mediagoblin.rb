# GNU Mediagoblin tag
#
# A Jekyll plug-in for embedding Mediagoblin media in your Liquid templates.
#
# Usage:
#
#   {% gmg [preset] <media-entry> %}

#
#   ... where <media-entry> is the id of the media to include
#
# Roughly based on: https://github.com/robwierzbowski/jekyll-image-tag/blob/master/image_tag.rb
#
# The only thing needed in _config.yml is:
# gmg:
#   instance: 'http://yourgmg.com'


require 'fileutils'
require 'pathname'
require 'digest/md5'
require 'logger'
require 'net/https'

$log = Logger.new(STDOUT)
$log.level = Logger::DEBUG

module Jekyll

  class MediaGoblinTag < Liquid::Tag

    def initialize(tag_name, markup, tokens)
      @markup = markup
      super
    end

    def render(context)
      # Render any liquid variables in tag arguments and unescape template code
      render_markup = Liquid::Template.parse(@markup).render(context).gsub(/\\\{\\\{|\\\{\\%/, '\{\{' => '{{', '\{\%' => '{%')

      # Gather settings from _config.yml
      site = context.registers[:site]
      settings = site.config['gmg']
      markup = /^(?:(?<preset>[^\s.:\/]+)\s+)?(?:(?<media_id>[0-9]+)\s*)(?<html_attr>[\s\S]+)?\s?$/.match(render_markup)

      # Get the preset, if any
      begin
        chosenpreset='default'
        if markup[:preset]
          # If tag has preset, override
          chosenpreset = markup[:preset]
        end
        
        preset = settings['presets'][chosenpreset]
      rescue
        # Nothing specified in tag, what do we need/want?
        preset = {'attr' => {'class' => "gmg-media"}}
      end

      # Get instance
      # Support for one instance now, add to presets later to support multiple instances
      raise "GMG: gmg instance needs to be specified in configuration file" unless settings and settings['instance']
      gmg_site = settings['instance']

      # Valid ID?
      raise "GMG tag seems invalid. Usage: {% gmg [preset] ID [attr=\"value\"] %}." unless markup[:media_id]
      gmg_media_id = markup[:media_id]

      # Process html attributes in the tag
      html_attr = {}
      if markup[:html_attr]
        html_attr = Hash[ *markup[:html_attr].scan(/(?<attr>[^\s="]+)(?:="(?<value>[^"]+)")?\s?/).flatten ]
      end

      # Both preset attributes and tag attr? -> combine
      if preset && preset['attr']
        html_attr = preset['attr'].merge(html_attr)
      end

      html_attr_string = html_attr.inject('') { |string, attrs|
        if attrs[1]
          string << "#{attrs[0]}=\"#{attrs[1]}\" "
        else
          string << "#{attrs[0]} "
        end
      }

      # http://gmg.com/api/entries?id= is the base url to get the metadata in json format
      uri = URI.join(gmg_site,'api/entries?id=' + markup[:media_id])
      begin
        https = Net::HTTP.new(uri.host,443)
        https.use_ssl = true
        https.verify_mode = OpenSSL::SSL::VERIFY_NONE
        req = Net::HTTP::Get.new(uri.request_uri())
        resp = https.request(req)
        # Note: although we fetch one, it still is an array we get back!
        media_data = JSON.parse(resp.body)[0]
      end
      raise "GMD: Media item at: '" + uri.to_s + "' does not exist." unless media_data

      # Construct our output with the json data
      gmg_src = media_data['media_files']['original']
      # Use description for caption (or title, if empty?)
      gmg_caption = media_data['description']
      
      # Return the markup and, just for fun, the json data we got from GMG, so
      # html people can work with that.
      # TODO: what should we do here? return data only and let templates handle it?
      out  = <<END
      <figure class="#{chosenpreset}">
      <script type="application/json" id="gmg-media-#{gmg_media_id}">#{media_data}</script>
      <img src="#{gmg_src}" #{html_attr_string} />
      <figcaption>#{gmg_caption}</figcaption>
      </figure>
END

      return out
    end
  end
end

Liquid::Template.register_tag('gmg', Jekyll::MediaGoblinTag)
