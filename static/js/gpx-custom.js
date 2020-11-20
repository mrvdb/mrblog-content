// Definition of the icons we want to use
var iB             = '/images/gpx/';

// While tileserver should we use?
// var tilesUrl = 'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png';
// This is nicer, but has a limit of tiles/month (which is probably more than enough for me)
var tilesUrl = 'https://{s}.tile.thunderforest.com/outdoors/{z}/{x}/{y}.png?apikey=b1987b4b4ef449a19332204f8b2dbdff';

var customIconUrls = {
    'Start Of Track'	: iB + 'pin-icon-start.png',
    'End Of Track'	: iB + 'pin-icon-end.png',
    'Shadow'		: iB + 'pin-shadow.png',
    'Campground'	: iB + 'campground.png',
    'Hotel'		: iB + 'hotel.png',
    'Restaurant'	: iB + 'restaurant.png',
    'Bed and Breakfast'	: iB + 'bed_breakfast.png',
    'Parking Area'	: iB + 'parking.png',
    'Bus Stop'		: iB + 'busstop.png',
    'Sign Post'		: iB + 'signpost.png'
};

function display_gpx(elt) {
    if (!elt) return;

    // Get source URI of gpx file
    var url = elt.getAttribute('gpx-src');
    if (!url) return;

    /* L is the Leaf variable, how can we make this known here? */
    var map = L.map(elt);
    L.tileLayer(tilesUrl).addTo(map);

    // Construct the GPX object 
    new L.GPX(url, {
        async: true,
        marker_options : {
            startIconUrl: customIconUrls['Start Of Track'],
            endIconUrl  : customIconUrls['End Of Track'],
            shadowUrl   : customIconUrls['Shadow'],
            wptIconUrls : customIconUrls
        }
    }).on('loaded', function(e) {
        map.fitBounds(e.target.getBounds());
    }).addTo(map);
}

// Find map classed elements and render them if ID starts with gpx-
function render_maps() {
    var ms = document.getElementsByClassName('gpx-map');
    for (var ix=0; ix<ms.length; ix++) {
            display_gpx(ms[ix]);
    }
}

// Add the render_maps to the conclusion of loading
window.onload = render_maps;
    
