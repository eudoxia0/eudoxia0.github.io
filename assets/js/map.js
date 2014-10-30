const map_layer_url = 'http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}';
const map_attribution = 'Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ, TomTom, Intermap, iPC, USGS, FAO, NPS, NRCAN, GeoBase, Kadaster NL, Ordnance Survey, Esri Japan, METI, Esri China (Hong Kong), and the GIS User Community';
const map_coords = [-34.9202699,-56.149931];
const map_zoom = 15;
const map_opts = {
    zoomControl: false
}

function createMap() {
    var map = L.map('map', map_opts).setView(map_coords, map_zoom);
    L.tileLayer(map_layer_url, {
        attribution: map_attribution,
        maxZoom: 18
    }).addTo(map);
    map.dragging.disable();
    map.touchZoom.disable();
    map.doubleClickZoom.disable();
    map.scrollWheelZoom.disable();
    map.boxZoom.disable();
    map.keyboard.disable();
}

$(document).ready(function() {
    createMap();
});