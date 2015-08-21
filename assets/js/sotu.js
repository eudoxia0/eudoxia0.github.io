function renderChart() {
  d3.json('/assets/data/sotu-2015/github.json', function(data) {
    /* Data is an array of [ISO 8601 string, count] pairs */
    nv.addGraph(function() {
      var chart = nv.models.lineChart()
                    .x(function(d) { return new Date(d[0]); })
                    .y(function(d) { return d[1]; })
                    .height(300)
                    .forceY([0,20])
                    .interpolate('linear');

      chart.xAxis.showMaxMin(false).tickFormat(function(date) {
        return d3.time.format('%Y-%m-%d')(new Date(date));
      });

      chart.yAxis.tickFormat(d3.format('d'));

      var input = [{
        key: 'New Repos',
        values: data
      }];

      d3.select('#projects-per-day svg').datum(input).transition().duration(500).call(chart);

      var min_date = new Date(data[data.length-1][0]);
      var max_date = new Date(data[0][0]);
      var total = data.reduce(function(a, b) {
        return a + b[1];
      }, 0);

      var date_format = '%B %e, %Y';

      d3.select('#earliest').text(d3.time.format(date_format)(min_date));
      d3.select('#latest').text(d3.time.format(date_format)(max_date));
      d3.select('#number').text(total.toString());

      nv.utils.windowResize(chart.update);

      return chart;
    });
  });
};

function scrollFooter(scrollY, heightFooter) {
  if(scrollY >= heightFooter)
  {
    $('footer').css({
      'bottom' : '0px'
    });
  } else {
    $('footer').css({
      'bottom' : '-' + heightFooter + 'px'
    });
  }
}

function parallaxScrolling() {
  var windowHeight = $(window).height();
  var footerHeight = $('footer').height();
  var heightDocument = (windowHeight) + ($('main').height()) + ($('footer').height()) - 20;

  $('#scroll-animate, #scroll-animate-main').css({
    'height' :  heightDocument + 'px'
  });

  $('header').css({
    'height' : windowHeight + 'px',
    'line-height' : windowHeight + 'px'
  });

  $('.wrapper-parallax').css({
    'margin-top' : windowHeight + 'px'
  });

  scrollFooter(window.scrollY, footerHeight);

  window.onscroll = function(){
    var scroll = window.scrollY;

    $('#scroll-animate-main').css({
      'top' : '-' + scroll + 'px'
    });

    $('header').css({
      'background-position-y' : 50 - (scroll * 100 / heightDocument) + '%'
    });

    scrollFooter(scroll, footerHeight);
  }
};

$(document).ready(function() {
  renderChart();
  parallaxScrolling();
  $('#toc').toc({
    selectors: 'h1, h2',
    container: 'article',
    highlightOnScroll: true
  });
  $("#toc").stick_in_parent();
});
