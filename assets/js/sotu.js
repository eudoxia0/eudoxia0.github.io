function renderQuicklispChart() {
  d3.json('/assets/data/sotu-2015/quicklisp-downloads.json', function(data) {
    nv.addGraph(function() {
      var chart = nv.models.discreteBarChart()
                    .x(function(d) { return d[0]; })
                    .y(function(d) { return d[1]; })
                    .forceY([0,250000])
                    .height(300);

      chart.xAxis.tickFormat(function(month) { return month; });
      chart.yAxis.tickFormat(d3.format(',d'));

      var input = [{
        key: 'Quicklisp Downloads',
        values: data
      }];

      d3.select('#quicklisp-downloads svg').datum(input).transition().duration(500).call(chart);

      nv.utils.windowResize(chart.update);

      return chart;
    });
  });
};

function renderGitHubChart() {
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

$(document).ready(function() {
  renderQuicklispChart();
  renderGitHubChart();
  $('#toc').toc({
    selectors: 'h1, h2',
    container: 'article',
    highlightOnScroll: true
  });
  $("#toc").stick_in_parent();
});
