// The d3 chart used for plotting real-time incoming data.
// Assumes global mtr.options variable
function monitor_chart(selection) {
  // based on https://bl.ocks.org/pjsier/fbf9317b31f070fd540c5523fef167ac
  var margin   = mtr.options.margin,
      height   = mtr.options.height,
      width    = mtr.options.width,
      duration = mtr.options.duration,
      color    = mtr.options.color;

  selection.each(function(data) {
    // Data stuff
    data = [{
      label  : "y",
      values : data.map((d, i) => { return({time: +d.time, value: d.y, number: i}) })
    }]

    // dynamic width
    if (mtr.options.sizing === "dynamic") {
      width = d3.select(this.parentNode).node().getBoundingClientRect().width;
    }

    // axis stuff
    // x axis
    if (mtr.options.xaxis === "time") {
      var x = d3.scaleTime().rangeRound([0, width-margin.left-margin.right])
      var xMin = d3.min(data, function(c) { return d3.min(c.values, function(d) { return d.time; })});
      var xMax = new Date(new Date(d3.max(data, function(c) {
        return d3.max(c.values, function(d) { return d.time; })
      })).getTime() - (duration*2));
    } else {
      var x = d3.scaleLinear().rangeRound([0, width-margin.left-margin.right])
      var xMin = 0;
      var xMax = mtr.options.maxlength;
    }
    
    // range
    var y = d3.scaleLinear().rangeRound([height-margin.top-margin.bottom, 0]);

    
    // domain
    x.domain([xMin, xMax]);
    y.domain([
      d3.min(data, function(c) { return d3.min(c.values, function(d) { return d.value; })}),
      d3.max(data, function(c) { return d3.max(c.values, function(d) { return d.value; })})
    ]);

    //transition
    var t = d3.transition().duration(duration).ease(d3.easeLinear);

    // line / curve
    if (mtr.options.xaxis === "time") {
      var line = d3.line()
        .curve(d3.curveBasis)
        .x(function(d) { return x(d.time); })
        .y(function(d) { return y(d.value); });
    } else {
      var line = d3.line()
        .curve(d3.curveBasis)
        .x(function(d) { return x(d.number); })
        .y(function(d) { return y(d.value); });
    }
    
    // create svg
    var svg = d3.select(this).selectAll("svg").data([data]);
    
    var gEnter = svg.enter().append("svg").append("g");
    gEnter.append("g").attr("class", "axis x");
    gEnter.append("g").attr("class", "axis y");
    gEnter.append("defs").append("clipPath")
      .attr("id", "clip")
      .append("rect")
      .attr("width", width-margin.left-margin.right)
      .attr("height", height-margin.top-margin.bottom);
    gEnter.append("g")
      .attr("class", "lines")
      .attr("clip-path", "url(#clip)")
      .selectAll(".data").data(data).enter()
      .append("path")
      .attr("class", "data");
    var svg = selection.select("svg");
    svg.attr('width', width).attr('height', height);
    var g = svg.select("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    if (mtr.options.xaxis === "time") {
      g.select("g.axis.x")
        .attr("transform", "translate(0," + (height-margin.bottom-margin.top) + ")")
        .transition(t)
        .call(d3.axisBottom(x).ticks(5).tickFormat(d3.timeFormat("%H:%M:%S")));
    } else {
      g.select("g.axis.x")
        .attr("transform", "translate(0," + (height-margin.bottom-margin.top) + ")")
        .transition(t)
        .call(d3.axisBottom(x).ticks(5));
    }
    g.select("g.axis.y")
      .transition(t)
      .attr("class", "axis y")
      .call(d3.axisLeft(y));

    g.select("defs clipPath rect")
      .transition(t)
      .attr("width", width-margin.left-margin.right)
      .attr("height", height-margin.top-margin.right);

    
    g.selectAll("g path.data")
      .data(data)
      .style("stroke", "#00008b")
      .style("stroke-width", 1)
      .style("fill", "none")
      .transition()
      .duration(duration)
      .ease(d3.easeLinear)
      .on("start", tick);  
        
    // For transitions https://bl.ocks.org/mbostock/1642874
    function tick() {
        d3.select(this)
        .attr("d", function(d) { return line(d.values); })
        .attr("transform", null);    
      }
  });
}