/*
  {
  "id": "kVkNa241q0X4N",
  "captured_at": "2020-03-02T17:41:13Z",
  "content_title": "",
  "content_type": null,
  "created_at": "2020-03-03T02:11:28Z",
  "file_size": 1301567725,
  "gopro_user_id": "26f2315c-5977-4d9e-953c-e627dc769375",
  "moments_count": 0,
  "on_public_profile": false,
  "play_as": "video",
  "ready_to_edit": true,
  "ready_to_view": "ready",
  "resolution": "2160p",
  "source_duration": "103737",
  "type": "TimeLapseVideo",
  "token": ""
  }
*/

function process(media) {
    // Various formatters.
    const formatNumber = d3.format(",d"),
          formatChange = d3.format("+,d"),
          formatDate = d3.timeFormat("%B %d, %Y"),
          formatTime = d3.timeFormat("%H:%M");

    const nestByDate = d3.nest()
          .key(function(d) {return d3.timeDay(d.created_at)});

    // A little coercion, since the CSV is untyped.
    media.forEach((d,i) => {
        d.index = i;
        d.created_at = new Date(d.created_at);
        d.captured_at = new Date(d.captured_at);
        d.camera_model = d.camera_model || 'Unknown';
    });

    // Create the crossfilter for the relevant dimensions and groups.
    const medium = crossfilter(media),
          all = medium.groupAll(),
          date = medium.dimension(d => d.captured_at),
          dates = date.group(d3.timeDay),
          mtypes = medium.dimension(d => d.type),
          mtype = mtypes.group();

    const fields = [
        {field: 'type'},
        {field: 'camera_model'}
    ];

    fields.forEach(function(dd,i) {
        dd.dim = medium.dimension(d => d[dd.field]);
        dd.group = dd.dim.group();
        dd.sel = d3.select('#' + dd.field);
        dd.sel.on("change", function() {
            dd.dim.filter(dd.sel.node().value || null);
            renderAll();
        });
    });

    const capBounds = d3.extent(media, d => d.captured_at);
    capBounds[0] = new Date(capBounds[0]).setDate(capBounds[0].getDate() - 1);
    capBounds[1] = new Date(capBounds[1]).setDate(capBounds[1].getDate() + 1);

    const charts = [
        barChart()
            .dimension(date)
            .group(dates)
            .round(d3.timeDay.round)
            .x(d3.scaleTime()
               .domain(capBounds)
               .rangeRound([0, 10 * 90]))
    ];

    // Given our array of charts, which we assume are in the same order as the
    // .chart elements in the DOM, bind the charts to the DOM and render them.
    // We also listen to the chart's brush events to update the display.
    const chart = d3.selectAll(".chart")
          .data(charts)

    // Render the initial lists.
    const list = d3.selectAll(".list")
          .data([mediaList]);

    // Render the total.
    d3.selectAll("#total")
        .text(formatNumber(media.length));

    renderAll();

    // Renders the specified chart or list.
    function render(method) {
        d3.select(this).call(method);
    }

    // Whenever the brush moves, re-rendering everything.
    function renderAll() {
        chart.each(render);
        
        fields.forEach(function(d, i) {
            const dd = d.sel.selectAll('.' + d.field)
                  .data(d.group.reduceCount().all().filter(d => d.value > 0).sort(),
                        d => d.key);
            dd.enter().append('option')
                .attr("class", d.field).attr("value", d => d.key);
            d.sel.selectAll('.' + d.field).text(d => d.key + " (" + d.value + ")");
            dd.exit().remove();
        });
        list.each(render);
        d3.select("#active").text(formatNumber(all.value()));
    }

    window.filter = function(filters) {
        filters.forEach((d,i) => charts[i].filter(d));
        renderAll();
    };

    window.reset = function(i) {
        charts[i].filter(null);
        renderAll();
    };

    function mediaList(div) {
        var mediaByDate = nestByDate.entries(date.top(40));

        div.each(function() {
            var date = d3.select(this).selectAll(".date")
                .data(mediaByDate, d => d.key);

            date.enter().append("div")
                .attr("class", "date")
                .append("div")
                .attr("class", "day")
                .text(d => formatDate(d.values[0].captured_at));

            date.exit().remove();

            var m = date.order().selectAll(".medium")
                .data(d => d.values, d => d.index);

            var mEnter = m.enter().append("div")
                .attr("class", "medium");

            mEnter.append("div")
                .attr("class", "time")
                .text(d => formatTime(d.captured_at));
            mEnter.append("img")
                .attr("class", "thumb")
                .attr("src", d => "/thumb/" + d.id);

            m.exit().remove();

            m.order();
        });
    }

    function barChart() {
        if (!barChart.id) barChart.id = 0;

        let margin = {top: 10, right: 10, bottom: 20, left: 10},
            x,
            y = d3.scaleLinear().range([100, 0]),
            id = barChart.id++,
            axis = d3.axisBottom(),
            brush = d3.brushX(),
            brushDirty,
            dimension,
            group,
            round,
            gBrush;

        function chart(div) {
            const width = x.range()[1],
                  height = y.range()[0];

            brush.extent([[0, 0], [width, height]])

            y.domain([0, group.top(1)[0].value]);

            div.each(function() {
                let div = d3.select(this),
                    g = div.select("g");

                // Create the skeletal chart.
                if (g.empty()) {
                    div.select(".title").append("a")
                        .attr("href", "javascript:reset(" + id + ")")
                        .attr("class", "reset")
                        .text("reset")
                        .style("display", "none");

                    g = div.append("svg")
                        .attr("width", width + margin.left + margin.right)
                        .attr("height", height + margin.top + margin.bottom)
                        .append("g")
                        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

                    g.append("clipPath")
                        .attr("id", "clip-" + id)
                        .append("rect")
                        .attr("width", width)
                        .attr("height", height);

                    g.selectAll(".bar")
                        .data(["background", "foreground"])
                        .enter().append("path")
                        .attr("class", d => d + " bar")
                        .datum(group.all());

                    g.selectAll(".foreground.bar")
                        .attr("clip-path", "url(#clip-" + id + ")");

                    g.append("g")
                        .attr("class", "axis")
                        .attr("transform", "translate(0," + height + ")")
                        .call(axis);

                    // Initialize the brush component with pretty resize handles.
                    gBrush = g.append("g")
                        .attr("class", "brush")
                        .call(brush);

                    gBrush.selectAll(".handle--custom")
                        .data([{type: "w"}, {type: "e"}])
                        .enter().append("path")
                        .attr("class", "brush-handle")
                        .attr("cursor", "ew-resize")
                        .attr("d", resizePath)
                        .style("display", "none")
                }

                // Only redraw the brush if set externally.
                if (brushDirty != false) {
                    var filterVal = brushDirty;
                    brushDirty = false;

                    div.select(".title a").style("display", d3.brushSelection(div) ? null : "none");

                    if (!filterVal) {
                        g.call(brush)

                        g.selectAll("#clip-" + id + " rect")
                            .attr("x", 0)
                            .attr("width", width);

                        g.selectAll(".brush-handle").style("display", "none")
                        renderAll();

                    } else {
                        var range = filterVal.map(x)
                        brush.move(gBrush, range)
                    }
                }

                g.selectAll(".bar").attr("d", barPath);
            });

            function barPath(groups) {
                var path = [],
                    i = -1,
                    n = groups.length,
                    d;
                while (++i < n) {
                    d = groups[i];
                    path.push("M", x(d.key), ",", height, "V", y(d.value), "h9V", height);
                }
                return path.join("");
            }

            function resizePath(d) {
                var e = +(d.type == "e"),
                    x = e ? 1 : -1,
                    y = height / 3;
                return "M" + (.5 * x) + "," + y
                    + "A6,6 0 0 " + e + " " + (6.5 * x) + "," + (y + 6)
                    + "V" + (2 * y - 6)
                    + "A6,6 0 0 " + e + " " + (.5 * x) + "," + (2 * y)
                    + "Z"
                    + "M" + (2.5 * x) + "," + (y + 8)
                    + "V" + (2 * y - 8)
                    + "M" + (4.5 * x) + "," + (y + 8)
                    + "V" + (2 * y - 8);
            }
        }

        brush.on("start.chart", function() {
            var div = d3.select(this.parentNode.parentNode.parentNode);
            div.select(".title a").style("display", null);
        });

        brush.on("brush.chart", function() {
            var g = d3.select(this.parentNode);
            var brushRange = d3.event.selection || d3.brushSelection(this); // attempt to read brush range
            var xRange = x && x.range(); // attempt to read range from x scale
            var activeRange = brushRange || xRange; // default to x range if no brush range available

            var hasRange = activeRange &&
                activeRange.length === 2 &&
                !isNaN(activeRange[0]) &&
                !isNaN(activeRange[1]);

            if (!hasRange) return; // quit early if we don't have a valid range

            // calculate current brush extents using x scale
            var extents = activeRange.map(x.invert);

            // if rounding fn supplied, then snap to rounded extents
            // and move brush rect to reflect rounded range bounds if it was set by user interaction
            if (round) {
                extents = extents.map(round);
                activeRange = extents.map(x);

                if (d3.event.sourceEvent &&
                    d3.event.sourceEvent.type === "mousemove") {
                    d3.select(this).call(brush.move, activeRange)
                }
            }

            // move brush handles to start and end of range
            g.selectAll(".brush-handle")
                .style("display", null)
                .attr("transform", function(d, i) {
                    return "translate(" + activeRange[i] + ", 0)"
                });

            // resize sliding window to reflect updated range
            g.select("#clip-" + id + " rect")
                .attr("x", activeRange[0])
                .attr("width", activeRange[1] - activeRange[0]);

            // filter the active dimension to the range extents
            dimension.filterRange(extents);

            // re-render the other charts accordingly
            renderAll();
        });

        brush.on("end.chart", function() {
            // reset corresponding filter if the brush selection was cleared
            // (e.g. user "clicked off" the active range)
            if (!d3.brushSelection(this)) {
                reset(id);
            }
        });

        chart.margin = function(_) {
            if (!arguments.length) return margin;
            margin = _;
            return chart;
        };

        chart.x = function(_) {
            if (!arguments.length) return x;
            x = _;
            axis.scale(x);
            return chart;
        };

        chart.y = function(_) {
            if (!arguments.length) return y;
            y = _;
            return chart;
        };

        chart.dimension = function(_) {
            if (!arguments.length) return dimension;
            dimension = _;
            return chart;
        };

        chart.filter = function(_) {
            if (!_) dimension.filterAll();
            brushDirty = _;
            return chart;
        };

        chart.group = function(_) {
            if (!arguments.length) return group;
            group = _;
            return chart;
        };

        chart.round = function(_) {
            if (!arguments.length) return round;
            round = _;
            return chart;
        };

        chart.gBrush = function() {
            return gBrush
        }

        return chart;
    }
}

async function initGoPro() {
    try {
        const data = await d3.json("/api/media");
        d3.select('#message').style('visibility', 'hidden');
        process(data);
    } catch (err) {
        console.log(err);
        d3.select('#message').text('Error loading data: ' + err);
    }
}
